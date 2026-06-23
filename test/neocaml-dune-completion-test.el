;;; neocaml-dune-completion-test.el --- Tests for dune completion-at-point -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-dune-completion-at-point': stanza-name,
;; field-name, and library-name completion contexts.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-dune)

;; Declared special so the `let' bindings below bind it dynamically (the
;; real defcustom lives in neocaml-dune-interaction.el).
(defvar neocaml-dune-use-opam-exec)

(defun neocaml-dune-test--capf (content)
  "Run the dune capf on CONTENT, where `|' marks point.
Return the raw capf result, or nil."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (let ((pos (point)))
      (neocaml-dune-mode)
      (goto-char pos))
    (neocaml-dune-completion-at-point)))

(defun neocaml-dune-test--candidates (content)
  "Return the candidate list the dune capf offers for CONTENT.
Resolves both plain lists and dynamic completion tables."
  (let ((result (neocaml-dune-test--capf content)))
    (when result
      (all-completions "" (nth 2 result)))))

(describe "neocaml-dune completion-at-point"
  (before-all
    (unless (treesit-language-available-p 'dune)
      (signal 'buttercup-pending "tree-sitter dune grammar not available")))

  (describe "stanza-name completion"
    (it "offers stanza names at the head of a top-level form"
      (let ((cands (neocaml-dune-test--candidates "(lib|")))
        (expect (member "library" cands) :to-be-truthy)
        (expect (member "executable" cands) :to-be-truthy)
        (expect (member "rule" cands) :to-be-truthy)))

    (it "offers stanza names right after the opening paren"
      (expect (member "test" (neocaml-dune-test--candidates "(|"))
              :to-be-truthy))

    (it "spans the partial stanza name being typed"
      (let ((result (neocaml-dune-test--capf "(lib|")))
        ;; "lib" occupies buffer positions 2..4 in "(lib", so the
        ;; completion span is [2, 5).
        (expect (nth 0 result) :to-equal 2)
        (expect (nth 1 result) :to-equal 5))))

  (describe "field-name completion"
    (it "offers fields for the enclosing library stanza"
      (let ((cands (neocaml-dune-test--candidates "(library (lib|")))
        (expect (member "libraries" cands) :to-be-truthy)
        (expect (member "public_name" cands) :to-be-truthy)))

    (it "offers fields right after an inner opening paren"
      (expect (member "name" (neocaml-dune-test--candidates "(executable (|"))
              :to-be-truthy))

    (it "scopes fields to the stanza type"
      ;; `targets' is a rule field, not an executable field
      (let ((exe (neocaml-dune-test--candidates "(executable (|")))
        (expect (member "targets" exe) :to-be nil))
      (let ((rule (neocaml-dune-test--candidates "(rule (|")))
        (expect (member "targets" rule) :to-be-truthy)
        (expect (member "action" rule) :to-be-truthy)))

    (it "falls back to the union of fields for unknown stanzas"
      (let ((cands (neocaml-dune-test--candidates "(mystery_stanza (|")))
        (expect (member "name" cands) :to-be-truthy))))

  (describe "library-name completion"
    ;; Stub the candidate source so these tests don't shell out to ocamlfind
    ;; or scan the filesystem.
    (before-each
      (spy-on 'neocaml-dune--library-candidates
              :and-return-value '("str" "re" "ppx_jane" "ppx_deriving")))

    (it "offers findlib libraries inside a libraries field"
      (let ((cands (neocaml-dune-test--candidates "(library (libraries |")))
        (expect (member "re" cands) :to-be-truthy)
        (expect (member "ppx_jane" cands) :to-be-truthy)))

    (it "offers libraries for a subsequent entry in the field"
      (expect (member "re" (neocaml-dune-test--candidates
                            "(library (libraries str |"))
              :to-be-truthy))

    (it "offers libraries inside a pps field"
      (expect (member "ppx_deriving"
                      (neocaml-dune-test--candidates
                       "(library (preprocess (pps |"))
              :to-be-truthy))

    (it "offers nothing for libraries when disabled"
      (let ((neocaml-dune-complete-libraries nil))
        (expect (neocaml-dune-test--capf "(library (libraries |")
                :to-be nil)))

    (it "treats the field head itself as field completion, not a library"
      ;; at `(library (lib|' the token is the field name, not a value
      (expect (member "libraries"
                      (neocaml-dune-test--candidates "(library (lib|"))
              :to-be-truthy)))

  (describe "library candidate sources"
    (it "builds an opam-exec command when so configured"
      (let ((neocaml-dune-use-opam-exec t))
        (expect (neocaml-dune--ocamlfind-argv nil)
                :to-equal '("opam" "exec" "--" "ocamlfind" "list")))
      (let ((neocaml-dune-use-opam-exec nil))
        (expect (neocaml-dune--ocamlfind-argv nil)
                :to-equal '("ocamlfind" "list"))))

    (it "uses opam exec automatically for a project-local switch"
      (let ((root (file-name-as-directory (make-temp-file "neocaml-dune" t)))
            (neocaml-dune-use-opam-exec nil))
        (unwind-protect
            (progn
              (expect (neocaml-dune--use-opam-exec-p root) :to-be nil)
              (make-directory (expand-file-name "_opam" root))
              (expect (neocaml-dune--use-opam-exec-p root) :to-be-truthy)
              (expect (car (neocaml-dune--ocamlfind-argv root))
                      :to-equal "opam"))
          (delete-directory root t))))

    (it "scans local dune files for library and public_name values"
      (let ((root (file-name-as-directory (make-temp-file "neocaml-dune" t))))
        (unwind-protect
            (progn
              (write-region "(library (name my_lib) (public_name my.pub))" nil
                            (expand-file-name "dune" root))
              (make-directory (expand-file-name "sub" root))
              (write-region "(executable (name main))" nil
                            (expand-file-name "sub/dune" root))
              (let ((libs (neocaml-dune--local-libraries root)))
                (expect (member "my_lib" libs) :to-be-truthy)
                (expect (member "my.pub" libs) :to-be-truthy)
                ;; executable names are not library names
                (expect (member "main" libs) :to-be nil)))
          (delete-directory root t))))

    (it "ignores dune files under _build"
      (let ((root (file-name-as-directory (make-temp-file "neocaml-dune" t))))
        (unwind-protect
            (progn
              (make-directory (expand-file-name "_build" root))
              (write-region "(library (name stale))" nil
                            (expand-file-name "_build/dune" root))
              (expect (neocaml-dune--local-libraries root) :to-be nil))
          (delete-directory root t))))

    (it "does not scan for local libraries outside a dune project"
      ;; Without a dune-project there is no bounded root, so the recursive
      ;; scan must not run (it would otherwise walk the whole directory).
      (let ((root (file-name-as-directory (make-temp-file "neocaml-nodune" t))))
        (unwind-protect
            (progn
              (write-region "(library (name should_not_appear))" nil
                            (expand-file-name "dune" root))
              (spy-on 'neocaml-dune--local-libraries)
              (spy-on 'neocaml-dune--ocamlfind-libraries :and-return-value nil)
              (clrhash neocaml-dune--library-cache)
              (let ((default-directory root))
                (neocaml-dune--library-candidates))
              (expect 'neocaml-dune--local-libraries :not :to-have-been-called))
          (delete-directory root t)))))

  (describe "context boundaries"
    (it "offers nothing inside a comment"
      (expect (neocaml-dune-test--capf "; (lib|") :to-be nil))

    (it "offers nothing inside a string"
      (expect (neocaml-dune-test--capf "(name \"foo|") :to-be nil))

    (it "offers nothing at the top level outside any form"
      (expect (neocaml-dune-test--capf "lib|") :to-be nil))

    (it "offers nothing in a stanza body away from a form head"
      (expect (neocaml-dune-test--capf "(library (name foo) |") :to-be nil))))

(provide 'neocaml-dune-completion-test)

;;; neocaml-dune-completion-test.el ends here
