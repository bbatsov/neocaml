;;; neocaml-opam-completion-test.el --- Tests for opam completion-at-point -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-opam-completion-at-point': field-name and
;; dependency package-name completion contexts.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-opam)

(defun neocaml-opam-test--capf (content)
  "Run the opam capf on CONTENT, where `|' marks point.
Return the raw capf result, or nil."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (let ((pos (point)))
      (neocaml-opam-mode)
      (goto-char pos))
    (neocaml-opam-completion-at-point)))

(defun neocaml-opam-test--candidates (content)
  "Return the candidate list the opam capf offers for CONTENT.
Resolves both plain lists and dynamic completion tables."
  (let ((result (neocaml-opam-test--capf content)))
    (when result
      (all-completions "" (nth 2 result)))))

(describe "neocaml-opam completion-at-point"
  (before-all
    (unless (treesit-language-available-p 'opam)
      (signal 'buttercup-pending "tree-sitter opam grammar not available")))

  (describe "field-name completion"
    (it "offers field names at the start of a line"
      (let ((cands (neocaml-opam-test--candidates "synop|")))
        (expect (member "synopsis" cands) :to-be-truthy)
        (expect (member "depends" cands) :to-be-truthy)))

    (it "offers field names on an empty line"
      (expect (member "maintainer" (neocaml-opam-test--candidates "|"))
              :to-be-truthy))

    (it "offers section kinds alongside fields"
      (expect (member "url" (neocaml-opam-test--candidates "u|"))
              :to-be-truthy))

    (it "spans the partial field name being typed"
      (let ((result (neocaml-opam-test--capf "synop|")))
        (expect (nth 0 result) :to-equal 1)
        (expect (nth 1 result) :to-equal 6)))

    (it "offers nothing in the value position of a field"
      (expect (neocaml-opam-test--capf "version: 1|") :to-be nil)))

  (describe "package-name completion"
    ;; Stub the candidate source so these tests don't shell out to opam.
    (before-each
      (spy-on 'neocaml-opam--package-candidates
              :and-return-value '("dune" "ppxlib" "cmdliner")))

    (it "offers packages inside a depends list"
      (let ((cands (neocaml-opam-test--candidates "depends: [ \"du|")))
        (expect (member "dune" cands) :to-be-truthy)
        (expect (member "cmdliner" cands) :to-be-truthy)))

    (it "offers packages inside conflicts and depopts"
      (expect (member "ppxlib"
                      (neocaml-opam-test--candidates "conflicts: [ \"pp|"))
              :to-be-truthy)
      (expect (member "dune"
                      (neocaml-opam-test--candidates "depopts: [ \"du|"))
              :to-be-truthy))

    (it "spans only the string contents"
      (let ((result (neocaml-opam-test--capf "depends: [ \"du|")))
        ;; the opening quote is at position 12, so the span starts at 13
        (expect (nth 0 result) :to-equal 13)))

    (it "does not offer packages inside a version constraint"
      (expect (neocaml-opam-test--capf "depends: [ \"dune\" {>= \"3|")
              :to-be nil))

    (it "does not offer packages in a non-dependency string"
      (expect (neocaml-opam-test--capf "maintainer: \"du|") :to-be nil))

    (it "offers nothing for packages when disabled"
      (let ((neocaml-opam-complete-packages nil))
        (expect (neocaml-opam-test--capf "depends: [ \"du|") :to-be nil))))

  (describe "package candidate source"
    (it "builds an opam-exec command when so configured"
      (let ((neocaml-opam-use-opam-exec t))
        (expect (neocaml-opam--list-command)
                :to-equal '("opam" "exec" "--" "opam" "list" "--all" "--short")))
      (let ((neocaml-opam-use-opam-exec nil))
        (expect (neocaml-opam--list-command)
                :to-equal '("opam" "list" "--all" "--short")))))

  (describe "context boundaries"
    (it "offers nothing inside a comment"
      (expect (neocaml-opam-test--capf "# synop|") :to-be nil))))

(provide 'neocaml-opam-completion-test)

;;; neocaml-opam-completion-test.el ends here
