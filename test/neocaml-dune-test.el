;;; neocaml-dune-test.el --- Tests for neocaml-dune-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-dune-mode: font-lock and indentation.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-dune)

;;;; Font-lock helpers (dune-specific)

(defmacro when-fontifying-dune-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in dune code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-dune-mode content specs)))))

;;;; Indentation helpers (dune-specific)

(defmacro when-indenting-dune-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-dune-mode'."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-dune-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

;;;; Tests

(describe "neocaml-dune font-lock"
  (before-all
    (unless (treesit-language-available-p 'dune)
      (signal 'buttercup-pending "tree-sitter dune grammar not available")))

  (describe "comment feature"
    (when-fontifying-dune-it "fontifies line comments"
      ("; a comment"
       ("; a comment" font-lock-comment-face))))

  (describe "keyword feature"
    (when-fontifying-dune-it "fontifies stanza names"
      ("(library\n (name mylib))"
       ("library" font-lock-keyword-face)))

    (when-fontifying-dune-it "fontifies action names"
      ("(rule\n (action\n  (run foo)))"
       ("run" font-lock-keyword-face)))

    (when-fontifying-dune-it "fontifies various stanza types"
      ("(executable (name main))"
       ("executable" font-lock-keyword-face))
      ("(test (name foo))"
       ("test" font-lock-keyword-face))))

  (describe "property feature"
    (when-fontifying-dune-it "fontifies field names"
      ("(library\n (name mylib))"
       ("name" font-lock-property-name-face)))

    (when-fontifying-dune-it "fontifies various field names"
      ("(library\n (libraries base))"
       ("libraries" font-lock-property-name-face))))

  (describe "string feature"
    (when-fontifying-dune-it "fontifies quoted strings"
      ("(rule\n (action (bash \"echo hi\")))"
       ("\"echo hi\"" font-lock-string-face))))

  (describe "constant feature"
    (when-fontifying-dune-it "fontifies booleans"
      ("(generate_opam_files true)"
       ("true" font-lock-constant-face))))

  (describe "type feature"
    (when-fontifying-dune-it "fontifies module names"
      ("(library\n (name mylib))"
       ("mylib" font-lock-type-face)))

    (when-fontifying-dune-it "fontifies library names"
      ("(library\n (libraries base stdio))"
       ("base" font-lock-type-face)
       ("stdio" font-lock-type-face))))

  (describe "operator feature"
    (when-fontifying-dune-it "fontifies blang operators"
      ("(library\n (enabled_if (>= %{ocaml_version} 4.14)))"
       (">=" font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-dune-it "fontifies parentheses"
      ("(library)"
       (1 1 font-lock-bracket-face)
       (9 9 font-lock-bracket-face)))))

(describe "neocaml-dune indentation"
  (before-all
    (unless (treesit-language-available-p 'dune)
      (signal 'buttercup-pending "tree-sitter dune grammar not available")))

  (when-indenting-dune-it "indents stanza fields"
    "(library
 (name mylib)
 (libraries base stdio))")

  (when-indenting-dune-it "indents nested sexps"
    "(env
 (dev
  (ocamlopt_flags
   (:standard -O2))))")

  (when-indenting-dune-it "indents multiple top-level stanzas"
    "(library
 (name mylib))

(executable
 (name main))")

  (when-indenting-dune-it "indents action fields"
    "(rule
 (action
 (bash \"echo hi\")))")

  (when-indenting-dune-it "indents enabled_if with blang"
    "(library
 (enabled_if
 (>= %{ocaml_version} 4.14)))"))

(describe "neocaml-dune integration"
  (before-all
    (unless (treesit-language-available-p 'dune)
      (signal 'buttercup-pending "tree-sitter dune grammar not available")))

  (it "preserves correct indentation of sample-dune"
    (let* ((file (expand-file-name "test/resources/sample-dune"
                                   (file-name-directory (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-dune-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "preserves correct indentation of sample-dune-project"
    (let* ((file (expand-file-name "test/resources/sample-dune-project"
                                   (file-name-directory (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-dune-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces to sample-dune"
    (let ((file (expand-file-name "test/resources/sample-dune"
                                  (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory)))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-dune-mode))
        (font-lock-ensure)
        ;; Check stanza name
        (goto-char (point-min))
        (search-forward "library")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check field name
        (search-forward "name")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-property-name-face)
        ;; Check comment
        (goto-char (point-min))
        (search-forward "; A sample")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face)))))

;;; neocaml-dune-test.el ends here
