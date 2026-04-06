;;; neocaml-odoc-test.el --- Tests for neocaml-odoc-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-odoc-mode: font-lock, indentation, and integration.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-odoc)

;;;; Font-lock helpers (odoc-specific)

(defmacro when-fontifying-odoc-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in odoc code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-odoc-mode content specs)))))

;;;; Indentation helpers (odoc-specific)

(defmacro when-indenting-odoc-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-odoc-mode'."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-odoc-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

;;;; Tests

(describe "neocaml-odoc font-lock"
  (before-all
    (unless (treesit-language-available-p 'odoc)
      (signal 'buttercup-pending "tree-sitter odoc grammar not available")))

  (describe "heading feature"
    (when-fontifying-odoc-it "fontifies headings"
      ("{0 My Library}"
       ("{0 My Library}" font-lock-function-name-face))))

  (describe "tag feature"
    (when-fontifying-odoc-it "fontifies @param tag"
      ("@param name The name to use."
       ("@param" font-lock-keyword-face)))

    (when-fontifying-odoc-it "fontifies @return tag"
      ("@return A greeting string."
       ("@return" font-lock-keyword-face)))

    (when-fontifying-odoc-it "fontifies @since tag"
      ("@since 1.0.0"
       ("@since" font-lock-keyword-face)))

    (when-fontifying-odoc-it "fontifies @deprecated tag"
      ("@deprecated Use something else."
       ("@deprecated" font-lock-keyword-face)))

    (when-fontifying-odoc-it "fontifies @author tag"
      ("@author Jane Doe"
       ("@author" font-lock-keyword-face)))

    (when-fontifying-odoc-it "fontifies param name"
      ("@param name The name to use."
       ("name" font-lock-variable-name-face))))

  (describe "markup feature"
    (when-fontifying-odoc-it "fontifies bold markup"
      ("{b bold text}"
       ("{b bold text}" font-lock-type-face)))

    (when-fontifying-odoc-it "fontifies italic markup"
      ("{i italic text}"
       ("{i italic text}" font-lock-variable-name-face)))

    (when-fontifying-odoc-it "fontifies emphasis markup"
      ("{e emphasis text}"
       ("{e emphasis text}" font-lock-variable-name-face))))

  (describe "code feature"
    (when-fontifying-odoc-it "fontifies code spans"
      ("[some_code]"
       ("[some_code]" font-lock-string-face)))

    (when-fontifying-odoc-it "fontifies plain code blocks"
      ("{[\nlet x = 1\n]}"
       ("let x = 1" font-lock-string-face)))

    (when-fontifying-odoc-it "fontifies language tag in code blocks"
      ("{@ocaml[\nlet x = 1\n]}"
       ("ocaml" font-lock-type-face)))

    (when-fontifying-odoc-it "fontifies verbatim blocks"
      ("{v some verbatim text v}"
       ("some verbatim text" font-lock-string-face))))

  (describe "math feature"
    (when-fontifying-odoc-it "fontifies math spans"
      ("{m x^2}"
       ("{m x^2}" font-lock-number-face))))

  (describe "reference feature"
    (when-fontifying-odoc-it "fontifies simple references"
      ("{!Mylib.greet}"
       ("{!Mylib.greet}" font-lock-constant-face)))

    (when-fontifying-odoc-it "fontifies reference targets in references with text"
      ("{{!Mylib} the module}"
       ("Mylib" font-lock-constant-face)))

    (when-fontifying-odoc-it "fontifies simple links"
      ("{:https://example.com}"
       ("{:https://example.com}" font-lock-constant-face))))

  (describe "escape-sequence feature"
    (when-fontifying-odoc-it "fontifies escape sequences"
      ("\\{escaped"
       ("\\{" font-lock-escape-face)))))

(describe "neocaml-odoc indentation"
  (before-all
    (unless (treesit-language-available-p 'odoc)
      (signal 'buttercup-pending "tree-sitter odoc grammar not available")))

  (when-indenting-odoc-it "indents top-level content at column 0"
    "{0 My Library}

This is some text."))

(when (>= emacs-major-version 30)
  (describe "neocaml-odoc language injection"
    (before-all
      (unless (treesit-language-available-p 'odoc)
        (signal 'buttercup-pending "tree-sitter odoc grammar not available"))
      (unless (treesit-language-available-p 'ocaml)
        (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

    (it "sets up range settings for OCaml injection"
      (with-temp-buffer
        (insert "{@ocaml[\nlet x = 1\n]}")
        (let ((treesit-font-lock-level 4))
          (neocaml-odoc-mode))
        ;; Verify range settings are configured
        (expect treesit-range-settings :not :to-be nil)))

    (it "includes OCaml font-lock rules"
      (with-temp-buffer
        (insert "{@ocaml[\nlet x = 1\n]}")
        (let ((treesit-font-lock-level 4))
          (neocaml-odoc-mode))
        ;; Check that OCaml font-lock rules were appended
        (expect (length treesit-font-lock-settings)
                :to-be-greater-than
                (length neocaml-odoc--font-lock-settings))))

    (it "does not inject OCaml into plain code blocks"
      (with-temp-buffer
        (insert "{[\nlet x = 1\n]}")
        (let ((treesit-font-lock-level 4))
          (neocaml-odoc-mode))
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "let")
        ;; Plain code blocks get string face, not keyword face
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-string-face)))))

(describe "neocaml-odoc integration"
  (before-all
    (unless (treesit-language-available-p 'odoc)
      (signal 'buttercup-pending "tree-sitter odoc grammar not available")))

  (it "applies expected font-lock faces to sample.mld"
    (let ((file (expand-file-name "test/resources/sample.mld"
                                  (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory)))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-odoc-mode))
        (font-lock-ensure)
        ;; Check heading
        (goto-char (point-min))
        (search-forward "{0 My Library}")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-function-name-face)
        ;; Check bold markup
        (goto-char (point-min))
        (search-forward "{b bold}")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-type-face)
        ;; Check code span
        (goto-char (point-min))
        (search-forward "[t]")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-string-face)
        ;; Check @since tag
        (goto-char (point-min))
        (search-forward "@since")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check simple reference
        (goto-char (point-min))
        (search-forward "{!Mylib.greet}")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-constant-face)))))

;;; neocaml-odoc-test.el ends here
