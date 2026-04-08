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
       ("{0 My Library}" neocaml-odoc-heading-face))))

  (describe "tag feature"
    (when-fontifying-odoc-it "fontifies @param tag"
      ("@param name The name to use."
       ("@param" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @return tag"
      ("@return A greeting string."
       ("@return" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @since tag"
      ("@since 1.0.0"
       ("@since" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @deprecated tag"
      ("@deprecated Use something else."
       ("@deprecated" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @author tag"
      ("@author Jane Doe"
       ("@author" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies param name"
      ("@param name The name to use."
       ("name" neocaml-odoc-tag-name-face))))

  (describe "markup feature"
    (when-fontifying-odoc-it "fontifies bold markup"
      ("{b bold text}"
       ("{b bold text}" neocaml-odoc-bold-face)))

    (when-fontifying-odoc-it "fontifies italic markup"
      ("{i italic text}"
       ("{i italic text}" neocaml-odoc-italic-face)))

    (when-fontifying-odoc-it "fontifies emphasis markup"
      ("{e emphasis text}"
       ("{e emphasis text}" neocaml-odoc-emphasis-face))))

  (describe "code feature"
    (when-fontifying-odoc-it "fontifies code spans"
      ("[some_code]"
       ("[some_code]" neocaml-odoc-code-face)))

    (when-fontifying-odoc-it "fontifies plain code blocks"
      ("{[\nlet x = 1\n]}"
       ("let x = 1" neocaml-odoc-code-face)))

    (when-fontifying-odoc-it "fontifies language tag in code blocks"
      ("{@ocaml[\nlet x = 1\n]}"
       ("ocaml" neocaml-odoc-language-face)))

    (when-fontifying-odoc-it "fontifies verbatim blocks"
      ("{v some verbatim text v}"
       ("some verbatim text" neocaml-odoc-verbatim-face))))

  (describe "math feature"
    (when-fontifying-odoc-it "fontifies math spans"
      ("{m x^2}"
       ("{m x^2}" neocaml-odoc-math-face))))

  (describe "reference feature"
    (when-fontifying-odoc-it "fontifies simple references"
      ("{!Mylib.greet}"
       ("{!Mylib.greet}" neocaml-odoc-reference-face)))

    (when-fontifying-odoc-it "fontifies reference targets in references with text"
      ("{{!Mylib} the module}"
       ("Mylib" neocaml-odoc-reference-face)))

    (when-fontifying-odoc-it "fontifies simple links"
      ("{:https://example.com}"
       ("{:https://example.com}" neocaml-odoc-link-face))))

  (describe "escape-sequence feature"
    (when-fontifying-odoc-it "fontifies escape sequences"
      ("\\{escaped"
       ("\\{" neocaml-odoc-escape-face)))))

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
        ;; Plain code blocks get code face, not keyword face
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'neocaml-odoc-code-face)))

    (it "fontifies OCaml keywords inside {@ocaml[...]} blocks"
      (with-temp-buffer
        (insert "{@ocaml[\nlet x = 1\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "let")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)))

    (it "fontifies dune keywords inside {@dune[...]} blocks"
      (unless (treesit-language-available-p 'dune)
        (signal 'buttercup-pending "tree-sitter dune grammar not available"))
      (with-temp-buffer
        (insert "{@dune[\n(library\n (name mylib))\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "library")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)))

    (it "fontifies opam keywords inside {@opam[...]} blocks"
      (unless (treesit-language-available-p 'opam)
        (signal 'buttercup-pending "tree-sitter opam grammar not available"))
      (with-temp-buffer
        (insert "{@opam[\ndepends: [\n  \"ocaml\"\n]\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "depends")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)))))

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
                :to-equal 'neocaml-odoc-heading-face)
        ;; Check bold markup
        (goto-char (point-min))
        (search-forward "{b bold}")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'neocaml-odoc-bold-face)
        ;; Check code span
        (goto-char (point-min))
        (search-forward "[t]")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'neocaml-odoc-code-face)
        ;; Check @since tag
        (goto-char (point-min))
        (search-forward "@since")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'neocaml-odoc-tag-face)
        ;; Check simple reference
        (goto-char (point-min))
        (search-forward "{!Mylib.greet}")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'neocaml-odoc-reference-face)))))

;;; neocaml-odoc-test.el ends here
