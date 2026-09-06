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
       ("name" neocaml-odoc-tag-name-face)))

    (when-fontifying-odoc-it "fontifies @children_order tag"
      ("@children_order intro.mld api.mld"
       ("@children_order" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @short_title tag"
      ("@short_title My Page"
       ("@short_title" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @toc_status tag"
      ("@toc_status open"
       ("@toc_status" neocaml-odoc-tag-face)))

    (when-fontifying-odoc-it "fontifies @order_category tag"
      ("@order_category basics"
       ("@order_category" neocaml-odoc-tag-face))))

  (describe "markup feature"
    (when-fontifying-odoc-it "fontifies bold markup"
      ("{b bold text}"
       ("{b bold text}" neocaml-odoc-bold-face)))

    (when-fontifying-odoc-it "fontifies italic markup"
      ("{i italic text}"
       ("{i italic text}" neocaml-odoc-italic-face)))

    (when-fontifying-odoc-it "fontifies emphasis markup"
      ("{e emphasis text}"
       ("{e emphasis text}" neocaml-odoc-emphasis-face)))

    (when-fontifying-odoc-it "fontifies markup that spans a newline"
      ("Some {b bold text\nspanning lines} here."
       ("bold text" neocaml-odoc-bold-face)
       ("spanning lines" neocaml-odoc-bold-face)))

    ;; The page title of every dune-released library looks like this; the
    ;; heading face covers the embedded raw markup along with the rest.
    (when-fontifying-odoc-it "fontifies a heading that embeds raw markup"
      ("{0 Fmt {%html: <span>v0.9.0</span>%}}"
       ("{0 Fmt {%html: <span>v0.9.0</span>%}}" neocaml-odoc-heading-face)))

    (when-fontifying-odoc-it "fontifies raw markup in a paragraph"
      ("This is a {%html: <strong>strong</strong>%} tag."
       ("{%html: <strong>strong</strong>%}" neocaml-odoc-raw-markup-face)))

    (when-fontifying-odoc-it "fontifies markup inside a light table cell"
      ("{t | {e emph} | plain |}"
       ("{e emph}" neocaml-odoc-emphasis-face))))

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
       ("some verbatim text" neocaml-odoc-verbatim-face)))

    (when-fontifying-odoc-it "fontifies a delimited code block"
      ("{delim@ocaml[\nlet x = 1\n]delim}"
       ("ocaml" neocaml-odoc-language-face))))

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
       ("{:https://example.com}" neocaml-odoc-link-face)))

    (when-fontifying-odoc-it "fontifies image media target"
      ("{image!media/diagram.png}"
       ("media/diagram.png" neocaml-odoc-link-face)))

    (when-fontifying-odoc-it "fontifies media target in media with replacement text"
      ("{{audio!sound.mp3} listen here}"
       ("sound.mp3" neocaml-odoc-link-face))))

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
        ;; Plain code blocks get code face, not keyword face
        (expect "let" :to-have-face 'neocaml-odoc-code-face)))

    (it "fontifies OCaml keywords inside {@ocaml[...]} blocks"
      (with-temp-buffer
        (insert "{@ocaml[\nlet x = 1\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "let" :to-have-face 'font-lock-keyword-face)))

    (it "fontifies dune keywords inside {@dune[...]} blocks"
      (unless (treesit-language-available-p 'dune)
        (signal 'buttercup-pending "tree-sitter dune grammar not available"))
      (with-temp-buffer
        (insert "{@dune[\n(library\n (name mylib))\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "library" :to-have-face 'font-lock-keyword-face)))

    (it "fontifies OCaml keywords inside a delimited code block"
      (with-temp-buffer
        (insert "{delim@ocaml[\nlet x = 1\n]delim}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "let" :to-have-face 'font-lock-keyword-face)))

    (it "falls back to a code face for languages it cannot inject"
      (with-temp-buffer
        (insert "{@python[\nprint(1)\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "print" :to-have-face 'neocaml-odoc-code-face)))

    (it "fontifies opam keywords inside {@opam[...]} blocks"
      (unless (treesit-language-available-p 'opam)
        (signal 'buttercup-pending "tree-sitter opam grammar not available"))
      (with-temp-buffer
        (insert "{@opam[\ndepends: [\n  \"ocaml\"\n]\n]}")
        (neocaml-odoc-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "depends" :to-have-face 'font-lock-keyword-face)))))

(describe "neocaml-odoc imenu"
  (before-all
    (unless (treesit-language-available-p 'odoc)
      (signal 'buttercup-pending "tree-sitter odoc grammar not available")))

  (it "names a heading by its title, without the marker"
    (with-temp-buffer
      (insert "{0 My Library}\n\n{1:intro Getting\nStarted}\n")
      (neocaml-odoc-mode)
      (expect (mapcar #'car (treesit-simple-imenu))
              :to-equal '("Heading"))
      (expect (mapcar #'car (cdr (assoc "Heading" (treesit-simple-imenu))))
              :to-equal '("My Library" "Getting Started"))))

  (it "names a tag by its whole marker"
    (with-temp-buffer
      (insert "{0 T}\n\n@param name The name to greet.\n")
      (neocaml-odoc-mode)
      (expect (mapcar #'car (cdr (assoc "Tag" (treesit-simple-imenu))))
              :to-equal '("@param name")))))

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
        (goto-char (point-min))
        (expect "{0 My Library}" :to-have-face 'neocaml-odoc-heading-face)
        (goto-char (point-min))
        (expect "{b bold}" :to-have-face 'neocaml-odoc-bold-face)
        (goto-char (point-min))
        (expect "[t]" :to-have-face 'neocaml-odoc-code-face)
        (goto-char (point-min))
        (expect "@since" :to-have-face 'neocaml-odoc-tag-face)
        (goto-char (point-min))
        (expect "{!Mylib.greet}" :to-have-face 'neocaml-odoc-reference-face)))))

;;; neocaml-odoc-test.el ends here
