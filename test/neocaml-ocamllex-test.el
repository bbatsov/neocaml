;;; neocaml-ocamllex-test.el --- Tests for neocaml-ocamllex-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-ocamllex-mode: font-lock, indentation, and integration.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-ocamllex)

;;;; Font-lock helpers (ocamllex-specific)

(defmacro when-fontifying-ocamllex-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in OCamllex code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-ocamllex-mode content specs)))))

;;;; Indentation helpers (ocamllex-specific)

(defmacro when-indenting-ocamllex-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-ocamllex-mode'."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-ocamllex-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

;;;; Tests

(describe "neocaml-ocamllex font-lock"
  (before-all
    (unless (treesit-language-available-p 'ocamllex)
      (signal 'buttercup-pending "tree-sitter OCamllex grammar not available")))

  (describe "comment feature"
    (when-fontifying-ocamllex-it "fontifies comments"
      ("(* a comment *)"
       ("(* a comment *)" font-lock-comment-face))))

  (describe "keyword feature"
    (when-fontifying-ocamllex-it "fontifies rule keyword"
      ("rule token = parse\n  | eof { () }"
       ("rule" font-lock-keyword-face)))

    (when-fontifying-ocamllex-it "fontifies let keyword"
      ("let digit = ['0'-'9']"
       ("let" font-lock-keyword-face)))

    (when-fontifying-ocamllex-it "fontifies parse keyword"
      ("rule token = parse\n  | eof { () }"
       ("parse" font-lock-keyword-face)))

    (when-fontifying-ocamllex-it "fontifies and keyword"
      ("rule token = parse\n  | eof { () }\nand other = parse\n  | eof { () }"
       ("and" font-lock-keyword-face))))

  (describe "definition feature"
    (when-fontifying-ocamllex-it "fontifies lexer entry names"
      ("rule token = parse\n  | eof { () }"
       ("token" font-lock-function-name-face)))

    (when-fontifying-ocamllex-it "fontifies named regexp definitions"
      ("let digit = ['0'-'9']"
       ("digit" font-lock-variable-name-face))))

  (describe "string feature"
    (when-fontifying-ocamllex-it "fontifies string literals"
      ("let newline = \"hello\""
       ("\"hello\"" font-lock-string-face))))

  (describe "constant feature"
    (when-fontifying-ocamllex-it "fontifies eof"
      ("rule token = parse\n  | eof { () }"
       ("eof" font-lock-constant-face))))

  (describe "bracket feature"
    (when-fontifying-ocamllex-it "fontifies character set brackets"
      ("let digit = ['0'-'9']"
       (13 13 font-lock-bracket-face)
       (21 21 font-lock-bracket-face)))))

(describe "neocaml-ocamllex indentation"
  (before-all
    (unless (treesit-language-available-p 'ocamllex)
      (signal 'buttercup-pending "tree-sitter OCamllex grammar not available")))

  (when-indenting-ocamllex-it "indents named regexps at column 0"
    "let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']")

  (when-indenting-ocamllex-it "indents rule with cases"
    "rule token = parse
  | eof { () }"))

(describe "neocaml-ocamllex integration"
  (before-all
    (unless (treesit-language-available-p 'ocamllex)
      (signal 'buttercup-pending "tree-sitter OCamllex grammar not available")))

  (it "preserves correct indentation of sample.mll"
    (let* ((file (expand-file-name "test/resources/sample.mll"
                                   (file-name-directory (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-ocamllex-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces to sample.mll"
    (let ((file (expand-file-name "test/resources/sample.mll"
                                  (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory)))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-ocamllex-mode))
        (font-lock-ensure)
        ;; Check that "rule" is fontified as keyword
        (goto-char (point-min))
        (search-forward "rule")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check that "token" is fontified as function name
        (search-forward "token")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-function-name-face)
        ;; Check comment
        (goto-char (point-min))
        (search-forward "(* A sample")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face)))))

;;; neocaml-ocamllex-test.el ends here
