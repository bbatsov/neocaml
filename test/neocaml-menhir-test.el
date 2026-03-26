;;; neocaml-menhir-test.el --- Tests for neocaml-menhir-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-menhir-mode: font-lock, indentation, and integration.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-menhir)

;;;; Font-lock helpers (menhir-specific)

(defmacro when-fontifying-menhir-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in Menhir code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-menhir-mode content specs)))))

;;;; Indentation helpers (menhir-specific)

(defmacro when-indenting-menhir-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-menhir-mode'."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-menhir-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

;;;; Tests

(describe "neocaml-menhir font-lock"
  (before-all
    (unless (treesit-language-available-p 'menhir)
      (signal 'buttercup-pending "tree-sitter Menhir grammar not available")))

  (describe "comment feature"
    (when-fontifying-menhir-it "fontifies OCaml comments"
      ("(* a comment *)\n%%\n"
       ("(* a comment *)" font-lock-comment-face))))

  (describe "keyword feature"
    (when-fontifying-menhir-it "fontifies %token"
      ("%token EOF\n%%\n"
       ("%token" font-lock-keyword-face)))

    (when-fontifying-menhir-it "fontifies %left"
      ("%token PLUS\n%left PLUS\n%%\n"
       ("%left" font-lock-keyword-face)))

    (when-fontifying-menhir-it "fontifies %%"
      ("%token EOF\n%%\n"
       ("%%" font-lock-keyword-face))))

  (describe "definition feature"
    (when-fontifying-menhir-it "fontifies rule names"
      ("%token EOF\n%%\nexpr:\n  | EOF { () }\n"
       ("expr" font-lock-function-name-face)))

    (when-fontifying-menhir-it "fontifies token declarations"
      ("%token EOF\n%%\n"
       ("EOF" font-lock-constant-face))))

  (describe "variable feature"
    (when-fontifying-menhir-it "fontifies token references in rules"
      ("%token PLUS MINUS\n%%\nexpr:\n  | expr PLUS expr { () }\n"
       ("PLUS" font-lock-constant-face)
       ("MINUS" font-lock-constant-face))))

  (describe "operator feature"
    (when-fontifying-menhir-it "fontifies pipe operator"
      ("%token EOF\n%%\nexpr:\n  | EOF { () }\n"
       ("|" font-lock-operator-face)))))

(describe "neocaml-menhir indentation"
  (before-all
    (unless (treesit-language-available-p 'menhir)
      (signal 'buttercup-pending "tree-sitter Menhir grammar not available")))

  (when-indenting-menhir-it "indents declarations at column 0"
    "%token EOF
%%")

  (when-indenting-menhir-it "indents rule cases"
    "%token EOF
%%
expr:
  | EOF { () }"))

(when (>= emacs-major-version 30)
  (describe "neocaml-menhir language injection"
    (before-all
      (unless (treesit-language-available-p 'menhir)
        (signal 'buttercup-pending "tree-sitter Menhir grammar not available"))
      (unless (treesit-language-available-p 'ocaml)
        (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "highlights OCaml keywords in header blocks"
    (with-temp-buffer
      (insert "%{\n  open Ast\n%}\n%token EOF\n%%\n")
      (let ((treesit-font-lock-level 4))
        (neocaml-menhir-mode))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "open")
      (expect (get-text-property (match-beginning 0) 'face)
              :to-equal 'font-lock-keyword-face)))

  (it "highlights OCaml keywords in action blocks"
    (with-temp-buffer
      (insert "%token<int> INT\n%%\nexpr:\n  | INT { Int $1 }\n")
      (let ((treesit-font-lock-level 4))
        (neocaml-menhir-mode))
      (font-lock-ensure)
      ;; Int is an OCaml constructor — should get a face from OCaml injection
      (goto-char (point-min))
      (search-forward "{ Int")
      (let ((face (get-text-property (+ 2 (match-beginning 0)) 'face)))
        (expect face :not :to-be nil))))))

(describe "neocaml-menhir integration"
  (before-all
    (unless (treesit-language-available-p 'menhir)
      (signal 'buttercup-pending "tree-sitter Menhir grammar not available")))

  (it "preserves correct indentation of sample.mly"
    (let* ((file (expand-file-name "test/resources/sample.mly"
                                   (file-name-directory (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-menhir-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces to sample.mly"
    (let ((file (expand-file-name "test/resources/sample.mly"
                                  (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory)))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-menhir-mode))
        (font-lock-ensure)
        ;; Check %token keyword
        (goto-char (point-min))
        (search-forward "%token")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check comment
        (goto-char (point-min))
        (search-forward "(* A sample")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face)))))

;;; neocaml-menhir-test.el ends here
