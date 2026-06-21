;;; neocaml-syntax-test.el --- Syntax-propertize tests -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml--syntax-propertize': character literals and
;; quoted strings must not confuse the syntactic layer (sexp motion,
;; paren matching, `syntax-ppss').

;;; Code:

(require 'neocaml-test-helpers)

(defun neocaml-test--ppss-at (code pos)
  "Return the `syntax-ppss' state at POS after inserting CODE in `neocaml-mode'.
POS may be `max' for `point-max'."
  (with-temp-buffer
    (neocaml-mode)
    (insert code)
    (syntax-ppss (if (eq pos 'max) (point-max) pos))))

(describe "neocaml syntax propertization"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (describe "character literals"
    (it "does not let a double-quote char open a string"
      (let ((ppss (neocaml-test--ppss-at "let x = '\"' in x" 'max)))
        (expect (nth 3 ppss) :to-be nil)))

    (it "does not let a paren char unbalance the buffer"
      (let ((ppss (neocaml-test--ppss-at "let y = '(' in y" 'max)))
        (expect (nth 0 ppss) :to-equal 0)))

    (it "handles escaped char literals"
      (let ((ppss (neocaml-test--ppss-at "let n = '\\n' in n" 'max)))
        (expect (nth 3 ppss) :to-be nil)
        (expect (nth 0 ppss) :to-equal 0)))

    (it "leaves type variables alone"
      ;; 'a / 'b have no closing quote and must not be marked as strings
      (let ((ppss (neocaml-test--ppss-at "type t = 'a -> 'b" 'max)))
        (expect (nth 3 ppss) :to-be nil))))

  (describe "quoted strings"
    (it "treats the body as a string"
      ;; mid-string position should report being inside a string
      (with-temp-buffer
        (neocaml-mode)
        (insert "let z = {|abcdef|}")
        (goto-char (point-min))
        (search-forward "abc")
        (expect (nth 3 (syntax-ppss (point))) :to-be-truthy)))

    (it "keeps inner quotes and comment openers inert"
      (let ((ppss (neocaml-test--ppss-at "let z = {|a \"b\" (* c *)|}" 'max)))
        (expect (nth 3 ppss) :to-be nil)
        (expect (nth 4 ppss) :to-be nil)))

    (it "handles tagged delimiters with unbalanced inner characters"
      (let ((ppss (neocaml-test--ppss-at "let w = {d|x \"y (* z|d}" 'max)))
        (expect (nth 3 ppss) :to-be nil)
        (expect (nth 4 ppss) :to-be nil))))

  (describe "regular strings still work"
    (it "reports being inside an ordinary string"
      (with-temp-buffer
        (neocaml-mode)
        (insert "let s = \"hello")
        (expect (nth 3 (syntax-ppss (point-max))) :to-be-truthy)))))

(provide 'neocaml-syntax-test)

;;; neocaml-syntax-test.el ends here
