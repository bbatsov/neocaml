;;; neocaml-thing-at-test.el --- Tests for `treesit-thing-at' -*- lexical-binding: t; -*-

(require 'neocaml-test-helpers)

;; sexp

(describe "sexp at point"
  (it "works at the beginning of strings"
    (with-neocaml-buffer "let x = \"hello\""
      (search-forward "= ")
      (expect (treesit-node-text (treesit-thing-at (point) 'sexp))
              :to-equal "\"hello\"")))

  (it "works for characters"
    (with-neocaml-buffer "let x = 'a'"
      (search-forward "= ")
      (expect (treesit-node-text (treesit-thing-at (point) 'sexp))
              :to-equal "'a'")))

  (it "works for quoted strings"
    (with-neocaml-buffer "let x = {|hello|}"
      (search-forward "= ")
      (expect (treesit-node-text (treesit-thing-at (point) 'sexp))
              :to-equal "{|hello|}"))))



