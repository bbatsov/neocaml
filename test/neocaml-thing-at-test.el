;;; neocaml-thing-at-test.el --- Tests for `treesit-thing-at' -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for the `sexp' thing in `neocaml--thing-settings':
;; string, character, and quoted-string literals should resolve to the
;; whole literal node, not their delimiter tokens.

;;; Code:

(require 'buttercup)
(require 'neocaml)
(require 'neocaml-test-helpers)

(describe "sexp at point"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
    ;; `treesit-thing-at' arrived in Emacs 30.
    (unless (fboundp 'treesit-thing-at)
      (signal 'buttercup-pending "treesit-thing-at not available")))

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

(provide 'neocaml-thing-at-test)

;;; neocaml-thing-at-test.el ends here
