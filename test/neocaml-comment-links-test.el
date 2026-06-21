;;; neocaml-comment-links-test.el --- Comment-link tests -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests verifying that neocaml modes enable clickable URLs and
;; bug references in comments (goto-address-prog-mode / bug-reference-prog-mode).

;;; Code:

(require 'neocaml-test-helpers)

(describe "comment links"
  (it "enables goto-address and bug-reference in neocaml-mode"
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
    (with-temp-buffer
      (neocaml-mode)
      (expect (bound-and-true-p goto-address-prog-mode) :to-be-truthy)
      (expect (bound-and-true-p bug-reference-prog-mode) :to-be-truthy)))

  (it "enables goto-address and bug-reference in neocaml-interface-mode"
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available"))
    (with-temp-buffer
      (neocaml-interface-mode)
      (expect (bound-and-true-p goto-address-prog-mode) :to-be-truthy)
      (expect (bound-and-true-p bug-reference-prog-mode) :to-be-truthy))))

(provide 'neocaml-comment-links-test)

;;; neocaml-comment-links-test.el ends here
