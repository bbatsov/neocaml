;;; neocaml-common-test.el --- Tests for neocaml-common -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for the shared helpers in neocaml-common.el.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-common)

(describe "neocaml-common-merge-feature-lists"
  (it "merges level by level and drops duplicates"
    (expect (neocaml-common-merge-feature-lists
             '((comment definition) (keyword string))
             '((comment keyword definition) (property string)))
            :to-equal
            '((comment definition keyword) (keyword string property))))

  (it "keeps the longer list's extra levels"
    (expect (neocaml-common-merge-feature-lists '((a)) '((b) (c) (d)))
            :to-equal '((a b) (c) (d))))

  ;; Callers pass defconsts -- `neocaml--font-lock-feature-list' and
  ;; `html-ts-mode--treesit-font-lock-feature-list'.  An earlier version
  ;; shared structure with its second argument and then ran the destructive
  ;; `delete-dups' over it, which silently deleted features from the caller's
  ;; own list and broke font-lock for every buffer opened afterwards.
  (it "does not modify its arguments"
    (let* ((a '((comment definition) (keyword string)))
           (b '((comment keyword definition) (property string)))
           (a-before (copy-tree a))
           (b-before (copy-tree b)))
      (neocaml-common-merge-feature-lists a b)
      (expect a :to-equal a-before)
      (expect b :to-equal b-before))))

(provide 'neocaml-common-test)

;;; neocaml-common-test.el ends here
