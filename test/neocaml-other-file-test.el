;;; neocaml-other-file-test.el --- Tests for ff-find-other-file pairs -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml-other-file-alist'.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "neocaml-other-file-alist"
  (it "maps .mli to .ml, .mll, and .mly"
    (let ((entry (assoc "\\.mli\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".ml" ".mll" ".mly"))))

  (it "maps .ml to .mli"
    (let ((entry (assoc "\\.ml\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".mli"))))

  (it "maps .mll to .mli"
    (let ((entry (assoc "\\.mll\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".mli"))))

  (it "maps .mly to .mli"
    (let ((entry (assoc "\\.mly\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".mli"))))

  (it "maps .eliom to .eliomi"
    (let ((entry (assoc "\\.eliom\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".eliomi"))))

  (it "maps .eliomi to .eliom"
    (let ((entry (assoc "\\.eliomi\\'" neocaml-other-file-alist)))
      (expect entry :not :to-be nil)
      (expect (cadr entry) :to-equal '(".eliom")))))

;;; neocaml-other-file-test.el ends here
