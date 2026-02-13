;;; neocaml-integration-test.el --- Integration tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Integration tests that load sample resource files and verify that
;; indentation and font-lock work correctly end-to-end.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(defvar neocaml-test--resources-dir
  (expand-file-name "resources" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test resource files.")

(defun neocaml-test--resource-file (name)
  "Return the absolute path of resource file NAME."
  (expand-file-name name neocaml-test--resources-dir))

(describe "integration: sample.ml"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "preserves correct indentation after indent-region"
    (let* ((file (neocaml-test--resource-file "sample.ml"))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces"
    (let ((file (neocaml-test--resource-file "sample.ml")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-mode))
        (font-lock-ensure)
        ;; Check that "let" keyword is fontified
        (goto-char (point-min))
        (search-forward "let area")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check that a type name is fontified
        (goto-char (point-min))
        (search-forward "type shape")
        (let ((type-start (+ (match-beginning 0) 5)))
          (expect (get-text-property type-start 'face)
                  :to-equal 'font-lock-type-face))
        ;; Check that a comment is fontified
        (goto-char (point-min))
        (search-forward "(* A simple")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face)))))

(describe "integration: sample.mli"
  (before-all
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available")))

  (it "preserves correct indentation after indent-region"
    (let* ((file (neocaml-test--resource-file "sample.mli"))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-interface-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces"
    (let ((file (neocaml-test--resource-file "sample.mli")))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-interface-mode))
        (font-lock-ensure)
        ;; Check that "val" keyword is fontified
        (goto-char (point-min))
        (search-forward "val area")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check that a type name is fontified
        (goto-char (point-min))
        (search-forward "type shape")
        (let ((type-start (+ (match-beginning 0) 5)))
          (expect (get-text-property type-start 'face)
                  :to-equal 'font-lock-type-face))
        ;; Check that a doc comment is fontified
        (goto-char (point-min))
        (search-forward "(** Sample")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-doc-face)))))

;;; neocaml-integration-test.el ends here
