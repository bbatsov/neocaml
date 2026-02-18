;;; neocaml-fill-test.el --- Tests for fill-paragraph support -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml--fill-paragraph' and related comment settings.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "fill-paragraph"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (describe "configuration"
    (it "sets fill-paragraph-function"
      (with-temp-buffer
        (neocaml-mode)
        (expect fill-paragraph-function :to-be #'neocaml--fill-paragraph)))

    (it "sets comment-end-skip"
      (with-temp-buffer
        (neocaml-mode)
        (expect comment-end-skip :to-equal "[ \t]*\\*+)")))

    (it "enables adaptive-fill-mode"
      (with-temp-buffer
        (neocaml-mode)
        (expect adaptive-fill-mode :to-be t)))

    (it "sets fill-paragraph-function in interface mode"
      (with-temp-buffer
        (neocaml-interface-mode)
        (expect fill-paragraph-function :to-be #'neocaml--fill-paragraph))))

  (describe "filling comments"
    (it "fills a long single-line comment"
      (with-temp-buffer
        (insert "(* This is a very long comment that should be wrapped because it exceeds the fill column by quite a bit and needs reformatting *)")
        (neocaml-mode)
        (let ((fill-column 40))
          (goto-char (point-min))
          (forward-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "^(\\*")
        (expect (buffer-string) :to-match "\\*)$")
        ;; Should have been wrapped into multiple lines
        (expect (count-lines (point-min) (point-max)) :to-be-greater-than 1)))

    (it "fills a multi-line comment"
      (with-temp-buffer
        (insert "(* word1 word2 word3\nword4 word5 word6\nword7 word8 *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char (point-min))
          (forward-char 5)
          (fill-paragraph))
        ;; With fill-column 70, everything fits on one line inside the comment
        (expect (buffer-string) :to-match "^(\\*")
        (expect (buffer-string) :to-match "\\*)$")))

    (it "does not touch code outside comments"
      (with-temp-buffer
        (insert "let x = 1")
        (neocaml-mode)
        (goto-char (point-min))
        ;; neocaml--fill-paragraph should return nil and leave buffer unchanged
        (expect (neocaml--fill-paragraph) :to-be nil)
        (expect (buffer-string) :to-equal "let x = 1")))

    (it "returns t when inside a comment"
      (with-temp-buffer
        (insert "(* a comment *)")
        (neocaml-mode)
        (goto-char 5)
        (expect (neocaml--fill-paragraph) :to-be t)))

    (it "returns nil when outside a comment"
      (with-temp-buffer
        (insert "let x = 42")
        (neocaml-mode)
        (goto-char 5)
        (expect (neocaml--fill-paragraph) :to-be nil)))

    (it "handles doc comments"
      (with-temp-buffer
        (insert "(** This is a documentation comment that is quite long and should be wrapped properly when fill-paragraph is invoked on it *)")
        (neocaml-mode)
        (let ((fill-column 40))
          (goto-char 10)
          (fill-paragraph))
        (expect (buffer-string) :to-match "^(\\*\\*")
        (expect (buffer-string) :to-match "\\*)$")
        (expect (count-lines (point-min) (point-max)) :to-be-greater-than 1)))

    (it "preserves comment delimiters"
      (with-temp-buffer
        (insert "(* short *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "^(\\*")
        (expect (buffer-string) :to-match "\\*)$")))))

;;; neocaml-fill-test.el ends here
