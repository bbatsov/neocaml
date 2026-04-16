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
        ;; Continuation lines indented to align with text after (* ,
        ;; and space before *) preserved.
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (concat "(* This is a very long comment that should\n"
                        "   be wrapped because it exceeds the\n"
                        "   fill column by quite a bit and needs\n"
                        "   reformatting *)"))))

    (it "fills a multi-line comment"
      (with-temp-buffer
        (insert "(* word1 word2 word3\n   word4 word5 word6\n   word7 word8 *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char (point-min))
          (forward-char 5)
          (fill-paragraph))
        ;; With fill-column 70, all words join onto one line
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "(* word1 word2 word3 word4 word5 word6 word7 word8 *)")))

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
        ;; Continuation lines indented to align with text after (** ,
        ;; and space before *) preserved.
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (concat "(** This is a documentation comment that is\n"
                        "    quite long and should be wrapped\n"
                        "    properly when fill-paragraph is\n"
                        "    invoked on it *)"))))

    (it "fills an indented comment"
      (with-temp-buffer
        (insert "  (* This is an indented comment that should also be wrapped correctly when fill-paragraph runs *)")
        (neocaml-mode)
        (let ((fill-column 40))
          (goto-char 10)
          (fill-paragraph))
        ;; fill-prefix respects the actual column offset (5 spaces for
        ;; 2-space indent + "(* ").
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (concat "  (* This is an indented comment that should\n"
                        "     also be wrapped correctly when\n"
                        "     fill-paragraph runs *)"))))

    (it "preserves comment delimiters"
      (with-temp-buffer
        (insert "(* short *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "^(\\*")
        (expect (buffer-string) :to-match "\\*)$")))

    (it "preserves list items in comments"
      (with-temp-buffer
        (insert "(* Some long comment text that explains the function behavior\n   - item one\n   - item two *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "\n   - item one")
        (expect (buffer-string) :to-match "\n   - item two")))

    (it "preserves list items in doc comments"
      (with-temp-buffer
        (insert "(** Some long comment text that explains the function behavior\n    - item one\n    - item two *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "\n    - item one")
        (expect (buffer-string) :to-match "\n    - item two")))

    (it "wraps long prose before list items"
      (with-temp-buffer
        (insert "(** Long prose that needs to be wrapped at the fill column and should not eat the list items below\n    - first item\n    - second item *)")
        (neocaml-mode)
        (let ((fill-column 50))
          (goto-char 5)
          (fill-paragraph))
        ;; Prose should be wrapped
        (expect (buffer-string) :to-match "\n    ")
        ;; List items must survive
        (expect (buffer-string) :to-match "\n    - first item")
        (expect (buffer-string) :to-match "\n    - second item")))

    (it "preserves + list markers"
      (with-temp-buffer
        (insert "(* Description\n   + first\n   + second *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "\n   \\+ first")
        (expect (buffer-string) :to-match "\n   \\+ second")))

    (it "preserves odoc tags as paragraph boundaries"
      (with-temp-buffer
        (insert "(** Some description text\n    @param x the input\n    @return the output *)")
        (neocaml-mode)
        (let ((fill-column 70))
          (goto-char 5)
          (fill-paragraph))
        (expect (buffer-string) :to-match "\n    @param x the input")
        (expect (buffer-string) :to-match "\n    @return the output"))))

  (describe "comment continuation"
    (it "inserts newline with correct indentation in regular comment"
      (with-temp-buffer
        (insert "(* some text")
        (neocaml-mode)
        (goto-char (point-max))
        (neocaml--comment-indent-new-line)
        (expect (line-number-at-pos) :to-equal 2)
        (expect (current-column) :to-equal 3)))

    (it "inserts newline with correct indentation in doc comment"
      (with-temp-buffer
        (insert "(** some text")
        (neocaml-mode)
        (goto-char (point-max))
        (neocaml--comment-indent-new-line)
        (expect (line-number-at-pos) :to-equal 2)
        (expect (current-column) :to-equal 4)))

    (it "inserts newline with correct indentation in indented comment"
      (with-temp-buffer
        (insert "  (* some text")
        (neocaml-mode)
        (goto-char (point-max))
        (neocaml--comment-indent-new-line)
        (expect (line-number-at-pos) :to-equal 2)
        (expect (current-column) :to-equal 5)))

    (it "falls back outside comments"
      (with-temp-buffer
        (insert "let x = 1")
        (neocaml-mode)
        (goto-char (point-max))
        ;; Should not error
        (neocaml--comment-indent-new-line)
        (expect (line-number-at-pos) :to-equal 2))))

  (describe "comment toggling"
    (it "comments a single line"
      (with-temp-buffer
        (insert "let x = 1")
        (neocaml-mode)
        (comment-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "(* let x = 1 *)")))

    (it "uncomments a single line"
      (with-temp-buffer
        (insert "(* let x = 1 *)")
        (neocaml-mode)
        (uncomment-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "let x = 1")))

    (it "round-trips comment then uncomment on a single line"
      (with-temp-buffer
        (insert "let x = 1")
        (neocaml-mode)
        (comment-region (point-min) (point-max))
        (uncomment-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "let x = 1")))

    (it "round-trips comment then uncomment on multiple lines"
      (with-temp-buffer
        (insert "let x = 1\nlet y = 2\nlet z = 3\n")
        (neocaml-mode)
        (comment-region (point-min) (point-max))
        (uncomment-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "let x = 1\nlet y = 2\nlet z = 3\n")))

    (it "comments multiple lines individually"
      (with-temp-buffer
        (insert "let x = 1\nlet y = 2\n")
        (neocaml-mode)
        (comment-region (point-min) (point-max))
        (expect (buffer-string) :to-match "(\\* let x = 1 \\*)")
        (expect (buffer-string) :to-match "(\\* let y = 2 \\*)")))))

;;; neocaml-fill-test.el ends here
