;;; neocaml-cram-test.el --- Tests for neocaml-cram-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-cram-mode font-lock.

;;; Code:

(require 'neocaml-cram)

(defun neocaml-cram-test--face-at (content substring)
  "Return the face applied to SUBSTRING in CONTENT after font-locking.
CONTENT is inserted into a temp buffer with `neocaml-cram-mode'."
  (with-temp-buffer
    (insert content)
    (neocaml-cram-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward substring)
    (get-text-property (match-beginning 0) 'face)))

(describe "neocaml-cram font-lock"
  (describe "command lines"
    (it "highlights the $ prompt as keyword"
      (expect (neocaml-cram-test--face-at "  $ echo hello\n" "$")
              :to-equal 'font-lock-keyword-face))

    (it "highlights the command text"
      (expect (neocaml-cram-test--face-at "  $ echo hello\n" "echo hello")
              :to-equal 'font-lock-function-call-face)))

  (describe "continuation lines"
    (it "highlights the > prompt as keyword"
      (expect (neocaml-cram-test--face-at "  $ echo \\\n  > hello\n" ">")
              :to-equal 'font-lock-keyword-face))

    (it "highlights the continuation text"
      (expect (neocaml-cram-test--face-at "  $ echo \\\n  > hello\n" "hello")
              :to-equal 'font-lock-function-call-face)))

  (describe "output lines"
    (it "highlights plain output as string"
      (expect (neocaml-cram-test--face-at "  $ echo hi\n  hello\n" "hello")
              :to-equal 'font-lock-string-face))

    (it "highlights output with (re) modifier"
      (expect (neocaml-cram-test--face-at "  $ echo foo\n  .* (re)\n" "(re)")
              :to-equal 'font-lock-type-face))

    (it "highlights output with (glob) modifier"
      (expect (neocaml-cram-test--face-at "  $ echo foo\n  f* (glob)\n" "(glob)")
              :to-equal 'font-lock-type-face))

    (it "highlights output with (no-eol) modifier"
      (expect (neocaml-cram-test--face-at "  $ printf x\n  x (no-eol)\n" "(no-eol)")
              :to-equal 'font-lock-type-face))

    (it "highlights output with (esc) modifier"
      (expect (neocaml-cram-test--face-at "  $ printf '\\0'\n  \\x00 (esc)\n" "(esc)")
              :to-equal 'font-lock-type-face)))

  (describe "exit codes"
    (it "highlights exit codes as constants"
      (expect (neocaml-cram-test--face-at "  $ exit 1\n  [1]\n" "[1]")
              :to-equal 'font-lock-constant-face)))

  (describe "unreachable marker"
    (it "highlights as warning"
      (expect (neocaml-cram-test--face-at "  ***** UNREACHABLE *****\n"
                                          "UNREACHABLE")
              :to-equal 'font-lock-warning-face)))

  (describe "prose/comments"
    (it "highlights unindented lines as comments"
      (expect (neocaml-cram-test--face-at "Some prose here\n\n  $ echo hi\n"
                                          "Some prose here")
              :to-equal 'font-lock-comment-face)))

  (describe "unknown modifiers"
    (it "does not highlight unknown modifiers specially"
      (expect (neocaml-cram-test--face-at "  $ echo\n  output (unknown)\n" "(unknown)")
              :to-equal 'font-lock-string-face))))

(describe "neocaml-cram imenu"
  (it "indexes command lines"
    (with-temp-buffer
      (require 'imenu)
      (insert "Prose\n\n  $ echo hello\n  hello\n\n  $ cat file.ml\n  let x = 1\n")
      (neocaml-cram-mode)
      (let ((index (funcall imenu-create-index-function)))
        (expect (length index) :to-equal 1)
        (let ((commands (cdr (assoc "Command" index))))
          (expect (length commands) :to-equal 2)
          (expect (caar commands) :to-equal "echo hello")
          (expect (caadr commands) :to-equal "cat file.ml"))))))

(describe "neocaml-cram auto-mode"
  (it "associates .t files with neocaml-cram-mode"
    (expect (cdr (assoc "\\.t\\'" auto-mode-alist))
            :to-equal 'neocaml-cram-mode)))

;;; neocaml-cram-test.el ends here
