;;; neocaml-repl-test.el --- Tests for neocaml-repl -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-repl'.  Covers the pure helpers
;; (prompt regex, input sender, phrase delimiter search) and the
;; buffer-switching plumbing, without spawning an actual OCaml
;; toplevel process.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-repl)
(require 'cl-lib)

(describe "neocaml-repl prompt regex"
  (it "matches the standard ocaml prompt"
    (expect (string-match-p neocaml-repl--prompt-regexp "# ") :to-be-truthy))

  (it "matches the plain utop prompt"
    (expect (string-match-p neocaml-repl--prompt-regexp "utop # ") :to-be-truthy))

  (it "matches numbered utop prompts"
    (expect (string-match-p neocaml-repl--prompt-regexp "utop[0] # ") :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "utop[42] # ") :to-be-truthy))

  (it "anchors at beginning of line"
    (expect (string-match-p neocaml-repl--prompt-regexp "foo # ") :not :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "  # ") :not :to-be-truthy))

  (it "does not match unrelated text"
    (expect (string-match-p neocaml-repl--prompt-regexp "Welcome to OCaml") :not :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "val x : int") :not :to-be-truthy)))

(describe "neocaml-repl input sender"
  ;; Stub out `comint-send-string' so we can capture what would be
  ;; sent to the toplevel without a real process.
  (let (captured)
    (before-each (setq captured nil))

    (cl-flet ((send (input)
                (cl-letf (((symbol-function 'comint-send-string)
                           (lambda (_proc str) (setq captured str))))
                  (neocaml-repl--input-sender 'fake-proc input))
                captured))

      (it "appends `;;' and a newline when input is unterminated"
        (expect (send "let x = 1") :to-equal "let x = 1\n;;\n"))

      (it "leaves input alone when it already ends with `;;'"
        (expect (send "let x = 1;;") :to-equal "let x = 1;;\n"))

      (it "treats trailing whitespace as still terminated"
        (expect (send "let x = 1;;   ") :to-equal "let x = 1;;   \n")
        (expect (send "let x = 1;;\n") :to-equal "let x = 1;;\n\n"))

      (it "does not get fooled by `;;' inside a string"
        ;; The trailing-`;;' check is purely textual; if the literal
        ;; last two chars aren't `;;' we must terminate.
        (expect (send "let s = \";;\"") :to-equal "let s = \";;\"\n;;\n")))))

(describe "neocaml-repl phrase delimiter search"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "finds a top-level `;;' delimiter"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        (expect pos :not :to-be nil)
        ;; Forward search returns the position past the delimiter.
        (expect (buffer-substring-no-properties (- pos 2) pos) :to-equal ";;"))))

  (it "skips `;;' inside a string literal"
    (with-neocaml-buffer "let s = \"a;;b\";;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        ;; The first real delimiter is the one after the string.
        (expect pos :not :to-be nil)
        (expect (char-before pos) :to-equal ?\;)
        (expect (char-before (1- pos)) :to-equal ?\;)
        ;; And it should sit past the closing quote, not inside the string.
        (expect (> pos (1+ (string-search "\"" (buffer-string)))) :to-be-truthy))))

  (it "skips `;;' inside a block comment"
    (with-neocaml-buffer "let x = (* ;; *) 1;;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        (expect pos :not :to-be nil)
        ;; The found `;;' must come after the closing `*)' of the comment.
        (expect (> pos (string-search "*)" (buffer-string))) :to-be-truthy))))

  (it "returns nil when no delimiter exists"
    (with-neocaml-buffer "let x = 1"
      (goto-char (point-min))
      (expect (neocaml-repl--search-phrase-delimiter 1) :to-be nil)))

  (it "searches backward when direction is negative"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2"
      (goto-char (point-max))
      (let ((pos (neocaml-repl--search-phrase-delimiter -1)))
        (expect pos :not :to-be nil)
        ;; Backward search returns the position before the delimiter.
        (expect (buffer-substring-no-properties pos (+ pos 2)) :to-equal ";;")))))

(describe "neocaml-repl source-to-repl switch"
  (it "pops to an existing REPL buffer and records the source"
    (let ((source (generate-new-buffer "*neocaml-repl-test-source*"))
          (repl (get-buffer-create neocaml-repl-buffer-name)))
      (unwind-protect
          (with-current-buffer source
            (spy-on 'comint-check-proc :and-return-value t)
            (spy-on 'pop-to-buffer)
            (neocaml-repl-switch-to-repl)
            (expect 'pop-to-buffer :to-have-been-called-with neocaml-repl-buffer-name)
            (expect (buffer-local-value 'neocaml-repl--source-buffer repl)
                    :to-equal source))
        (kill-buffer source)
        (when (buffer-live-p repl) (kill-buffer repl)))))

  (it "starts a new REPL when none is running and records the source"
    (let ((source (generate-new-buffer "*neocaml-repl-test-source*"))
          (repl (get-buffer-create neocaml-repl-buffer-name)))
      (unwind-protect
          (with-current-buffer source
            (spy-on 'comint-check-proc :and-return-value nil)
            (spy-on 'neocaml-repl-start)
            (neocaml-repl-switch-to-repl)
            (expect 'neocaml-repl-start :to-have-been-called)
            (expect (buffer-local-value 'neocaml-repl--source-buffer repl)
                    :to-equal source))
        (kill-buffer source)
        (when (buffer-live-p repl) (kill-buffer repl))))))

(describe "neocaml-repl buffer switching"
  (it "pops to the saved source buffer"
    (let ((source (generate-new-buffer "*neocaml-repl-test-source*"))
          (repl (generate-new-buffer "*neocaml-repl-test-repl*")))
      (unwind-protect
          (with-current-buffer repl
            (setq-local neocaml-repl--source-buffer source)
            (spy-on 'pop-to-buffer)
            (neocaml-repl-switch-to-source)
            (expect 'pop-to-buffer :to-have-been-called-with source))
        (kill-buffer source)
        (kill-buffer repl))))

  (it "messages when no source buffer is set"
    (let ((repl (generate-new-buffer "*neocaml-repl-test-repl*")))
      (unwind-protect
          (with-current-buffer repl
            (setq-local neocaml-repl--source-buffer nil)
            (spy-on 'message)
            (neocaml-repl-switch-to-source)
            (expect 'message :to-have-been-called-with "No source buffer to return to"))
        (kill-buffer repl))))

  (it "messages when the saved source buffer is dead"
    (let ((source (generate-new-buffer "*neocaml-repl-test-source*"))
          (repl (generate-new-buffer "*neocaml-repl-test-repl*")))
      (unwind-protect
          (with-current-buffer repl
            (setq-local neocaml-repl--source-buffer source)
            (kill-buffer source)
            (spy-on 'message)
            (neocaml-repl-switch-to-source)
            (expect 'message :to-have-been-called-with "No source buffer to return to"))
        (when (buffer-live-p repl) (kill-buffer repl))))))

(describe "neocaml-repl restart"
  (it "kills the running process and starts a fresh REPL"
    (spy-on 'comint-check-proc :and-return-value t)
    (spy-on 'neocaml-repl--process :and-return-value 'fake-proc)
    (spy-on 'set-process-query-on-exit-flag)
    (spy-on 'delete-process)
    (spy-on 'neocaml-repl-start)
    (neocaml-repl-restart)
    (expect 'delete-process :to-have-been-called-with 'fake-proc)
    (expect 'neocaml-repl-start :to-have-been-called))

  (it "just starts a REPL when none is running"
    (spy-on 'comint-check-proc :and-return-value nil)
    (spy-on 'delete-process)
    (spy-on 'neocaml-repl-start)
    (neocaml-repl-restart)
    (expect 'delete-process :not :to-have-been-called)
    (expect 'neocaml-repl-start :to-have-been-called)))

;;; neocaml-repl-test.el ends here
