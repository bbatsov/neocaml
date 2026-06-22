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
            ;; Existing REPL has the same flavor as requested, so no restart prompt.
            (with-current-buffer repl (setq neocaml-repl--flavor neocaml-repl-flavor))
            (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
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
            (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
            (spy-on 'comint-check-proc :and-return-value nil)
            (spy-on 'neocaml-repl-start)
            (neocaml-repl-switch-to-repl)
            (expect 'neocaml-repl-start :to-have-been-called)
            (expect (buffer-local-value 'neocaml-repl--source-buffer repl)
                    :to-equal source))
        (kill-buffer source)
        (when (buffer-live-p repl) (kill-buffer repl)))))

  (it "offers to restart the REPL when the requested flavor differs"
    (let ((source (generate-new-buffer "*neocaml-repl-test-source*"))
          (repl (get-buffer-create neocaml-repl-buffer-name)))
      (unwind-protect
          (with-current-buffer source
            (with-current-buffer repl (setq neocaml-repl--flavor 'ocaml))
            (let ((neocaml-repl-flavor 'utop))
              (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
              (spy-on 'comint-check-proc :and-return-value t)
              (spy-on 'y-or-n-p :and-return-value t)
              (spy-on 'neocaml-repl--kill)
              (spy-on 'neocaml-repl-start)
              (neocaml-repl-switch-to-repl)
              (expect 'neocaml-repl--kill :to-have-been-called)
              (expect 'neocaml-repl-start :to-have-been-called)))
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

(describe "neocaml-repl--start-command"
  (it "records the flavor and command and sets the mode line"
    (let ((repl "*neocaml-repl-startcmd-test*"))
      (unwind-protect
          (progn
            (spy-on 'make-comint-in-buffer :and-call-fake
                    (lambda (_name buffer &rest _) (get-buffer-create buffer)))
            (spy-on 'neocaml-repl-mode)
            (let ((buf (neocaml-repl--start-command repl '("utop") 'utop)))
              (with-current-buffer buf
                (expect neocaml-repl--flavor :to-equal 'utop)
                (expect neocaml-repl--command-line :to-equal '("utop"))
                (expect mode-name :to-equal "OCaml-REPL[utop]"))))
        (when (get-buffer repl) (kill-buffer repl))))))

(describe "neocaml-repl restart"
  (it "relaunches with the recorded flavor and command"
    (let ((repl (get-buffer-create "*neocaml-repl-restart-test*")))
      (unwind-protect
          (progn
            (with-current-buffer repl
              (setq-local neocaml-repl--flavor 'utop)
              (setq-local neocaml-repl--command-line '("utop")))
            (spy-on 'neocaml-repl--buffer :and-return-value "*neocaml-repl-restart-test*")
            (spy-on 'neocaml-repl--kill)
            (spy-on 'neocaml-repl--start-command)
            (neocaml-repl-restart)
            (expect 'neocaml-repl--kill
                    :to-have-been-called-with "*neocaml-repl-restart-test*")
            (expect 'neocaml-repl--start-command
                    :to-have-been-called-with "*neocaml-repl-restart-test*" '("utop") 'utop))
        (when (buffer-live-p repl) (kill-buffer repl)))))

  (it "falls back to a fresh start when nothing was recorded"
    (spy-on 'neocaml-repl--buffer :and-return-value "*neocaml-repl-absent*")
    (spy-on 'neocaml-repl--kill)
    (spy-on 'neocaml-repl-start)
    (neocaml-repl-restart)
    (expect 'neocaml-repl-start :to-have-been-called)))

(describe "neocaml-repl send-phrase-and-step"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "sends the current phrase and moves to the next one"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2;;\n"
      (goto-char (point-min))
      (let (sent)
        (spy-on 'neocaml-repl-send-region :and-call-fake
                (lambda (s e) (setq sent (string-trim
                                          (buffer-substring-no-properties s e)))))
        (neocaml-repl-send-phrase-and-step)
        (expect sent :to-match "let x = 1")
        ;; point should now sit at the start of the second phrase
        (expect (looking-at "let y") :to-be-truthy)))))

(describe "neocaml-repl per-project buffer name"
  (it "derives a per-project name from the base name"
    (spy-on 'neocaml-repl--project-id :and-return-value "myproj")
    (let ((neocaml-repl-buffer-name "*OCaml*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*OCaml: myproj*"))))

  (it "falls back to the base name outside a project"
    (spy-on 'neocaml-repl--project-id :and-return-value nil)
    (let ((neocaml-repl-buffer-name "*OCaml*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*OCaml*"))))

  (it "honors a customized base name"
    (spy-on 'neocaml-repl--project-id :and-return-value "p")
    (let ((neocaml-repl-buffer-name "*ocaml-repl*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*ocaml-repl: p*"))))

  (it "targets the current buffer when inside a REPL buffer"
    (with-temp-buffer
      (rename-buffer "*OCaml: other*" t)
      (setq major-mode 'neocaml-repl-mode)
      (expect (neocaml-repl--buffer) :to-equal (buffer-name)))))

(describe "neocaml-repl require"
  (it "sends a #require directive for the given package"
    (let (sent)
      (spy-on 'neocaml-repl--ensure-repl-running)
      (spy-on 'neocaml-repl--process :and-return-value 'proc)
      (spy-on 'neocaml-repl--input-sender :and-call-fake
              (lambda (_proc input) (setq sent input)))
      (neocaml-repl-require "lwt")
      (expect sent :to-equal "#require \"lwt\""))))

(describe "neocaml-repl--command"
  (it "uses the program vars for the ocaml flavor"
    (let ((neocaml-repl-flavor 'ocaml)
          (neocaml-repl-program-name "ocaml")
          (neocaml-repl-program-args '("-nopromptcont")))
      (expect (neocaml-repl--command) :to-equal '("ocaml" "-nopromptcont"))))

  (it "uses utop for the utop flavor"
    (let ((neocaml-repl-flavor 'utop))
      (expect (neocaml-repl--command) :to-equal '("utop"))))

  (it "uses the program vars for the dune-utop flavor"
    (let ((neocaml-repl-flavor 'dune-utop)
          (neocaml-repl-program-name "dune")
          (neocaml-repl-program-args '("utop" ".")))
      (expect (neocaml-repl--command) :to-equal '("dune" "utop" ".")))))

;;; neocaml-repl-test.el ends here
