;;; neocaml-utop-test.el --- Tests for neocaml-utop -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-utop'.  The bulk of the suite exercises
;; the protocol layer (line parsing, the preoutput filter, the input
;; sender, prompt numbering, error-offset overlays and completion
;; accumulation) without spawning utop.  A final block runs against a
;; real `utop -emacs' process and is skipped when utop is not installed.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-utop)
(require 'cl-lib)

(describe "neocaml-utop prompt regex"
  (it "matches the numbered utop prompt"
    (expect (string-match-p neocaml-utop--prompt-regexp "utop[0]> ") :to-be-truthy)
    (expect (string-match-p neocaml-utop--prompt-regexp "utop[42]> ") :to-be-truthy))

  (it "anchors at beginning of line and ignores unrelated text"
    (expect (string-match-p neocaml-utop--prompt-regexp "  utop[0]> ") :not :to-be-truthy)
    (expect (string-match-p neocaml-utop--prompt-regexp "val x : int") :not :to-be-truthy)))

(describe "neocaml-utop command construction"
  (it "appends -emacs for the plain utop flavor"
    (let ((neocaml-utop-flavor 'utop)
          (neocaml-utop-program "utop")
          (neocaml-utop-program-args nil))
      (expect (neocaml-utop--command) :to-equal '("utop" "-emacs"))))

  (it "keeps extra program args before -emacs"
    (let ((neocaml-utop-flavor 'utop)
          (neocaml-utop-program "utop")
          (neocaml-utop-program-args '("-short-paths")))
      (expect (neocaml-utop--command) :to-equal '("utop" "-short-paths" "-emacs"))))

  (it "builds a dune utop command terminated by -emacs"
    (let* ((neocaml-utop-flavor 'dune-utop)
           (neocaml-utop-program-args nil)
           (cmd (neocaml-utop--command)))
      (expect (car cmd) :to-equal "dune")
      (expect (cadr cmd) :to-equal "utop")
      (expect (member "--" cmd) :to-be-truthy)
      (expect (car (last cmd)) :to-equal "-emacs"))))

(describe "neocaml-utop buffer naming"
  (it "uses the base name when there is no project"
    (spy-on 'neocaml-repl--project-id :and-return-value nil)
    (let ((neocaml-utop-buffer-name "*utop*"))
      (expect (neocaml-utop--buffer-name) :to-equal "*utop*")))

  (it "derives a per-project name from the base"
    (spy-on 'neocaml-repl--project-id :and-return-value "proj")
    (let ((neocaml-utop-buffer-name "*utop*"))
      (expect (neocaml-utop--buffer-name) :to-equal "*utop: proj*"))))

(describe "neocaml-utop input sender protocol"
  ;; Spy on `process-send-string' to capture the bytes sent to the process.
  (before-each (spy-on 'process-send-string))

  (cl-flet ((send (text)
              (neocaml-utop--send-eval 'fake-proc text)
              (mapcar (lambda (args) (nth 1 args))
                      (spy-calls-all-args 'process-send-string))))

    (it "wraps a phrase in input-multi/data/end, appending `;;'"
      (expect (send "let x = 1")
              :to-equal '("input-multi:\n"
                          "data:let x = 1;;\n"
                          "end:\n")))

    (it "does not double a present `;;'"
      (expect (send "let x = 1;;")
              :to-equal '("input-multi:\n"
                          "data:let x = 1;;\n"
                          "end:\n")))

    (it "sends every phrase of a multi-phrase region"
      ;; input-multi means both phrases evaluate, not just the first.
      (expect (send "let x = 1;; let y = 2;;")
              :to-equal '("input-multi:\n"
                          "data:let x = 1;; let y = 2;;\n"
                          "end:\n")))

    (it "splits a multi-line phrase into one data line per line"
      (expect (send "let x =\n  1 + 1")
              :to-equal '("input-multi:\n"
                          "data:let x =\n"
                          "data:  1 + 1;;\n"
                          "end:\n")))))

(describe "neocaml-utop protocol line handling"
  (it "renders stdout verbatim"
    (with-temp-buffer
      (expect (neocaml-utop--handle-line "stdout:val x : int = 2")
              :to-equal "val x : int = 2\n")))

  (it "faces stderr with the warning face"
    (with-temp-buffer
      (let ((out (neocaml-utop--handle-line "stderr:Error: boom")))
        (expect out :to-equal "Error: boom\n")
        (expect (get-text-property 0 'face out) :to-equal 'font-lock-warning-face))))

  (it "numbers prompts, advancing the counter"
    (with-temp-buffer
      (setq neocaml-utop--count -1)
      (expect (neocaml-utop--handle-line "prompt:") :to-equal "utop[0]> ")
      (expect (neocaml-utop--handle-line "prompt:") :to-equal "utop[1]> ")))

  (it "treats metadata messages as invisible"
    (with-temp-buffer
      (expect (neocaml-utop--handle-line "protocol-version:1") :to-equal "")
      (expect (neocaml-utop--handle-line "phrase-terminator:;;") :to-equal "")
      (expect (neocaml-utop--handle-line "continue:") :to-equal ""))))

(describe "neocaml-utop preoutput filter"
  (it "buffers a partial line until its newline arrives"
    (with-temp-buffer
      (setq neocaml-utop--partial "")
      (expect (neocaml-utop--preoutput "stdout:hel") :to-equal "")
      (expect (neocaml-utop--preoutput "lo\n") :to-equal "hello\n")))

  (it "handles several messages in one chunk"
    (with-temp-buffer
      (setq neocaml-utop--partial "" neocaml-utop--count -1)
      (expect (neocaml-utop--preoutput "stdout:a\nstdout:b\nprompt:\n")
              :to-equal "a\nb\nutop[0]> "))))

(describe "neocaml-utop completion accumulation"
  (it "collects candidates between start and stop"
    (with-temp-buffer
      (neocaml-utop--handle-line "completion-start:")
      (expect neocaml-utop--completion-state :to-equal 'collecting)
      (neocaml-utop--handle-line "completion:map")
      (neocaml-utop--handle-line "completion:map2")
      (neocaml-utop--handle-line "completion-stop:")
      (expect neocaml-utop--completion-state :to-equal 'done)
      ;; the collection order is an internal detail; assert the set
      (expect neocaml-utop--completions :to-have-same-items-as '("map" "map2")))))

(describe "neocaml-utop result echo"
  (cl-flet ((capture-message (thunk)
              (let (captured)
                (cl-letf (((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (when (stringp fmt)
                               (setq captured (apply #'format fmt args))))))
                  (funcall thunk))
                captured)))

    (it "captures output and echoes it when the capture is flushed"
      (with-temp-buffer
        (setq neocaml-utop--echo t neocaml-utop--echo-lines nil
              neocaml-utop--echo-timer nil)
        (neocaml-utop--handle-line "stdout:val x : int = 2")
        (expect neocaml-utop--echo-lines :to-equal '((out . "val x : int = 2")))
        ;; output arms a debounce timer; cancel it and flush synchronously
        (expect (timerp neocaml-utop--echo-timer) :to-be-truthy)
        (cancel-timer neocaml-utop--echo-timer)
        (expect (capture-message
                 (lambda () (neocaml-utop--echo-flush (current-buffer))))
                :to-equal "val x : int = 2")
        (expect neocaml-utop--echo :to-be nil)
        (expect neocaml-utop--echo-lines :to-be nil)))

    (it "echoes error text from stderr"
      (with-temp-buffer
        (setq neocaml-utop--echo t neocaml-utop--echo-lines nil
              neocaml-utop--echo-timer nil)
        (neocaml-utop--handle-line "stderr:Error: Unbound value foo")
        (when (timerp neocaml-utop--echo-timer)
          (cancel-timer neocaml-utop--echo-timer))
        (expect (substring-no-properties
                 (capture-message
                  (lambda () (neocaml-utop--echo-flush (current-buffer)))))
                :to-equal "Error: Unbound value foo")))

    (it "does not capture when disarmed"
      (with-temp-buffer
        (setq neocaml-utop--echo nil neocaml-utop--echo-lines nil
              neocaml-utop--echo-timer nil)
        (neocaml-utop--handle-line "stdout:val x : int = 2")
        (expect neocaml-utop--echo-lines :to-be nil)
        (expect neocaml-utop--echo-timer :to-be nil)
        (expect (capture-message
                 (lambda () (neocaml-utop--echo-flush (current-buffer))))
                :to-be nil)))))

(describe "neocaml-utop error overlays"
  (let (source transcript)
    (before-each
      (setq source (neocaml-test-track-buffer (generate-new-buffer "src"))
            transcript (neocaml-test-track-buffer (generate-new-buffer "tr"))))
    (after-each (neocaml-test-kill-tracked-buffers))

    (it "underlines the exact character range reported by accept"
      (with-current-buffer source (insert "let y = 1 + \"a\""))
      (with-current-buffer transcript
        ;; (BUFFER START END): offsets are relative to position 1
        (setq neocaml-utop--error-target (list source 1 999))
        (neocaml-utop--handle-accept "12,15")
        (expect (length neocaml-utop--overlays) :to-equal 1)
        (let ((ov (car neocaml-utop--overlays)))
          (expect (with-current-buffer source
                    (buffer-substring-no-properties
                     (overlay-start ov) (overlay-end ov)))
                  :to-equal "\"a\"")
          (expect (overlay-get ov 'face) :to-equal 'neocaml-utop-error-face))))

    (it "maps character offsets correctly across multibyte source"
      ;; utop reports character offsets, not bytes; a multibyte char before
      ;; the error span must not shift the overlay.
      (with-current-buffer source (insert "let x = \"héllo\" + 1"))
      (with-current-buffer transcript
        (setq neocaml-utop--error-target (list source 1 999))
        (neocaml-utop--handle-accept "8,15")
        (expect (length neocaml-utop--overlays) :to-equal 1)
        (let ((ov (car neocaml-utop--overlays)))
          (expect (with-current-buffer source
                    (buffer-substring-no-properties
                     (overlay-start ov) (overlay-end ov)))
                  :to-equal "\"héllo\""))))

    (it "does not crash or overshoot when offsets run past the phrase"
      ;; `let x = 1 +' + appended `;;' makes utop report accept:11,13; with
      ;; an 11-char source those offsets must clamp away rather than signal.
      (with-current-buffer source (insert "let x = 1 +"))
      (with-current-buffer transcript
        (setq neocaml-utop--error-target
              (list source 1 (with-current-buffer source (point-max))))
        (neocaml-utop--handle-accept "11,13")
        (expect neocaml-utop--overlays :to-be nil)))

    (it "clears overlays on a clean accept"
      (with-current-buffer source (insert "let y = 1 + \"a\""))
      (with-current-buffer transcript
        (setq neocaml-utop--error-target (list source 1 999))
        (neocaml-utop--handle-accept "12,15")
        (expect (length neocaml-utop--overlays) :to-equal 1)
        (neocaml-utop--handle-accept "")
        (expect neocaml-utop--overlays :to-be nil)))

    (it "attaches the following stderr text as the overlay tooltip"
      (with-current-buffer source (insert "let y = 1 + \"a\""))
      (with-current-buffer transcript
        (setq neocaml-utop--error-target (list source 1 999))
        (neocaml-utop--handle-accept "12,15")
        ;; the error message arrives next, on stderr
        (neocaml-utop--handle-line "stderr:Error: This expression has type string")
        (expect (overlay-get (car neocaml-utop--overlays) 'help-echo)
                :to-equal "Error: This expression has type string")))

    (it "clears an error overlay when the underlined text is edited"
      (with-current-buffer source (insert "let y = 1 + \"a\""))
      (with-current-buffer transcript
        (setq neocaml-utop--error-target (list source 1 999))
        (neocaml-utop--handle-accept "12,15"))
      (let ((ov (car (buffer-local-value 'neocaml-utop--overlays transcript))))
        (expect (overlay-buffer ov) :to-equal source)
        ;; edit inside the underlined span
        (with-current-buffer source
          (goto-char 14)
          (insert "X"))
        (expect (overlay-buffer ov) :to-be nil)))))

(describe "neocaml-utop inline result overlay"
  (let (source transcript)
    (before-each
      (setq source (neocaml-test-track-buffer (generate-new-buffer "src"))
            transcript (neocaml-test-track-buffer (generate-new-buffer "tr"))))
    (after-each (neocaml-test-kill-tracked-buffers))

    (it "shows the value as an after-string at the region's end of line"
      (with-current-buffer source (insert "let x = 1 + 1"))
      (with-current-buffer transcript
        (neocaml-utop--inline-result
         '((out . "val x : int = 2")) (list source 1 14) transcript)
        (expect (length neocaml-utop--result-overlays) :to-equal 1)
        (let ((ov (car neocaml-utop--result-overlays)))
          (expect (overlay-buffer ov) :to-equal source)
          (expect (overlay-get ov 'after-string)
                  :to-equal (propertize "  => val x : int = 2"
                                        'face 'neocaml-utop-result-face)))))

    (it "shows nothing for interactive input (target is the transcript)"
      (with-current-buffer transcript
        (insert "let x = 1 + 1")
        (neocaml-utop--inline-result
         '((out . "val x : int = 2")) (list transcript 1 14) transcript)
        (expect neocaml-utop--result-overlays :to-be nil)))

    (it "shows nothing when the result is only an error"
      (with-current-buffer source (insert "let x = 1"))
      (with-current-buffer transcript
        (neocaml-utop--inline-result
         '((err . "Error: boom")) (list source 1 10) transcript)
        (expect neocaml-utop--result-overlays :to-be nil)))))

(describe "neocaml-utop input completeness"
  (it "treats a `;;'-terminated phrase as complete"
    (expect (neocaml-utop--phrase-complete-p "let x = 1;;") :to-be-truthy)
    (expect (neocaml-utop--phrase-complete-p "let x = 1;;\n  ") :to-be-truthy))

  (it "treats a toplevel directive as complete"
    (expect (neocaml-utop--phrase-complete-p "#use \"foo.ml\"") :to-be-truthy)
    (expect (neocaml-utop--phrase-complete-p "  #require \"str\"") :to-be-truthy))

  (it "treats empty input as complete"
    (expect (neocaml-utop--phrase-complete-p "   ") :to-be-truthy))

  (it "treats an unterminated phrase as incomplete"
    (expect (neocaml-utop--phrase-complete-p "let x =") :not :to-be-truthy)
    (expect (neocaml-utop--phrase-complete-p "let x = 1") :not :to-be-truthy)))

(describe "neocaml-utop input fontification"
  (it "enables comint-fontify-input-mode when configured"
    (let ((neocaml-utop-fontify-input t)
          (neocaml-utop-history-file nil))
      (with-temp-buffer
        (neocaml-utop-mode)
        (expect comint-fontify-input-mode :to-be-truthy)
        (expect comint-indirect-setup-function :to-equal #'neocaml-mode))))

  (it "leaves input unfontified when disabled"
    (let ((neocaml-utop-fontify-input nil)
          (neocaml-utop-history-file nil))
      (with-temp-buffer
        (neocaml-utop-mode)
        (expect comint-fontify-input-mode :not :to-be-truthy)))))

;;;; Live integration against a real utop -emacs process

(describe "neocaml-utop live session"
  (before-all
    (unless (executable-find "utop")
      (signal 'buttercup-pending "utop not installed")))

  (after-each (neocaml-test-kill-tracked-buffers))

  (cl-flet ((contents (buf)
              (with-current-buffer buf
                (buffer-substring-no-properties (point-min) (point-max))))
            (wait (pred)
              (let ((end (+ (float-time) 20)))
                (while (and (not (funcall pred)) (< (float-time) end))
                  (accept-process-output nil 0.05))
                (funcall pred))))

    (it "evaluates a phrase and shows the result"
      ;; Bind the history file off so tests never touch the user's history.
      (let ((neocaml-utop-history-file nil)
            (source (neocaml-test-track-buffer (generate-new-buffer "src.ml")))
            transcript)
        (with-current-buffer source
          (insert "let x = 1 + 1")
          (setq transcript (neocaml-test-track-buffer (neocaml-utop--get-or-create)))
          (expect (wait (lambda () (string-match-p "utop\\[0\\]> " (contents transcript))))
                  :to-be-truthy)
          (neocaml-utop-send-region (point-min) (point-max))
          (expect (wait (lambda () (string-match-p "val x : int = 2" (contents transcript))))
                  :to-be-truthy))))

    (it "evaluates every phrase of a multi-phrase region"
      ;; Regression: with plain `input' only the first phrase ran.
      (let ((neocaml-utop-history-file nil)
            (source (neocaml-test-track-buffer (generate-new-buffer "src.ml")))
            transcript)
        (with-current-buffer source
          (insert "let x = 1;; let y = 2;;")
          (setq transcript (neocaml-test-track-buffer (neocaml-utop--get-or-create)))
          (expect (wait (lambda () (string-match-p "utop\\[0\\]> " (contents transcript))))
                  :to-be-truthy)
          (neocaml-utop-send-region (point-min) (point-max))
          (expect (wait (lambda ()
                          (and (string-match-p "val x : int = 1" (contents transcript))
                               (string-match-p "val y : int = 2" (contents transcript)))))
                  :to-be-truthy))))

    (it "echoes a source evaluation's result in the minibuffer"
      (let ((neocaml-utop-history-file nil)
            (neocaml-utop-echo-eval-result t)
            (source (neocaml-test-track-buffer (generate-new-buffer "src.ml")))
            transcript messages)
        (with-current-buffer source
          (insert "let x = 1 + 1")
          (setq transcript (neocaml-test-track-buffer (neocaml-utop--get-or-create)))
          (expect (wait (lambda () (string-match-p "utop\\[0\\]> " (contents transcript))))
                  :to-be-truthy)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when (stringp fmt) (push (apply #'format fmt args) messages)))))
            (neocaml-utop-send-region (point-min) (point-max))
            (wait (lambda ()
                    (seq-find (lambda (m) (string-match-p "val x : int = 2" m))
                              messages))))
          (expect (seq-find (lambda (m) (string-match-p "val x : int = 2" m)) messages)
                  :to-be-truthy))))

    (it "underlines a type error precisely in the source"
      (let ((neocaml-utop-history-file nil)
            (source (neocaml-test-track-buffer (generate-new-buffer "src.ml")))
            transcript)
        (with-current-buffer source
          (insert "let y = 1 + \"a\"")
          (setq transcript (neocaml-test-track-buffer (neocaml-utop--get-or-create)))
          (expect (wait (lambda () (string-match-p "utop\\[0\\]> " (contents transcript))))
                  :to-be-truthy)
          (neocaml-utop-send-region (point-min) (point-max))
          (expect (wait (lambda () (string-match-p "type int" (contents transcript))))
                  :to-be-truthy)
          (let ((errs (seq-filter
                       (lambda (o) (eq (overlay-get o 'face) 'neocaml-utop-error-face))
                       (overlays-in (point-min) (point-max)))))
            (expect (length errs) :to-equal 1)
            (expect (buffer-substring-no-properties
                     (overlay-start (car errs)) (overlay-end (car errs)))
                    :to-equal "\"a\"")))))

    (it "returns completion candidates from utop"
      (let ((neocaml-utop-history-file nil)
            (source (neocaml-test-track-buffer (generate-new-buffer "src.ml")))
            transcript)
        (with-current-buffer source
          (setq transcript (neocaml-test-track-buffer (neocaml-utop--get-or-create)))
          (expect (wait (lambda () (string-match-p "utop\\[0\\]> " (contents transcript))))
                  :to-be-truthy)
          (with-current-buffer transcript
            (goto-char (point-max))
            (let ((cands (neocaml-utop--complete "List.ma")))
              (expect (member "map" cands) :to-be-truthy))))))))

(provide 'neocaml-utop-test)

;;; neocaml-utop-test.el ends here
