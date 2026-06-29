;;; neocaml-utop.el --- utop toplevel integration via its Emacs protocol -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An alternative OCaml toplevel integration that drives `utop' through
;; its native editor protocol (`utop -emacs') instead of treating it as a
;; plain comint stream like `neocaml-repl' does.
;;
;; In `-emacs' mode utop does no rendering of its own: it speaks a small
;; line-based protocol of `command:argument' messages over stdin/stdout.
;; This lets neocaml present a proper transcript while getting things a
;; raw stream cannot provide:
;;
;; - stdout, stderr, results, errors and warnings arrive already
;;   separated, so no regexp sniffing is needed to tell them apart;
;; - parse and type errors come with character ranges into the submitted
;;   phrase, which we map back onto the *source* buffer and underline
;;   precisely (see `neocaml-utop-error-face');
;; - completion is answered by utop's own engine, wired here into
;;   `completion-at-point'.
;;
;; The transcript buffer is a `comint' derivative, so input editing,
;; history (with optional persistence) and read-only prompts come for
;; free; only the transport is custom (a preoutput filter that parses the
;; protocol and an input sender that emits it).
;;
;; This module is independent of `neocaml-repl': use whichever you
;; prefer.  `neocaml-repl' remains the right choice for the plain `ocaml'
;; toplevel, which has no protocol of its own.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'comint)
(require 'pulse)
(require 'subr-x)
(require 'neocaml)
(require 'neocaml-common)
;; Reused for the transport-agnostic source-side helpers (project naming
;; and `;;'-delimited phrase bounds); `neocaml-repl' itself is untouched.
(require 'neocaml-repl)

(defgroup neocaml-utop nil
  "OCaml toplevel integration via utop's Emacs protocol."
  :prefix "neocaml-utop-"
  :group 'neocaml)

(defcustom neocaml-utop-program "utop"
  "Program name used to invoke utop in protocol mode.
The `-emacs' flag is always appended by neocaml."
  :type 'string
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-program-args nil
  "Extra command-line arguments passed to utop before `-emacs'.
For the `dune-utop' flavor these are passed to the utop that `dune utop'
launches (i.e. after the `--' separator)."
  :type '(repeat string)
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-flavor 'utop
  "Which utop toplevel `neocaml-utop' launches.
- `utop': run `neocaml-utop-program' directly.
- `dune-utop': run `dune utop' for the current project, so the project's
  libraries are available in the toplevel.

Set this globally or per project via a `.dir-locals.el' file."
  :type '(choice (const :tag "utop" utop)
                 (const :tag "dune utop" dune-utop))
  :safe (lambda (v) (memq v '(utop dune-utop)))
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-buffer-name "*utop*"
  "Base name of the utop transcript buffer.
Per-project sessions derive their name from this (e.g. \"*utop: proj*\")."
  :type 'string
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-history-file
  (expand-file-name "neocaml-utop-history" user-emacs-directory)
  "File to persist utop input history across sessions.
Set to nil to disable history persistence."
  :type '(choice (file :tag "History file")
                 (const :tag "Disable" nil))
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-history-size 1000
  "Maximum number of input history entries to persist."
  :type 'integer
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-completion-at-point t
  "When non-nil, offer utop-backed completion in the transcript buffer.
Completion candidates come from utop's own completion engine via the
protocol's `complete-company' request."
  :type 'boolean
  :package-version '(neocaml . "0.10.0"))

(defcustom neocaml-utop-echo-eval-result t
  "When non-nil, echo the result of a source evaluation in the minibuffer.
This mirrors a SLIME/CIDER-style flow: sending a phrase or definition
from a source buffer shows its value (or error) in the echo area, while
the full output still goes to the transcript buffer."
  :type 'boolean
  :package-version '(neocaml . "0.10.0"))

(defface neocaml-utop-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t :inherit error))
  "Face used to underline utop error spans in the source buffer."
  :group 'neocaml-utop)

(defconst neocaml-utop--prompt-regexp "^utop\\[[0-9]+\\]> "
  "Regexp matching the prompt neocaml-utop renders for utop.")

(defvar neocaml-utop-font-lock-keywords
  '(("\\(val\\) \\([^:]*\\)\\( *:\\)"
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
    ("\\(type\\) \\([^ =]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-type-face)))
  "Font-lock keywords for utop result lines in the transcript buffer.")

;;;; Buffer-local state
;;
;; All of these live in the transcript buffer; the preoutput filter runs
;; there, so it reads and writes them directly.

(defvar-local neocaml-utop--partial ""
  "Incomplete trailing protocol line buffered between filter calls.")

(defvar-local neocaml-utop--count -1
  "Number of prompts shown so far, used to number the prompt.")

(defvar-local neocaml-utop--error-target nil
  "Where to map error offsets for the phrase currently being evaluated.
A list (BUFFER START END): error character offsets reported by utop are
relative to buffer position START in BUFFER, and overlays are clamped to
END (the end of the submitted phrase) so the `;;' we append can't push a
span past the source.  Nil when the last input was not source-derived
\(e.g. `#require'/`#use'), so no overlay is drawn.")

(defvar-local neocaml-utop--overlays nil
  "Error overlays currently shown for the last evaluated phrase.")

(defvar-local neocaml-utop--echo nil
  "When non-nil, capture this evaluation's output for minibuffer echo.
Set when a source evaluation is sent; cleared when its terminating
`prompt' arrives and the result has been echoed.")

(defvar-local neocaml-utop--echo-lines nil
  "Accumulated (KIND . TEXT) output lines for the current echo capture.
KIND is `out' (stdout) or `err' (stderr); the list is reversed.")

(defvar-local neocaml-utop--echo-timer nil
  "Debounce timer that flushes the echo capture once output settles.")

(defvar-local neocaml-utop--completions nil
  "Completion candidates accumulated from the current `complete' request.")

(defvar-local neocaml-utop--completion-state nil
  "State of an in-flight completion request: `collecting', `done', or nil.")

(defvar-local neocaml-utop--source-buffer nil
  "Source buffer that last switched to this transcript.")

(defvar-local neocaml-utop--command-line nil
  "The (PROGRAM ARG...) this transcript was launched with.
Recorded so `neocaml-utop-restart' relaunches the same toplevel even
though it runs from the transcript buffer, which lacks the source's
directory-local `neocaml-utop-flavor'.")

;;;; Session naming and command

(defun neocaml-utop--buffer-name ()
  "Return the transcript buffer name for the current context.
In a transcript buffer that is the buffer itself; in a source buffer it
is the per-project session derived from `neocaml-utop-buffer-name'."
  (cond
   ((derived-mode-p 'neocaml-utop-mode) (buffer-name))
   ((neocaml-repl--project-id)
    (let ((base (if (string-suffix-p "*" neocaml-utop-buffer-name)
                    (substring neocaml-utop-buffer-name 0 -1)
                  neocaml-utop-buffer-name)))
      (format "%s: %s*" base (neocaml-repl--project-id))))
   (t neocaml-utop-buffer-name)))

(defun neocaml-utop--command ()
  "Return (PROGRAM ARG...) for launching utop per `neocaml-utop-flavor'."
  (pcase neocaml-utop-flavor
    ('dune-utop
     (let ((root (or (neocaml-common-dune-project-root) default-directory)))
       (append (list "dune" "utop" root "--")
               neocaml-utop-program-args (list "-emacs"))))
    (_ (append (list neocaml-utop-program)
               neocaml-utop-program-args (list "-emacs")))))

;;;; Error overlays

(defun neocaml-utop--clear-overlays ()
  "Remove the error overlays tracked in this transcript buffer."
  (mapc #'delete-overlay neocaml-utop--overlays)
  (setq neocaml-utop--overlays nil))

(defun neocaml-utop--handle-accept (arg)
  "Handle an `accept:ARG' message: ARG is empty on success.
Otherwise ARG is a comma-separated list of (a,b) character-offset pairs
into the submitted phrase, which we map onto the source via
`neocaml-utop--error-target' and underline.  Offsets are clamped to the
phrase bounds so the appended `;;' (or an end-of-input error) cannot
push a span past the source buffer."
  (neocaml-utop--clear-overlays)
  (when (and (not (string-empty-p arg)) neocaml-utop--error-target)
    (let* ((buf (nth 0 neocaml-utop--error-target))
           (start (nth 1 neocaml-utop--error-target))
           (end (nth 2 neocaml-utop--error-target))
           (nums (mapcar #'string-to-number (split-string arg "," t)))
           (overlays nil))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((limit (min end (point-max))))
            (while (>= (length nums) 2)
              (let* ((a (pop nums))
                     (b (pop nums))
                     ;; utop reports character offsets, so map by simple
                     ;; addition; clamp to the phrase to stay in bounds.
                     (beg (max start (min (+ start a) limit)))
                     (ovend (max start (min (+ start b) limit))))
                (when (< beg ovend)
                  (let ((ov (make-overlay beg ovend)))
                    (overlay-put ov 'face 'neocaml-utop-error-face)
                    (overlay-put ov 'help-echo "neocaml-utop: error here")
                    (push ov overlays))))))))
      ;; Store back in the transcript buffer (we're running in its filter).
      (setq neocaml-utop--overlays overlays))))

;;;; Result echo
;;
;; utop's evaluated value is written to its redirected stdout, which
;; reaches us on a different channel than the command messages and can
;; land just *after* the terminating `prompt' (errors, sent inline, land
;; before it).  So rather than bound the capture on the prompt, we
;; debounce: each output line refreshes a short timer, and the result is
;; echoed once output settles.

(defconst neocaml-utop--echo-settle-delay 0.2
  "Seconds of output quiet before a captured result is echoed.")

(defun neocaml-utop--echo-result (lines)
  "Echo captured LINES (each (KIND . TEXT)) in the minibuffer.
Error and warning text is highlighted; empty lines are dropped."
  (let* ((kept (seq-remove (lambda (l) (string-empty-p (cdr l))) lines))
         (text (mapconcat
                (lambda (l)
                  (if (eq (car l) 'err)
                      (propertize (cdr l) 'face 'font-lock-warning-face)
                    (cdr l)))
                kept "\n")))
    (unless (string-empty-p text)
      (message "%s" text))))

(defun neocaml-utop--echo-flush (buffer)
  "Echo and clear BUFFER's pending echo capture."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((lines (nreverse neocaml-utop--echo-lines)))
        (setq neocaml-utop--echo nil
              neocaml-utop--echo-lines nil
              neocaml-utop--echo-timer nil)
        (neocaml-utop--echo-result lines)))))

(defun neocaml-utop--echo-accumulate (kind text)
  "Record a TEXT line of KIND for echo and (re)arm the settle timer."
  (when neocaml-utop--echo
    (push (cons kind text) neocaml-utop--echo-lines)
    (when (timerp neocaml-utop--echo-timer)
      (cancel-timer neocaml-utop--echo-timer))
    (setq neocaml-utop--echo-timer
          (run-at-time neocaml-utop--echo-settle-delay nil
                       #'neocaml-utop--echo-flush (current-buffer)))))

;;;; Protocol parsing

(defun neocaml-utop--prompt-string ()
  "Return the prompt string for the next phrase, advancing the counter."
  (setq neocaml-utop--count (1+ neocaml-utop--count))
  (format "utop[%d]> " neocaml-utop--count))

(defun neocaml-utop--handle-line (line)
  "Interpret one protocol LINE and return the text to insert (maybe \"\")."
  (let* ((idx (string-search ":" line))
         (cmd (if idx (substring line 0 idx) line))
         (arg (if idx (substring line (1+ idx)) "")))
    (pcase cmd
      ("stdout"
       (neocaml-utop--echo-accumulate 'out arg)
       (concat arg "\n"))
      ("stderr"
       (neocaml-utop--echo-accumulate 'err arg)
       (concat (propertize arg 'face 'font-lock-warning-face) "\n"))
      ("prompt" (neocaml-utop--prompt-string))
      ("accept" (neocaml-utop--handle-accept arg) "")
      ("completion-start"
       (setq neocaml-utop--completions nil
             neocaml-utop--completion-state 'collecting)
       "")
      ("completion" (push arg neocaml-utop--completions) "")
      ("completion-stop" (setq neocaml-utop--completion-state 'done) "")
      ("no-such-package"
       (concat (propertize (format "No such package: %s" arg)
                           'face 'font-lock-warning-face)
               "\n"))
      ;; Control/metadata messages with nothing to display.
      ((or "protocol-version" "phrase-terminator" "continue"
           "history-data" "history-end" "history-bound")
       "")
      (_ ""))))

(defun neocaml-utop--preoutput (string)
  "Parse protocol STRING from utop and return the human-visible text.
Installed in `comint-preoutput-filter-functions'."
  (setq neocaml-utop--partial (concat neocaml-utop--partial string))
  (let ((lines (split-string neocaml-utop--partial "\n"))
        (out ""))
    ;; The last element is an incomplete line (or "" if STRING ended in \n).
    (setq neocaml-utop--partial (car (last lines)))
    (dolist (line (butlast lines))
      (setq out (concat out (neocaml-utop--handle-line line))))
    out))

;;;; Sending phrases

(defun neocaml-utop--send-data (proc text)
  "Send TEXT to PROC as a protocol data block (`data:' lines then `end:')."
  (dolist (line (split-string text "\n"))
    (process-send-string proc (concat "data:" line "\n")))
  (process-send-string proc "end:\n"))

(defun neocaml-utop--send-eval (proc text)
  "Send TEXT to PROC as a single phrase, appending `;;' when missing.
The terminator is required: without it utop reports a syntax error at
end of input rather than waiting for more."
  (let ((payload (if (string-suffix-p ";;" (string-trim-right text))
                     text
                   (concat text ";;"))))
    (process-send-string proc "input:add-to-history\n")
    (neocaml-utop--send-data proc payload)))

(defun neocaml-utop--input-sender (proc input)
  "Comint input sender: evaluate INPUT typed in the transcript.
Records the input's buffer span so error offsets land on it."
  (with-current-buffer (process-buffer proc)
    (let ((start (marker-position comint-last-input-start)))
      (setq neocaml-utop--error-target
            (list (current-buffer) start (+ start (length input))))))
  (neocaml-utop--send-eval proc input))

;;;; Completion

(defun neocaml-utop--complete (input)
  "Ask utop to complete INPUT and return the candidate list.
Blocks (briefly) waiting for the protocol round-trip."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (setq neocaml-utop--completions nil
            neocaml-utop--completion-state 'collecting)
      (process-send-string proc "complete-company:\n")
      (neocaml-utop--send-data proc input)
      (with-timeout (2 nil)
        (while (eq neocaml-utop--completion-state 'collecting)
          (accept-process-output proc 0.05)))
      (nreverse neocaml-utop--completions))))

(defun neocaml-utop-completion-at-point ()
  "Completion-at-point function backed by utop's completion engine.
Active only in the transcript buffer, on the current input line."
  (when (and neocaml-utop-completion-at-point
             (derived-mode-p 'neocaml-utop-mode))
    (let ((proc (get-buffer-process (current-buffer))))
      (when (and (process-live-p proc)
                 (>= (point) (comint-line-beginning-position)))
        (let ((input (buffer-substring-no-properties
                      (comint-line-beginning-position) (point))))
          (when (> (length (string-trim input)) 0)
            (let ((start (save-excursion
                           (skip-chars-backward "A-Za-z0-9_'")
                           (point)))
                  (candidates (neocaml-utop--complete input)))
              (when candidates
                (neocaml-common-capf start (point) candidates "utop" 'text)))))))))

;;;; Mode

(defvar neocaml-utop-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-z") #'neocaml-utop-switch-to-source)
    (easy-menu-define neocaml-utop-mode-menu map "utop Menu"
      '("utop"
        ["Switch to Source" neocaml-utop-switch-to-source
         :help "Switch back to the source buffer that last invoked utop"]
        "--"
        ["Interrupt" neocaml-utop-interrupt
         :enable (comint-check-proc (current-buffer))
         :help "Interrupt the utop process"]
        ["Restart" neocaml-utop-restart
         :help "Kill and restart the utop toplevel"]
        ["Clear Buffer" neocaml-utop-clear-buffer
         :help "Erase the transcript buffer contents"]
        "--"
        ["Customize utop..." (customize-group 'neocaml-utop)
         :help "Customize the neocaml-utop settings"]))
    map)
  "Keymap for `neocaml-utop-mode'.")

(define-derived-mode neocaml-utop-mode comint-mode "utop"
  "Major mode for the utop toplevel driven via its Emacs protocol.

\\{neocaml-utop-mode-map}"
  (setq-local comint-prompt-regexp neocaml-utop--prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-input-sender #'neocaml-utop--input-sender)
  (setq-local comint-process-echoes nil)
  (setq-local neocaml-utop--partial "")
  (setq-local neocaml-utop--count -1)
  (setq-local neocaml-utop--overlays nil)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local font-lock-defaults '(neocaml-utop-font-lock-keywords t))
  (add-hook 'comint-preoutput-filter-functions #'neocaml-utop--preoutput nil t)
  (add-hook 'completion-at-point-functions
            #'neocaml-utop-completion-at-point nil t)
  (setq-local prettify-symbols-alist (neocaml--prettify-symbols-alist))
  ;; Error overlays live in source buffers but are tracked here; drop them
  ;; when this transcript is killed so they don't linger on the source.
  (add-hook 'kill-buffer-hook #'neocaml-utop--clear-overlays nil t)
  (when neocaml-utop-history-file
    (setq-local comint-input-ring-file-name neocaml-utop-history-file)
    (setq-local comint-input-ring-size neocaml-utop-history-size)
    (setq-local comint-input-ignoredups t)
    (comint-read-input-ring t)
    (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)))

;;;; Lifecycle

(defun neocaml-utop--get-or-create (&optional command)
  "Return the transcript buffer for the current context, starting it if needed.
COMMAND, when given, is the (PROGRAM ARG...) to launch (used by
`neocaml-utop-restart').  Otherwise it is computed here, while the
current buffer is still the source one, so that directory-local settings
like `neocaml-utop-flavor' are honored."
  (let ((bufname (neocaml-utop--buffer-name))
        (command (or command (neocaml-utop--command))))
    (unless (comint-check-proc bufname)
      (with-current-buffer (get-buffer-create bufname)
        (unless (derived-mode-p 'neocaml-utop-mode)
          (neocaml-utop-mode))
        (setq neocaml-utop--partial "")
        (setq neocaml-utop--count -1)
        (setq neocaml-utop--command-line command)
        (apply #'make-comint-in-buffer "neocaml-utop" (current-buffer)
               (car command) nil (cdr command))))
    (get-buffer bufname)))

(defun neocaml-utop--ensure-process ()
  "Return a live utop process for the current context, starting one if needed."
  (get-buffer-process (neocaml-utop--get-or-create)))

(defun neocaml-utop--process ()
  "Return the running utop process for the current context, or nil."
  (get-buffer-process (neocaml-utop--buffer-name)))

(defun neocaml-utop--set-error-target (target)
  "Record TARGET in the transcript buffer for the current context.
TARGET is a (BUFFER START END) list, or nil when the next input is not
source-derived (so no error overlay should be drawn)."
  (when-let* ((buf (get-buffer (neocaml-utop--buffer-name))))
    (with-current-buffer buf
      (setq neocaml-utop--error-target target))))

(defun neocaml-utop--arm-echo ()
  "Begin capturing the next evaluation's output for minibuffer echo.
Honors `neocaml-utop-echo-eval-result'."
  (when-let* ((buf (get-buffer (neocaml-utop--buffer-name))))
    (with-current-buffer buf
      (when (timerp neocaml-utop--echo-timer)
        (cancel-timer neocaml-utop--echo-timer))
      (setq neocaml-utop--echo neocaml-utop-echo-eval-result
            neocaml-utop--echo-lines nil
            neocaml-utop--echo-timer nil))))

;;;###autoload
(defun neocaml-utop-run ()
  "Start a utop session for the current buffer's project, or switch to it."
  (interactive)
  (pop-to-buffer (neocaml-utop--get-or-create)))

;;;###autoload
(defun neocaml-utop-switch-to-utop ()
  "Switch to the utop transcript, saving the current buffer as the source.
Use \\[neocaml-utop-switch-to-source] in the transcript to return."
  (interactive)
  (let ((source (current-buffer))
        (buffer (neocaml-utop--get-or-create)))
    (with-current-buffer buffer
      (setq neocaml-utop--source-buffer source))
    (pop-to-buffer buffer)))

(defun neocaml-utop-switch-to-source ()
  "Switch from the transcript back to the source buffer that invoked it."
  (interactive)
  (if (and neocaml-utop--source-buffer
           (buffer-live-p neocaml-utop--source-buffer))
      (pop-to-buffer neocaml-utop--source-buffer)
    (message "No source buffer to return to")))

(defun neocaml-utop-interrupt ()
  "Interrupt the utop process for the current context."
  (interactive)
  (when-let* ((proc (neocaml-utop--process)))
    (interrupt-process proc)))

;;;###autoload
(defun neocaml-utop-restart ()
  "Kill and restart the utop toplevel for the current buffer's project.
The same toplevel command is reused, so a `dune-utop' session comes back
as `dune-utop' even though restart runs from the transcript buffer."
  (interactive)
  (let* ((bufname (neocaml-utop--buffer-name))
         (buf (get-buffer bufname))
         (command (and buf (buffer-local-value 'neocaml-utop--command-line buf))))
    (when (comint-check-proc bufname)
      (let ((proc (get-buffer-process bufname)))
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc)))
    (pop-to-buffer (neocaml-utop--get-or-create command))))

(defun neocaml-utop-clear-buffer ()
  "Erase the utop transcript buffer for the current context.
Works whether or not the toplevel is still running."
  (interactive)
  (when-let* ((buf (get-buffer (neocaml-utop--buffer-name))))
    (with-current-buffer buf
      (if (comint-check-proc buf)
          (comint-clear-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun neocaml-utop-quit ()
  "Ask the utop session to exit."
  (interactive)
  (when-let* ((proc (neocaml-utop--process)))
    (process-send-string proc "exit:0\n")))

;;;; Sending source to utop

;;;###autoload
(defun neocaml-utop-send-region (start end)
  "Send the region between START and END to utop for evaluation."
  (interactive "r")
  (let ((proc (neocaml-utop--ensure-process))
        (source (current-buffer)))
    (neocaml-utop--set-error-target (list source start end))
    (neocaml-utop--arm-echo)
    (neocaml-utop--send-eval proc (buffer-substring-no-properties start end))
    (pulse-momentary-highlight-region start end)))

;;;###autoload
(defun neocaml-utop-send-buffer ()
  "Send the entire buffer to utop."
  (interactive)
  (neocaml-utop-send-region (point-min) (point-max)))

;;;###autoload
(defun neocaml-utop-send-definition ()
  "Send the definition at point to utop."
  (interactive)
  (if-let* ((node (treesit-defun-at-point))
            (start (treesit-node-start node))
            (end (treesit-node-end node)))
      (neocaml-utop-send-region start end)
    (user-error "No definition at point")))

;;;###autoload
(defun neocaml-utop-send-phrase ()
  "Send the phrase at point (code between `;;') to utop."
  (interactive)
  (let ((bounds (neocaml-repl--phrase-bounds)))
    (neocaml-utop-send-region (car bounds) (cdr bounds))))

;;;###autoload
(defun neocaml-utop-send-phrase-and-step ()
  "Send the phrase at point to utop, then move to the next phrase."
  (interactive)
  (let ((bounds (neocaml-repl--phrase-bounds)))
    (neocaml-utop-send-region (car bounds) (cdr bounds))
    (goto-char (cdr bounds))
    (skip-chars-forward " \t\n")))

;;;###autoload
(defun neocaml-utop-load-file (file)
  "Load FILE into utop via the `#use' directive."
  (interactive (list (buffer-file-name)))
  (unless file
    (user-error "Buffer is not visiting a file"))
  (let ((proc (neocaml-utop--ensure-process)))
    ;; Not a source region: clear any stale target so a #use error can't
    ;; underline unrelated code in a previously evaluated buffer.
    (neocaml-utop--set-error-target nil)
    (neocaml-utop--send-eval proc (format "#use %S" file))))

;;;###autoload
(defun neocaml-utop-require (package)
  "Load the findlib PACKAGE into utop via the `#require' directive."
  (interactive (list (read-string "Require package: ")))
  (let ((proc (neocaml-utop--ensure-process)))
    (neocaml-utop--set-error-target nil)
    (neocaml-utop--send-eval proc (format "#require %S" package))))

;;;; Source-buffer minor mode

(defvar neocaml-utop-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'neocaml-utop-switch-to-utop)
    (define-key map (kbd "C-c C-c") #'neocaml-utop-send-definition)
    (define-key map (kbd "C-c C-r") #'neocaml-utop-send-region)
    (define-key map (kbd "C-c C-b") #'neocaml-utop-send-buffer)
    (define-key map (kbd "C-c C-p") #'neocaml-utop-send-phrase)
    (define-key map (kbd "C-c C-n") #'neocaml-utop-send-phrase-and-step)
    (define-key map (kbd "C-c C-l") #'neocaml-utop-load-file)
    (define-key map (kbd "C-c C-i") #'neocaml-utop-interrupt)
    (define-key map (kbd "C-c C-k") #'neocaml-utop-clear-buffer)
    (easy-menu-define neocaml-utop-minor-mode-menu map "utop Menu"
      '("utop"
        ["Start/Switch to utop" neocaml-utop-switch-to-utop
         :help "Start utop or switch to a running session"]
        "--"
        ["Send Definition" neocaml-utop-send-definition
         :help "Send the definition at point to utop"]
        ["Send Region" neocaml-utop-send-region
         :enable (use-region-p)
         :help "Send the active region to utop"]
        ["Send Buffer" neocaml-utop-send-buffer
         :help "Send the whole buffer to utop"]
        ["Send Phrase" neocaml-utop-send-phrase
         :help "Send the phrase at point to utop"]
        ["Send Phrase and Step" neocaml-utop-send-phrase-and-step
         :help "Send the phrase at point, then move to the next phrase"]
        ["Load File" neocaml-utop-load-file
         :help "Load a file into utop with #use"]
        ["Require Package..." neocaml-utop-require
         :help "Load a findlib package into utop with #require"]
        "--"
        ["Interrupt utop" neocaml-utop-interrupt
         :enable (comint-check-proc (neocaml-utop--buffer-name))
         :help "Interrupt the utop process"]
        ["Clear utop Buffer" neocaml-utop-clear-buffer
         :help "Erase the transcript buffer contents"]))
    map)
  "Keymap for `neocaml-utop-minor-mode'.")

;;;###autoload
(define-minor-mode neocaml-utop-minor-mode
  "Minor mode for interacting with utop via its Emacs protocol.

\\{neocaml-utop-minor-mode-map}"
  :init-value nil
  :lighter " utop"
  :keymap neocaml-utop-minor-mode-map
  :group 'neocaml-utop)

(provide 'neocaml-utop)

;;; neocaml-utop.el ends here
