;;; neocaml-repl.el --- OCaml toplevel integration for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides integration with the OCaml toplevel (REPL)
;; for the neocaml package.

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
(require 'neocaml)

(defgroup neocaml-repl nil
  "OCaml REPL (toplevel) integration for neocaml."
  :prefix "neocaml-repl-"
  :group 'neocaml)

(defcustom neocaml-repl-program-name "ocaml"
  "Program name for invoking an OCaml toplevel."
  :type 'string
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-repl-program-args '("-nopromptcont")
  "Command line arguments for `neocaml-repl-program-name'.

By default this passes \"-nopromptcont\" to the standard `ocaml` toplevel
to disable continuation prompts for multi-line input, which produces a
cleaner REPL experience in `comint' buffers.

If you use an alternative OCaml toplevel or REPL (for example, by setting
`neocaml-repl-program-name' to a program other than \"ocaml\"), that
program may not understand \"-nopromptcont\".  In such cases you should
customize this variable and remove or adapt the default flag so it matches
the command-line interface of the selected REPL."
  :type '(repeat string)
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-repl-buffer-name "*OCaml*"
  "Base name of the OCaml toplevel buffer.
Per-project REPLs derive their name from this (e.g. \"*OCaml: proj*\")."
  :type 'string
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-repl-flavor 'ocaml
  "Which OCaml toplevel `neocaml-repl' launches.
- `ocaml': the standard ocaml toplevel, using `neocaml-repl-program-name'
  and `neocaml-repl-program-args'.
- `utop': the utop toplevel.
- `dune-utop': `dune utop' for the current project (see `neocaml-dune-utop').

Set this globally or per project via a `.dir-locals.el' file; the REPL
reads it when it starts."
  :type '(choice (const :tag "Standard ocaml toplevel" ocaml)
                 (const :tag "utop" utop)
                 (const :tag "dune utop" dune-utop))
  :safe (lambda (v) (memq v '(ocaml utop dune-utop)))
  :group 'neocaml-repl
  :package-version '(neocaml . "0.9.0"))

(defcustom neocaml-repl-history-file
  (expand-file-name "neocaml-repl-history" user-emacs-directory)
  "File to persist OCaml REPL input history across sessions.
Set to nil to disable history persistence."
  :type '(choice (file :tag "History file")
                 (const :tag "Disable" nil))
  :group 'neocaml-repl
  :package-version '(neocaml . "0.2.0"))

(defcustom neocaml-repl-history-size 1000
  "Maximum number of input history entries to persist."
  :type 'integer
  :group 'neocaml-repl
  :package-version '(neocaml . "0.2.0"))

(defcustom neocaml-repl-fontify-input t
  "When non-nil, fontify REPL input using tree-sitter via `neocaml-mode'.
This uses `comint-fontify-input-mode' (Emacs 29.1+) to provide full
syntax highlighting for OCaml code you type in the REPL, while REPL
output (errors, warnings, values) keeps its own highlighting.

Set to nil to use only the basic REPL font-lock keywords for input."
  :type 'boolean
  :group 'neocaml-repl
  :package-version '(neocaml . "0.6.0"))

(defconst neocaml-repl--prompt-regexp
  "^\\(utop\\(\\[[0-9]+\\]\\)? \\)?# "
  "Regexp matching OCaml toplevel prompts.
Matches both the standard \"# \" prompt and utop's \"utop # \" / \"utop[0] # \".")

(defvar-local neocaml-repl--source-buffer nil
  "Source buffer from which the REPL was last invoked.
Used by `neocaml-repl-switch-to-source' to return to the source buffer.")

;;;; Per-project REPL buffers
;;
;; Each project gets its own dedicated REPL, so source files send to the
;; toplevel for their project.  The buffer name is derived from
;; `neocaml-repl-buffer-name' plus the project identifier; buffers outside
;; any project share the base name.

(defun neocaml-repl--project-id ()
  "Return a short identifier for the current buffer's project, or nil.
Uses `project.el' when available, falling back to the directory that
contains a `dune-project' file."
  (when-let* ((root (or (and (fboundp 'project-current)
                             (when-let* ((proj (project-current)))
                               (project-root proj)))
                        (locate-dominating-file default-directory "dune-project"))))
    (file-name-nondirectory (directory-file-name root))))

(defun neocaml-repl--buffer ()
  "Return the name of the REPL buffer for the current context.
In a REPL buffer, that is the buffer itself.  In a source buffer, it is
the per-project REPL derived from `neocaml-repl-buffer-name' and the
project identifier (or the base name when there is no project)."
  (cond
   ((derived-mode-p 'neocaml-repl-mode) (buffer-name))
   ((neocaml-repl--project-id)
    (let ((base (if (string-suffix-p "*" neocaml-repl-buffer-name)
                    (substring neocaml-repl-buffer-name 0 -1)
                  neocaml-repl-buffer-name)))
      (format "%s: %s*" base (neocaml-repl--project-id))))
   (t neocaml-repl-buffer-name)))

(defvar-local neocaml-repl--flavor nil
  "The flavor (toplevel kind) of this REPL buffer.
One of the symbols accepted by `neocaml-repl-flavor'.")

(defvar-local neocaml-repl--command-line nil
  "The (PROGRAM . ARGS) this REPL buffer was started with.
Recorded so `neocaml-repl-restart' can relaunch the same toplevel,
including the `dune-utop' command, which isn't reconstructible from the
global configuration.")

(defun neocaml-repl--command ()
  "Return (PROGRAM . ARGS) for the current `neocaml-repl-flavor'.
The `ocaml' and `dune-utop' flavors use `neocaml-repl-program-name' and
`neocaml-repl-program-args' (the latter is configured by
`neocaml-dune-utop'); `utop' uses the utop toplevel."
  (pcase neocaml-repl-flavor
    ('utop (cons "utop" nil))
    (_ (cons neocaml-repl-program-name neocaml-repl-program-args))))

(defun neocaml-repl--start-command (bufname command flavor)
  "Start a REPL in BUFNAME running COMMAND for FLAVOR, and return the buffer.
COMMAND is a (PROGRAM . ARGS) pair.  Records FLAVOR and COMMAND
buffer-locally so the REPL can be restarted as the same toplevel."
  (let ((buffer (apply #'make-comint-in-buffer "OCaml" bufname
                       (car command) nil (cdr command))))
    (with-current-buffer buffer
      (neocaml-repl-mode)
      (setq neocaml-repl--flavor flavor)
      (setq neocaml-repl--command-line command)
      (setq mode-name (format "OCaml-REPL[%s]" flavor)))
    buffer))

(defun neocaml-repl--kill (bufname)
  "Kill the REPL process running in BUFNAME, if any."
  (when (comint-check-proc bufname)
    (let ((proc (get-buffer-process bufname)))
      (when proc
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc)))))

(defvar neocaml-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-z") #'neocaml-repl-switch-to-source)
    (easy-menu-define neocaml-repl-mode-menu map "OCaml REPL Menu"
      '("OCaml REPL"
        ["Switch to Source" neocaml-repl-switch-to-source
         :help "Switch back to the source buffer that last invoked the REPL"]
        "--"
        ["Interrupt" neocaml-repl-interrupt
         :help "Interrupt the OCaml toplevel process"]
        ["Restart" neocaml-repl-restart
         :help "Kill and restart the OCaml toplevel"]
        ["Clear Buffer" neocaml-repl-clear-buffer
         :help "Erase the REPL buffer contents"]
        "--"
        ["Customize OCaml REPL..." (customize-group 'neocaml-repl)
         :help "Customize the OCaml REPL settings"]))
    map)
  "Keymap for `neocaml-repl-mode'.")

(defvar neocaml-repl-font-lock-keywords
  '(("^\\(# *\\)\\([^;]*\\)\\(;;\\)" (1 font-lock-comment-face) (3 font-lock-comment-face))
    ;; errors
    ("^\\(Error:\\)" (1 font-lock-warning-face))
    ;; warnings
    ("^\\(Warning:\\)" (1 font-lock-warning-face))
    ;; values and types from result
    ("\\(val\\) \\([^:]*\\)\\( *:\\)" (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
    ("\\(type\\) \\([^ =]*\\)" (1 font-lock-keyword-face) (2 font-lock-type-face))
    ;; prompt (standard "# " and utop's "utop # " / "utop[0] # ")
    (neocaml-repl--prompt-regexp . font-lock-comment-face))
  "Font-lock keywords for the OCaml REPL buffer.
Highlights prompts, errors, warnings, and toplevel response values.")

(define-derived-mode neocaml-repl-mode comint-mode "OCaml-REPL"
  "Major mode for interacting with an OCaml toplevel.

\\{neocaml-repl-mode-map}"
  (setq comint-prompt-regexp neocaml-repl--prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq comint-input-sender 'neocaml-repl--input-sender)
  (setq comint-process-echoes nil)
  ;; Strip ANSI escape sequences emitted by utop and other enhanced toplevels
  (ansi-color-for-comint-mode-on)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local font-lock-defaults '(neocaml-repl-font-lock-keywords t))

  ;; Add error navigation
  (setq-local compilation-error-regexp-alist
              '(("^\\(Error\\): \\(.+\\), line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" 2 3 4)
                ("^\\(Warning\\|Alert\\): \\(.+\\), line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" 2 3 4 1)))
  (compilation-shell-minor-mode)

  ;; Input history persistence
  (when neocaml-repl-history-file
    (setq comint-input-ring-file-name neocaml-repl-history-file)
    (setq comint-input-ring-size neocaml-repl-history-size)
    (setq comint-input-ignoredups t)
    (comint-read-input-ring t)
    (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t))

  ;; Setup prettify-symbols (users enable prettify-symbols-mode via hooks)
  (setq-local prettify-symbols-alist (neocaml--prettify-symbols-alist))

  ;; Tree-sitter fontification for REPL input
  (when neocaml-repl-fontify-input
    (setq-local comint-indirect-setup-function #'neocaml-mode)
    (comint-fontify-input-mode)))

(defun neocaml-repl--input-sender (proc input)
  "Send INPUT to PROC, appending `;;' terminator if missing.
Only checks whether the input ends with `;;' (ignoring trailing whitespace),
to avoid false positives from `;;' inside strings or comments."
  (let* ((trimmed (string-trim-right input))
         (terminated (string-suffix-p ";;" trimmed)))
    (comint-send-string proc (if terminated
                                 (concat input "\n")
                               (concat input "\n;;\n")))))

;;;###autoload
(defun neocaml-repl-start ()
  "Start an OCaml toplevel for the current buffer's project.
If a REPL for the project is already running, switch to its buffer."
  (interactive)
  (let ((bufname (neocaml-repl--buffer)))
    (if (comint-check-proc bufname)
        (pop-to-buffer bufname)
      (pop-to-buffer
       (neocaml-repl--start-command bufname (neocaml-repl--command)
                                    neocaml-repl-flavor)))))

(defun neocaml-repl-switch-to-source ()
  "Switch from the REPL back to the source buffer that last invoked it."
  (interactive)
  (if (and neocaml-repl--source-buffer
           (buffer-live-p neocaml-repl--source-buffer))
      (pop-to-buffer neocaml-repl--source-buffer)
    (message "No source buffer to return to")))

;;;###autoload
(defun neocaml-repl-switch-to-repl ()
  "Switch to the OCaml REPL, saving the current buffer as the source.
If a REPL is already running, switch to it; otherwise start a new one.
Use \\[neocaml-repl-switch-to-source] in the REPL to return."
  (interactive)
  (let* ((source (current-buffer))
         (bufname (neocaml-repl--buffer))
         (running (comint-check-proc bufname))
         (running-flavor (and running
                              (buffer-local-value 'neocaml-repl--flavor
                                                  (get-buffer bufname)))))
    (cond
     ;; A REPL with a different toplevel is running; offer to restart it
     ;; with the requested flavor (this is how `neocaml-dune-utop' and a
     ;; changed `neocaml-repl-flavor' take effect).
     ((and running (not (eq running-flavor neocaml-repl-flavor))
           (y-or-n-p (format "An existing %s REPL is running for this project; \
restart it as %s? " running-flavor neocaml-repl-flavor)))
      (neocaml-repl--kill bufname)
      (neocaml-repl-start))
     (running (pop-to-buffer bufname))
     (t (neocaml-repl-start)))
    (with-current-buffer bufname
      (setq neocaml-repl--source-buffer source))))

;;;###autoload
(defun neocaml-repl-send-region (start end)
  "Send the region between START and END to the OCaml REPL."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (neocaml-repl--ensure-repl-running)
    (neocaml-repl--input-sender (neocaml-repl--process) text)
    (pulse-momentary-highlight-region start end)))

;;;###autoload
(defun neocaml-repl-send-buffer ()
  "Send the entire buffer to the OCaml REPL."
  (interactive)
  (neocaml-repl-send-region (point-min) (point-max)))

;;;###autoload
(defun neocaml-repl-send-definition ()
  "Send the current definition to the OCaml REPL."
  (interactive)
  (if-let* ((node (treesit-defun-at-point))
            (start (treesit-node-start node))
            (end (treesit-node-end node)))
      (neocaml-repl-send-region start end)
    (user-error "No definition at point")))

(defun neocaml-repl--search-phrase-delimiter (direction)
  "Search for a `;;' phrase delimiter in DIRECTION (1 or -1).
Skips matches inside strings and comments.  Returns the position
after the delimiter (forward) or before it (backward), or nil."
  (let ((search-fn (if (> direction 0) #'search-forward #'search-backward))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (funcall search-fn ";;" nil t))
        (let ((state (syntax-ppss)))
          (unless (or (nth 3 state) (nth 4 state))
            (setq found (if (> direction 0) (point) (match-beginning 0)))))))
    found))

(defun neocaml-repl--phrase-bounds ()
  "Return (START . END) of the phrase at point, bounded by `;;'.
START falls back to `point-min' and END to `point-max'."
  (cons (or (neocaml-repl--search-phrase-delimiter -1) (point-min))
        (or (neocaml-repl--search-phrase-delimiter 1) (point-max))))

;;;###autoload
(defun neocaml-repl-send-phrase ()
  "Send the current phrase (code between `;;' delimiters) to the OCaml REPL.
Skips `;;' that appear inside strings or comments."
  (interactive)
  (let ((bounds (neocaml-repl--phrase-bounds)))
    (neocaml-repl-send-region (car bounds) (cdr bounds))))

;;;###autoload
(defun neocaml-repl-send-phrase-and-step ()
  "Send the phrase at point to the OCaml REPL, then move to the next phrase.
Like `neocaml-repl-send-phrase', but advances point past the closing
`;;' to the start of the following phrase, so repeated invocations walk
through the buffer."
  (interactive)
  (let ((bounds (neocaml-repl--phrase-bounds)))
    (neocaml-repl-send-region (car bounds) (cdr bounds))
    (goto-char (cdr bounds))
    (skip-chars-forward " \t\n")))

(defun neocaml-repl--process ()
  "Return the REPL process for the current context, or nil if not running."
  (get-buffer-process (neocaml-repl--buffer)))

(defun neocaml-repl--ensure-repl-running ()
  "Start a REPL for the current buffer's project if one is not running."
  (unless (comint-check-proc (neocaml-repl--buffer))
    (neocaml-repl-start)))

;;;###autoload
(defun neocaml-repl-load-file (file)
  "Load FILE into the OCaml REPL via the `#use' directive."
  (interactive (list (buffer-file-name)))
  (unless file
    (user-error "Buffer is not visiting a file"))
  (neocaml-repl--ensure-repl-running)
  (neocaml-repl--input-sender (neocaml-repl--process)
                               (format "#use %S" file)))

;;;###autoload
(defun neocaml-repl-require (package)
  "Load the findlib PACKAGE into the OCaml REPL via the `#require' directive.
This needs the toplevel to have findlib loaded (e.g. via topfind or utop)."
  (interactive (list (read-string "Require package: ")))
  (neocaml-repl--ensure-repl-running)
  (neocaml-repl--input-sender (neocaml-repl--process)
                               (format "#require %S" package)))

(defun neocaml-repl-clear-buffer ()
  "Clear the OCaml REPL buffer for the current context."
  (interactive)
  (with-current-buffer (neocaml-repl--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (comint-send-input))))

(defun neocaml-repl-interrupt ()
  "Interrupt the OCaml REPL process for the current context."
  (interactive)
  (when (comint-check-proc (neocaml-repl--buffer))
    (interrupt-process (neocaml-repl--process))))

;;;###autoload
(defun neocaml-repl-restart ()
  "Restart the OCaml toplevel for the current buffer's project.
Kill the running process, if any, and start a fresh one in the same
buffer, preserving the toplevel flavor it was launched with (so e.g. a
`dune-utop' REPL comes back as `dune-utop', not the default toplevel)."
  (interactive)
  (let* ((bufname (neocaml-repl--buffer))
         (buf (get-buffer bufname))
         (flavor (and buf (buffer-local-value 'neocaml-repl--flavor buf)))
         (command (and buf (buffer-local-value 'neocaml-repl--command-line buf))))
    (neocaml-repl--kill bufname)
    (if (and command flavor)
        (pop-to-buffer (neocaml-repl--start-command bufname command flavor))
      (neocaml-repl-start))))

;; Installation of the toplevel integration
(defvar neocaml-repl-mode-hook nil
  "Hook run after entering OCaml toplevel mode.")

(defvar neocaml-repl-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'neocaml-repl-switch-to-repl)
    (define-key map (kbd "C-c C-c") #'neocaml-repl-send-definition)
    (define-key map (kbd "C-c C-r") #'neocaml-repl-send-region)
    (define-key map (kbd "C-c C-b") #'neocaml-repl-send-buffer)
    (define-key map (kbd "C-c C-p") #'neocaml-repl-send-phrase)
    (define-key map (kbd "C-c C-n") #'neocaml-repl-send-phrase-and-step)
    (define-key map (kbd "C-c C-l") #'neocaml-repl-load-file)
    (define-key map (kbd "C-c C-i") #'neocaml-repl-interrupt)
    (define-key map (kbd "C-c C-k") #'neocaml-repl-clear-buffer)

    (easy-menu-define neocaml-repl-minor-mode-menu map "OCaml REPL Menu"
      '("OCaml REPL"
        ["Start/Switch to REPL" neocaml-repl-switch-to-repl
         :help "Start the OCaml REPL or switch to a running one"]
        "--"
        ["Send Definition" neocaml-repl-send-definition
         :help "Send the definition at point to the REPL"]
        ["Send Region" neocaml-repl-send-region
         :enable (use-region-p)
         :help "Send the active region to the REPL"]
        ["Send Buffer" neocaml-repl-send-buffer
         :help "Send the whole buffer to the REPL"]
        ["Send Phrase" neocaml-repl-send-phrase
         :help "Send the phrase at point to the REPL"]
        ["Send Phrase and Step" neocaml-repl-send-phrase-and-step
         :help "Send the phrase at point, then move to the next phrase"]
        ["Load File" neocaml-repl-load-file
         :help "Load a file into the REPL with #use"]
        ["Require Package..." neocaml-repl-require
         :help "Load a findlib package into the REPL with #require"]
        "--"
        ["Interrupt REPL" neocaml-repl-interrupt
         :enable (comint-check-proc (neocaml-repl--buffer))
         :help "Interrupt the OCaml toplevel process"]
        ["Clear REPL Buffer" neocaml-repl-clear-buffer
         :enable (comint-check-proc (neocaml-repl--buffer))
         :help "Erase the REPL buffer contents"]))
    map)
  "Keymap for OCaml toplevel integration.")

;;;###autoload
(define-minor-mode neocaml-repl-minor-mode
  "Minor mode for interacting with the OCaml toplevel.

\\{neocaml-repl-minor-mode-map}"
  :init-value nil
  :lighter " OCaml-REPL"
  :keymap neocaml-repl-minor-mode-map
  :group 'neocaml-repl)

(provide 'neocaml-repl)

;;; neocaml-repl.el ends here
