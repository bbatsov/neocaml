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

(defcustom neocaml-repl-program-args '()
  "Command line arguments for `neocaml-repl-program-name'."
  :type '(repeat string)
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-repl-buffer-name "*OCaml*"
  "Name of the OCaml toplevel buffer."
  :type 'string
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-repl-use-prettify-symbols nil
  "If non-nil, the REPL buffer will use `prettify-symbols-mode'."
  :type 'boolean
  :group 'neocaml-repl
  :package-version '(neocaml . "0.1.0"))

(defvar neocaml-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
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
    ;; prompt
    ("^# " . font-lock-comment-face))
  "Font-lock keywords for the OCaml REPL buffer.
Highlights prompts, errors, warnings, and toplevel response values.")

(define-derived-mode neocaml-repl-mode comint-mode "OCaml-REPL"
  "Major mode for interacting with an OCaml toplevel.

\\{neocaml-repl-mode-map}"
  (setq comint-prompt-regexp "^# ")
  (setq comint-prompt-read-only t)
  (setq comint-input-sender 'neocaml-repl--input-sender)
  (setq comint-process-echoes nil)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local font-lock-defaults '(neocaml-repl-font-lock-keywords t))

  ;; Add error navigation
  (setq-local compilation-error-regexp-alist
              '(("^\\(Error\\): \\(.+\\), line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" 2 3 4)
                ("^\\(Warning\\|Alert\\): \\(.+\\), line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" 2 3 4 1)))
  (compilation-shell-minor-mode)

  ;; Setup prettify-symbols if enabled
  (when neocaml-repl-use-prettify-symbols
    (setq-local prettify-symbols-alist neocaml-prettify-symbols-alist)
    (prettify-symbols-mode 1)))

(defun neocaml-repl--input-sender (proc input)
  "Send INPUT to PROC, appending `;;' terminator if missing."
  (let ((input-with-terminator (if (string-match-p ";;" (string-trim input))
                                   input
                                 (concat input "\n;;"))))
    (comint-send-string proc (concat input-with-terminator "\n"))))

;;;###autoload
(defun neocaml-repl-start ()
  "Start an OCaml toplevel process in a new buffer.
If a process is already running, switch to its buffer."
  (interactive)
  (if (comint-check-proc neocaml-repl-buffer-name)
      (pop-to-buffer neocaml-repl-buffer-name)
    (let* ((cmdlist (append (list neocaml-repl-program-name) neocaml-repl-program-args))
           (buffer (apply #'make-comint-in-buffer "OCaml" neocaml-repl-buffer-name
                         (car cmdlist) nil (cdr cmdlist))))
      (with-current-buffer buffer
        (neocaml-repl-mode))
      (pop-to-buffer buffer))))

;;;###autoload
(defun neocaml-repl-switch-to-repl ()
  "Switch to the OCaml REPL buffer if it exists, otherwise start a new one."
  (interactive)
  (if (get-buffer neocaml-repl-buffer-name)
      (pop-to-buffer neocaml-repl-buffer-name)
    (neocaml-repl-start)))

;;;###autoload
(defun neocaml-repl-send-region (start end)
  "Send the region between START and END to the OCaml REPL."
  (interactive "r")
  (let ((region (buffer-substring-no-properties start end)))
    (neocaml-repl--ensure-repl-running)
    (comint-send-string (get-buffer-process neocaml-repl-buffer-name) region)
    (comint-send-string (get-buffer-process neocaml-repl-buffer-name) "\n;;\n")))

;;;###autoload
(defun neocaml-repl-send-buffer ()
  "Send the entire buffer to the OCaml REPL."
  (interactive)
  (neocaml-repl-send-region (point-min) (point-max)))

;;;###autoload
(defun neocaml-repl-send-definition ()
  "Send the current definition to the OCaml REPL."
  (interactive)
  (if (treesit-available-p)
      (neocaml-repl--send-definition-treesit)
    (neocaml-repl--send-definition-fallback)))

(defun neocaml-repl--send-definition-treesit ()
  "Send the current definition to the OCaml REPL using tree-sitter."
  (let* ((node (treesit-defun-at-point))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    (when (and start end)
      (neocaml-repl-send-region start end))))

(defun neocaml-repl--send-definition-fallback ()
  "Send the current definition to the REPL using `beginning-of-defun'.
Used as a fallback when tree-sitter is not available."
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (neocaml-repl-send-region (point) end))))

;;;###autoload
(defun neocaml-repl-send-phrase ()
  "Send the current phrase (code up to next ;;) to the OCaml REPL."
  (interactive)
  (save-excursion
    (let ((end (save-excursion
                 (if (search-forward ";;" nil t)
                     (point)
                   (goto-char (point-max))
                   (point))))
          (start (save-excursion
                   (if (search-backward ";;" nil t)
                       (match-end 0)
                     (goto-char (point-min))
                     (point)))))
      (neocaml-repl-send-region start end))))

(defun neocaml-repl--ensure-repl-running ()
  "Start an OCaml REPL if one is not already running."
  (unless (comint-check-proc neocaml-repl-buffer-name)
    (neocaml-repl-start)))

(defun neocaml-repl-clear-buffer ()
  "Clear the OCaml REPL buffer."
  (interactive)
  (with-current-buffer neocaml-repl-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (comint-send-input))))

(defun neocaml-repl-interrupt ()
  "Interrupt the OCaml REPL process."
  (interactive)
  (when (comint-check-proc neocaml-repl-buffer-name)
    (interrupt-process (get-buffer-process neocaml-repl-buffer-name))))

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
    (define-key map (kbd "C-c C-i") #'neocaml-repl-interrupt)
    (define-key map (kbd "C-c C-k") #'neocaml-repl-clear-buffer)

    (easy-menu-define neocaml-repl-minor-mode-menu map "OCaml REPL Menu"
      '("OCaml REPL"
        ["Start/Switch to REPL" neocaml-repl-switch-to-repl]
        "--"
        ["Send Definition" neocaml-repl-send-definition]
        ["Send Region" neocaml-repl-send-region]
        ["Send Buffer" neocaml-repl-send-buffer]
        ["Send Phrase" neocaml-repl-send-phrase]
        "--"
        ["Interrupt REPL" neocaml-repl-interrupt]
        ["Clear REPL Buffer" neocaml-repl-clear-buffer]))
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
