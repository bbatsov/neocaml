;;; neocaml-objinfo.el --- View OCaml compiled artifacts via ocamlobjinfo -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides a major mode for viewing OCaml compiled
;; artifacts (.cmi, .cmo, .cmx, .cma, .cmxa, .cmxs, .cmt, .cmti)
;; using the `ocamlobjinfo' command-line tool.
;;
;; When you open a compiled artifact, the binary content is replaced
;; with the human-readable output of `ocamlobjinfo'.  Press `g' to
;; refresh after recompilation.

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

(defgroup neocaml-objinfo nil
  "Viewing OCaml compiled artifact information."
  :prefix "neocaml-objinfo-"
  :group 'neocaml)

(defcustom neocaml-objinfo-program "ocamlobjinfo"
  "Program name for running ocamlobjinfo.
Can be set to a wrapper script, e.g. \"opam exec -- ocamlobjinfo\"."
  :type 'string
  :group 'neocaml-objinfo
  :package-version '(neocaml . "0.5.0"))

(defcustom neocaml-objinfo-program-args
  '("-no-approx" "-no-code" "-shape" "-index" "-decls" "-uid-deps")
  "Arguments passed to `neocaml-objinfo-program'.
The flags -no-approx and -no-code suppress verbose output from
.cmx files.  The remaining flags request additional metadata from
.cmt/.cmti files and are silently ignored for other file types."
  :type '(repeat string)
  :group 'neocaml-objinfo
  :package-version '(neocaml . "0.5.0"))

(defvar-local neocaml-objinfo--file nil
  "The compiled artifact file being displayed.")

(defconst neocaml-objinfo--unit-re
  (rx bol (or "Name: " "Unit name: " "Cmt unit name: "))
  "Regexp matching the start of a compilation unit in ocamlobjinfo output.")

(defvar neocaml-objinfo-font-lock-keywords
  `(;; Section headers: "Unit name:", "Interfaces imported:", etc.
    (,(rx bol (group (any alpha) (+ (any alnum blank ?_ ?-))) ":")
     (1 font-lock-keyword-face))
    ;; Unit name value (after "Name:", "Unit name:", "Cmt unit name:")
    (,(rx bol (or "Name: " "Unit name: " "Cmt unit name: ")
          (group (+ (any alnum ?_))))
     (1 font-lock-type-face))
    ;; CRC hashes (32-char hex or dashes)
    (,(rx bol "\t" (group (= 32 (any hex-digit ?-))) "\t")
     (1 font-lock-comment-face))
    ;; Module names after CRC hash
    (,(rx "\t" (group upper (* (any alnum ?_))) eol)
     (1 font-lock-type-face)))
  "Font-lock keywords for `neocaml-objinfo-mode'.")

(defun neocaml-objinfo--run (file)
  "Run ocamlobjinfo on FILE and fill the current buffer with output."
  (unless (executable-find neocaml-objinfo-program)
    (user-error "`%s' not found in PATH" neocaml-objinfo-program))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((exit-code
           (apply #'call-process neocaml-objinfo-program nil t nil
                  (append neocaml-objinfo-program-args (list file)))))
      (unless (zerop exit-code)
        (goto-char (point-min))
        (insert (format "%s exited with code %d\n\n"
                        neocaml-objinfo-program exit-code))))
    (goto-char (point-min))))

(defun neocaml-objinfo-revert (&optional _ignore-auto _noconfirm)
  "Re-run ocamlobjinfo on the original file."
  (interactive)
  (when neocaml-objinfo--file
    (neocaml-objinfo--run neocaml-objinfo--file)
    (font-lock-flush)
    (message "Refreshed objinfo for %s"
             (file-name-nondirectory neocaml-objinfo--file))))

(defun neocaml-objinfo-next-unit ()
  "Move to the next compilation unit."
  (interactive)
  (end-of-line)
  (if (re-search-forward neocaml-objinfo--unit-re nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (message "No more units")))

(defun neocaml-objinfo-previous-unit ()
  "Move to the previous compilation unit."
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward neocaml-objinfo--unit-re nil t)
    (goto-char (point-min))
    (message "No previous unit")))

(defvar neocaml-objinfo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'neocaml-objinfo-revert)
    (define-key map "n" #'neocaml-objinfo-next-unit)
    (define-key map "p" #'neocaml-objinfo-previous-unit)
    map)
  "Keymap for `neocaml-objinfo-mode'.")

;;;###autoload
(define-derived-mode neocaml-objinfo-mode special-mode "OCamlobjinfo"
  "Major mode for viewing ocamlobjinfo output on compiled OCaml artifacts.

When activated on a buffer visiting a compiled OCaml file (.cmi, .cmo,
.cmx, etc.), replaces the binary content with the human-readable output
of `ocamlobjinfo'.

Press \\`g' to refresh after recompilation, \\`q' to quit.

\\{neocaml-objinfo-mode-map}"
  (setq-local font-lock-defaults '(neocaml-objinfo-font-lock-keywords))
  (setq-local revert-buffer-function #'neocaml-objinfo-revert)
  (setq-local imenu-generic-expression
              `((nil ,(rx bol (or "Name: " "Unit name: " "Cmt unit name: ")
                         (group (+ (any alnum ?_))))
                     1)))
  (buffer-disable-undo)
  (when buffer-file-name
    (setq neocaml-objinfo--file buffer-file-name)
    (neocaml-objinfo--run buffer-file-name)
    (setq buffer-file-name nil
          buffer-offer-save nil)))

;;;###autoload
(defun neocaml-objinfo-view-file (file)
  "View ocamlobjinfo output for FILE in a dedicated buffer."
  (interactive "fOCaml compiled file: ")
  (let* ((file (expand-file-name file))
         (name (format "*objinfo: %s*" (file-name-nondirectory file)))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (neocaml-objinfo-mode)
      (setq neocaml-objinfo--file file)
      (neocaml-objinfo--run file))
    (pop-to-buffer buf)))

;;;###autoload
(progn
  (dolist (ext '("\\.cmi\\'" "\\.cmo\\'" "\\.cmx\\'" "\\.cma\\'"
                 "\\.cmxa\\'" "\\.cmxs\\'" "\\.cmt\\'" "\\.cmti\\'"))
    (add-to-list 'auto-mode-alist (cons ext 'neocaml-objinfo-mode))))

(provide 'neocaml-objinfo)

;;; neocaml-objinfo.el ends here
