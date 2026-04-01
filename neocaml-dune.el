;;; neocaml-dune.el --- Major mode for dune files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml dune

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing dune build files,
;; dune-project files, and dune-workspace files.
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-dune.

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

(require 'treesit)

(defgroup neocaml-dune nil
  "Major mode for editing dune files with tree-sitter."
  :prefix "neocaml-dune-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-dune-indent-offset 1
  "Number of spaces for each indentation step in `neocaml-dune-mode'.
Dune files conventionally use 1-space indentation."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-dune
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-dune-format-on-save nil
  "When non-nil, format the buffer with `dune format-dune-file' before saving."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-dune
  :package-version '(neocaml . "0.8.0"))

;;; Grammar installation

(defconst neocaml-dune-grammar-recipes
  '((dune "https://github.com/tmcgilchrist/tree-sitter-dune"
          "master"
          "src"))
  "Tree-sitter grammar recipe for dune files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-dune-install-grammar (&optional force)
  "Install the dune tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'dune nil)))
    (message "Installing dune tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-dune-grammar-recipes))
      (treesit-install-language-grammar 'dune))))

;;; Formatting

(defun neocaml-dune-format-buffer ()
  "Format the current buffer using `dune format-dune-file'.
Pipes the buffer content through the command and replaces the
buffer text with the formatted output, preserving point."
  (interactive)
  (let ((outbuf (generate-new-buffer " *neocaml-dune-format*"))
        (orig-point (point))
        (orig-window-start (window-start)))
    (unwind-protect
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              "dune" nil outbuf nil
                                              "format-dune-file")))
          (if (zerop exit-code)
              (progn
                (erase-buffer)
                (insert-buffer-substring outbuf)
                (goto-char (min orig-point (point-max)))
                (set-window-start (selected-window) orig-window-start))
            (user-error "Dune format-dune-file failed: %s"
                        (with-current-buffer outbuf
                          (string-trim (buffer-string))))))
      (kill-buffer outbuf))))

(defun neocaml-dune--format-before-save ()
  "Format the buffer before saving if `neocaml-dune-format-on-save' is non-nil."
  (when neocaml-dune-format-on-save
    (neocaml-dune-format-buffer)))

;;; Font-lock

(defvar neocaml-dune--font-lock-settings
  (treesit-font-lock-rules
   :language 'dune
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'dune
   :feature 'keyword
   '((stanza_name) @font-lock-keyword-face
     (action_name) @font-lock-keyword-face)

   :language 'dune
   :feature 'property
   '((field_name) @font-lock-property-name-face)

   :language 'dune
   :feature 'string
   '((quoted_string) @font-lock-string-face
     (multiline_string) @font-lock-string-face)

   :language 'dune
   :feature 'constant
   '(["true" "false"] @font-lock-constant-face)

   :language 'dune
   :feature 'type
   '((module_name) @font-lock-type-face
     (library_name) @font-lock-type-face
     (package_name) @font-lock-type-face
     (public_name) @font-lock-type-face)

   :language 'dune
   :feature 'operator
   '((blang_op) @font-lock-operator-face)

   :language 'dune
   :feature 'bracket
   '(["(" ")"] @font-lock-bracket-face))
  "Font-lock settings for `neocaml-dune-mode'.")

;;; Indentation

;; Known limitation: the grammar flattens field-value pairs into
;; direct stanza children (no wrapper node for e.g. "(action (run ...))").
;; This means field values on continuation lines indent at the same
;; level as the field name rather than one deeper:
;;   (action
;;   (run foo))     ; actual — both at stanza indent + 1
;; instead of the conventional:
;;   (action
;;    (run foo))    ; expected — value at stanza indent + 2
;; See https://github.com/tmcgilchrist/tree-sitter-dune/issues/9

(defvar neocaml-dune--indent-rules
  `((dune
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ;; Don't reindent inside strings
     ((parent-is "quoted_string") no-indent 0)
     ((parent-is "multiline_string") no-indent 0)
     ;; All parenthesized constructs: indent 1 from parent bol
     ((parent-is "stanza") parent-bol neocaml-dune-indent-offset)
     ((parent-is "sexp") parent-bol neocaml-dune-indent-offset)
     ((parent-is "action") parent-bol neocaml-dune-indent-offset)
     ((parent-is "blang") parent-bol neocaml-dune-indent-offset)
     ((parent-is "_list") parent-bol neocaml-dune-indent-offset)
     (no-node parent-bol neocaml-dune-indent-offset)))
  "Indentation rules for `neocaml-dune-mode'.")

;;; Imenu

(defvar neocaml-dune--imenu-settings
  '(("Stanza" "\\`stanza\\'" nil nil))
  "Imenu settings for `neocaml-dune-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Navigation

(defun neocaml-dune--defun-name (node)
  "Return a name for NODE suitable for imenu and which-func.
For stanzas, returns the stanza type and its name field if present."
  (let* ((first-child (treesit-node-child node 0 t))
         (stanza-name (when (and first-child
                                 (string= (treesit-node-type first-child) "stanza_name"))
                        (treesit-node-text first-child t))))
    (when stanza-name
      ;; Try to find a name-like field for a more descriptive label.
      ;; First check named fields, then scan for a (name ...) field.
      (let ((name-value
             (or (let ((n (treesit-node-child-by-field-name node "project_name")))
                   (when n (treesit-node-text n t)))
                 (let ((n (treesit-node-child-by-field-name node "alias_name")))
                   (when n (treesit-node-text n t)))
                 (let ((child first-child)
                       (result nil))
                   (while (and (not result) (setq child (treesit-node-next-sibling child t)))
                     (when (and (string= (treesit-node-type child) "field_name")
                                (string= (treesit-node-text child t) "name"))
                       (let ((next (treesit-node-next-sibling child t)))
                         (when next
                           (setq result (treesit-node-text next t))))))
                   result))))
        (if name-value
            (format "%s %s" stanza-name name-value)
          stanza-name)))))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-dune-mode prog-mode "dune"
  "Major mode for editing dune build files.

Supports dune, dune-project, and dune-workspace files.

\\{neocaml-dune-mode-map}"
  (when (< (treesit-library-abi-version) 15)
    (error "The dune grammar requires tree-sitter ABI version 15+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.25.0" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'dune)
    (when (y-or-n-p "Dune tree-sitter grammar is not installed.  Install it now?")
      (neocaml-dune-install-grammar))
    (unless (treesit-ready-p 'dune)
      (error "Cannot activate neocaml-dune-mode without the dune grammar")))
  (treesit-parser-create 'dune)

  ;; Comments
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")

  ;; Font-lock
  (setq-local treesit-font-lock-settings neocaml-dune--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword)
                (string property)
                (constant type)
                (operator bracket)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-dune--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-dune--imenu-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp "\\`stanza\\'")
  (setq-local treesit-defun-name-function #'neocaml-dune--defun-name)

  ;; which-func-mode / add-log integration
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Format on save
  (add-hook 'before-save-hook #'neocaml-dune--format-before-save nil t)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  (treesit-major-mode-setup))

(define-key neocaml-dune-mode-map (kbd "C-c C-f") #'neocaml-dune-format-buffer)

;;;###autoload
(progn
  ;; Matches "dune" files (e.g., src/dune) but not dune-project or dune-workspace
  (add-to-list 'auto-mode-alist '("/dune\\'" . neocaml-dune-mode))
  ;; dune-project and dune-workspace use the same grammar and mode
  (add-to-list 'auto-mode-alist '("/dune-project\\'" . neocaml-dune-mode))
  (add-to-list 'auto-mode-alist '("/dune-workspace\\'" . neocaml-dune-mode)))

(provide 'neocaml-dune)

;;; neocaml-dune.el ends here
