;;; neocaml-menhir.el --- Major mode for Menhir files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing Menhir (.mly) parser
;; definition files.  Provides font-lock for the parser DSL (rules,
;; declarations, tokens, priorities) and, when the OCaml tree-sitter
;; grammar is installed, full syntax highlighting for embedded OCaml
;; code inside { } and %{ %} blocks via language injection.
;;
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/Kerl13/tree-sitter-menhir.

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

(declare-function neocaml-mode--font-lock-settings "neocaml")

(defgroup neocaml-menhir nil
  "Major mode for editing Menhir files with tree-sitter."
  :prefix "neocaml-menhir-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-menhir-indent-offset 2
  "Number of spaces for each indentation step in `neocaml-menhir-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-menhir
  :package-version '(neocaml . "0.7.0"))

;;; Grammar installation

(defconst neocaml-menhir-grammar-recipes
  '((menhir "https://github.com/Kerl13/tree-sitter-menhir"
            "v0.4.0"
            "src"))
  "Tree-sitter grammar recipe for Menhir files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-menhir-install-grammar (&optional force)
  "Install the Menhir tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'menhir nil)))
    (message "Installing Menhir tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-menhir-grammar-recipes))
      (treesit-install-language-grammar 'menhir))))

;;; Font-lock

(defvar neocaml-menhir--font-lock-settings
  (treesit-font-lock-rules
   :language 'menhir
   :feature 'comment
   '((comment) @font-lock-comment-face
     (line_comment) @font-lock-comment-face
     (ocaml_comment) @font-lock-comment-face)

   :language 'menhir
   :feature 'keyword
   '(["%token" "%type" "%start" "%on_error_reduce"
      "%left" "%right" "%nonassoc"
      "%parameter" "%attribute"
      "%public" "%inline"
      "%%"]
     @font-lock-keyword-face
     (priority_keyword) @font-lock-keyword-face)

   :language 'menhir
   :feature 'definition
   '((old_rule (symbol) @font-lock-function-name-face)
     (new_rule (lid) @font-lock-function-name-face)
     ;; Token declarations
     (terminal_alias_attrs (uid) @font-lock-constant-face)
     ;; Non-terminal names in %type/%start
     (non_terminal (lid) @font-lock-function-name-face))

   :language 'menhir
   :feature 'type
   '((type (ocaml_type) @font-lock-type-face))

   :language 'menhir
   :feature 'variable
   '(;; Binding names in producers (e = expr)
     (producer (lid) @font-lock-variable-name-face)
     ;; Symbol references in productions
     (symbol (uid) @font-lock-constant-face))

   :language 'menhir
   :feature 'operator
   '(["=" "|" "~"] @font-lock-operator-face)

   :language 'menhir
   :feature 'bracket
   '(["(" ")" "{" "}" "%{" "%}"] @font-lock-bracket-face)

   :language 'menhir
   :feature 'delimiter
   '([":" "," ";"] @font-lock-delimiter-face))
  "Font-lock settings for `neocaml-menhir-mode'.")

(defun neocaml-menhir--font-lock-settings ()
  "Return font-lock settings for `neocaml-menhir-mode'.
When the OCaml tree-sitter grammar is available, includes font-lock
rules for embedded OCaml code inside action blocks via language injection."
  (append
   neocaml-menhir--font-lock-settings
   (when (treesit-language-available-p 'ocaml)
     (require 'neocaml)
     (neocaml-mode--font-lock-settings 'ocaml))))

(defun neocaml-menhir--range-settings ()
  "Return range settings for embedded OCaml code injection.
Returns nil if the OCaml tree-sitter grammar is not available."
  (when (treesit-language-available-p 'ocaml)
    (treesit-range-rules
     :embed 'ocaml
     :host 'menhir
     :local t
     '((ocaml) @capture))))

;;; Indentation

(defvar neocaml-menhir--indent-rules
  `((menhir
     ((parent-is "source_file") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is "%}") column-0 0)
     ;; Don't reindent inside OCaml code blocks
     ((parent-is "ocaml") no-indent 0)
     ((parent-is "ocaml_type") no-indent 0)
     ;; Production cases (| pattern { action })
     ((node-is "production_group") parent-bol neocaml-menhir-indent-offset)
     ;; Inside a rule
     ((parent-is "old_rule") parent-bol neocaml-menhir-indent-offset)
     ((parent-is "new_rule") parent-bol neocaml-menhir-indent-offset)
     ;; Inside production groups
     ((parent-is "production_group") parent-bol neocaml-menhir-indent-offset)
     ;; Header/trailer content
     ((parent-is "header") parent-bol neocaml-menhir-indent-offset)
     ((parent-is "postlude") column-0 0)
     ;; Catch-all
     (no-node parent-bol neocaml-menhir-indent-offset)))
  "Indentation rules for `neocaml-menhir-mode'.")

;;; Imenu

(defvar neocaml-menhir--imenu-settings
  '(("Rule" "\\`old_rule\\'" nil nil)
    ("Rule (new)" "\\`new_rule\\'" nil nil))
  "Imenu settings for `neocaml-menhir-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Navigation

(defun neocaml-menhir--defun-name (node)
  "Return a name for NODE suitable for imenu and which-func."
  (pcase (treesit-node-type node)
    ("old_rule"
     (let ((sym (treesit-node-child-by-field-name node "symbol")))
       (unless sym
         (setq sym (treesit-search-subtree node "symbol" nil nil 1)))
       (when sym (treesit-node-text sym t))))
    ("new_rule"
     (let ((name (treesit-search-subtree node "lid" nil nil 1)))
       (when name (treesit-node-text name t))))))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-menhir-mode prog-mode "Menhir"
  "Major mode for editing Menhir parser definition files.

When the OCaml tree-sitter grammar is installed, embedded OCaml
code inside { } and %{ %} blocks gets full syntax highlighting
via language injection.

\\{neocaml-menhir-mode-map}"
  (unless (treesit-ready-p 'menhir)
    (when (y-or-n-p "Menhir tree-sitter grammar is not installed.  Install it now?")
      (neocaml-menhir-install-grammar))
    (unless (treesit-ready-p 'menhir)
      (error "Cannot activate neocaml-menhir-mode without the Menhir grammar")))
  (treesit-parser-create 'menhir)

  ;; Comments (Menhir supports OCaml, C, and line comment styles)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+ *\\|//+ *\\|/\\*+ *")

  ;; Language injection for embedded OCaml code
  (let ((range-settings (neocaml-menhir--range-settings)))
    (when range-settings
      (setq-local treesit-range-settings range-settings)))

  ;; Font-lock
  (setq-local treesit-font-lock-settings (neocaml-menhir--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (constant escape-sequence attribute builtin number)
                (variable operator bracket delimiter property label function)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-menhir--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-menhir--imenu-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              "\\`\\(?:old_rule\\|new_rule\\)\\'")
  (setq-local treesit-defun-name-function #'neocaml-menhir--defun-name)
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mly\\'" . neocaml-menhir-mode))

(provide 'neocaml-menhir)

;;; neocaml-menhir.el ends here
