;;; neocaml-odoc.el --- Major mode for odoc (.mld) files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml odoc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing odoc documentation (.mld)
;; files.  Provides font-lock for the odoc markup language including
;; headings, inline formatting (bold, italic, emphasis), code spans,
;; references, links, tags, lists, tables, and code blocks.  When the
;; OCaml tree-sitter grammar is installed, embedded OCaml code inside
;; {@ocaml[...]} blocks gets full syntax highlighting via language
;; injection.
;;
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-odoc.

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

(defgroup neocaml-odoc nil
  "Major mode for editing odoc files with tree-sitter."
  :prefix "neocaml-odoc-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-odoc-indent-offset 2
  "Number of spaces for each indentation step in `neocaml-odoc-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.8.0"))

;;; Grammar installation

(defconst neocaml-odoc-grammar-recipes
  '((odoc "https://github.com/tmcgilchrist/tree-sitter-odoc"
          "master"
          "src"))
  "Tree-sitter grammar recipe for odoc files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-odoc-install-grammar (&optional force)
  "Install the odoc tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'odoc nil)))
    (message "Installing odoc tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-odoc-grammar-recipes))
      (treesit-install-language-grammar 'odoc))))

;;; Font-lock

(defvar neocaml-odoc--font-lock-settings
  (treesit-font-lock-rules
   :language 'odoc
   :feature 'heading
   '((heading) @font-lock-function-name-face)

   :language 'odoc
   :feature 'tag
   '((tag_author) @font-lock-keyword-face
     (tag_param) @font-lock-keyword-face
     (tag_raise) @font-lock-keyword-face
     (tag_return) @font-lock-keyword-face
     (tag_see) @font-lock-keyword-face
     (tag_since) @font-lock-keyword-face
     (tag_before) @font-lock-keyword-face
     (tag_version) @font-lock-keyword-face
     (tag_deprecated) @font-lock-keyword-face
     (tag_canonical) @font-lock-keyword-face
     (tag_inline) @font-lock-keyword-face
     (tag_open) @font-lock-keyword-face
     (tag_closed) @font-lock-keyword-face
     (tag_hidden) @font-lock-keyword-face)

   :language 'odoc
   :feature 'tag
   '((param_name) @font-lock-variable-name-face
     (raise_name) @font-lock-variable-name-face
     (before_version) @font-lock-constant-face)

   :language 'odoc
   :feature 'markup
   '((bold) @font-lock-type-face
     (italic) @font-lock-variable-name-face
     (emphasis) @font-lock-variable-name-face)

   :language 'odoc
   :feature 'code
   '((code_span) @font-lock-string-face
     (code_block) @font-lock-string-face
     (code_block_with_lang (language) @font-lock-type-face)
     (verbatim_block) @font-lock-string-face)

   :language 'odoc
   :feature 'math
   '((math_span) @font-lock-number-face
     (math_block) @font-lock-number-face)

   :language 'odoc
   :feature 'reference
   '((simple_reference) @font-lock-constant-face
     (reference_with_text (reference_target) @font-lock-constant-face)
     (simple_link) @font-lock-constant-face
     (link_with_text (link_target) @font-lock-constant-face)
     (module_name) @font-lock-constant-face)

   :language 'odoc
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'odoc
   :feature 'markup
   '((superscript) @font-lock-type-face
     (subscript) @font-lock-type-face
     (raw_markup) @font-lock-preprocessor-face)

   :language 'odoc
   :feature 'list
   '((unordered_list) @font-lock-bracket-face
     (ordered_list) @font-lock-bracket-face)

   :language 'odoc
   :feature 'bracket
   '(["[" "]" "]}" "{" "}" "{b" "{i" "{e" "{^" "{_"
      "{!" "{{!" "{:" "{{:" "{%" "{m" "{math"
      "{[" "{v" "{ul" "{ol" "{li" "{table" "{tr" "{th" "{td" "{t"
      "{L" "{C" "{R" "{!modules:" "%}"]
     @font-lock-bracket-face))
  "Font-lock settings for `neocaml-odoc-mode'.")

(defun neocaml-odoc--injection-available-p ()
  "Non-nil if OCaml language injection is available.
Requires Emacs 30+ (for `treesit-range-rules' with `:embed') and
the OCaml tree-sitter grammar."
  (and (>= emacs-major-version 30)
       (treesit-language-available-p 'ocaml)))

(defun neocaml-odoc--font-lock-settings ()
  "Return font-lock settings for `neocaml-odoc-mode'.
When OCaml injection is available, includes font-lock rules for
embedded OCaml code inside {@ocaml[...]} blocks.  Otherwise,
code block content gets a plain string face as fallback."
  (append
   neocaml-odoc--font-lock-settings
   (if (neocaml-odoc--injection-available-p)
       (progn
         (require 'neocaml)
         (neocaml-mode--font-lock-settings 'ocaml))
     ;; No injection: highlight code_block_content as string
     (treesit-font-lock-rules
      :language 'odoc
      :feature 'code
      '((code_block_with_lang (code_block_content) @font-lock-string-face))))))

(defun neocaml-odoc--range-settings ()
  "Return range settings for embedded OCaml code injection.
Injects the OCaml parser into `{@ocaml[...]}' code blocks.
Returns nil if injection is not available."
  (when (neocaml-odoc--injection-available-p)
    (treesit-range-rules
     :embed 'ocaml
     :host 'odoc
     :local t
     '((code_block_with_lang
        (language) @_lang
        (code_block_content) @capture
        (:match "\\`ocaml\\'" @_lang))))))

;;; Indentation

(defvar neocaml-odoc--indent-rules
  `((odoc
     ((parent-is "document") column-0 0)
     ;; Don't reindent inside code/verbatim blocks
     ((parent-is "code_block") no-indent 0)
     ((parent-is "code_block_with_lang") no-indent 0)
     ((parent-is "verbatim_block") no-indent 0)
     ;; Lists
     ((parent-is "unordered_list") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "ordered_list") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "light_list") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "li_list_item") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "dash_list_item") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "light_list_item") parent-bol neocaml-odoc-indent-offset)
     ;; Tables
     ((parent-is "table_heavy") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "table_light") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "table_row") parent-bol neocaml-odoc-indent-offset)
     ;; Headings and tags
     ((parent-is "heading") parent-bol neocaml-odoc-indent-offset)
     ((parent-is "tag") parent-bol neocaml-odoc-indent-offset)
     ;; Catch-all
     (no-node parent-bol neocaml-odoc-indent-offset)))
  "Indentation rules for `neocaml-odoc-mode'.")

;;; Imenu

(defvar neocaml-odoc--imenu-settings
  '(("Heading" "\\`heading\\'" nil nil)
    ("Tag" "\\`tag_\\(?:param\\|return\\|raise\\|deprecated\\|since\\|version\\|author\\)\\'" nil nil))
  "Imenu settings for `neocaml-odoc-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Navigation

(defun neocaml-odoc--defun-name (node)
  "Return a name for NODE suitable for imenu and which-func."
  (let ((type (treesit-node-type node)))
    (cond
     ((string= type "heading")
      (string-trim (treesit-node-text node t)))
     ((string-prefix-p "tag_" type)
      (string-trim (treesit-node-text node t))))))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-odoc-mode text-mode "odoc"
  "Major mode for editing odoc documentation files.

When the OCaml tree-sitter grammar is installed, embedded OCaml
code inside {@ocaml[...]} blocks gets full syntax highlighting
via language injection.

\\{neocaml-odoc-mode-map}"
  (unless (treesit-ready-p 'odoc)
    (when (y-or-n-p "Odoc tree-sitter grammar is not installed.  Install it now?")
      (neocaml-odoc-install-grammar))
    (unless (treesit-ready-p 'odoc)
      (error "Cannot activate neocaml-odoc-mode without the odoc grammar")))
  (treesit-parser-create 'odoc)

  ;; Language injection for embedded OCaml code
  (let ((range-settings (neocaml-odoc--range-settings)))
    (when range-settings
      (setq-local treesit-range-settings range-settings)))

  ;; Font-lock
  (setq-local treesit-font-lock-settings (neocaml-odoc--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment heading tag definition)
                (keyword markup code math string type)
                (constant escape-sequence attribute builtin number reference)
                (variable operator bracket delimiter list property label function)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-odoc--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-odoc--imenu-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              "\\`\\(?:heading\\|tag_param\\|tag_return\\|tag_raise\\)\\'")
  (setq-local treesit-defun-name-function #'neocaml-odoc--defun-name)
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mld\\'" . neocaml-odoc-mode))

(provide 'neocaml-odoc)

;;; neocaml-odoc.el ends here
