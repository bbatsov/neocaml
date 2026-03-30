;;; neocaml-ocamllex.el --- Major mode for OCamllex files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing OCamllex (.mll) lexer
;; definition files.  Provides font-lock for the lexer DSL (rules,
;; regexps, keywords) and, when the OCaml tree-sitter grammar is
;; installed, full syntax highlighting for embedded OCaml code inside
;; { } blocks via language injection.
;;
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/314eter/tree-sitter-ocamllex.

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

(defgroup neocaml-ocamllex nil
  "Major mode for editing OCamllex files with tree-sitter."
  :prefix "neocaml-ocamllex-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-ocamllex-indent-offset 2
  "Number of spaces for each indentation step in `neocaml-ocamllex-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-ocamllex
  :package-version '(neocaml . "0.7.0"))

;;; Grammar installation

(defconst neocaml-ocamllex-grammar-recipes
  '((ocamllex "https://github.com/314eter/tree-sitter-ocamllex"
              "v0.25.0"
              "src"))
  "Tree-sitter grammar recipe for OCamllex files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-ocamllex-install-grammar (&optional force)
  "Install the OCamllex tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'ocamllex nil)))
    (message "Installing OCamllex tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-ocamllex-grammar-recipes))
      (treesit-install-language-grammar 'ocamllex))))

;;; Font-lock

(defvar neocaml-ocamllex--font-lock-settings
  (treesit-font-lock-rules
   :language 'ocamllex
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'ocamllex
   :feature 'keyword
   '(["and" "as" "let" "parse" "refill" "rule" "shortest"]
     @font-lock-keyword-face)

   :language 'ocamllex
   :feature 'definition
   '((lexer_entry name: (lexer_entry_name) @font-lock-function-name-face)
     (named_regexp name: (regexp_name) @font-lock-variable-name-face))

   :language 'ocamllex
   :feature 'string
   '((string) @font-lock-string-face
     (character) @font-lock-string-face)

   :language 'ocamllex
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'ocamllex
   :feature 'constant
   '((any) @font-lock-constant-face
     (eof) @font-lock-constant-face)

   :language 'ocamllex
   :feature 'variable
   '((regexp_name) @font-lock-variable-use-face)

   :language 'ocamllex
   :feature 'operator
   '((regexp_repetition ["*" "+" "?"] @font-lock-operator-face)
     (regexp_difference "#" @font-lock-operator-face))

   :language 'ocamllex
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'ocamllex
   :feature 'delimiter
   '(["=" "|" "-"] @font-lock-delimiter-face))
  "Font-lock settings for `neocaml-ocamllex-mode'.")

(defun neocaml-ocamllex--injection-available-p ()
  "Non-nil if OCaml language injection is available.
Requires Emacs 30+ (for `treesit-range-rules' with `:embed') and
the OCaml tree-sitter grammar."
  (and (>= emacs-major-version 30)
       (treesit-language-available-p 'ocaml)))

(defun neocaml-ocamllex--font-lock-settings ()
  "Return font-lock settings for `neocaml-ocamllex-mode'.
When OCaml injection is available, includes font-lock rules for
embedded OCaml code inside { } blocks."
  (append
   neocaml-ocamllex--font-lock-settings
   (when (neocaml-ocamllex--injection-available-p)
     (require 'neocaml)
     (neocaml-mode--font-lock-settings 'ocaml))))

(defun neocaml-ocamllex--range-settings ()
  "Return range settings for embedded OCaml code injection.
Returns nil if injection is not available."
  (when (neocaml-ocamllex--injection-available-p)
    (treesit-range-rules
     :embed 'ocaml
     :host 'ocamllex
     :local t
     '((ocaml) @capture))))

;;; Indentation

(defvar neocaml-ocamllex--indent-rules
  `((ocamllex
     ((parent-is "lexer_definition") column-0 0)
     ((node-is "}") parent-bol 0)
     ;; Don't reindent inside OCaml code blocks
     ((parent-is "ocaml") no-indent 0)
     ;; Lexer cases (| regexp { action })
     ((node-is "lexer_case") parent-bol neocaml-ocamllex-indent-offset)
     ;; Inside a lexer entry
     ((parent-is "lexer_entry") parent-bol neocaml-ocamllex-indent-offset)
     ;; Inside character sets
     ((parent-is "character_set") parent-bol 0)
     ;; Catch-all
     (no-node parent-bol neocaml-ocamllex-indent-offset)))
  "Indentation rules for `neocaml-ocamllex-mode'.")

;;; Imenu

(defvar neocaml-ocamllex--imenu-settings
  '(("Rule" "\\`lexer_entry\\'" nil nil)
    ("Regexp" "\\`named_regexp\\'" nil nil))
  "Imenu settings for `neocaml-ocamllex-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Navigation

(defun neocaml-ocamllex--defun-name (node)
  "Return a name for NODE suitable for imenu and which-func."
  (pcase (treesit-node-type node)
    ("lexer_entry"
     (let ((name (treesit-node-child-by-field-name node "name")))
       (when name (treesit-node-text name t))))
    ("named_regexp"
     (let ((name (treesit-node-child-by-field-name node "name")))
       (when name (treesit-node-text name t))))))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-ocamllex-mode prog-mode "OCamllex"
  "Major mode for editing OCamllex lexer definition files.

When the OCaml tree-sitter grammar is installed, embedded OCaml
code inside { } blocks gets full syntax highlighting via language
injection.

\\{neocaml-ocamllex-mode-map}"
  (when (< (treesit-library-abi-version) 15)
    (error "The OCamllex grammar requires tree-sitter ABI version 15+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.25.0" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'ocamllex)
    (when (y-or-n-p "OCamllex tree-sitter grammar is not installed.  Install it now?")
      (neocaml-ocamllex-install-grammar))
    (unless (treesit-ready-p 'ocamllex)
      (error "Cannot activate neocaml-ocamllex-mode without the OCamllex grammar")))
  (treesit-parser-create 'ocamllex)

  ;; Comments (OCaml-style)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+ *")

  ;; Language injection for embedded OCaml code
  (let ((range-settings (neocaml-ocamllex--range-settings)))
    (when range-settings
      (setq-local treesit-range-settings range-settings)))

  ;; Font-lock
  (setq-local treesit-font-lock-settings (neocaml-ocamllex--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (constant escape-sequence attribute builtin number)
                (variable operator bracket delimiter property label function)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-ocamllex--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-ocamllex--imenu-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              "\\`\\(?:lexer_entry\\|named_regexp\\)\\'")
  (setq-local treesit-defun-name-function #'neocaml-ocamllex--defun-name)
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mll\\'" . neocaml-ocamllex-mode))

(provide 'neocaml-ocamllex)

;;; neocaml-ocamllex.el ends here
