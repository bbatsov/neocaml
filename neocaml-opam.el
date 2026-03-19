;;; neocaml-opam.el --- Major mode for opam files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml opam

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing opam package files.
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-opam.

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

(defgroup neocaml-opam nil
  "Major mode for editing opam files with tree-sitter."
  :prefix "neocaml-opam-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-opam-indent-offset 2
  "Number of spaces for each indentation step in `neocaml-opam-mode'."
  :type 'natnum
  :safe 'natnump
  :package-version '(neocaml . "0.6.0"))

;;; Grammar installation

(defconst neocaml-opam-grammar-recipes
  '((opam "https://github.com/tmcgilchrist/tree-sitter-opam"
          "master"
          "src"))
  "Tree-sitter grammar recipe for opam files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-opam-install-grammar (&optional force)
  "Install the opam tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'opam nil)))
    (message "Installing opam tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-opam-grammar-recipes))
      (treesit-install-language-grammar 'opam))))

;;; Known field names

(defconst neocaml-opam--known-fields
  '("opam-version" "name" "version" "maintainer" "authors"
    "license" "homepage" "doc" "bug-reports" "dev-repo"
    "tags" "patches" "substs" "build" "install" "run-test"
    "remove" "depends" "depopts" "conflicts" "conflict-class"
    "depexts" "messages" "post-messages" "available"
    "flags" "features" "synopsis" "description" "url" "setenv"
    "build-env" "extra-files" "pin-depends" "extra-source")
  "Known opam field names.")

(defconst neocaml-opam--scope-constants
  '("build" "with-test" "with-doc" "pinned")
  "Opam filter scope identifiers.")

;;; Font-lock

(defun neocaml-opam--field-name-p (_node)
  "Non-nil when NODE is a known opam field name."
  ;; We use the `variable > name:' query instead of checking against
  ;; neocaml-opam--known-fields so that user-defined x-fields are also
  ;; highlighted (they start with "x-").
  t)

(defvar neocaml-opam--font-lock-settings
  (treesit-font-lock-rules
   :language 'opam
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'opam
   :feature 'keyword
   '((variable name: (ident) @font-lock-keyword-face)
     (section kind: (ident) @font-lock-keyword-face))

   :language 'opam
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'opam
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'opam
   :feature 'number
   '((int) @font-lock-number-face)

   :language 'opam
   :feature 'constant
   '((bool) @font-lock-constant-face)

   :language 'opam
   :feature 'operator
   '((relop) @font-lock-operator-face
     (pfxop) @font-lock-operator-face
     (envop) @font-lock-operator-face
     "&" @font-lock-operator-face
     "|" @font-lock-operator-face)

   :language 'opam
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'opam
   :feature 'delimiter
   '(":" @font-lock-delimiter-face)

   :language 'opam
   :feature 'variable
   '((ident) @font-lock-variable-use-face))
  "Font-lock settings for `neocaml-opam-mode'.")

;;; Indentation

(defvar neocaml-opam--indent-rules
  `((opam
     ((parent-is "source_file") column-0 0)
     ;; Don't reindent inside strings (e.g., triple-quoted descriptions)
     ((parent-is "string") no-indent 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "section") parent-bol neocaml-opam-indent-offset)
     ((parent-is "list") parent-bol neocaml-opam-indent-offset)
     ((parent-is "group") parent-bol neocaml-opam-indent-offset)
     ((parent-is "option_value") parent-bol neocaml-opam-indent-offset)
     (no-node parent-bol neocaml-opam-indent-offset)))
  "Indentation rules for `neocaml-opam-mode'.")

;;; Imenu

(defvar neocaml-opam--imenu-settings
  '(("Variable" "\\`variable\\'" nil nil)
    ("Section" "\\`section\\'" nil nil))
  "Imenu settings for `neocaml-opam-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Prettify symbols

(defvar neocaml-opam-prettify-symbols-alist
  '(("&" . ?∧)
    ("|" . ?∨)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("!=" . ?≠))
  "Alist of symbol prettifications for opam files.
See `prettify-symbols-alist' for more information.")

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-opam-mode prog-mode "opam"
  "Major mode for editing opam package files.

\\{neocaml-opam-mode-map}"
  (unless (treesit-ready-p 'opam)
    (when (y-or-n-p "Opam tree-sitter grammar is not installed.  Install it now?")
      (neocaml-opam-install-grammar))
    (unless (treesit-ready-p 'opam)
      (error "Cannot activate neocaml-opam-mode without the opam grammar")))
  (treesit-parser-create 'opam)

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Font-lock
  (setq-local treesit-font-lock-settings neocaml-opam--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword)
                (string constant)
                (number escape-sequence)
                (operator bracket delimiter variable)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-opam--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-opam--imenu-settings)

  ;; Prettify symbols
  (setq-local prettify-symbols-alist neocaml-opam-prettify-symbols-alist)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  (treesit-major-mode-setup))

;;;###autoload
;; Matches both bare "opam" files (e.g., repo/opam) and named
;; "foo.opam" files — the [./] covers the dot in ".opam".
(add-to-list 'auto-mode-alist '("[./]opam\\'" . neocaml-opam-mode))

(provide 'neocaml-opam)

;;; neocaml-opam.el ends here
