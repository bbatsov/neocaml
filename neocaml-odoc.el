;;; neocaml-odoc.el --- Major mode for odoc (.mld) files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Tim McGilchrist <timmcgil@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml odoc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing odoc documentation (.mld)
;; files.  Provides font-lock for the odoc markup language including
;; headings, inline formatting (bold, italic, emphasis), code spans,
;; references, links, tags, lists, tables, and code blocks.  When the
;; relevant tree-sitter grammars are installed, embedded code inside
;; {@lang[...]} blocks gets full syntax highlighting via language
;; injection.  Supported languages: OCaml, dune, opam, and
;; sh/bash.
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

(declare-function neocaml-mode--font-lock-settings "neocaml" (language))

(defvar neocaml-dune--font-lock-settings)
(defvar neocaml-opam--font-lock-settings)

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

;;; Faces

(defface neocaml-odoc-heading-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for odoc section headings."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-bold-face
  '((t :inherit bold))
  "Face for bold markup in odoc."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-italic-face
  '((t :inherit italic))
  "Face for italic markup in odoc."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-emphasis-face
  '((t :inherit bold-italic))
  "Face for emphasis markup in odoc."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-tag-face
  '((t :inherit font-lock-keyword-face))
  "Face for documentation tags (@param, @return, etc.)."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-tag-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for parameter and exception names in tags."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-tag-value-face
  '((t :inherit font-lock-constant-face))
  "Face for version strings in tags."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-code-face
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline code spans and plain code blocks."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-verbatim-face
  '((t :inherit (fixed-pitch font-lock-string-face)))
  "Face for verbatim blocks."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-language-face
  '((t :inherit font-lock-type-face))
  "Face for the language tag in code blocks ({@ocaml[...]})."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-math-face
  '((t :inherit font-lock-string-face))
  "Face for math spans and blocks."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-reference-face
  '((t :inherit link))
  "Face for identifier references ({!Module.foo})."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-link-face
  '((t :inherit link))
  "Face for URL links ({:https://...})."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-escape-face
  '((t :inherit font-lock-escape-face))
  "Face for escape sequences."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-markup-face
  '((t :inherit shadow))
  "Face for structural markup (superscript, subscript delimiters)."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-raw-markup-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for raw markup (embedded HTML/LaTeX)."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-list-face
  '((t :inherit neocaml-odoc-markup-face))
  "Face for list markup."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

(defface neocaml-odoc-bracket-face
  '((t :inherit shadow))
  "Face for odoc bracket delimiters."
  :group 'neocaml-odoc
  :package-version '(neocaml . "0.9.0"))

;;; Font-lock

(defvar neocaml-odoc--font-lock-settings
  (treesit-font-lock-rules
   :language 'odoc
   :feature 'heading
   '((heading) @neocaml-odoc-heading-face)

   :language 'odoc
   :feature 'tag
   '((tag_author) @neocaml-odoc-tag-face
     (tag_param) @neocaml-odoc-tag-face
     (tag_raise) @neocaml-odoc-tag-face
     (tag_return) @neocaml-odoc-tag-face
     (tag_see) @neocaml-odoc-tag-face
     (tag_since) @neocaml-odoc-tag-face
     (tag_before) @neocaml-odoc-tag-face
     (tag_version) @neocaml-odoc-tag-face
     (tag_deprecated) @neocaml-odoc-tag-face
     (tag_canonical) @neocaml-odoc-tag-face
     (tag_inline) @neocaml-odoc-tag-face
     (tag_open) @neocaml-odoc-tag-face
     (tag_closed) @neocaml-odoc-tag-face
     (tag_hidden) @neocaml-odoc-tag-face)

   :language 'odoc
   :feature 'tag
   :override t
   '((param_name) @neocaml-odoc-tag-name-face
     (raise_name) @neocaml-odoc-tag-name-face
     (before_version) @neocaml-odoc-tag-value-face)

   :language 'odoc
   :feature 'markup
   '((bold) @neocaml-odoc-bold-face
     (italic) @neocaml-odoc-italic-face
     (emphasis) @neocaml-odoc-emphasis-face)

   :language 'odoc
   :feature 'code
   '((code_span) @neocaml-odoc-code-face
     (code_block) @neocaml-odoc-code-face
     (code_block_with_lang (language) @neocaml-odoc-language-face)
     (verbatim_block) @neocaml-odoc-verbatim-face)

   :language 'odoc
   :feature 'math
   '((math_span) @neocaml-odoc-math-face
     (math_block) @neocaml-odoc-math-face)

   :language 'odoc
   :feature 'reference
   '((simple_reference) @neocaml-odoc-reference-face
     (reference_with_text (reference_target) @neocaml-odoc-reference-face)
     (simple_link) @neocaml-odoc-link-face
     (link_with_text (link_target) @neocaml-odoc-link-face)
     (module_name) @neocaml-odoc-reference-face)

   :language 'odoc
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @neocaml-odoc-escape-face)

   :language 'odoc
   :feature 'markup
   '((superscript) @neocaml-odoc-markup-face
     (subscript) @neocaml-odoc-markup-face
     (raw_markup) @neocaml-odoc-raw-markup-face)

   :language 'odoc
   :feature 'list
   '((unordered_list) @neocaml-odoc-list-face
     (ordered_list) @neocaml-odoc-list-face)

   :language 'odoc
   :feature 'bracket
   '(["[" "]" "]}" "}" "{b" "{i" "{e" "{^" "{_"
      "{!" "{{!" "{:" "{{:" "{%" "{m" "{math"
      "{[" "{v" "{ul" "{ol" "{li" "{table" "{tr" "{th" "{td" "{t"
      "{L" "{C" "{R" "{!modules:" "%}"]
     @neocaml-odoc-bracket-face))
  "Font-lock settings for `neocaml-odoc-mode'.")

(defvar neocaml-odoc--injection-language-alist
  '(("ocaml" . ocaml)
    ("dune"  . dune)
    ("opam"  . opam)
    ("sh"    . bash)
    ("bash"  . bash))
  "Alist mapping odoc language tags to tree-sitter grammar symbols.
Each entry is (TAG . GRAMMAR) where TAG is the string used in
`{@tag[...]}' blocks and GRAMMAR is the tree-sitter language symbol.")

(defun neocaml-odoc--injection-available-p ()
  "Non-nil if language injection is supported.
Requires Emacs 30+ for `treesit-range-rules' with `:embed'."
  (>= emacs-major-version 30))

(defun neocaml-odoc--available-injections ()
  "Return the subset of `neocaml-odoc--injection-language-alist' with installed grammars."
  (when (neocaml-odoc--injection-available-p)
    (seq-filter (lambda (entry)
                  (treesit-language-available-p (cdr entry)))
                neocaml-odoc--injection-language-alist)))

(defun neocaml-odoc--font-lock-for-grammar (grammar)
  "Return font-lock settings for GRAMMAR, or nil if unavailable."
  (pcase grammar
    ('ocaml
     (require 'neocaml)
     (neocaml-mode--font-lock-settings 'ocaml))
    ('dune
     (require 'neocaml-dune)
     neocaml-dune--font-lock-settings)
    ('opam
     (require 'neocaml-opam)
     neocaml-opam--font-lock-settings)))

(defun neocaml-odoc--font-lock-settings ()
  "Return font-lock settings for `neocaml-odoc-mode'.
When language injection is available, includes font-lock rules for
embedded code inside `{@lang[...]}' blocks.  Otherwise, code block
content gets a plain string face as fallback."
  (let ((injections (neocaml-odoc--available-injections)))
    (append
     neocaml-odoc--font-lock-settings
     (if injections
         (mapcan (lambda (grammar)
                   (copy-sequence
                    (neocaml-odoc--font-lock-for-grammar grammar)))
                 (seq-uniq (mapcar #'cdr injections)))
       ;; No injection: highlight code_block_content as string
       (treesit-font-lock-rules
        :language 'odoc
        :feature 'code
        '((code_block_with_lang (code_block_content) @font-lock-string-face)))))))

(defun neocaml-odoc--range-settings ()
  "Return range settings for language injection in code blocks.
Injects parsers into `{@lang[...]}' blocks for each available grammar.
Returns nil if no injections are available."
  (let ((injections (neocaml-odoc--available-injections)))
    (when injections
      ;; Group tags by grammar, so e.g. sh and bash produce one rule
      ;; matching either tag.
      (let ((by-grammar (make-hash-table :test #'eq)))
        (dolist (entry injections)
          (push (car entry) (gethash (cdr entry) by-grammar)))
        (let ((result nil))
          (maphash
           (lambda (grammar tags)
             (setq result
                   (nconc result
                          (treesit-range-rules
                           :embed grammar
                           :host 'odoc
                           :local t
                           `((code_block_with_lang
                              (language) @_lang
                              (code_block_content) @capture
                              (:match ,(concat "\\`" (regexp-opt tags) "\\'") @_lang)))))))
           by-grammar)
          result)))))

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
     ;; Paragraph text stays at column 0
     ((parent-is "paragraph") column-0 0)
     ;; Catch-all
     (no-node parent-bol neocaml-odoc-indent-offset)))
  "Indentation rules for `neocaml-odoc-mode'.")

;;; Imenu

(defvar neocaml-odoc--imenu-settings
  '(("Heading" "\\`heading\\'" nil neocaml-odoc--defun-name)
    ("Tag" "\\`tag_\\(?:param\\|return\\|raise\\|deprecated\\|since\\|version\\|author\\)\\'" nil neocaml-odoc--defun-name))
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

When tree-sitter grammars are installed, embedded code inside
`{@lang[...]}' blocks gets full syntax highlighting via language
injection.  See `neocaml-odoc--injection-language-alist' for
supported languages.

\\{neocaml-odoc-mode-map}"
  (when (< (treesit-library-abi-version) 14)
    (error "The odoc grammar requires tree-sitter ABI version 14+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.22.0" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'odoc)
    (when (y-or-n-p "Odoc tree-sitter grammar is not installed.  Install it now?")
      (neocaml-odoc-install-grammar))
    (unless (treesit-ready-p 'odoc)
      (error "Cannot activate neocaml-odoc-mode without the odoc grammar")))
  (treesit-parser-create 'odoc)

  ;; Language injection for embedded code blocks
  (let ((range-settings (neocaml-odoc--range-settings)))
    (when range-settings
      (setq-local treesit-range-settings range-settings)))

  ;; Font-lock
  (setq-local treesit-font-lock-settings (neocaml-odoc--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '(( heading tag
                  ;; Injected grammar features — levels mirror those used
                  ;; by `neocaml-mode', `neocaml-dune-mode', etc. so that
                  ;; embedded code gets highlighted at the default level.
                  comment definition keyword)
                (markup code math
                 string type property)
                (reference escape-sequence
                 attribute builtin constant number)
                (list bracket
                 variable operator delimiter label function)))

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
