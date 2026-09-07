;;; neocaml-mlx.el --- Major mode for OCaml/JSX (.mlx) files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov <bozhidar@batsov.dev>
;;
;; Author:  Akira Komamura <akira.komamura@gmail.com>
;;          Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing OCaml files that embed
;; JSX syntax (Melange/React and similar PPX-driven OCaml-JSX setups),
;; conventionally given the `.mlx' extension.

;; The host grammar is the ordinary `ocaml' grammar, so every feature
;; of `neocaml-mode' (font-lock, indentation, navigation, imenu,
;; compilation, ...) is available unchanged.  On top of that, when the
;; `tsx' tree-sitter grammar is installed, the JSX subtrees are
;; highlighted via language injection: Emacs detects `let' bindings
;; carrying a configurable JSX-transform attribute (by default
;; `[@react.component]'), finds the JSX element they contain, and feeds
;; just that region to a `tsx' parser, reusing the built-in
;; `typescript-ts-mode' font-lock rules for the embedded JSX.  See
;; `neocaml-mlx-jsx-attribute-regexp' to teach it about other PPXes.
;;
;; For the host grammar, see
;; https://github.com/tree-sitter/tree-sitter-ocaml.
;; For the embedded JSX grammar, see
;; https://github.com/tree-sitter/tree-sitter-typescript (the `tsx'
;; language).

;; Limitations:
;;
;; The OCaml grammar itself has no notion of JSX, so it parses `<div>
;; ... </div>' as a tangle of infix expressions and `ERROR' nodes.
;; JSX-aware indentation is provided by appending tsx indent rules
;; for the injected JSX regions, but the boundaries between OCaml and
;; JSX may still produce imprecise results in some cases.

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
(require 'neocaml)
(require 'typescript-ts-mode)

(declare-function neocaml--setup-mode "neocaml")
(declare-function typescript-ts-mode--font-lock-settings
                  "typescript-ts-mode" (language))
(declare-function tsx-ts-mode--font-lock-compatibility-bb1f97b
                  "typescript-ts-mode" (language))
(defvar neocaml--imenu-settings)

(defgroup neocaml-mlx nil
  "Major mode for editing OCaml/JSX (.mlx) files with tree-sitter."
  :prefix "neocaml-mlx-"
  :group 'neocaml
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

;;; Grammar installation

(defconst neocaml-mlx-grammar-recipes
  '((tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         "v0.23.2"
         "tsx/src"))
  "Tree-sitter grammar recipe for the `tsx' (JSX) language.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.

The host `ocaml' grammar is installed by `neocaml-install-grammars'.")

(defun neocaml-mlx-install-grammar (&optional force)
  "Install the `tsx' tree-sitter grammar if not already available.
The host `ocaml' grammar is a prerequisite and is installed separately
via `neocaml-install-grammars'.  With prefix argument FORCE, reinstall
even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'tsx nil)))
    (message "Installing tsx (JSX) tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-mlx-grammar-recipes))
      (treesit-install-language-grammar 'tsx))))

;;; JSX injection

(defcustom neocaml-mlx-jsx-attribute-regexp "react\\.component"
  "Regexp matched against an OCaml attribute id to detect JSX bindings.
When a `let' binding carries an attribute whose id matches this regexp
\(the `react.component' part of `[@react.component]'), neocaml-mlx
injects the `tsx' grammar into its body so the embedded JSX is
highlighted.  Extend this to support other JSX-transform PPXes, e.g.
\"\\\\(react\\\\.component\\\\|jsx\\\\)\".

The match is performed against the text of the `attribute_id' node,
which excludes the surrounding `[@' and `]'."
  :type 'regexp
  :group 'neocaml-mlx
  :package-version '(neocaml . "0.11.0"))

(defun neocaml-mlx--injection-available-p ()
  "Non-nil if `tsx' language injection is available.
Requires Emacs 30+ and the `tsx' tree-sitter grammar."
  (and (>= emacs-major-version 30)
       (treesit-language-available-p 'tsx)))

(defun neocaml-mlx--jsx-range (node _offset)
  "Return the JSX region contained in NODE as a list of (BEG . END).
NODE is the `let_binding' of a React (or other JSX-transform) component
binding.  We locate the first `<tag' that opens a JSX element and span
up to the last `>' at or immediately after NODE's end.  Return nil when
NODE contains no JSX.

The OCaml parser's error recovery can place the end of NODE immediately
before the final `>' of a closing tag.  Account for that boundary before
looking backwards for the last delimiter.

This function avoids narrowing to prevent conflicts with `syntax-propertize'
during jit-lock fontification."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char start)
        (when (re-search-forward (rx "<" (in "A-Za-z_")) end t)
          (let ((jsx-start (match-beginning 0)))
            (when (eq (char-after end) ?>)
              (setq end (1+ end)))
            (goto-char end)
            (when (re-search-backward ">" jsx-start t)
              (list (cons jsx-start (match-end 0))))))))))

(defun neocaml-mlx--set-ranges (_start _end)
  "Set `tsx' parser ranges for JSX component bindings.
START and END are described in `treesit-range-rules'.  This function
recomputes ranges for the whole buffer, which encompasses that region."
  (save-restriction
    ;; Range functions can run while syntax-propertize has narrowed the
    ;; buffer.  Querying in that restriction both misses definitions and
    ;; can let query predicates invoke syntax-propertize out of order.
    (widen)
    (let* ((tsx-parser (treesit-parser-create 'tsx))
           ;; NB: don't pass NODE-ONLY to `treesit-query-capture' - on
           ;; Emacs 30 that drops the capture names before the :match
           ;; predicates run, which then error with "Cannot find
           ;; captured node".  Filter the (NAME . NODE) alist instead.
           (captures
            (treesit-query-capture
             (treesit-buffer-root-node 'ocaml)
             `(((value_definition
                 (attribute (attribute_id) @_jsx_attr)
                 (let_binding) @mlx)
                (:match ,neocaml-mlx-jsx-attribute-regexp @_jsx_attr)
                (:match "<[A-Za-z_]" @mlx)))))
           (ranges (mapcan (lambda (capture)
                             (when (eq (car capture) 'mlx)
                               (neocaml-mlx--jsx-range (cdr capture) nil)))
                           captures)))
      ;; An empty list makes a parser cover the whole buffer, so use a
      ;; degenerate range when there is no JSX to parse.
      (treesit-parser-set-included-ranges
       tsx-parser (or ranges `((,(point-min) . ,(point-min))))))))

(defun neocaml-mlx--range-settings ()
  "Return range settings for injecting `tsx' into OCaml JSX regions.
Returns nil when injection is not available.  The ranges share a single
`tsx' parser; each detected component binding contributes one span
covering the entire JSX region from the first tag to the end of the
let binding."
  (when (neocaml-mlx--injection-available-p)
    (treesit-range-rules #'neocaml-mlx--set-ranges)))

;;; Font-lock

(defface neocaml-mlx-jsx-tag-delimiter-face
  '((t :inherit typescript-ts-jsx-tag-face))
  "Face used for JSX delimiters in `neocaml-mlx-mode'.
This complements `typescript-ts-jsx-tag-face' by colouring the `<', `</',
`>' and `/>' punctuation around JSX tags, which the default
`typescript-ts-mode' rules leave unfontified."
  :group 'neocaml-mlx
  :package-version '(neocaml . "0.11.0"))

(defun neocaml-mlx--tsx-font-lock-settings ()
  "Return `tsx' font-lock settings for embedded JSX in `neocaml-mlx-mode'.
Reuses Emacs' built-in `typescript-ts-mode' rules and adds an
overriding `jsx' rule so JSX tags, delimiters, and attributes win
out over the host `ocaml' grammar's faces on the same text, which
the OCaml parser mis-parses as ordinary identifiers/operators.
Returns nil when injection is not available."
  (when (neocaml-mlx--injection-available-p)
    (append
     (typescript-ts-mode--font-lock-settings 'tsx)
     (treesit-font-lock-rules
      :language 'tsx
      :feature 'jsx
      :override t
      (append
       ;; Tag names, adjusted to the installed tsx grammar version.
       (tsx-ts-mode--font-lock-compatibility-bb1f97b 'tsx)
       ;; Tag delimiters and attributes the default rules leave plain.
       '((jsx_opening_element ["<" ">"] @neocaml-mlx-jsx-tag-delimiter-face)
         (jsx_closing_element ["</" ">"] @neocaml-mlx-jsx-tag-delimiter-face)
         (jsx_self_closing_element ["<" "/>"] @neocaml-mlx-jsx-tag-delimiter-face)
         (jsx_attribute (property_identifier) @typescript-ts-jsx-attribute-face)))))))

(defun neocaml-mlx--merge-feature-lists (a b)
  "Merge the feature lists A and B level by level, dropping duplicates.
Fallback for `treesit-merge-font-lock-feature-list', which only
exists on Emacs 31+."
  (let (result)
    (while (or a b)
      (push (delete-dups (append (pop a) (pop b))) result))
    (nreverse result)))

(defun neocaml-mlx--font-lock-feature-list ()
  "Return the feature list merging `ocaml' and `tsx' feature levels.
The current buffer is expected to have been configured by
`neocaml--setup-mode'.  Obtain the TSX list from `tsx-ts-mode' so changes
to either mode's feature levels are reflected here automatically."
  (let ((ocaml-features treesit-font-lock-feature-list)
        (tsx-features (with-temp-buffer
                        (tsx-ts-mode)
                        treesit-font-lock-feature-list)))
    (if (fboundp 'treesit-merge-font-lock-feature-list)
        (treesit-merge-font-lock-feature-list ocaml-features tsx-features)
      (neocaml-mlx--merge-feature-lists ocaml-features tsx-features))))

;;; Indentation

(defun neocaml-mlx--jsx-indent-rules ()
  "Return JSX-specific indentation rules for `neocaml-mlx-mode'.
These rules are based on the neovim MLX support queries (indents.scm),
translated to the tsx grammar node names (jsx_opening_element,
jsx_closing_element, jsx_self_closing_element, jsx_expression, etc.).

The translation from neovim indent queries to Emacs treesit rules:
- @indent.begin on jsx_element_opening/self_closing/expression:
  children of these nodes are indented by `neocaml-indent-offset'.
- @indent.end on \">\" in jsx_element_closing and \"/>\" in self-closing:
  these delimiters end an indent block.
- @indent.branch on jsx_element_closing and \">\":
  closing elements and their delimiters align with the opening element.
- @indent.branch on \"/>\" in self-closing:
  the self-closing delimiter aligns with the opening tag."
  `(;; JSX closing elements align with parent jsx_element (branch)
    ((node-is "jsx_closing_element") parent-bol 0)

    ;; > delimiter aligns with parent opening element (branch/end)
    ((node-is ">") parent-bol 0)

    ;; /> delimiter aligns with parent self-closing element (branch/end)
    ((node-is "/>") parent-bol 0)

    ;; Content inside JSX elements is indented (begin)
    ((parent-is "jsx_element") parent-bol neocaml-indent-offset)

    ;; Content inside JSX fragments is indented (begin)
    ((parent-is "jsx_fragment") parent-bol neocaml-indent-offset)

    ;; Attributes on new lines in opening elements are indented (begin)
    ((parent-is "jsx_opening_element") parent-bol neocaml-indent-offset)

    ;; Attributes on new lines in self-closing elements are indented (begin)
    ((parent-is "jsx_self_closing_element") parent-bol neocaml-indent-offset)

    ;; Content inside JSX expressions { } is indented (begin)
    ((parent-is "jsx_expression") parent-bol neocaml-indent-offset)))

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-mlx-mode neocaml-base-mode "OCaml[MLX]"
  "Major mode for editing OCaml files with embedded JSX (.mlx).

`neocaml-mlx-mode' is `neocaml-mode' plus highlighting and indentation
for embedded JSX.  When the `tsx' tree-sitter grammar is installed,
`let' bindings marked with a JSX-transform attribute (see
`neocaml-mlx-jsx-attribute-regexp', default `[@react.component]') have
their JSX bodies highlighted via language injection using the built-in
`typescript-ts-mode' font-lock rules, with JSX-aware indentation rules
appended for the injected regions.  If the `tsx' grammar is absent,
the mode degrades gracefully to plain `neocaml-mode' behaviour.

\\{neocaml-base-mode-map}"
  (setq-local treesit-simple-imenu-settings neocaml--imenu-settings)

  ;; Set up embedded-JSX injection before the OCaml setup creates the
  ;; OCaml parser and runs `treesit-major-mode-setup'; the range
  ;; settings reference the host `ocaml' grammar and are honoured once
  ;; that parser exists.  The TSX parser only covers JSX structural
  ;; elements (tags, attributes, delimiters), not the content between
  ;; tags — that content is left to the host OCaml parser so OCaml
  ;; expressions like `(React.string "...")' are highlighted correctly.
  (when (neocaml-mlx--injection-available-p)
    (setq-local treesit-range-settings (neocaml-mlx--range-settings)))

  ;; Full OCaml setup: ocaml parser, font-lock, indent, navigation, ...
  (neocaml--setup-mode 'ocaml)

  ;; Layer the tsx font-lock and indent rules on top of the OCaml ones.
  (when (neocaml-mlx--injection-available-p)
    (let ((tsx-settings (neocaml-mlx--tsx-font-lock-settings)))
      (when tsx-settings
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings tsx-settings))
        (setq-local treesit-font-lock-feature-list
                    (neocaml-mlx--font-lock-feature-list))
        (treesit-font-lock-recompute-features)
        (font-lock-flush)))

    ;; Append JSX-specific indent rules for the tsx parser.
    ;; The OCaml indent rules (set up by `neocaml--setup-mode') handle
    ;; OCaml regions; these rules handle JSX regions parsed by tsx.
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        `((tsx ,@(neocaml-mlx--jsx-indent-rules))))))

  ;; Offer to install the JSX grammar so highlighting works out of the
  ;; box; the host `ocaml' grammar is handled by `neocaml--setup-mode'.
  (unless (or (not (>= emacs-major-version 30))
              (treesit-language-available-p 'tsx))
    (when (y-or-n-p "The tsx (JSX) tree-sitter grammar is not installed; \
JSX highlighting needs it.  Install it now?")
      (neocaml-mlx-install-grammar))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlx\\'" . neocaml-mlx-mode))

(provide 'neocaml-mlx)

;;; neocaml-mlx.el ends here
