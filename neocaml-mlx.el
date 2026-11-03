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
;; it, without the OCaml in it, to a `tsx' parser of its own, reusing
;; the built-in `typescript-ts-mode' font-lock rules for the embedded
;; JSX.  See
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
;; ... </div>' as a tangle of infix expressions and `ERROR' nodes, and
;; the `tsx' one has no notion of OCaml.  JSX inside OCaml that's inside
;; JSX, like the `<li>' in `<ul>(List.map (fun x -> <li>x</li>) xs)</ul>',
;; is highlighted as OCaml.  Every line is indented with the OCaml
;; parser, the JSX ones too, so those can come out wrong.  The OCaml
;; parser skips whatever looks like the end of a JSX element, OCaml
;; operators like `( /> )' too.

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

(defun neocaml-mlx--tsx-parsers ()
  "Return the buffer's `tsx' parsers."
  ;; Emacs 29's `treesit-parser-list' takes no language to filter by.
  (seq-filter (lambda (parser) (eq (treesit-parser-language parser) 'tsx))
              (treesit-parser-list)))

(defconst neocaml-mlx--jsx-start-regexp
  (rx "<" (or (in "A-Za-z_") (seq ">" (not (in " \t")))))
  "Regexp matching the start of a JSX element or fragment.
A `<>' with a space after it is OCaml's inequality operator.")

(defconst neocaml-mlx--jsx-end-regexp
  (rx (or (seq "</" (? (in "A-Za-z_") (* (in "A-Za-z0-9_.'")))
               (* (in " \t\n")) ">")
          "/>"))
  "Regexp matching the end of a JSX element, a closing tag or a `/>'.")

(defvar-local neocaml-mlx--ranges-state nil
  "The `buffer-chars-modified-tick' and bounds the ranges were set for.
Emacs trims the parsers' ranges to the accessible part of a narrowed
buffer, so those count too.")

(defun neocaml-mlx--gaps (beg end spans)
  "Return the ranges between BEG and END that SPANS leave out.
SPANS are sorted by their start, and can overlap."
  (let (ranges)
    (dolist (span spans)
      (when (< beg (car span))
        (push (cons beg (car span)) ranges))
      (setq beg (max beg (cdr span))))
    (when (< beg end)
      (push (cons beg end) ranges))
    (nreverse ranges)))

(defun neocaml-mlx--ocaml-ranges ()
  "Return the ranges of the buffer for the `ocaml' parser to parse.
That's all of it but the ends of JSX elements, or nil (the whole
buffer) when there are none.  To the OCaml grammar `<div>...</div>' is
a chain of infix operators, and the final `>' has nothing on its right,
so the parser takes the code after it for that and the rest of the
buffer ends up in an `ERROR' node.  Without the ends, the JSX reads as
an expression ending in a name.  The ends in strings and comments go
too, which leaves those as they are, faces and all, but so do OCaml
operators that look like them, such as `( /> )'."
  (let (ends)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward neocaml-mlx--jsx-end-regexp nil t)
        (push (cons (match-beginning 0) (match-end 0)) ends)))
    (when ends
      (neocaml-mlx--gaps (point-min) (point-max) (nreverse ends)))))

(defun neocaml-mlx--jsx-range (node _offset)
  "Return the JSX region contained in NODE as a list of (BEG . END).
NODE is the `let_binding' of a React (or other JSX-transform) component
binding.  We locate the first `<tag' (or `<>') that opens a JSX element
and span up to the last `>' at or after NODE's end.  Return nil when
NODE contains no JSX.

The OCaml parser doesn't see the ends of JSX elements (see
`neocaml-mlx--ocaml-ranges'), so NODE ends before those that end its
JSX, and its error recovery can place the end of NODE immediately
before the `>' of an opening tag, in `<div></div>' or an unclosed
`<div>'.  Account for both before looking backwards for the last
delimiter.

This function avoids narrowing to prevent conflicts with `syntax-propertize'
during jit-lock fontification."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char start)
        (when (re-search-forward neocaml-mlx--jsx-start-regexp end t)
          (let ((jsx-start (match-beginning 0)))
            (when (eq (char-after end) ?>)
              (setq end (1+ end)))
            (goto-char end)
            (while (progn (skip-chars-forward " \t\n")
                          (looking-at neocaml-mlx--jsx-end-regexp))
              (goto-char (setq end (match-end 0))))
            (goto-char end)
            (when (re-search-backward ">" jsx-start t)
              (list (cons jsx-start (match-end 0))))))))))

(defvar neocaml-mlx--jsx-query nil
  "A cons of `neocaml-mlx-jsx-attribute-regexp' and the query for it.
The query, compiled, finds the bindings `neocaml-mlx--jsx-ranges' looks
for JSX in.")

(defun neocaml-mlx--jsx-ranges ()
  "Return the ranges of the JSX in component bindings."
  (unless (equal (car neocaml-mlx--jsx-query) neocaml-mlx-jsx-attribute-regexp)
    (setq neocaml-mlx--jsx-query
          (cons neocaml-mlx-jsx-attribute-regexp
                (treesit-query-compile
                 'ocaml
                 `(((value_definition
                     (attribute (attribute_id) @_jsx_attr)
                     (let_binding) @mlx)
                    (:match ,neocaml-mlx-jsx-attribute-regexp @_jsx_attr)
                    (:match ,neocaml-mlx--jsx-start-regexp @mlx)))
                 t))))
  ;; NB: don't pass NODE-ONLY to `treesit-query-capture' - on Emacs 30
  ;; that drops the capture names before the :match predicates run,
  ;; which then error with "Cannot find captured node".  Filter the
  ;; (NAME . NODE) alist instead.
  (mapcan (lambda (capture)
            (when (eq (car capture) 'mlx)
              (neocaml-mlx--jsx-range (cdr capture) nil)))
          (treesit-query-capture (treesit-buffer-root-node 'ocaml)
                                 (cdr neocaml-mlx--jsx-query))))

(defconst neocaml-mlx--ocaml-holes-query
  '([(parenthesized_expression) (list_expression) (array_expression)
     (record_expression) (string) (quoted_string) (character) (comment)
     (extension) (quoted_extension)]
    @hole)
  "Query for the OCaml in JSX that the `tsx' parser skips.")

(defvar neocaml-mlx--compiled-ocaml-holes-query nil
  "`neocaml-mlx--ocaml-holes-query', compiled the first time it's used.")

(defun neocaml-mlx--ocaml-holes (beg end)
  "Return the spans of OCaml in the JSX between BEG and END, sorted.
Those are the strings, comments, extensions and bracketed expressions,
such as children like `(React.string name)', the values of props with
their `=', and the `?' of optional props, all of which the `tsx'
grammar would take for broken JSX.  The spans can overlap."
  (let (holes)
    (dolist (node (treesit-query-capture
                   'ocaml
                   (or neocaml-mlx--compiled-ocaml-holes-query
                       (setq neocaml-mlx--compiled-ocaml-holes-query
                             (treesit-query-compile
                              'ocaml neocaml-mlx--ocaml-holes-query t)))
                   beg end t))
      (when (and (<= beg (treesit-node-start node))
                 (<= (treesit-node-end node) end))
        (push (cons (treesit-node-start node) (treesit-node-end node)) holes)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              (rx (or (seq (in " \t\n") (group "?") (in "a-z_"))
                      (seq (in "A-Za-z0-9_'") (group "=")
                           (* (in "A-Za-z0-9_.'`#!~-")))))
              end t)
        (push (if (match-beginning 1)
                  (cons (match-beginning 1) (match-end 1))
                (cons (match-beginning 2) (match-end 0)))
              holes)))
    (sort holes #'car-less-than-car)))

(defun neocaml-mlx--tsx-ranges (block)
  "Return the ranges for the `tsx' parser of BLOCK, a JSX range.
That's BLOCK without the OCaml in it (see `neocaml-mlx--ocaml-holes')."
  (neocaml-mlx--gaps (car block) (cdr block)
                     (neocaml-mlx--ocaml-holes (car block) (cdr block))))

(defun neocaml-mlx--sync-tsx-parsers (blocks)
  "Make the buffer's `tsx' parsers parse BLOCKS, a parser for each.
A parser reads all its ranges as one text, so with a single parser the
JSX of one binding would run into that of the next.  Parsers already
parsing one of BLOCKS keep it."
  (let ((parsers (neocaml-mlx--tsx-parsers))
        unparsed)
    (dolist (ranges (mapcar #'neocaml-mlx--tsx-ranges blocks))
      (if-let* ((parser (seq-find (lambda (parser)
                                    (equal (treesit-parser-included-ranges parser)
                                           ranges))
                                  parsers)))
          (setq parsers (delq parser parsers))
        (push ranges unparsed)))
    (dolist (ranges unparsed)
      (treesit-parser-set-included-ranges
       (or (pop parsers) (treesit-parser-create 'tsx nil t))
       ranges))
    (mapc #'treesit-parser-delete parsers)))

(defun neocaml-mlx--set-ranges (_start _end)
  "Set the ranges of the `ocaml' parser and the `tsx' ones.
The OCaml parser skips the ends of JSX elements, and a `tsx' parser
parses the JSX of each component binding.  START and END are described
in `treesit-range-rules'.  This function recomputes ranges for the whole
buffer, which encompasses that region, when it changed since the last
time."
  (unless (equal neocaml-mlx--ranges-state
                 (list (buffer-chars-modified-tick) (point-min) (point-max)))
    (save-restriction
      ;; Range functions can run while syntax-propertize has narrowed the
      ;; buffer.  Querying in that restriction both misses definitions and
      ;; can let query predicates invoke syntax-propertize out of order.
      (widen)
      (let ((parser (treesit-parser-create 'ocaml))
            (ranges (neocaml-mlx--ocaml-ranges)))
        (unless (equal ranges (treesit-parser-included-ranges parser))
          (treesit-parser-set-included-ranges parser ranges)))
      (when (neocaml-mlx--injection-available-p)
        (neocaml-mlx--sync-tsx-parsers (neocaml-mlx--jsx-ranges))))
    (setq neocaml-mlx--ranges-state
          (list (buffer-chars-modified-tick) (point-min) (point-max)))))

(defun neocaml-mlx--shift-ranges (beg end old-len)
  "Move the parsers' ranges with the text after a change.
BEG, END and OLD-LEN are as for `after-change-functions'.  The ranges
move as markers would, so they stay on the text they were set for
until `neocaml-mlx--set-ranges' recomputes them.  That's for the
parses in between: `treesit-indent-region' changes the buffer between
its batches of lines, and Emacs can parse the buffer, for imenu, say,
before it next fontifies it."
  (let* ((old-end (+ beg old-len))
         (delta (- end old-end))
         (shift (lambda (pos)
                  (cond ((< pos beg) pos)
                        ((< pos old-end) beg)
                        (t (+ pos delta))))))
    (unless (zerop delta)
      (save-restriction
        (widen)
        (dolist (parser (treesit-parser-list))
          (when-let* ((ranges (treesit-parser-included-ranges parser)))
            (treesit-parser-set-included-ranges
             parser
             (mapcar (lambda (range)
                       (cons (funcall shift (car range))
                             (funcall shift (cdr range))))
                     ranges))))))))

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

  ;; Set up the parser ranges before the OCaml setup creates the OCaml
  ;; parser and runs `treesit-major-mode-setup'; the range settings
  ;; reference the host `ocaml' grammar and are honoured once that
  ;; parser exists.  The OCaml parser skips the ends of JSX elements, so
  ;; the code after them parses, with or without the `tsx' grammar.  The
  ;; OCaml parser still covers the content between tags, so OCaml
  ;; expressions like `(React.string "...")' are highlighted correctly.
  (setq-local treesit-range-settings
              (treesit-range-rules #'neocaml-mlx--set-ranges))
  (add-hook 'after-change-functions #'neocaml-mlx--shift-ranges nil t)

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
                        `((tsx ,@(neocaml-mlx--jsx-indent-rules)))))
    ;; Emacs 30 picks the parser to indent a line with by the language
    ;; at it, and without this takes the first parser in the buffer,
    ;; which can be the tsx one.  The OCaml parser covers the whole
    ;; buffer and the tsx one only the structure of the JSX, so OCaml is
    ;; the language everywhere, as it is for Emacs 31 with the OCaml
    ;; parser as the primary one.
    (setq-local treesit-language-at-point-function (lambda (_pos) 'ocaml)))

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
