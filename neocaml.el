;;; neocaml.el --- Major mode for OCaml code -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml
;; Version: 0.10.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the
;; OCaml programming language (http://ocaml.org).

;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tree-sitter/tree-sitter-ocaml.

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
(require 'seq)
(require 'project)
(require 'neocaml-common)

(defgroup neocaml nil
  "Major mode for editing OCaml code with tree-sitter."
  :prefix "neocaml-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml")
  :link '(emacs-commentary-link :tag "Commentary" "neocaml"))

(defcustom neocaml-indent-offset 2
  "Number of spaces for each indentation step in the major modes."
  :type 'natnum
  :safe 'natnump
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-indent-style nil
  "Style of indentation in `neocaml-mode' and `neocaml-interface-mode'.
The styles only differ in a few layouts:

- `ocp-indent' indents as ocp-indent does by default.
- `conventional' indents as ocamlformat's conventional (default)
  profile does, which puts match case bodies further past the |.
- `janestreet' indents as ocamlformat's janestreet profile does,
  which puts parameters on continuation lines further in.

nil picks the style from the profile in the project's .ocamlformat,
taking its cases-exp-indent into account, and falls back to
`ocp-indent' in projects that don't have one.  To switch the style
of the current buffer, use \\[neocaml-set-indent-style]."
  :type '(choice (const :tag "From .ocamlformat" nil)
                 (const :tag "ocp-indent" ocp-indent)
                 (const :tag "ocamlformat's conventional profile" conventional)
                 (const :tag "ocamlformat's janestreet profile" janestreet))
  :safe (lambda (value) (memq value '(nil ocp-indent conventional janestreet)))
  :package-version '(neocaml . "0.11.0"))

(defcustom neocaml-other-file-alist
  '(("\\.mli\\'" (".ml" ".mll" ".mly"))
    ("\\.ml\\'" (".mli"))
    ("\\.mll\\'" (".mli"))
    ("\\.mly\\'" (".mli"))
    ("\\.eliomi\\'" (".eliom"))
    ("\\.eliom\\'" (".eliomi")))
  "Associative list of alternate extensions to find.
See `ff-other-file-alist' and `ff-find-other-file'."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-prettify-symbols-alist
  '(("=>" . ?⇒)
    ("<-" . ?←)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("<>" . ?≠)
    ("==" . ?≡)
    ("!=" . ?≢)
    ("||" . ?∨)
    ("&&" . ?∧))
  "Prettify symbols alist used by neocaml modes.
All replacements preserve column width."
  :type '(alist :key-type string :value-type character)
  :group 'neocaml
  :package-version '(neocaml . "0.1.0"))

(defcustom neocaml-prettify-symbols-extra-alist
  '(("fun" . ?λ)
    ("->" . ?→)
    ("not" . ?¬))
  "Extra prettify symbols that may affect column alignment.
Used when `neocaml-prettify-symbols-full' is non-nil."
  :type '(alist :key-type string :value-type character)
  :group 'neocaml
  :package-version '(neocaml . "0.2.0"))

(defcustom neocaml-prettify-symbols-full nil
  "When non-nil, include `neocaml-prettify-symbols-extra-alist'.
The extra symbols may affect column alignment."
  :type 'boolean
  :group 'neocaml
  :package-version '(neocaml . "0.2.0"))

(defcustom neocaml-redirect-build-files t
  "When non-nil, offer to switch to the source file from `_build/'.
Set to nil if you work with build artifacts directly, e.g. when debugging."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml
  :package-version '(neocaml . "0.8.0"))

(defcustom neocaml-ocamlformat-program "ocamlformat"
  "Program used to format OCaml source via `neocaml-format-buffer'."
  :type 'string
  :group 'neocaml
  :package-version '(neocaml . "0.9.0"))

(defcustom neocaml-format-on-save nil
  "When non-nil, format the buffer with `ocamlformat' before saving.
Only applies in `neocaml-mode' and `neocaml-interface-mode' buffers."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml
  :package-version '(neocaml . "0.9.0"))

(defvar neocaml--debug nil
  "Enable debugging messages and show the current node in the mode-line.
When set to t, show indentation debug info.
When set to `font-lock', show fontification info as well.

Only intended for use at development time.")

(defun neocaml--prettify-symbols-alist ()
  "Return the prettify symbols alist for the current settings.
Includes extra symbols when `neocaml-prettify-symbols-full' is non-nil."
  (if neocaml-prettify-symbols-full
      (append neocaml-prettify-symbols-alist
              neocaml-prettify-symbols-extra-alist)
    neocaml-prettify-symbols-alist))

(defconst neocaml-version "0.10.0")

(defun neocaml-version ()
  "Display the current package version in the minibuffer.
Fallback to `neocaml-version' when the package version is missing.
When called from other Elisp code returns the version instead of
displaying it."
  (interactive)
  (let ((pkg-version (package-get-version)))
    (if (called-interactively-p 'interactively)
        (if pkg-version
            (message "neocaml %s (package: %s)" neocaml-version pkg-version)
          (message "neocaml %s" neocaml-version))
      (or pkg-version neocaml-version))))

(defconst neocaml-grammar-recipes
  ;; We pin to the `v0.26.0-abi14' tag: tree-sitter-ocaml v0.26.0 itself
  ;; generates an ABI 15 parser, which most Emacs builds can't yet load,
  ;; so upstream provides this ABI-14 regeneration of the same grammar.
  ;; Move to a plain `v0.26.x' tag once ABI 15 is broadly supported by the
  ;; libtree-sitter Emacs links against.
  '((ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"
           "v0.26.0-abi14"
           "grammars/ocaml/src")
    ;; that's the grammar for mli code
    (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml"
                     "v0.26.0-abi14"
                     "grammars/interface/src"))
  "Tree-sitter grammar recipes for OCaml and OCaml Interface.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-install-grammars (&optional force)
  "Install required language grammars if not already available.
With prefix argument FORCE, reinstall grammars even if they are
already installed.  This is useful after upgrading neocaml to a
version that requires a newer grammar."
  (interactive "P")
  (dolist (recipe neocaml-grammar-recipes)
    (let ((grammar (car recipe)))
      (when (or force (not (treesit-language-available-p grammar nil)))
        (message "Installing %s tree-sitter grammar..." grammar)
        ;; `treesit-language-source-alist' is dynamically scoped.
        ;; Binding it in this let expression allows
        ;; `treesit-install-language-grammar' to pick up the grammar recipes
        ;; without modifying what the user has configured themselves.
        (let ((treesit-language-source-alist neocaml-grammar-recipes))
          (treesit-install-language-grammar grammar))))))

(defvar neocaml--grammar-compatibility-checked nil
  "Non-nil if grammar compatibility has already been checked this session.")

(defun neocaml--check-grammar-compatibility ()
  "Check that installed grammars are compatible with this version of neocaml.
Emit a warning if an outdated grammar is detected."
  (unless neocaml--grammar-compatibility-checked
    (setq neocaml--grammar-compatibility-checked t)
    ;; v0.26.0 gave `_' node types of its own (`any_pattern' in
    ;; patterns, `any_type' in types), and our queries name them, so a
    ;; grammar that parses `_' differently is too old.
    (when (seq-some
           (pcase-lambda (`(,language ,code ,type))
             (and (treesit-language-available-p language)
                  (with-temp-buffer
                    (insert code)
                    (not (treesit-search-subtree
                          (treesit-parser-root-node
                           (treesit-parser-create language))
                          type)))))
           '((ocaml "let _ = ()" "\\`any_pattern\\'")
             (ocaml-interface "val x : _" "\\`any_type\\'")))
      (display-warning
       'neocaml
       "The installed tree-sitter OCaml grammars appear older than the \
version required by neocaml.  Run C-u M-x neocaml-install-grammars to \
reinstall."))))

;; adapted from tuareg-mode
(defvar neocaml-base-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "'" st)     ;Make qualified names a single symbol.
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?~ ". p" st)
    ;; See https://v2.ocaml.org/manual/lex.html.
    (dolist (c '(?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))
    (modify-syntax-entry ?' "_" st) ; ' is part of symbols (for primes).
    (modify-syntax-entry ?` "." st)
    (modify-syntax-entry ?\" "\"" st) ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    st)
  "Syntax table in use in neocaml mode buffers.")

;;;; Syntax propertization
;;
;; A static syntax table can't classify OCaml's character literals and
;; quoted strings correctly, because they may contain characters that the
;; table would otherwise read as a string quote (`"'), a paren (`(' / `)'),
;; or a comment opener (`(*').  We fix this with a `syntax-propertize-function'
;; that marks those lexemes so the syntactic layer (sexp motion,
;; `electric-pair-mode', `delete-pair', `syntax-ppss') agrees with the parse
;; tree.  Font-lock is handled by tree-sitter and is unaffected.

(defun neocaml--syntax-propertize (start end)
  "Apply syntax properties to OCaml char literals and quoted strings.
Operates between START and END.  See the commentary above for why this
is needed.  This avoids calling `syntax-ppss' (which would be re-entrant,
since `syntax-propertize' runs from within it); quoted strings are
matched whole from their opening delimiter."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; Character literals: 'a', '\\n', '\\xFF', '"', '(', etc.  Mark both
    ;; quotes as string delimiters so the contents are inert.  A closing
    ;; quote is required, so type variables (e.g. 'a) are left untouched.
    ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 "\"") (2 "\""))
    ;; Quoted strings {tag|...|tag}.  Mark the opening brace as a generic
    ;; string fence and, if the matching |tag} is found, the closing brace
    ;; too, so the contents are inert.
    ("\\({\\)\\([[:lower:]_]*\\)|"
     (1 (let* ((tag (match-string 2))
               (close-pos (save-excursion
                            (when (search-forward (concat "|" tag "}") end t)
                              (point)))))
          (when close-pos
            (put-text-property (1- close-pos) close-pos
                               'syntax-table (string-to-syntax "|")))
          (string-to-syntax "|")))))
   (point) end))

;;;; Font-locking
;;
;;
;; See https://github.com/tree-sitter/tree-sitter-ocaml/blob/master/queries/highlights.scm
;;
;; Ideally the font-locking done by neocaml should be aligned with the upstream highlights.scm.

(defvar neocaml-mode--keywords
  '("and" "as" "assert" "begin" "class" "constraint" "do" "done" "downto" "effect"
    "else" "end" "exception" "external" "for" "fun" "function" "functor" "if" "in"
    "include" "inherit" "initializer" "lazy" "let" "match" "method" "module"
    "mutable" "new" "nonrec" "object" "of" "open" "private" "rec" "sig" "struct"
    "then" "to" "try" "type" "val" "virtual" "when" "while" "with"
    ;; OxCaml
    "borrow_" "exclave_" "global_" "kind_" "kind_of_" "local_" "stack_")
  "OCaml keywords for tree-sitter font-locking.

The list in the upstream highlights.scm, plus the OxCaml keywords it
leaves out (`exclave_', `global_' and `kind_of_').  See
https://github.com/tree-sitter/tree-sitter-ocaml/blob/master/queries/highlights.scm.")

(defvar neocaml-mode--constants
  '((unit) (boolean))
  "OCaml constants for tree-sitter font-locking.")

(defvar neocaml-mode--builtin-ids
  '("raise" "raise_notrace" "invalid_arg" "failwith" "ignore" "ref"
    "exit" "at_exit"
    ;; builtin exceptions
    "Exit" "Match_failure" "Assert_failure" "Invalid_argument"
    "Failure" "Not_found" "Out_of_memory" "Stack_overflow" "Sys_error"
    "End_of_file" "Division_by_zero" "Sys_blocked_io"
    "Undefined_recursive_module"
    ;; parser access
    "__LOC__" "__FILE__" "__LINE__" "__MODULE__" "__POS__"
    "__FUNCTION__" "__LOC_OF__" "__LINE_OF__" "__POS_OF__")
  "OCaml builtin identifiers for tree-sitter font-locking.")

(defvar neocaml-mode--builtin-types
  '("int" "char" "bytes" "string" "float" "bool" "unit" "exn"
    "array" "iarray" "list" "option" "int32" "int64" "nativeint" "format6" "lazy_t"
    "floatarray" "extension_constructor" "eff" "continuation" "atomic_loc"
    ;; OxCaml, with the unboxed versions of builtin types
    "float32" "int8" "int16" "or_null" "lexing_position"
    "float#" "float32#" "int8#" "int16#" "int32#" "int64#" "nativeint#"
    "char#" "bool#" "unit#")
  "OCaml builtin type names for tree-sitter font-locking.
List taken from the upstream highlights.scm.")

(defvar neocaml-mode--builtin-kinds
  '(;; Layouts
    "any" "value_or_null" "void" "float64" "float32" "word"
    "bits8" "bits16" "bits32" "bits64" "vec128" "vec256" "vec512"
    ;; Kind abbreviations
    "value" "immediate" "immediate_or_null" "immediate64" "immediate64_or_null"
    "immutable_data" "immutable_data_or_null" "sync_data" "sync_data_or_null"
    "mutable_data" "mutable_data_or_null" "value_maybe_separable" "value_maybe_null")
  "OxCaml builtin kind names for tree-sitter font-locking.
The layouts and kind abbreviations of https://oxcaml.org/documentation/kinds/syntax/.")

;;;; Ocamldoc markup rendering
;;
;; Doc comments ((** ... *)) are written in the ocamldoc markup
;; language: {b bold}, {i italic}, [inline code], {[ code blocks ]},
;; {!references}, @tags, and so on.  The comment text is opaque to the
;; parse tree, so the `doc-markup' font-lock feature scans doc comment
;; nodes with regular expressions and renders the markup with real
;; faces (bold, italic, a code face), layered on top of
;; `font-lock-doc-face' so the markup reads in place instead of as
;; punctuation soup.  Disable by removing `doc-markup' from
;; `treesit-font-lock-feature-list'.
;;
;; Longer term this could be replaced by injecting the tree-sitter odoc
;; grammar into doc comments (the way `js-ts-mode' injects jsdoc); the
;; scanner keeps the feature working without an extra grammar.

(defconst neocaml--doc-comment-regexp "^(\\*\\*[^*]"
  "Regexp matching the text of a doc comment node ((** ...).
The [^*] excludes (*** separator comments.")

(defface neocaml-doc-markup-face
  '((t :inherit font-lock-doc-markup-face))
  "Face for ocamldoc markup delimiters and @tags in doc comments."
  :group 'neocaml)

(defface neocaml-doc-bold-face
  '((t :inherit bold))
  "Face for {b ...} markup in doc comments."
  :group 'neocaml)

(defface neocaml-doc-italic-face
  '((t :inherit italic))
  "Face for {i ...} and {e ...} markup in doc comments."
  :group 'neocaml)

(defface neocaml-doc-heading-face
  '((t :inherit bold))
  "Face for {0 ...} to {5 ...} section headings in doc comments."
  :group 'neocaml)

(defface neocaml-doc-code-face
  '((t :inherit font-lock-constant-face))
  "Face for [inline code], {[ code blocks ]} and verbatim text in doc comments."
  :group 'neocaml)

(defface neocaml-doc-reference-face
  '((t :inherit font-lock-constant-face))
  "Face for {!reference} and {:link} targets in doc comments."
  :group 'neocaml)

(defconst neocaml--doc-markup-tag-regexp
  (concat "@" (regexp-opt '("author" "before" "canonical" "children_order"
                            "closed" "deprecated" "hidden" "inline" "open"
                            "order_category" "param" "raise" "return" "see"
                            "short_title" "since" "toc_status" "version"))
          "\\_>")
  "Regexp matching an ocamldoc/odoc tag such as @param or @return.")

(defun neocaml--doc-markup-close (open close limit)
  "Return the position of the CLOSE char balancing one open construct.
Scan from point up to LIMIT, treating OPEN and CLOSE (characters) as
nesting delimiters and a backslash as escaping the next character.
Point should be just past the opening delimiter.  Return nil if the
construct is unbalanced."
  (save-excursion
    ;; `skip-chars-forward' jumps over runs of uninteresting text at C
    ;; speed; only delimiters and backslashes are handled in Lisp.
    (let ((skip (concat "^" (string open close) "\\\\"))
          (depth 1) found)
      (while (and (not found)
                  (progn (skip-chars-forward skip limit)
                         (< (point) limit)))
        (let ((c (char-after)))
          (cond ((eq c ?\\) (forward-char 1))
                ((eq c open) (setq depth (1+ depth)))
                ((eq c close)
                 (setq depth (1- depth))
                 (when (zerop depth) (setq found (point))))))
        (forward-char 1))
      found)))

(defun neocaml--doc-markup-content-face (marker)
  "Return the face for the content of a {MARKER ...} construct, if any.
MARKER is the character after the opening brace."
  (cond ((eq marker ?b) 'neocaml-doc-bold-face)
        ((memq marker '(?i ?e)) 'neocaml-doc-italic-face)
        ;; Section headings: {0 ...} to {5 ...}
        ((<= ?0 marker ?9) 'neocaml-doc-heading-face)))

(defun neocaml--doc-markup-fontify-span (open open-end close-beg close-end
                                              content-face override)
  "Fontify one markup construct.
OPEN..OPEN-END is the opening delimiter and CLOSE-BEG..CLOSE-END the
closing one; both get `neocaml-doc-markup-face'.  The content between
them gets CONTENT-FACE, when non-nil.  OVERRIDE is the override flag
from `treesit-font-lock-rules'."
  (treesit-fontify-with-override
   open open-end 'neocaml-doc-markup-face override)
  (when content-face
    (treesit-fontify-with-override open-end close-beg content-face override))
  (treesit-fontify-with-override
   close-beg close-end 'neocaml-doc-markup-face override))

(defun neocaml--fontify-doc-markup (node override _start end &rest _)
  "Fontify ocamldoc markup inside the comment NODE.
Only doc comments are touched; plain comments are left alone.
OVERRIDE is the override flag from `treesit-font-lock-rules'.
Scanning always starts from the beginning of the node - starting
mid-comment could miss an opening marker - but stops once past END,
the end of the region being fontified; jit-lock asks for the rest
later."
  (let ((limit (treesit-node-end node)))
    (save-excursion
      (goto-char (treesit-node-start node))
      (when (looking-at neocaml--doc-comment-regexp)
        (forward-char 3)                ; skip the (**
        (while (< (point) (min limit end))
          (let ((c (char-after)))
            (cond
             ;; Escape sequences: \{ \} \[ \] \@
             ((eq c ?\\)
              (forward-char
               (if (memq (char-after (1+ (point))) '(?\{ ?\} ?\[ ?\] ?@)) 2 1)))
             ((eq c ?\{)
              (cond
               ;; {[ code block ]} and {v verbatim v} - no markup inside
               ((looking-at "{\\(?:\\(\\[\\)\\|v[ \t\n]\\)")
                (let* ((open (point))
                       (opener-end (match-end 0))
                       (closer (if (match-beginning 1) "]}" "v}"))
                       (close (save-excursion
                                (search-forward closer limit t))))
                  (if (not close)
                      (goto-char opener-end)
                    (neocaml--doc-markup-fontify-span
                     open opener-end (- close 2) close
                     'neocaml-doc-code-face override)
                    (goto-char close))))
               ;; {!ref}, {:url}, {{!ref} ...}, {{:url} ...} - the target
               ;; runs to the brace closing the innermost opener
               ((looking-at "{{?[!:]")
                (let* ((open (point))
                       (target-beg (match-end 0))
                       (close (save-excursion
                                (goto-char target-beg)
                                (neocaml--doc-markup-close ?\{ ?\} limit))))
                  (if (not close)
                      (goto-char target-beg)
                    (neocaml--doc-markup-fontify-span
                     open target-beg close (1+ close)
                     'neocaml-doc-reference-face override)
                    (goto-char (1+ close)))))
               ;; {b ...}, {i ...}, {e ...}, {2 ...} headings, {^ ...},
               ;; {_ ...}, {C ...}, {L ...}, {R ...}
               ((looking-at
                 "{\\(?:[0-9]\\(?::[^ \t\n{}]+\\)?\\|[bie^_CLR]\\)[ \t\n]")
                (let* ((open (point))
                       (marker-end (match-end 0))
                       (close (save-excursion
                                (goto-char marker-end)
                                (neocaml--doc-markup-close ?\{ ?\} limit))))
                  (if (not close)
                      (goto-char marker-end)
                    (neocaml--doc-markup-fontify-span
                     open marker-end close (1+ close)
                     (neocaml--doc-markup-content-face (char-after (1+ open)))
                     override)
                    ;; Continue scanning inside the construct so that
                    ;; nested markup like {b ... {i ...}} stacks its faces.
                    (goto-char marker-end))))
               (t (forward-char 1))))
             ;; [inline code]
             ((eq c ?\[)
              (let ((open (point))
                    (close (save-excursion
                             (forward-char 1)
                             (neocaml--doc-markup-close ?\[ ?\] limit))))
                (if (not close)
                    (forward-char 1)
                  (neocaml--doc-markup-fontify-span
                   open (1+ open) close (1+ close)
                   'neocaml-doc-code-face override)
                  (goto-char (1+ close)))))
             ;; @tags, at the start of a line or after whitespace
             ((eq c ?@)
              (if (and (memq (char-before) '(?\s ?\t ?\n ?*))
                       (looking-at neocaml--doc-markup-tag-regexp))
                  (progn
                    (treesit-fontify-with-override
                     (point) (match-end 0) 'neocaml-doc-markup-face override)
                    (goto-char (match-end 0)))
                (forward-char 1)))
             ;; Plain text: hop to the next markup candidate at C speed.
             (t
              (forward-char 1)
              (skip-chars-forward "^\\\\{[@" limit)))))))))

;; The `ocaml-interface' grammar inherits all node types from the base
;; `ocaml' grammar (overriding only `compilation_unit'), so queries
;; referencing .ml-only constructs (e.g. `application_expression',
;; `let_binding') silently produce no matches in .mli files.  This
;; lets us use a single set of font-lock rules for both languages.
(defun neocaml-mode--font-lock-settings (language)
  "Return tree-sitter font-lock settings for LANGUAGE.
The return value is suitable for `treesit-font-lock-settings'."
  (append
   (treesit-font-lock-rules
    :language language
    :feature 'comment
    `((((comment) @font-lock-doc-face)
       (:match ,neocaml--doc-comment-regexp @font-lock-doc-face))
      (comment) @font-lock-comment-face
      ;; Preprocessor directives
      (line_number_directive) @font-lock-comment-face
      (directive) @font-lock-comment-face)

   ;; Must come after `comment' so the markup faces stack on top of
   ;; `font-lock-doc-face' rather than being painted over by it.  The
   ;; doc-comment check happens inside the function (a `:match' here
   ;; would cons the whole comment text just to test its first chars).
   :language language
   :feature 'doc-markup
   :override 'prepend
   '((comment) @neocaml--fontify-doc-markup)

   :language language
   :feature 'definition
   '(;; let-bound functions: with parameters, or with fun/function body
     (let_binding pattern: (value_name) @font-lock-function-name-face (parameter)+)
     (let_binding pattern: (value_name) @font-lock-function-name-face body: (fun_expression))
     (let_binding pattern: (value_name) @font-lock-function-name-face body: (function_expression))
     ;; let-bound variables (must come after function patterns above)
     (let_binding pattern: (value_name) @font-lock-variable-name-face (":" (_)) :? (":>" (_)) :? :anchor body: (_))
     (method_definition (method_name) @font-lock-function-name-face)
     (method_specification (method_name) @font-lock-function-name-face)
     ;; patterns containing bound variables, and the `_' wildcard
     (value_pattern) @font-lock-variable-name-face
     (any_pattern) @font-lock-variable-name-face
     (constructor_pattern pattern: (value_name) @font-lock-variable-name-face)
     (tuple_pattern (value_name) @font-lock-variable-name-face)
     ;; punned record fields in patterns
     (field_pattern (field_path (field_name) @font-lock-variable-name-face) :anchor)
     (field_pattern (field_path (field_name) @font-lock-variable-name-face) (type_constructor_path) :anchor)
     ;; signatures and misc
     (instance_variable_name) @font-lock-variable-name-face
     (value_specification (value_name) @font-lock-variable-name-face)
     (value_specification ":" @font-lock-keyword-face)
     (external (value_name) @font-lock-variable-name-face)
     ;; assignment of bindings in various circumstances
     (type_binding ["="] @font-lock-keyword-face)
     (let_binding ["="] @font-lock-keyword-face)
     (field_expression ["="] @font-lock-keyword-face)
     (for_expression ["="] @font-lock-keyword-face)
     (comprehension_binding ["="] @font-lock-keyword-face)
     (kind_definition ["="] @font-lock-keyword-face))

   ;; Upgrade function-typed `val' specifications (in .mli files) to the
   ;; function face, mirroring how function `let' bindings are highlighted
   ;; in implementations.  This overrides the generic `value_specification'
   ;; rule above, so it lives in its own `:override' block under the same
   ;; `definition' feature.
   :language language
   :feature 'definition
   :override t
   '((value_specification (value_name) @font-lock-function-name-face (function_type)))

   :language language
   :feature 'keyword
   `([,@neocaml-mode--keywords] @font-lock-keyword-face
     (fun_expression "->" @font-lock-keyword-face)
     (match_case "->" @font-lock-keyword-face)
     (value_definition [(let_operator) (let_and_operator)] @font-lock-keyword-face)
     (match_expression (match_operator) @font-lock-keyword-face)
     ;; Only in kinds: `x mod 2' uses the same token as an operator
     (mod_bounded_kind "mod" @font-lock-keyword-face))

   ;; See https://ocaml.org/manual/5.3/attributes.html
   ;; and https://ocaml.org/manual/5.3/extensionnodes.html
   :language language
   :feature 'attribute
   '((attribute) @font-lock-preprocessor-face
     (item_attribute) @font-lock-preprocessor-face
     (floating_attribute) @font-lock-preprocessor-face
     ;; PPX extension nodes: [%foo ...], [%%foo ...], {%foo| ... |},
     ;; {%%foo| ... |}
     (extension) @font-lock-preprocessor-face
     (item_extension) @font-lock-preprocessor-face
     (quoted_extension) @font-lock-preprocessor-face
     (quoted_item_extension) @font-lock-preprocessor-face)

   :language language
   :feature 'string
   :override t
   '([(string) (quoted_string) (character)] @font-lock-string-face)

   :language language
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face
     (conversion_specification) @font-lock-regexp-face)

   :language language
   :feature 'number
   :override t
   '([(number) (signed_number)] @font-lock-number-face)

   :language language
   :feature 'builtin
   ;; NOTE: these predicates must not use `regexp-opt' with `symbols', which
   ;; wraps the alternation in \_< ... \_>.  Evaluating a symbol-boundary
   ;; assertion inside a `:match' predicate pulls in lazy `syntax-propertize'
   ;; while `treesit-query-capture' is still walking the query cursor, which
   ;; segfaults Emacs 31.1 (bug#81729, neocaml#67).  The captures are whole
   ;; nodes, so anchoring the match to the node text is both safe and exact.
   `(((value_path :anchor (value_name) @font-lock-builtin-face)
      (:match ,(concat "\\`" (regexp-opt neocaml-mode--builtin-ids) "\\'") @font-lock-builtin-face))
     ((constructor_path :anchor (constructor_name) @font-lock-builtin-face)
      (:match ,(concat "\\`" (regexp-opt neocaml-mode--builtin-ids) "\\'") @font-lock-builtin-face))
     ;; Builtin types (int, string, bool, etc.)
     ((type_constructor) @font-lock-builtin-face
      (:match ,(concat "\\`" (regexp-opt neocaml-mode--builtin-types) "\\'") @font-lock-builtin-face))
     ;; OxCaml's SIMD vectors of 128, 256 and 512 bits, boxed and unboxed
     ;; (upstream's pattern misses some, tree-sitter/tree-sitter-ocaml#156)
     ((type_constructor) @font-lock-builtin-face
      (:match ,(rx bos (or (seq "int8x" (or "16" "32" "64"))
                           (seq (or "int" "float") "16x" (or "8" "16" "32"))
                           (seq (or "int" "float") "32x" (or "4" "8" "16"))
                           (seq (or "int" "float") "64x" (or "2" "4" "8")))
                   (? "#") eos)
              @font-lock-builtin-face))
     ;; OxCaml's builtin kinds (value, immediate, float64, etc.), unless
     ;; qualified with a module
     ((kind_path :anchor (kind_name) @font-lock-builtin-face)
      (:match ,(concat "\\`" (regexp-opt neocaml-mode--builtin-kinds) "\\'") @font-lock-builtin-face))
     ;; OxCaml's block index types
     ((block_access_type) @font-lock-builtin-face
      (:match "\\`idx_\\(?:imm\\|mut\\)\\'" @font-lock-builtin-face)))

   ;; See https://ocaml.org/manual/5.3/const.html
   :language language
   :feature 'constant
   `(;; some literals TODO: any more?
     [,@neocaml-mode--constants] @font-lock-constant-face)

   ;; Variant constructors and polymorphic variant tags
   :language language
   :feature 'type
   '((constructor_name) @font-lock-constant-face
     (tag) @font-lock-constant-face
     [(type_constructor) (type_variable) (hash_type) (any_type)
      (class_name) (class_type_name)] @font-lock-type-face
      ;; OxCaml modes, modalities and kinds
      [(mode) (modality) (kind_name) (any_kind) (scannable_axis)
       (block_access_type)] @font-lock-type-face
      (product_kind "&" @font-lock-type-face)
      (function_type "->" @font-lock-type-face)
      (tuple_type ["*" "#(" ")"] @font-lock-type-face)
      (polymorphic_variant_type ["[>" "[<" ">" "|" "[" "]"] @font-lock-type-face)
      (object_type ["<" ">" ";" ".."] @font-lock-type-face)
      (constructor_declaration ["->" "*"] @font-lock-type-face)
      (record_declaration ["{" "#{" "}" ";"] @font-lock-type-face)
      (parenthesized_type ["(" ")"] @font-lock-type-face)
      (polymorphic_type "." @font-lock-type-face)
      (module_name) @font-lock-type-face
      (module_type_name) @font-lock-type-face)

   ;; Level 4 font-locking features

   :language language
   :feature 'operator
   '((method_invocation "#" @font-lock-operator-face)
     (infix_expression operator: _  @font-lock-operator-face)
     (prefix_expression operator: _ @font-lock-operator-face)
     ;; Standalone operator tokens not inside infix/prefix expressions
     ["::" "<-"] @font-lock-operator-face)

   :language language
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}" "[|" "|]" "[<" "[>" "[:" ":]" "#(" "#{"])
     @font-lock-bracket-face)

   :language language
   :feature 'delimiter
   '((["," "." ";" ":" ";;" ".#" "@" "@@"]) @font-lock-delimiter-face)

   :language language
   :feature 'variable
   '((value_name) @font-lock-variable-use-face)

   :language language
   :feature 'property
   '((field_name) @font-lock-property-use-face)

   :language language
   :feature 'label
   '(;; Labeled arguments: ~label, ?label
     (label_name) @font-lock-property-use-face)

   :language language
   :feature 'function
   :override t
   '((application_expression function: (value_path (value_name) @font-lock-function-call-face))
     (application_expression function: (value_path (module_path (_) @font-lock-type-face) (value_name) @font-lock-function-call-face))
     ;; x |> f — highlight f as function call
     ((infix_expression
       operator: (rel_operator) @_op
       right: (value_path (value_name) @font-lock-function-call-face))
      (:match "^|>$" @_op))
     ;; f @@ x — highlight f as function call
     ((infix_expression
       left: (value_path (value_name) @font-lock-function-call-face)
       operator: (concat_operator) @_op)
      (:match "^@@$" @_op))))
   ;; shebang is only valid in the ocaml grammar, not ocaml-interface
   (when (eq language 'ocaml)
     (treesit-font-lock-rules
      :language 'ocaml
      :feature 'comment
      '((shebang) @font-lock-comment-face)))))


;;;; Indentation

;; Tree-sitter indentation rules for OCaml
;; Adapted from nvim indentation queries in nvim-treesitter
;;
;; The `ocaml-interface' grammar shares all node types with `ocaml',
;; so a single set of indentation rules works for both languages.
;;
;; NB: `treesit--indent-1' sets NODE to the largest node
;; whose start equals BOL.  For continuation lines inside a multi-line
;; node (e.g., lines inside a comment), NODE is nil and PARENT is the
;; enclosing node.  This means `node-is' won't match those lines —
;; use `parent-is' instead, and place such rules before `no-node'.

;; The handful of layouts the indentation styles disagree on consult
;; `neocaml--indent-style' when they're applied, so a style set by a
;; directory-local variable or switched in a buffer takes effect without
;; rebuilding the rules.

(defvar-local neocaml--ocamlformat-indentation 'unknown
  "The indentation style and case offset of the project's .ocamlformat.
A cons (STYLE . CASE-OFFSET), where CASE-OFFSET is its
cases-exp-indent, or nil when it doesn't set one.  nil when there's
no .ocamlformat, and `unknown' until it's been looked up.")

(defun neocaml--read-ocamlformat-indentation ()
  "Return the indentation style and case offset of the file's .ocamlformat.
See `neocaml--ocamlformat-indentation'.  As in ocamlformat, the files
in the file's directory and its parents up to the project root (the
nearest one with a .git, .hg or dune-project) apply from the root
down, and a profile resets cases-exp-indent to the profile's own.  A
file without a profile gets the conventional one, ocamlformat's
default."
  (when buffer-file-name
    (let (files profile cases)
      (locate-dominating-file
       (file-name-directory buffer-file-name)
       (lambda (dir)
         (let ((file (expand-file-name ".ocamlformat" dir)))
           (when (file-readable-p file)
             (push file files)))
         ;; Stop at the project root
         (seq-some (lambda (marker) (file-exists-p (expand-file-name marker dir)))
                   '(".git" ".hg" "dune-project"))))
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (while (re-search-forward
                  "^[ \t]*\\(profile\\|cases-exp-indent\\)[ \t]*=[ \t]*\\([^ \t\n#]+\\)"
                  nil t)
            (if (equal (match-string 1) "profile")
                (setq profile (match-string 2)
                      cases nil)
              (setq cases (string-to-number (match-string 2)))))))
      (when files
        (cons (if (equal profile "janestreet") 'janestreet 'conventional)
              cases)))))

(defun neocaml--ocamlformat-indentation ()
  "Return the indentation style and case offset of the project's .ocamlformat.
They're looked up once per buffer (see `neocaml--ocamlformat-indentation')."
  (when (eq neocaml--ocamlformat-indentation 'unknown)
    (setq neocaml--ocamlformat-indentation (neocaml--read-ocamlformat-indentation)))
  neocaml--ocamlformat-indentation)

(defun neocaml--indent-style ()
  "Return the indentation style of the current buffer.
That's `neocaml-indent-style', or when it's nil, the style of the
project's .ocamlformat, or `ocp-indent'."
  (or neocaml-indent-style
      (car (neocaml--ocamlformat-indentation))
      'ocp-indent))

(defun neocaml-set-indent-style (style)
  "Set the indentation style of the current buffer to STYLE.
STYLE is one of the styles of `neocaml-indent-style', or nil to take
the style from the project's .ocamlformat.  This doesn't reindent the
buffer."
  (interactive
   (let ((choices '(("from .ocamlformat") ("ocp-indent" . ocp-indent)
                    ("conventional" . conventional) ("janestreet" . janestreet))))
     (list (cdr (assoc (completing-read
                        "Indentation style: " choices nil t nil nil
                        (car (rassq neocaml-indent-style choices)))
                       choices)))))
  (setq-local neocaml-indent-style style))

(defun neocaml--case-body-offset (_node parent &rest _)
  "Return the offset of the body of PARENT, a match case, on a line of its own.
It's counted from the start of the case's line.  When that's the
case's |, the offset is the cases-exp-indent of the project's
.ocamlformat, when it sets one and `neocaml-indent-style' is nil, or
else twice `neocaml-indent-offset' in the conventional style and
`neocaml-indent-offset' in the others.  A case after a `with' on the
same line has its body `neocaml-indent-offset' in, in every style:
  try f x with
  | Not_found ->
      g x
  try f x
  with Not_found ->
    g x"
  (cond
   ((not (eq (char-after (neocaml--node-bol parent)) ?|))
    neocaml-indent-offset)
   ((and (null neocaml-indent-style)
         (cdr (neocaml--ocamlformat-indentation))))
   ((eq (neocaml--indent-style) 'conventional)
    (* 2 neocaml-indent-offset))
   (t neocaml-indent-offset)))

(defun neocaml--parameter-offset (&rest _)
  "Return the offset of a parameter starting a line from its definition.
The janestreet style puts parameters three times `neocaml-indent-offset'
in, and the others twice."
  (* (if (eq (neocaml--indent-style) 'janestreet) 3 2) neocaml-indent-offset))

(defun neocaml--grand-parent-bol (_node parent _bol &rest _)
  "Return the first non-whitespace position on PARENT's parent's line.
This is like `parent-bol' but goes one level up in the tree.
Useful when PARENT (like `variant_declaration') starts on the same
line as its first child, causing `parent-bol' to shift after that
child is indented."
  (when-let* ((gp (treesit-node-parent parent)))
    (save-excursion
      (goto-char (treesit-node-start gp))
      (back-to-indentation)
      (point))))

(defun neocaml--comment-body-anchor (node parent _bol &rest _)
  "Return the position of the comment body start.
Uses NODE if non-nil, otherwise PARENT (for continuation lines
where NODE is nil).  Used as an indentation anchor so that lines
inside a multi-line comment align with the text after the opening
delimiter."
  (let ((comment (or node parent)))
    (save-excursion
      (goto-char (treesit-node-start comment))
      (if (looking-at "(\\*+[ \t]*")
          (goto-char (match-end 0))
        (forward-char))
      (point))))

(defun neocaml--exclave-body-anchor (_node parent bol &rest _)
  "Return the anchor for the line at BOL in the body of PARENT, an `exclave_'.
`exclave_' doesn't nest: as ocamlformat lays it out, its body goes
where the `exclave_' expression itself would, whether that follows a
`=' or `->' or starts a line of its own.  So the indentation rules are
applied to PARENT in place of the line's node."
  (car (treesit-simple-indent parent (treesit-node-parent parent) bol)))

(defun neocaml--exclave-body-offset (_node parent bol &rest _)
  "Return the offset for the line at BOL in the body of PARENT, an `exclave_'.
See `neocaml--exclave-body-anchor'."
  (cdr (treesit-simple-indent parent (treesit-node-parent parent) bol)))

(defun neocaml--chain-start (parent)
  "Return the operator chain PARENT belongs to.
PARENT is an infix expression, a cons expression or pattern, or a
function type.  A chain like `a |> f |> g' or `a ^ b ^ c' nests one
node per operator, so climb out through the parents with the same
operator to the whole chain."
  (let ((type (treesit-node-type parent))
        (operator (treesit-node-text
                   (treesit-node-child-by-field-name parent "operator") t)))
    (treesit-parent-while
     parent
     (lambda (node)
       (and (equal (treesit-node-type node) type)
            (equal (treesit-node-text
                    (treesit-node-child-by-field-name node "operator") t)
                   operator))))))

(defun neocaml--or-pattern-anchor (_node parent _bol &rest _)
  "Return the anchor for a | that continues PARENT, an or-pattern.
It aligns with the | of the match case, or goes under the parenthesis
of a parenthesized or-pattern, as ocamlformat lays them out:
  | A
  | ( B
    | C ) -> ..."
  (let* ((pattern (treesit-parent-while
                   parent
                   (lambda (node) (equal (treesit-node-type node) "or_pattern"))))
         (outer (treesit-node-parent pattern)))
    (if (equal (treesit-node-type outer) "parenthesized_pattern")
        (treesit-node-start outer)
      (save-excursion
        (goto-char (treesit-node-start pattern))
        (back-to-indentation)
        (point)))))

(defun neocaml--at-bol-p (node)
  "Return non-nil if NODE is the first thing on its line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (skip-chars-backward " \t")
    (bolp)))

(defun neocaml--node-bol (node)
  "Return the first non-whitespace position on the first line of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun neocaml--opens-parens-p (node)
  "Return non-nil if NODE comes right after the parenthesis around it."
  (let ((outer (treesit-node-parent node)))
    (and (member (treesit-node-type outer)
                 '("parenthesized_expression" "parenthesized_pattern"
                   "parenthesized_type"))
         (= (treesit-node-start node) (1+ (treesit-node-start outer))))))

(defun neocaml--paren-content-anchor (node)
  "Return where lines inside the parentheses NODE opens line up.
That's NODE, but the parenthesis in the conventional style, as
ocp-indent and ocamlformat's profiles lay them out:
  (match x with    (match x with
   | A -> ...)     | A -> ...)"
  (if (eq (neocaml--indent-style) 'conventional)
      (treesit-node-start (treesit-node-parent node))
    (treesit-node-start node)))

(defun neocaml--chain-anchor (node parent _bol &rest _)
  "Return the anchor for the line at NODE, a continuation of PARENT's chain.
PARENT is part of the chain (see `neocaml--chain-start' for what
counts), and the line aligns with the start of the whole chain (see
`neocaml--chain-start'), as ocp-indent and ocamlformat lay it out.
The exceptions:
- in a chain right inside parentheses, an operator starting a line,
  and every line of a function type, go where the parentheses' content
  does (see `neocaml--paren-content-anchor'), which is under the
  parenthesis in the conventional style:
    (\"a\"
    ^ \"b\")
    (int ->
    int)
- a leading -> goes under the : before the type when that starts its
  line, as in ocamlformat's janestreet profile:
    val f
      :  int
      -> int"
  (let* ((chain (neocaml--chain-start parent))
         (outer (treesit-node-parent chain)))
    (cond
     ((and (neocaml--opens-parens-p chain)
           (or (equal (treesit-node-type outer) "parenthesized_type")
               ;; An operator (:: has no field of its own)
               (and node
                    (or (equal (treesit-node-type node) "::")
                        (equal (treesit-node-field-name node) "operator")))))
      (neocaml--paren-content-anchor chain))
     ((and (equal (treesit-node-type node) "->")
           (let ((before (treesit-node-prev-sibling chain)))
             (and (equal (treesit-node-type before) ":")
                  (neocaml--at-bol-p before)
                  (treesit-node-start before)))))
     (t (treesit-node-start chain)))))

(defun neocaml--chain-offset (node parent _bol &rest _)
  "Return the offset for the line at NODE, a continuation of PARENT's chain.
The right side of an assignment (`r :=' at the end of a line) is a body,
so it's indented.  Everything else aligns (see `neocaml--chain-anchor')."
  (if (and node
           (equal (treesit-node-type
                   (treesit-node-child-by-field-name parent "operator"))
                  "assign_operator")
           (equal (treesit-node-field-name node) "right"))
      neocaml-indent-offset
    0))

(defun neocaml--case-anchor (_node parent &rest _)
  "Return the anchor for a case of PARENT, a `match' or `try'.
The cases line up with the start of PARENT's line, or when PARENT is
right inside parentheses, with the parentheses' content (see
`neocaml--paren-content-anchor')."
  (if (neocaml--opens-parens-p parent)
      (neocaml--paren-content-anchor parent)
    (neocaml--node-bol parent)))

(defun neocaml--with-anchor (_node parent &rest _)
  "Return the anchor for the `with' of PARENT, a `match' or `try'.
It goes at the start of PARENT's line, or under the keyword when
PARENT is right inside parentheses, in every style:
  (try f ()
   with Not_found -> 0)"
  (if (neocaml--opens-parens-p parent)
      (treesit-node-start parent)
    (neocaml--node-bol parent)))

(defun neocaml--case-body-parens-p (_node parent &rest _)
  "Return non-nil if PARENT is parentheses opening a match case's body.
They open at the end of the case's line:
  | A -> (
      match ...)
  | B -> begin
      ...
    end"
  (and (equal (treesit-node-type parent) "parenthesized_expression")
       (equal (treesit-node-type (treesit-node-parent parent)) "match_case")
       (equal (treesit-node-field-name parent) "body")
       (not (neocaml--at-bol-p parent))))

(defun neocaml--parenthesized-offset (_node parent &rest _)
  "Return the offset for a line in PARENT, a parenthesized expression.
That's `neocaml-indent-offset', but in parentheses opening a match
case's body (see `neocaml--case-body-parens-p'), the lines go one step
past the case's body, as ocp-indent lays them out, and where the body
goes in the conventional style, as ocamlformat's conventional profile
lays them out."
  (if (neocaml--case-body-parens-p nil parent)
      (+ (neocaml--case-body-offset nil (treesit-node-parent parent))
         (if (eq (neocaml--indent-style) 'conventional) 0 neocaml-indent-offset))
    neocaml-indent-offset))

(defun neocaml--functor-argument (node)
  "Return the functor argument or parameter NODE belongs to, or nil.
NODE is a structure passed to a functor in parentheses, which belongs
to the whole application, as in `F (struct ... end) (struct ... end)',
or the signature of a functor's parameter, as in
`module F (X : sig ... end)', which belongs to the parameter."
  (pcase (treesit-node-type node)
    ("structure"
     (let ((outer (treesit-node-parent node)))
       (when (and (equal (treesit-node-type outer) "parenthesized_module_expression")
                  (equal (treesit-node-field-name outer) "argument"))
         (treesit-parent-while
          outer
          (lambda (n)
            (member (treesit-node-type n)
                    '("parenthesized_module_expression" "module_application")))))))
    ("signature"
     (let ((outer (treesit-node-parent node)))
       (and (equal (treesit-node-type outer) "module_parameter") outer)))))

(defun neocaml--functor-argument-p (_node parent &rest _)
  "Return non-nil if PARENT is a functor argument or parameter to indent more.
That's a structure passed to a functor in parentheses further along a
line, or the signature of a functor's parameter (see
`neocaml--functor-argument'), outside the conventional style, which
indents them like any other.  See `neocaml--functor-indentation' for
where they go."
  (and (not (eq (neocaml--indent-style) 'conventional))
       (when-let* ((argument (neocaml--functor-argument parent)))
         (or (equal (treesit-node-type argument) "module_parameter")
             (not (neocaml--at-bol-p argument))))))

(defun neocaml--functor-indentation (parent)
  "Return (ANCHOR BODY END) for the lines of PARENT, a functor argument.
PARENT can also be a functor parameter's signature.
BODY and END are the offsets of its body and `end' from ANCHOR.  A
structure passed to a functor, and the signature of the first
parameter of a module definition on the definition's first line, go
as if the functor started a line of its own, as ocp-indent and
ocamlformat's janestreet profile lay them out:
  module M = F (struct
      let x = 1
    end)
  module F (X : sig
      type t
    end) =
The signatures of other parameters go from inside their parentheses:
  module F
      (X : sig
         type t
       end)"
  (let ((argument (neocaml--functor-argument parent))
        (step neocaml-indent-offset))
    (if (or (not (equal (treesit-node-type argument) "module_parameter"))
            (let ((definition (treesit-node-parent argument)))
              (and (equal (treesit-node-type definition) "module_binding")
                   (= (neocaml--node-bol argument) (neocaml--node-bol definition))
                   (not (neocaml--at-bol-p argument))
                   (setq argument definition))))
        (list (neocaml--node-bol argument) (* 2 step) step)
      (list (1+ (treesit-node-start argument)) step 0))))

(defun neocaml--functor-anchor (_node parent &rest _)
  "Return the anchor for a line in PARENT, a functor argument or parameter.
See `neocaml--functor-indentation'."
  (nth 0 (neocaml--functor-indentation parent)))

(defun neocaml--functor-body-offset (_node parent &rest _)
  "Return the offset for a line of the body of PARENT, a functor argument.
See `neocaml--functor-indentation'."
  (nth 1 (neocaml--functor-indentation parent)))

(defun neocaml--functor-end-offset (_node parent &rest _)
  "Return the offset for the `end' of PARENT, a functor argument or parameter.
See `neocaml--functor-indentation'."
  (nth 2 (neocaml--functor-indentation parent)))

(defun neocaml--double-offset (&rest _)
  "Return twice `neocaml-indent-offset'."
  (* 2 neocaml-indent-offset))

(defun neocaml--module-definition (node)
  "Return the module definition NODE, a struct or sig, is the body of, or nil.
That's also through a functor, as in `module F = functor (X : S) -> struct'."
  (when (member (treesit-node-type node) '("structure" "signature"))
    (let ((outer (treesit-node-parent node)))
      (while (equal (treesit-node-type outer) "functor")
        (setq outer (treesit-node-parent outer)))
      (and (member (treesit-node-type outer) '("module_binding" "module_type_definition"))
           outer))))

(defun neocaml--module-body-anchor (_node parent &rest _)
  "Return the anchor for a line in PARENT, a struct, sig or the like.
That's the start of the line of the module definition PARENT is the
body of (see `neocaml--module-definition'), or else of PARENT's line.
The lines of a struct or sig go from the line the definition starts
on, as ocp-indent and ocamlformat lay them out, also when that's not
the line of the struct or sig:
  module M
    : S = struct
    let x = 1
  end"
  (neocaml--node-bol (or (neocaml--module-definition parent) parent)))

(defun neocaml--attribute-anchor (node &rest _)
  "Return the anchor for NODE, an attribute after an item.
The grammar makes an attribute that ends an item's body on a line of
its own, as in
  let f x =
    g x
    [@nontail]
a sibling of the item, so the anchor is the start of the item's line,
past any other attributes like it and comments."
  (let ((item (treesit-node-prev-sibling node t)))
    (while (member (treesit-node-type item) '("attribute" "comment"))
      (setq item (treesit-node-prev-sibling item t)))
    (save-excursion
      (goto-char (treesit-node-start (or item (treesit-node-parent node))))
      (back-to-indentation)
      (point))))

(defun neocaml--leading-semicolon-anchor (_node parent _bol &rest _)
  "Return the anchor for a ; starting a line in PARENT, a record, list or array.
It goes under the last character of the opening bracket, so the
elements line up, as in ocamlformat's janestreet profile:
  [| a
   ; b |]"
  (1- (treesit-node-end (treesit-node-child parent 0))))

(defun neocaml--payload-offset (node parent &rest _)
  "Return the offset for a line in an attribute or extension payload.
NODE is the payload, or PARENT is.  Payloads are indented, except for
a ppx_template [%%template ...] holding several items, which stay
where they would be without it, as ocamlformat lays them out."
  (let ((payload (if (equal (treesit-node-type parent) "attribute_payload")
                     parent
                   node)))
    (if (and (> (treesit-node-child-count payload t) 1)
             (let ((owner (treesit-node-parent payload)))
               (and (equal (treesit-node-type owner) "item_extension")
                    (equal (treesit-node-text (treesit-node-child owner 0 t) t)
                           "template"))))
        0
      neocaml-indent-offset)))

(defun neocaml--operand-function-p (_node parent &rest _)
  "Return non-nil if PARENT is a `fun' or `function' after an operator.
As in `x >>= fun y ->' or `x >>= function', but not after `:=', whose
right side is a body (see `neocaml--chain-offset')."
  (and (member (treesit-node-type parent) '("fun_expression" "function_expression"))
       (let ((outer (treesit-node-parent parent)))
         (and (equal (treesit-node-type outer) "infix_expression")
              (not (equal (treesit-node-type
                           (treesit-node-child-by-field-name outer "operator"))
                          "assign_operator"))))))

(defun neocaml--operand-function-anchor (_node parent &rest _)
  "Return the anchor for a line in the body of PARENT, a `fun' or `function'.
PARENT follows an operator (see `neocaml--operand-function-p'), so its
body or cases line up with the start of the operator chain, as
ocamlformat lays them out."
  (treesit-node-start (neocaml--chain-start (treesit-node-parent parent))))

(defun neocaml--application-of (arg)
  "Return the application ARG is passed to, possibly with a label, or nil."
  (let ((outer (treesit-node-parent arg)))
    (when (equal (treesit-node-type outer) "labeled_argument")
      (setq outer (treesit-node-parent outer)))
    (and (equal (treesit-node-type outer) "application_expression")
         outer)))

(defun neocaml--definition-item-p (node parent &rest _)
  "Return non-nil if NODE is an `and' of PARENT, or a comment between its items.
PARENT is a definition with `let', `type', `module' or `class',
whose `and', `and*' and `and+' line up with its keyword, and so do
comments after one of its bindings."
  (and (member (treesit-node-type parent)
               '("value_definition" "type_definition" "module_definition"
                 "class_definition" "class_type_definition"))
       (pcase (treesit-node-type node)
         ((or "and" "let_and_operator") t)
         ("comment"
          (string-suffix-p "_binding" (or (treesit-node-type (treesit-node-prev-sibling node t))
                                          ""))))))

(defun neocaml--let-fun-body-p (node parent &rest _)
  "Return non-nil if NODE is the body of PARENT, a `fun' starting a `let' body.
The body goes where the `fun' would, as ocp-indent and ocamlformat
lay it out:
  let f : int -> int =
    fun x ->
    x"
  (and (equal (treesit-node-type parent) "fun_expression")
       (equal (treesit-node-type (treesit-node-parent parent)) "let_binding")
       (neocaml--at-bol-p parent)
       (equal (treesit-node-field-name node) "body")))

(defun neocaml--paren-fun-body-p (node parent &rest _)
  "Return non-nil if NODE is the body of PARENT, a `fun' right inside parens.
See `neocaml--paren-fun-body-anchor' for where it goes."
  (and (equal (treesit-node-type parent) "fun_expression")
       (neocaml--opens-parens-p parent)
       (equal (treesit-node-field-name node) "body")))

(defun neocaml--paren-fun-body-anchor (_node parent &rest _)
  "Return the anchor for the body of PARENT, a `fun' right inside parentheses.
When the parenthesis starts its line, the body goes past the `fun',
as ocp-indent lays it out:
  List.map
    (fun x ->
       x + 1)
The conventional style indents it from the parenthesis instead, and
so does the janestreet style, unless PARENT is passed to a function,
as ocamlformat's profiles lay it out.  Further along a line, the body
is indented from the start of the line (see
`neocaml--paren-fun-body-offset'), but in the conventional style from
the function PARENT is passed to, as its arguments are:
  List.iter
    ~f:(fun x ->
      print x)"
  (let* ((paren (treesit-node-parent parent))
         (application (neocaml--application-of paren)))
    (cond
     ((neocaml--at-bol-p paren)
      (if (pcase (neocaml--indent-style)
            ('conventional nil)
            ('janestreet
             (and application
                  (not (equal (treesit-node-type
                               (treesit-node-child-by-field-name application "function"))
                              "constructor_path"))))
            (_ t))
          (treesit-node-start parent)
        (treesit-node-start paren)))
     ((and application (eq (neocaml--indent-style) 'conventional))
      (neocaml--argument-anchor nil application))
     (t (neocaml--node-bol paren)))))

(defun neocaml--paren-fun-body-offset (_node parent &rest _)
  "Return the offset for the body of PARENT, a `fun' right inside parentheses.
That's `neocaml-indent-offset', but outside the janestreet style,
the body of a `fun' further along its line goes twice as far, as
ocp-indent and ocamlformat's conventional profile lay it out:
  List.iter l (fun x ->
      print x)"
  (if (or (neocaml--at-bol-p (treesit-node-parent parent))
          (eq (neocaml--indent-style) 'janestreet))
      neocaml-indent-offset
    (* 2 neocaml-indent-offset)))

(defun neocaml--function-case-offset (_node parent &rest _)
  "Return the offset for a line in the cases of PARENT, a `function'.
The cases are indented from the line the `function' is on, unless the
`function' starts that line, as after `let open M in'."
  (if (neocaml--at-bol-p parent) 0 neocaml-indent-offset))

(defvar neocaml--indent-body-tokens
  '("=" "->" "then" "else" "do" "struct" "sig"
    "begin" "object" "with" "fun" "function" "try"
    ;; OxCaml (`exclave_' doesn't nest, see `neocaml--empty-line-offset')
    "stack_" "local_" "borrow_")
  "Tokens that expect a body on the next line.
Used by `neocaml--empty-line-offset' to decide whether an empty line
should be indented relative to the previous line, and by
`neocaml--argument-indentation'.")

(defun neocaml--token-before (pos)
  "Return the token before POS on its line, or nil at the start of the line.
A trailing `exclave_' doesn't nest (see `neocaml--empty-line-offset'),
so the token before it counts instead."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t")
    (unless (bolp)
      (let ((node (treesit-node-at (1- (point)))))
        (if (not (equal (treesit-node-type node) "exclave_"))
            node
          (goto-char (treesit-node-start node))
          (skip-chars-backward " \t")
          (unless (bolp)
            (treesit-node-at (1- (point)))))))))

(defun neocaml--empty-line-offset (_node _parent bol)
  "Compute extra indentation offset for an empty line at BOL.
If the last token on the previous line expects a body (e.g., `=',
`->', `then'), return `neocaml-indent-offset'.  Otherwise return 0,
which preserves the previous line's indentation level.  A trailing
`exclave_' doesn't nest, so the token before it decides."
  (save-excursion
    (goto-char bol)
    (if (and (zerop (forward-line -1))
             (member (treesit-node-type (neocaml--token-before (line-end-position)))
                     neocaml--indent-body-tokens))
        neocaml-indent-offset
      0)))

(defun neocaml--operand-chain (node)
  "Return the operator chain, tuple or sequence NODE is an operand of, or nil."
  (let* ((outer (treesit-node-parent node))
         (type (treesit-node-type outer)))
    (pcase type
      ((or "infix_expression" "cons_expression") (neocaml--chain-start outer))
      ((or "tuple_expression" "sequence_expression")
       (treesit-parent-while outer (lambda (n) (equal (treesit-node-type n) type)))))))

(defun neocaml--argument-indentation (application)
  "Return (ANCHOR . OFFSET) for an argument of APPLICATION starting a line.
Arguments go past the function, also when it comes further along its
line after a parenthesis or a bracket:
  (Printf.sprintf \"%s\" msg
     (to_string bt))
Where else they do depends on the style:
- After a token that starts a body (=, then, fun ... -> and the like)
  they go two steps past the start of the line, one after `in', as
  ocp-indent lays them out, in every style but janestreet.
- After an operator, a comma or a semicolon, they go past the start of
  the chain, tuple or sequence in the ocp-indent style, as ocp-indent
  lays them out:
    let v = x |> g a
              b
- The janestreet style puts them past the function everywhere, and
  the conventional style everywhere but after a body token, as
  ocamlformat's profiles and ocp-indent's JaneStreet preset do."
  (let* ((style (neocaml--indent-style))
         (before (and (not (eq style 'janestreet))
                      (neocaml--token-before (treesit-node-start application))))
         (type (treesit-node-type before)))
    (cond
     ((equal type "in")
      (cons (neocaml--node-bol application) neocaml-indent-offset))
     ((and (member type neocaml--indent-body-tokens)
           ;; The -> of a match case and the = of a record field don't
           ;; count, as ocp-indent has it
           (not (member (treesit-node-type (treesit-node-parent before))
                        '("match_case" "field_expression"))))
      (cons (neocaml--node-bol application) (* 2 neocaml-indent-offset)))
     ((when-let* (((and before (eq style 'ocp-indent)))
                  (chain (neocaml--operand-chain application)))
        (cons (treesit-node-start chain) neocaml-indent-offset)))
     (t (cons (treesit-node-start application) neocaml-indent-offset)))))

(defun neocaml--argument-anchor (_node parent &rest _)
  "Return the anchor for an argument of PARENT, an application, starting a line.
See `neocaml--argument-indentation'."
  (car (neocaml--argument-indentation parent)))

(defun neocaml--argument-offset (_node parent &rest _)
  "Return the offset for an argument of PARENT, an application, starting a line.
See `neocaml--argument-indentation'."
  (cdr (neocaml--argument-indentation parent)))

(defun neocaml--typed-pattern-anchor (_node parent &rest _)
  "Return the anchor for a line in PARENT, a typed pattern like `(x : t)'.
That's its pattern in the ocp-indent style, as ocp-indent lays out its
type, and its parenthesis in the others, as ocamlformat does:
  (x :         (x :
     t)          t)"
  (treesit-node-start
   (if (eq (neocaml--indent-style) 'ocp-indent)
       (treesit-node-child-by-field-name parent "pattern")
     parent)))

(defun neocaml--typed-expression-anchor (_node parent &rest _)
  "Return the anchor for the : of PARENT, a typed expression, starting a line.
It goes under the expression, but a column further in the
conventional style, as ocp-indent and ocamlformat's profiles lay it
out:
  ({ x = 1 }     ({ x = 1 }
   : t)            : t)"
  (let ((start (treesit-node-start (treesit-node-child-by-field-name parent "expression"))))
    (if (eq (neocaml--indent-style) 'conventional) (1+ start) start)))

(defun neocaml--polymorphic-type-aligned-p (_node parent &rest _)
  "Return non-nil if PARENT is a polymorphic type its type lines up with.
It does when PARENT starts its line, and in the janestreet style, as
ocp-indent and ocamlformat lay it out:
  val f :         val f
    \\='a.             :  \\='a.
    \\='a -> \\='a           \\='a -> \\='a"
  (and (equal (treesit-node-type parent) "polymorphic_type")
       (or (neocaml--at-bol-p parent)
           (eq (neocaml--indent-style) 'janestreet))))

(defun neocaml--polymorphic-type-offset (_node parent &rest _)
  "Return the offset for a line in PARENT, a polymorphic type like `type a.'.
When the type doesn't line up with PARENT (see
`neocaml--polymorphic-type-aligned-p'), it goes `neocaml-indent-offset'
past the start of the line, but for the type of a let binding in the
conventional style, ocamlformat's conventional profile indents it like
the binding's parameters."
  (if (and (eq (neocaml--indent-style) 'conventional)
           (equal (treesit-node-type (treesit-node-parent parent)) "let_binding"))
      (neocaml--parameter-offset)
    neocaml-indent-offset))

(defun neocaml--type-parameter-comma-anchor (_node parent &rest _)
  "Return the anchor for a comma starting a line in the parameters of PARENT.
PARENT is a type applied to them, like `(a, b) result'.  The comma goes
under the parenthesis, as ocp-indent lays it out, but under the first
parameter in the janestreet style, as ocamlformat's janestreet profile
does:
  ( int        ( int
  , string )     , string )"
  (treesit-node-start
   (if (eq (neocaml--indent-style) 'janestreet)
       (treesit-node-child parent 1)
     parent)))

(defun neocaml--first-element-anchor (_node parent &rest _)
  "Return the start of the first named child of PARENT.
That's the first element of a tuple, also past the #( of an OxCaml
unboxed one, or the pattern of a typed pattern."
  (treesit-node-start (treesit-node-child parent 0 t)))

(defun neocaml--let-binding-offset (&rest _)
  "Return the offset for a let binding on the line after its let.
As in `let%expect_test' followed by the test's name.  That's twice
`neocaml-indent-offset', as ocamlformat lays it out, but once in the
ocp-indent style, as ocp-indent does."
  (if (eq (neocaml--indent-style) 'ocp-indent)
      neocaml-indent-offset
    (neocaml--double-offset)))

(defun neocaml--indent-rules (language)
  "Return tree-sitter indentation rules for LANGUAGE.
The return value is suitable for `treesit-simple-indent-rules'."
  `((,language
     ;; Comment continuation lines: align with the body text after
     ;; the opening delimiter.  Must come before `no-node' because
     ;; lines inside a multi-line comment have node=nil, parent=comment.
     ((parent-is "comment") neocaml--comment-body-anchor 0)

     ;; Empty lines: use previous line's indentation, adding offset
     ;; when the previous line ends with a body-expecting token.
     ;; Must come before the top-level rule because Emacs sets
     ;; node=nil, parent=compilation_unit for empty lines.
     (no-node prev-line neocaml--empty-line-offset)

     ;; An attribute on a line of its own after an item continues the
     ;; item's body (see `neocaml--attribute-anchor')
     ((match "\\`attribute\\'" "\\`\\(?:compilation_unit\\|structure\\|attribute_payload\\|item_extension\\)\\'")
      neocaml--attribute-anchor neocaml-indent-offset)

     ;; Top-level definitions: column 0
     ((parent-is "compilation_unit") column-0 0)

     ;; Closing delimiters align with the opening construct, those of
     ;; parentheses opening a match case's body with the body
     ((and (node-is "\\`\\(?:)\\|end\\)\\'") neocaml--case-body-parens-p)
      parent-bol neocaml-indent-offset)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "done") parent-bol 0)
     ((and (node-is "end") neocaml--functor-argument-p)
      neocaml--functor-anchor neocaml--functor-end-offset)
     ((node-is "end") neocaml--module-body-anchor 0)

     ;; "with" keyword aligns with match/try (see `neocaml--with-anchor')
     ((match "\\`with\\'" "\\`\\(?:match\\|try\\)_expression\\'") neocaml--with-anchor 0)
     ((node-is "with") parent-bol 0)

     ;; then/else clauses align with their enclosing if
     ((node-is "then_clause") parent-bol 0)
     ((node-is "else_clause") parent-bol 0)

     ;; The cases of a match or try (a leading | or the first case
     ;; without one) align with the keyword (see `neocaml--case-anchor')
     ((match "\\`\\(?:|\\|match_case\\)\\'" "\\`\\(?:match\\|try\\)_expression\\'")
      neocaml--case-anchor 0)
     ((node-is "match_case") parent-bol 0)

     ;; Bodies inside then/else are indented
     ((parent-is "then_clause") parent-bol neocaml-indent-offset)
     ((parent-is "else_clause") parent-bol neocaml-indent-offset)

     ;; Match case bodies (after ->) are indented from | (see
     ;; `neocaml--case-body-offset'), a guard or -> starting a line less
     ((match "\\`\\(?:guard\\|->\\)\\'" "\\`match_case\\'") parent-bol neocaml-indent-offset)
     ((parent-is "match_case") parent-bol neocaml--case-body-offset)

     ;; let...in: body after "in" aligns with "let" (no accumulation)
     ((parent-is "let_expression") parent-bol 0)

     ;; and, and* and and+ align with their let, let*, let+ or type, and
     ;; so do comments between the definitions
     (neocaml--definition-item-p parent 0)

     ;; Item attributes ([@@deriving ...]) on a line of their own align
     ;; with their item
     ((node-is "\\`item_attribute\\'") parent-bol 0)

     ;; Let/type/external bindings: body after = is indented, and
     ;; parameters starting a line further (see `neocaml--parameter-offset')
     ((match "\\`\\(?:parameter\\|abstract_type\\)\\'" "\\`let_binding\\'")
      parent-bol neocaml--parameter-offset)
     ((parent-is "let_binding") parent-bol neocaml-indent-offset)
     ((parent-is "type_binding") parent-bol neocaml-indent-offset)
     ((parent-is "external") parent-bol neocaml-indent-offset)
     ((parent-is "value_specification") parent-bol neocaml-indent-offset)

     ;; A leading ; (janestreet style) in a record, list or array goes
     ;; under the end of the opening bracket
     ((match "\\`;\\'" "\\`\\(?:\\(?:record\\|list\\|i?array\\)_\\(?:expression\\|pattern\\)\\|record_declaration\\)\\'")
      neocaml--leading-semicolon-anchor 0)

     ;; Type definition components — use grand-parent-bol to avoid
     ;; shifting when the declaration starts on the same line as
     ;; its first child
     ((parent-is "record_declaration") parent-bol neocaml-indent-offset)
     ((parent-is "variant_declaration") neocaml--grand-parent-bol neocaml-indent-offset)

     ;; Module structures and signatures.  The @@ of an OxCaml sig @@
     ;; portable starting a line aligns with the sig, as end does
     ((match "\\`@@\\'" "\\`signature\\'") parent-bol 0)
     (neocaml--functor-argument-p neocaml--functor-anchor neocaml--functor-body-offset)
     ((parent-is "\\`\\(?:structure\\|signature\\|open_module_signature\\)\\'")
      neocaml--module-body-anchor neocaml-indent-offset)

     ;; Loop bodies
     ((parent-is "do_clause") parent-bol neocaml-indent-offset)

     ;; fun/function expressions.  After an operator (x >>= fun y ->)
     ;; the body lines up with the operator chain, and the body of a
     ;; fun starting a let body or right inside parentheses has rules of
     ;; its own
     (neocaml--operand-function-p neocaml--operand-function-anchor 0)
     (neocaml--let-fun-body-p neocaml--grand-parent-bol neocaml-indent-offset)
     (neocaml--paren-fun-body-p neocaml--paren-fun-body-anchor neocaml--paren-fun-body-offset)
     ((parent-is "fun_expression") parent-bol neocaml-indent-offset)
     ((parent-is "function_expression") parent-bol neocaml--function-case-offset)

     ;; try/with
     ((parent-is "try_expression") parent-bol neocaml-indent-offset)

     ;; Compound expressions.  parent-is matches anywhere in the node
     ;; type, so array_expression covers OxCaml's iarray_expression too,
     ;; as the "]" rule above covers its closing ":]".
     ((parent-is "parenthesized_expression") parent-bol neocaml--parenthesized-offset)
     ((parent-is "record_expression") parent-bol neocaml-indent-offset)
     ((parent-is "list_expression") parent-bol neocaml-indent-offset)
     ((parent-is "array_expression") parent-bol neocaml-indent-offset)

     ;; Application (see `neocaml--argument-indentation') and field access
     ((parent-is "application_expression") neocaml--argument-anchor neocaml--argument-offset)
     ((parent-is "field_expression") parent-bol neocaml-indent-offset)

     ;; Sequences (expr1; expr2) — keep aligned
     ((parent-is "sequence_expression") parent-bol 0)

     ;; Object-oriented features
     ((parent-is "object_expression") parent-bol neocaml-indent-offset)
     ((parent-is "class_body_type") parent-bol neocaml-indent-offset)

     ;; Operator chains broken across lines (`|>', `&&', `^', `::' and
     ;; the arrows of a function type) align with where the chain starts
     ((parent-is "\\`\\(?:infix_expression\\|cons_\\(?:expression\\|pattern\\)\\|function_type\\)\\'")
      neocaml--chain-anchor neocaml--chain-offset)
     ;; Tuple elements align with the first one, and a leading comma
     ;; (janestreet style) with the parenthesis
     ((n-p-gp "\\`,\\'" "\\`tuple_\\(?:expression\\|pattern\\)\\'"
              "\\`parenthesized_\\(?:expression\\|pattern\\)\\'")
      grand-parent 0)
     ((parent-is "\\`tuple_\\(?:expression\\|pattern\\|type\\)\\'") neocaml--first-element-anchor 0)
     ;; Or-patterns and polymorphic variants broken before a |
     ((match "\\`|\\'" "\\`or_pattern\\'") neocaml--or-pattern-anchor 0)
     ((match "\\`|\\'" "\\`polymorphic_variant_type\\'") parent 0)
     ;; Record patterns: fields indented from the brace, and so are the
     ;; arguments of a constructor, in a pattern or a declaration, and
     ;; the type or value of a record field on the next line from its start
     ((parent-is "\\`\\(?:record_pattern\\|\\(?:constructor\\|field\\)_\\(?:pattern\\|declaration\\)\\)\\'")
      parent neocaml-indent-offset)
     ;; A leading <- (janestreet style) aligns with what it sets, and a
     ;; comment between the cases of a match, or before an else, with them
     ((match "\\`<-\\'" "\\`set_expression\\'") parent 0)
     ((match "\\`comment\\'" "\\`\\(?:match\\|try\\)_expression\\'") neocaml--case-anchor 0)
     ((match "\\`comment\\'" "\\`if_expression\\'") parent-bol 0)
     ;; Otherwise these are bodies: the right side of x.f <-, a match or
     ;; if subject on a line of its own, a labeled argument's value after
     ;; ~label:, a lazy or extension payload
     ((parent-is "\\`\\(?:set_expression\\|match_expression\\|if_expression\\|labeled_argument\\|lazy_expression\\|extension\\)\\'")
      parent-bol neocaml-indent-offset)
     ;; Attribute and extension payloads (see `neocaml--payload-offset')
     ((node-is "\\`attribute_payload\\'") parent-bol neocaml--payload-offset)
     ((parent-is "\\`attribute_payload\\'")
      neocaml--grand-parent-bol neocaml--payload-offset)

     ;; Module definitions broken across lines, as ocp-indent and
     ;; ocamlformat lay them out: parameters two steps in, a struct or sig
     ;; starting a line under the module, and the rest one step in, like
     ;; the rest of an include, open, functor, local open or class binding
     ((match "\\`module_parameter\\'" "\\`module_binding\\'") parent-bol neocaml--double-offset)
     ((match "\\`\\(?:structure\\|signature\\)\\'" "\\`module_binding\\'") parent-bol 0)
     ((parent-is "\\`\\(?:module_binding\\|module_application\\|include_module\\(?:_type\\)?\\|open_module\\|functor\\|local_open_expression\\|class_binding\\)\\'")
      parent-bol neocaml-indent-offset)
     ;; Typed patterns and expressions, polymorphic types and let bindings
     ;; on the line after their let (see the functions for how the styles
     ;; differ)
     ((match nil "\\`typed_pattern\\'" "\\`type\\'") neocaml--typed-pattern-anchor neocaml-indent-offset)
     ((match "\\`:\\'" "\\`typed_pattern\\'") neocaml--first-element-anchor 0)
     ((match "\\`:\\'" "\\`typed_expression\\'") neocaml--typed-expression-anchor 0)
     (neocaml--polymorphic-type-aligned-p parent 0)
     ((parent-is "\\`polymorphic_type\\'") parent-bol neocaml--polymorphic-type-offset)
     ((match "\\`let_binding\\'" "\\`value_definition\\'") parent-bol neocaml--let-binding-offset)
     ;; A leading comma in type parameters (see the function)
     ((match "\\`,\\'" "\\`constructed_type\\'") neocaml--type-parameter-comma-anchor 0)

     ;; OxCaml.  No other rule matches these parents, so they come last,
     ;; where lines of plain OCaml don't have to try them.
     ;; Kind definitions, and a mod bound starting a line, which aligns
     ;; with the kind it bounds, as ocamlformat does
     ((parent-is "\\`kind_definition\\'") parent-bol neocaml-indent-offset)
     ((match "\\`mod\\'" "\\`mod_bounded_kind\\'") parent 0)
     ;; Allocation and borrowing with the body on the next line: stack_,
     ;; local_ and borrow_ indent it like an argument, while exclave_
     ;; leaves it where the exclave_ itself would go
     ((parent-is "\\`exclave_expression\\'")
      neocaml--exclave-body-anchor neocaml--exclave-body-offset)
     ((parent-is "\\`\\(?:stack\\|local\\|borrow\\)_expression\\'")
      parent-bol neocaml-indent-offset)
     ;; Comprehensions: for and when clauses align with the expression
     ;; they build, and parallel and clauses with their for
     ((parent-is "\\`comprehension\\(?:_iterator\\)?\\'") parent 0)

     ;; Error recovery
     ((parent-is "ERROR") parent-bol neocaml-indent-offset)

     ;; Comment continuation lines align with the body start
     ((node-is "comment") neocaml--comment-body-anchor 0)
     ;; Strings preserve previous indentation
     ((node-is "string") prev-line 0))))

(defun neocaml-cycle-indent-function ()
  "Cycle between `treesit-indent' and `indent-relative' for indentation."
  (interactive)
  (if (eq indent-line-function 'treesit-indent)
      (progn (setq indent-line-function #'indent-relative)
             (message "[neocaml] Switched indentation to indent-relative"))
    (setq indent-line-function #'treesit-indent)
    (message "[neocaml] Switched indentation to treesit-indent")))

(defun neocaml-set-font-lock-level (level)
  "Set `treesit-font-lock-level' to LEVEL in the current buffer and refontify.
LEVEL ranges from 1 (comments and definitions only) to 4 (maximum
fontification).  The change is buffer-local; use \\[customize-variable] on
`treesit-font-lock-level' to set the default for new buffers."
  (interactive (list (read-number "Font-lock level (1-4): " treesit-font-lock-level)))
  (setq-local treesit-font-lock-level level)
  (treesit-font-lock-recompute-features)
  (font-lock-flush)
  (message "[neocaml] Font-lock level set to %d" level))

;;;; Formatting

(defun neocaml--ocamlformat-args ()
  "Return the argument list for invoking `ocamlformat' on the current buffer.
When the buffer is visiting a file, pass its name via `--name' so
ocamlformat picks up the right `.ocamlformat' profile and infers
implementation vs. interface from the extension.  Otherwise fall back
to `--impl'/`--intf' based on the major mode.  ocamlformat reads the
source from stdin (the trailing \"-\")."
  (append
   (if buffer-file-name
       (list "--name" buffer-file-name)
     (list (if (derived-mode-p 'neocaml-interface-mode) "--intf" "--impl")))
   (list "-")))

(defun neocaml-format-buffer ()
  "Format the current buffer using `ocamlformat'.
Pipes the buffer through `neocaml-ocamlformat-program' and replaces the
buffer text with the formatted output, touching only the parts that
changed (see `neocaml-common-format-buffer').  ocamlformat formats
whole compilation units, so there is no region/defun variant."
  (interactive)
  (neocaml-common-format-buffer "ocamlformat" neocaml-ocamlformat-program
                                (neocaml--ocamlformat-args)))

(defun neocaml--format-before-save ()
  "Format the buffer before saving when `neocaml-format-on-save' is non-nil."
  (when neocaml-format-on-save
    (neocaml-format-buffer)))

;;;; Project integration
;;
;; Teach `project.el' that a directory containing a `dune-project' file is a
;; project root, so project-wide commands (find-file, search, compile, ...)
;; work for OCaml/dune projects.  Registered with APPEND so version-control
;; detection still takes precedence when a project is under git/hg/etc.; this
;; mainly fills the gap for dune projects that aren't under version control.

(defun neocaml-project-find (dir)
  "Return the dune project containing DIR, or nil.
The project root is the closest directory at or above DIR that contains
a `dune-project' file."
  (when-let* ((root (locate-dominating-file dir "dune-project")))
    (list 'neocaml (expand-file-name root))))

(cl-defmethod project-root ((project (head neocaml)))
  (cadr project))

(cl-defmethod project-ignores ((_project (head neocaml)) _dir)
  "Ignore OCaml build/switch artifacts in addition to the defaults."
  (append '("_build/" "_opam/") (cl-call-next-method)))

(add-hook 'project-find-functions #'neocaml-project-find t)

;;;; Find the definition at point (some Emacs commands use this internally)

(defvar neocaml--defun-type-regexp
  (regexp-opt '("type_binding"
                "exception_definition"
                "external"
                "let_binding"
                "value_specification"
                "method_definition"
                "method_specification"
                "include_module"
                "include_module_type"
                "instance_variable_definition"
                "instance_variable_specification"
                "module_binding"
                "module_type_definition"
                "class_binding"
                "class_type_binding"
                "kind_definition"))
  "Regex matching tree-sitter node types treated as defun-like.
Used as the value of `treesit-defun-type-regexp'.")

(defconst neocaml--nested-context-regexp
  (regexp-opt '("let_expression"
                "parenthesized_module_expression"
                "package_expression")
              'symbols)
  "Regexp matching node types that indicate a nested (non-top-level) context.")

(defun neocaml--defun-valid-p (node)
  "Return non-nil if NODE is a top-level definition.
Filters out nodes nested inside `let_expression',
`parenthesized_module_expression', or `package_expression'."
  (and (treesit-node-check node 'named)
       (not (treesit-node-top-level
             node neocaml--nested-context-regexp))))

(defun neocaml--subtree-text (node type &optional depth)
  "Return the text of the first TYPE child in NODE's subtree.
Search up to DEPTH levels deep (default 1).  Return nil if not found."
  (when-let* ((child (treesit-search-subtree node type nil nil (or depth 1))))
    (treesit-node-text child t)))

(defun neocaml--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("type_binding"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("module_binding"
     (neocaml--subtree-text node "module_name"))
    ("module_type_definition"
     (neocaml--subtree-text node "module_type_name"))
    ("class_binding"
     (neocaml--subtree-text node "class_name"))
    ("class_type_binding"
     (neocaml--subtree-text node "class_type_name"))
    ("method_definition"
     (neocaml--subtree-text node "method_name"))
    ("instance_variable_definition"
     (neocaml--subtree-text node "instance_variable_name"))
    ("exception_definition"
     (neocaml--subtree-text node "constructor_name" 2))
    ("external"
     (neocaml--subtree-text node "value_name"))
    ("let_binding"
     (treesit-node-text
      (treesit-node-child-by-field-name node "pattern") t))
    ("value_specification"
     (neocaml--subtree-text node "value_name"))
    ("method_specification"
     (neocaml--subtree-text node "method_name"))
    ("instance_variable_specification"
     (neocaml--subtree-text node "instance_variable_name"))
    ("kind_definition"
     (neocaml--subtree-text node "kind_name"))))


;;;; imenu integration

(defun neocaml--imenu-name (node)
  "Return the fully-qualified name of NODE by walking up the tree.
Joins ancestor defun names with `treesit-add-log-defun-delimiter'."
  (let ((name nil))
    (while node
      (when-let* ((new-name (treesit-defun-name node)))
        (if name
            (setq name (concat new-name
                               treesit-add-log-defun-delimiter
                               name))
          (setq name new-name)))
      (setq node (treesit-node-parent node)))
    name))

;; TODO: could add constructors / fields
(defvar neocaml--imenu-settings
  `(("Type" "\\`type_binding\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Kind" "\\`kind_definition\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Spec" "\\`\\(value_specification\\|method_specification\\)\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Exception" "\\`exception_definition\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Value" "\\`\\(let_binding\\|external\\)\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Method" "\\`\\(method_definition\\)\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ;; grouping module/class types under Type causes some weird nesting
    ("Module" "\\`\\(module_binding\\|module_type_definition\\)\\'"
     neocaml--defun-valid-p nil)
    ("Class" "\\`\\(class_binding\\|class_type_binding\\)\\'"
     neocaml--defun-valid-p neocaml--imenu-name))
  "Settings for `treesit-simple-imenu' in `neocaml-mode'.")

(defvar neocaml--interface-imenu-settings
  `(("Type" "\\`type_binding\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Kind" "\\`kind_definition\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Val" "\\`value_specification\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("External" "\\`external\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Exception" "\\`exception_definition\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Method" "\\`method_specification\\'"
     neocaml--defun-valid-p neocaml--imenu-name)
    ("Module" "\\`\\(module_binding\\|module_type_definition\\)\\'"
     neocaml--defun-valid-p nil)
    ("Class" "\\`\\(class_binding\\|class_type_binding\\)\\'"
     neocaml--defun-valid-p neocaml--imenu-name))
  "Settings for `treesit-simple-imenu' in `neocaml-interface-mode'.")

;;;; Structured navigation

(defvar neocaml--block-regex
  (regexp-opt `(,@neocaml-mode--keywords
                "do_clause"
                ;; "if_expression"
                ;; "fun_expression"
                ;; "match_expression"
                "local_open_expression"
                "typed_expression"
                "external_declaration"
                "array_expression"
                "iarray_expression"
                "list_expression"
                "parenthesized_expression"
                "parenthesized_pattern"
                "match_case"
                "parameter"
                ;; "value_definition"
                "let_binding"
                "value_specification"
                "value_name"
                "label_name"
                "constructor_name"
                "module_name"
                "module_type_name"
                "value_pattern"
                "value_path"
                "constructor_path"
                "infix_operator"
                "number" "boolean" "unit"
                "type_definition"
                "kind_definition"
                "type_constructor"
                ;; "module_definition"
                "package_expression"
                "typed_module_expression"
                "module_path"
                "signature"
                "structure"
                "string" "quoted_string" "character")
              'symbols)
  "Regex matching tree-sitter node types for sexp-based navigation.
Used by `neocaml-forward-sexp' to identify balanced expressions.")

(defun neocaml-forward-sexp (count)
  "Move forward across COUNT balanced OCaml expressions.
If COUNT is negative, move backward.  This function is intended
to be used as `forward-sexp-function'."
  (if (< count 0)
      (treesit-beginning-of-thing neocaml--block-regex (- count))
    (treesit-end-of-thing neocaml--block-regex count)))

(defun neocaml--delimiter-p ()
  "Return non-nil if point is on a delimiter character."
  (let ((syntax (syntax-after (point))))
    (and syntax
         (memq (syntax-class syntax) '(4 5)))))  ; 4=open, 5=close

(defun neocaml--forward-sexp-hybrid (arg)
  "Hybrid `forward-sexp-function' combining tree-sitter and syntax table.
When point is on a delimiter character (paren, bracket, brace),
fall back to syntax-table-based matching so that commands like
`delete-pair' find the correct matching delimiter.  Otherwise,
use tree-sitter sexp navigation.

Used on every Emacs version: `treesit-forward-sexp' lacks the
`scan-sexps' fallback and `treesit-forward-sexp-list' ignores the
`sexp' thing, so neither built-in does both.

ARG is as in `forward-sexp-function'."
  (let ((arg (or arg 1)))
    (if (or (neocaml--delimiter-p)
            ;; Moving backward: check if the character before point
            ;; (skipping whitespace) is a closing delimiter.
            (and (< arg 0)
                 (save-excursion
                   (skip-chars-backward " \t")
                   (not (bobp))
                   (let ((syntax (syntax-after (1- (point)))))
                     (and syntax (eq (syntax-class syntax) 5))))))
        ;; On a delimiter: use syntax-table paren matching.
        ;; `forward-sexp-default-function' wraps `scan-sexps' but is
        ;; only available from Emacs 30; fall back to raw `scan-sexps'
        ;; on Emacs 29.
        (if (fboundp 'forward-sexp-default-function)
            (forward-sexp-default-function arg)
          (goto-char (or (scan-sexps (point) arg) (buffer-end arg))))
      ;; Not on a delimiter: use tree-sitter node navigation.
      ;; `treesit-forward-sexp' is available from Emacs 30; on Emacs 29
      ;; fall back to the simpler `neocaml-forward-sexp'.
      (if (fboundp 'treesit-forward-sexp)
          (treesit-forward-sexp arg)
        (neocaml-forward-sexp arg)))))

(defconst neocaml--list-node-types
  '("parenthesized_expression"
    "parenthesized_operator"
    "parenthesized_pattern"
    "parenthesized_type"
    "parenthesized_class_expression"
    "parenthesized_module_expression"
    "parenthesized_module_type"
    "list_expression"
    "list_pattern"
    "list_binding_pattern"
    "array_expression"
    "array_pattern"
    "array_binding_pattern"
    "iarray_expression"
    "iarray_pattern"
    "record_expression"
    "record_pattern"
    "record_binding_pattern"
    "record_declaration"
    "object_expression"
    "object_type"
    "object_copy_expression"
    "polymorphic_variant_type"
    "package_type"
    "signature"
    "structure"
    "class_body_type"
    "parameter")
  "Tree-sitter node types treated as `list' things for navigation.")

(defconst neocaml--list-node-regex
  (regexp-opt neocaml--list-node-types 'symbols)
  "Regex matching `neocaml--list-node-types'.")

(defun neocaml--thing-settings (language)
  "Return `treesit-thing-settings' definitions for LANGUAGE.
Configures sexp, list, sentence, text, and comment navigation.

The `list' thing covers delimited container nodes (parentheses,
brackets, braces, arrays).  On Emacs 31+, defining it causes
`treesit-major-mode-setup' to use the hybrid
`treesit-forward-sexp-list' for `forward-sexp-function', which
falls back to syntax-table matching for delimiter characters.
This makes commands like `delete-pair' work correctly."
  `((,language
     (sexp (not ,(rx (or "{" "}" "(" ")" "[" "]" "[|" "|]"
                         "," "." ";" ";;" ":" "::" ":>" "->"
                         "<-" "=" "|" ".." "\"" "'"
                         ;; OxCaml modes and modalities
                         "@" "@@"))))
     (list ,neocaml--list-node-regex)
     (sentence ,(regexp-opt '("value_definition"
                              "type_definition"
                              "kind_definition"
                              "exception_definition"
                              "module_definition"
                              "module_type_definition"
                              "class_definition"
                              "class_type_definition"
                              "open_module"
                              "include_module"
                              "include_module_type"
                              "external"
                              "expression_item"
                              "value_specification"
                              "method_specification"
                              "inheritance_specification"
                              "instance_variable_specification")))
     (text ,(regexp-opt '("comment" "string" "quoted_string" "character")))
     (comment "comment"))))

(defun neocaml-backward-up-list (&optional arg)
  "Move backward out of one level of OCaml list-like construct.
With ARG, do this that many times.  A negative ARG means move
forward but still to a less deep spot.

Unlike the built-in `backward-up-list', this recognises OCaml
constructs delimited by keywords (`struct'/`end', `sig'/`end',
`object'/`end') in addition to ordinary parens, brackets and
braces.  Useful for jumping out to the enclosing module, signature
or object from somewhere inside its body.

On Emacs 31+, the built-in `backward-up-list' already understands
these constructs via `treesit-thing-settings', so this command
simply delegates to it there."
  (interactive "^p")
  (if (>= emacs-major-version 31)
      (backward-up-list arg t t)
    (let ((arg (or arg 1)))
      (dotimes (_ (abs arg))
        ;; Consider both the nearest enclosing tree-sitter list node and
        ;; the nearest syntactic delimiter, then jump to whichever is
        ;; closer to point.  This matches the Emacs 31 built-in, which
        ;; combines the two via `treesit-thing-settings'.
        (let* ((node (treesit-node-at (point)))
               (parent (treesit-parent-until
                        node
                        (lambda (n)
                          (and (string-match-p neocaml--list-node-regex
                                               (treesit-node-type n))
                               (if (< arg 0)
                                   (> (treesit-node-end n) (point))
                                 (< (treesit-node-start n) (point)))))))
               (tree-pos (and parent
                              (if (< arg 0)
                                  (treesit-node-end parent)
                                (treesit-node-start parent))))
               ;; Use `syntax-ppss' rather than `up-list': the OCaml
               ;; syntax table treats `(' as both a paren and part of the
               ;; `(*' comment delimiter, which confuses `up-list'.
               (syn-pos (let ((open (nth 1 (syntax-ppss))))
                          (cond ((null open) nil)
                                ((< arg 0) (ignore-errors (scan-sexps open 1)))
                                (t open))))
               (target (cond ((and tree-pos syn-pos)
                              (if (< arg 0)
                                  (min tree-pos syn-pos)
                                (max tree-pos syn-pos)))
                             (t (or tree-pos syn-pos)))))
          (if target
              (goto-char target)
            (user-error "At top level")))))))

(defun neocaml-mark-sentence ()
  "Mark the current statement around point.
Uses tree-sitter sentence navigation to select the entire statement
\(e.g. a `let' binding, type definition, or module definition)."
  (interactive)
  (backward-sentence)
  (push-mark (point) nil t)
  (forward-sentence))

;;;; Compilation support

(defconst neocaml--compilation-error-regexp
  (eval-when-compile
    (rx bol
        ;; Leading whitespace: 7 spaces = ancillary location (info level),
        ;; any other whitespace = default severity (error/warning per group 8).
        (or (group-n 9 "       ")
            (* (in " \t")))
        (group-n 1
                 (or "File "
                     ;; Exception backtraces (OCaml >= 4.11 includes function names)
                     (seq (or "Raised at" "Re-raised at"
                              "Raised by primitive operation at"
                              "Called from")
                          (* nonl)
                          " file "))
                 (group-n 2 (? "\""))
                 (group-n 3 (+ (not (in "\t\n \",<>"))))
                 (backref 2)
                 (? " (inlined)")
                 ", line" (? "s") " "
                 (group-n 4 (+ (in "0-9")))
                 (? "-" (group-n 5 (+ (in "0-9"))))
                 (? ", character" (? "s") " "
                    (group-n 6 (+ (in "0-9")))
                    (? "-" (group-n 7 (+ (in "0-9")))))
                 (? ":"))
        ;; Skip source-code snippets and match Warning/Alert on next line
        (? "\n"
           (* (in "\t "))
           (* (or (seq (+ (in "0-9"))
                       " | "
                       (* nonl))
                  (+ "^"))
              "\n"
              (* (in "\t ")))
           (group-n 8 (or "Warning" "Alert")
                    (* (not (in ":\n")))
                    ":"))))
  "Regexp matching OCaml compiler error, warning, and backtrace messages.")

;; OCaml error messages report 0-indexed byte positions with exclusive
;; end (e.g. "characters 2-8" means bytes 2..7).  Emacs compilation
;; mode expects 1-indexed inclusive columns.  We convert directly in
;; these functions rather than using `compilation-first-column' because
;; that variable is checked in the destination buffer -- which may not
;; be in `neocaml-mode' (e.g. JSON files processed by OCaml tools).
;; By converting here, the columns are correct regardless of the
;; destination buffer's major mode.

(defun neocaml--compilation-begin-column ()
  "Return the begin-column from an OCaml compilation message.
Converts from OCaml's 0-indexed column to Emacs's 1-indexed column."
  (when (match-beginning 6)
    (1+ (string-to-number (match-string 6)))))

(defun neocaml--compilation-end-column ()
  "Return the end-column from an OCaml compilation message.
OCaml reports an exclusive 0-indexed end-column; Emacs expects an
inclusive 1-indexed end-column.  The +1 (0-to-1 indexing) and -1
\(exclusive-to-inclusive) cancel out, so we return the raw value."
  (when (match-beginning 7)
    (string-to-number (match-string 7))))

(defconst neocaml--compilation-error-spec
  '(3 (4 . 5) (neocaml--compilation-begin-column . neocaml--compilation-end-column) (8 . 9) 1
      (8 font-lock-function-name-face))
  "Where the parts of OCaml messages are in `neocaml--compilation-error-regexp'.
The FILE, LINE, COLUMN, TYPE, HYPERLINK and HIGHLIGHT that follow the
regexp in a `compilation-error-regexp-alist' entry.")

(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)
(defvar compilation-transform-file-match-alist)

(defun neocaml--setup-compilation ()
  "Register OCaml compilation error regexp with compile.el.
The regexp and associated column functions are installed globally
in `compilation-error-regexp-alist-alist' because `*compilation*'
buffers are not in any language-specific mode.  All active entries
are tried against every line of compilation output."
  (require 'compile)
  (setq compilation-error-regexp-alist-alist
        (assq-delete-all 'ocaml compilation-error-regexp-alist-alist))
  (push `(ocaml ,neocaml--compilation-error-regexp ,@neocaml--compilation-error-spec)
        compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist
        (delq 'ocaml compilation-error-regexp-alist))
  (push 'ocaml compilation-error-regexp-alist)
  ;; "//toplevel//" is the toplevel's name for its input, which backtraces
  ;; through toplevel phrases mention: there's no file to visit
  (add-to-list 'compilation-transform-file-match-alist '("\\`//toplevel//\\'" nil)))

;;;; _build directory awareness

(defun neocaml--resolve-build-path (file)
  "Resolve FILE out of a `_build' directory, if applicable.
For dune-style paths like `_build/default/lib/foo.ml', strip
`_build/<context>/' and return `<project-root>/lib/foo.ml'.
For ocamlbuild-style `_build/lib/foo.ml', strip `_build/' only.
Return nil if FILE is not under `_build' or the resolved file
does not exist."
  (when-let* ((pos (string-search "/_build/" file))
              (root (substring file 0 pos))
              (rest (substring file (+ pos 8))) ;; skip "/_build/"
              (components (split-string rest "/" t)))
    (let* (;; Dune: skip first component (context dir like "default")
           (dune-path (when (cdr components)
                        (concat root "/" (string-join (cdr components) "/"))))
           ;; Ocamlbuild: keep everything after _build/
           (ocamlbuild-path (concat root "/" rest)))
      (cond
       ((and dune-path (file-readable-p dune-path)) dune-path)
       ((file-readable-p ocamlbuild-path) ocamlbuild-path)))))

(defun neocaml--check-build-dir ()
  "If the current file is under `_build/', offer to switch to the source.
Intended for use in `find-file-hook'."
  (when-let* ((file (buffer-file-name)))
    (when (and neocaml-redirect-build-files
               (derived-mode-p 'neocaml-base-mode)
               (string-match-p "/_build/" file))
      (if-let* ((source (neocaml--resolve-build-path file)))
          (when (y-or-n-p (format "This file is under _build.  Switch to %s? " source))
            (find-alternate-file source))
        (message "Note: this file is under _build/ (no source found)")))))

;;;; Fill paragraph

(defun neocaml--comment-at-point ()
  "Return the comment node at or around point, or nil."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (n) (equal (treesit-node-type n) "comment"))
   t))

(defun neocaml--fill-paragraph (&optional _justify)
  "Fill the OCaml comment at point.
Uses tree-sitter to find comment boundaries, then narrows to the
comment body (excluding delimiters) and fills.  Returns t if point
was in a comment, nil otherwise to let the default handler run."
  (let* ((comment (neocaml--comment-at-point)))
    (when comment
      (let ((start (treesit-node-start comment))
            (end (treesit-node-end comment)))
        (save-excursion
          (save-restriction
            ;; Narrow to comment body: skip (* prefix and *) suffix
            (goto-char start)
            (when (looking-at "(\\*+[ \t]*")
              (setq start (match-end 0)))
            (goto-char end)
            (when (looking-back "\\*+)" nil)
              (goto-char (match-beginning 0))
              (skip-chars-backward " \t")
              (setq end (point)))
            ;; Compute body column before narrowing.
            (goto-char start)
            (let ((body-col (current-column)))
              (narrow-to-region start end)
              ;; Treat list items (- foo, * foo, + foo) and odoc tags
              ;; (@param, @return) as paragraph boundaries so they
              ;; don't get merged into prose.
              (let* ((paragraph-start
                      (concat paragraph-start
                              "\\|[ \t]*[-*+][ \t]"
                              "\\|[ \t]*@[a-z]+\\b"))
                     par-start par-end)
                ;; Find the paragraph containing point (without
                ;; fill-prefix set, so list items are recognized as
                ;; paragraph boundaries).
                (save-excursion
                  (skip-chars-forward " \t")
                  (backward-paragraph)
                  (skip-chars-forward " \t\n")
                  (setq par-start (point))
                  (forward-paragraph)
                  (setq par-end (point)))
                ;; Use body-col for first-line paragraphs, actual
                ;; indentation for indented paragraphs (list items).
                (let* ((par-col (save-excursion
                                  (goto-char par-start)
                                  (skip-chars-forward " \t")
                                  (current-column)))
                       (fill-prefix
                        (make-string (max body-col par-col) ?\s))
                       (fill-paragraph-function nil))
                  (fill-region-as-paragraph par-start par-end)))))
          t)))))

;;;; Comment continuation (M-j)

(defun neocaml--comment-body-column ()
  "Return the column of the comment body text start, or nil."
  (let* ((comment (neocaml--comment-at-point)))
    (when comment
      (save-excursion
        (goto-char (treesit-node-start comment))
        (when (looking-at "(\\*+[ \t]*")
          (goto-char (match-end 0))
          (current-column))))))

(defun neocaml--forward-comment (&optional count)
  "Corrected `forward-comment-function' for OCaml block comments.
Emacs 31's `treesit-forward-comment' has an off-by-one bug: it
does (1+ (treesit-node-end ...)) but `treesit-node-end' already
returns an exclusive position, so point overshoots by one
character.  This breaks `uncomment-region' for multi-line
regions.  This function is identical to `treesit-forward-comment'
with the overshoot removed.

Only installed on Emacs 31+.  COUNT is the same as in
`forward-comment'; uses `funcall' to avoid a package-lint
warning about `treesit-thing-at' requiring Emacs 30.1 while the
package supports 29.1."
  (let ((res t) thing
        (thing-at (intern "treesit-thing-at")))
    (while (> count 0)
      (skip-chars-forward " \t\n")
      (setq thing (funcall thing-at (point) 'comment))
      (if (and thing (eq (point) (treesit-node-start thing)))
          (progn
            (goto-char (treesit-node-end thing))
            (setq count (1- count)))
        (setq count 0 res nil)))
    (while (< count 0)
      (skip-chars-backward " \t\n")
      (setq thing (funcall thing-at (max (1- (point)) (point-min)) 'comment))
      (if (and thing (eq (point) (treesit-node-end thing)))
          (progn
            (goto-char (treesit-node-start thing))
            (setq count (1+ count)))
        (setq count 0 res nil)))
    res))

(defun neocaml--comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
SOFT works the same as in `comment-indent-new-line'."
  (let ((body-col (neocaml--comment-body-column)))
    (if body-col
        (progn
          (if soft (insert-and-inherit ?\n) (newline 1))
          (insert-char ?\s body-col))
      (comment-indent-new-line soft))))

;;;; Utility commands

(defconst neocaml-report-bug-url "https://github.com/bbatsov/neocaml/issues/new"
  "The URL to report a `neocaml' issue.")

(defun neocaml--grammar-info (language)
  "Return a string describing the status of the LANGUAGE grammar."
  (if (treesit-language-available-p language)
      (let ((recipe (assq language neocaml-grammar-recipes))
            (abi (treesit-language-abi-version language)))
        (format "%s (ABI %s, expected: %s)" language abi (or (nth 2 recipe) "unknown")))
    (format "%s (not installed)" language)))

(defun neocaml-bug-report-info ()
  "Display debug information for bug reports.
The information is also copied to the kill ring."
  (interactive)
  (let* ((info (format (concat "Emacs: %s\n"
                                "System: %s\n"
                                "neocaml: %s\n"
                                "Tree-sitter ABI: %s\n"
                                "Grammars: %s, %s\n"
                                "Eglot: %s")
                       emacs-version
                       system-type
                       neocaml-version
                       (treesit-library-abi-version)
                       (neocaml--grammar-info 'ocaml)
                       (neocaml--grammar-info 'ocaml-interface)
                       (if (bound-and-true-p eglot--managed-mode) "active" "inactive"))))
    (kill-new info)
    (message "%s\n(copied to kill ring)" info)))

(defun neocaml-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (neocaml-bug-report-info)
  (browse-url neocaml-report-bug-url))

(defconst neocaml-ocaml-docs-base-url "https://ocaml.org/docs/"
  "The base URL for official OCaml guides.")

(defun neocaml-browse-ocaml-docs ()
  "Browse the official OCaml documentation in your default browser."
  (interactive)
  (browse-url neocaml-ocaml-docs-base-url))


;;;; Major mode definitions

;; Mode hierarchy (following the pattern used by c-ts-mode / c++-ts-mode):
;;
;;   prog-mode
;;     └─ neocaml-base-mode     — shared setup: syntax table, comments,
;;          │                      compilation, navigation, keybindings,
;;          │                      prettify-symbols
;;          ├─ neocaml-mode      — .ml: ocaml grammar, ml-specific imenu
;;          └─ neocaml-interface-mode — .mli: ocaml-interface grammar,
;;                                       mli-specific imenu
;;
;; Users hook into neocaml-base-mode-hook for configuration that applies
;; to both modes.  Language-specific setup lives in neocaml--setup-mode.

(defvar neocaml-base-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'ff-find-other-file)
    (define-key map (kbd "C-c 4 C-a") #'ff-find-other-file-other-window)
    (define-key map (kbd "C-c C-c") #'compile)
    (define-key map (kbd "C-c C-f") #'neocaml-format-buffer)
    (define-key map (kbd "C-M-u") #'neocaml-backward-up-list)
    (easy-menu-define neocaml-mode-menu map "Neocaml Mode Menu"
      '("OCaml"
        ("Navigate"
         ["Beginning of Definition" beginning-of-defun
          :help "Move to the beginning of the current definition"]
         ["End of Definition" end-of-defun
          :help "Move to the end of the current definition"]
         ["Forward Expression" forward-sexp
          :help "Move forward across one balanced expression"]
         ["Backward Expression" backward-sexp
          :help "Move backward across one balanced expression"]
         ["Forward Statement" forward-sentence
          :help "Move forward to the next top-level statement"]
         ["Backward Statement" backward-sentence
          :help "Move backward to the previous top-level statement"]
         ["Up to Enclosing Block" neocaml-backward-up-list
          :help "Move out of the enclosing block"])
        ("Find..."
         ["Find Interface/Implementation" ff-find-other-file
          :help "Switch between the .ml and .mli file"]
         ["Find Interface/Implementation in other window" ff-find-other-file-other-window
          :help "Switch between the .ml and .mli file in another window"])
        ("Edit"
         ["Comment Out Region" comment-region (use-region-p)
          :help "Comment out the lines in the region"]
         ["Uncomment Region" uncomment-region (use-region-p)
          :help "Uncomment the lines in the region"]
         ["Fill Comment Paragraph" fill-paragraph t
          :help "Fill the comment paragraph at point"]
         "--"
         ["Indent Region" indent-region (use-region-p)
          :help "Reindent the lines in the region"]
         "--"
         ["Format Buffer (ocamlformat)" neocaml-format-buffer
          :help "Format the whole buffer with ocamlformat"])
        ("Toggle"
         ["Prettify Symbols" prettify-symbols-mode
          :style toggle :selected (bound-and-true-p prettify-symbols-mode)
          :help "Display keywords and operators using Unicode symbols"]
         ["Subword Mode" subword-mode
          :style toggle :selected (bound-and-true-p subword-mode)
          :help "Treat CamelCase subwords as separate words when moving"]
         ["Outline Minor Mode" outline-minor-mode
          :style toggle :selected (bound-and-true-p outline-minor-mode)
          :help "Fold and navigate top-level definitions"])
        ("Font-Lock Level"
         ["1 - Comments & definitions" (neocaml-set-font-lock-level 1)
          :style radio :selected (= treesit-font-lock-level 1)
          :help "Fontify only comments and definitions"]
         ["2 - + keywords, strings, types" (neocaml-set-font-lock-level 2)
          :style radio :selected (= treesit-font-lock-level 2)
          :help "Also fontify keywords, strings, and data types"]
         ["3 - + constants, numbers, etc." (neocaml-set-font-lock-level 3)
          :style radio :selected (= treesit-font-lock-level 3)
          :help "Full fontification: constants, numbers, literals, etc."]
         ["4 - Everything" (neocaml-set-font-lock-level 4)
          :style radio :selected (= treesit-font-lock-level 4)
          :help "Also fontify delimiters, operators, variables, etc."]
         "--"
         ["Customize (persist)..." (customize-variable 'treesit-font-lock-level)
          :help "Set the default font-lock level for new buffers"])
        ("Indentation Style"
         ["From .ocamlformat" (neocaml-set-indent-style nil)
          :style radio :selected (null neocaml-indent-style)
          :help "Indent as the project's .ocamlformat says, or as ocp-indent does"]
         ["ocp-indent" (neocaml-set-indent-style 'ocp-indent)
          :style radio :selected (eq neocaml-indent-style 'ocp-indent)
          :help "Indent as ocp-indent does by default"]
         ["ocamlformat conventional" (neocaml-set-indent-style 'conventional)
          :style radio :selected (eq neocaml-indent-style 'conventional)
          :help "Indent as ocamlformat's conventional profile does"]
         ["ocamlformat janestreet" (neocaml-set-indent-style 'janestreet)
          :style radio :selected (eq neocaml-indent-style 'janestreet)
          :help "Indent as ocamlformat's janestreet profile does"]
         "--"
         ["Customize (persist)..." (customize-variable 'neocaml-indent-style)
          :help "Set the default indentation style for new buffers"])
        "--"
        ["Mark Definition" mark-defun
         :help "Mark the current definition"]
        ["Mark Expression" mark-sexp
         :help "Mark the expression after point"]
        ["Mark Statement" neocaml-mark-sentence
         :help "Mark the current statement"]
        "--"
        ["Transpose Expression" transpose-sexps
         :help "Transpose the expressions around point"]
        ["Transpose Statement" transpose-sentences
         :help "Transpose the statements around point"]
        "--"
        ["Start/Switch to REPL" neocaml-repl-switch-to-repl
         :help "Start the OCaml REPL or switch to a running one"]
        ["Compile..." compile
         :help "Compile the project"]
        ["Cycle indent function" neocaml-cycle-indent-function
         :help "Switch between tree-sitter and relative indentation"]
        ["Install tree-sitter grammars" neocaml-install-grammars
         :help "Install the OCaml tree-sitter grammars"]
        ("Documentation"
         ["Browse OCaml Docs" neocaml-browse-ocaml-docs
          :help "Browse the OCaml documentation in a web browser"])
        "--"
        ["Customize neocaml..." (customize-group 'neocaml)
         :help "Customize neocaml settings"]
        ["Report a neocaml bug" neocaml-report-bug
         :help "Report a bug to the neocaml issue tracker"]
        ["Show bug report info" neocaml-bug-report-info
         :help "Show environment info useful for bug reports"]
        ["neocaml version" neocaml-version
         :help "Display the neocaml version"]))
    map)
  "Keymap shared by `neocaml-mode' and `neocaml-interface-mode'.")

(defun neocaml--setup-mode (language)
  "Set up tree-sitter font-lock, indentation, and navigation for LANGUAGE.
Called from `neocaml-mode' and `neocaml-interface-mode' to configure
the language-specific parts of the mode."
  ;; Offer to install missing grammars
  (when-let* ((missing (seq-filter (lambda (r) (not (treesit-language-available-p (car r))))
                                   neocaml-grammar-recipes)))
    (when (y-or-n-p "OCaml tree-sitter grammars are not installed.  Install them now?")
      (neocaml-install-grammars)))

  ;; Warn if installed grammars are outdated
  (neocaml--check-grammar-compatibility)

  (when (treesit-ready-p language)
    ;; Emacs 31+ uses treesit-primary-parser to identify the main parser
    ;; when multiple parsers are active.
    (let ((parser (treesit-parser-create language)))
      (when (boundp 'treesit-primary-parser)
        (setq-local treesit-primary-parser parser)))

    (when neocaml--debug
      (setq-local treesit--indent-verbose t)

      (when (eq neocaml--debug 'font-lock)
        (setq-local treesit--font-lock-verbose t))

      ;; show the node at point in the minibuffer
      (treesit-inspect-mode))

    ;; font-lock settings
    (setq-local treesit-font-lock-settings
                (neocaml-mode--font-lock-settings language))

    ;; indentation
    (setq-local treesit-simple-indent-rules (neocaml--indent-rules language))

    ;; Navigation
    (when (boundp 'treesit-thing-settings)
      (setq-local treesit-thing-settings
                  (neocaml--thing-settings language)))

    (treesit-major-mode-setup)

    ;; Emacs 31's treesit-forward-comment has an off-by-one bug that
    ;; breaks uncomment-region on multi-line regions.  Override with
    ;; the corrected version.
    (when (and (>= emacs-major-version 31)
               (boundp 'forward-comment-function))
      (setq-local forward-comment-function #'neocaml--forward-comment))

    ;; treesit-major-mode-setup picks a `forward-sexp-function' for us, but
    ;; neither choice is right for OCaml:
    ;;
    ;;   - Emacs 30 uses `treesit-forward-sexp', which doesn't fall back to
    ;;     scan-sexps for delimiter characters, breaking `delete-pair'.
    ;;   - Emacs 31 sees our `list' thing and uses `treesit-forward-sexp-list',
    ;;     which moves across lists but delegates *atoms* to
    ;;     `forward-sexp-default-function', ignoring the `sexp' thing entirely.
    ;;     Keyword-led forms are then treated as bare words, so C-M-f stops
    ;;     after `fun', `function', `if', `match' or `external' instead of
    ;;     moving over the whole construct.
    ;;
    ;; Use the hybrid on every version: it walks sexps with tree-sitter and
    ;; delegates to scan-sexps only on delimiters, which keeps `delete-pair'
    ;; working without giving up sexp navigation.
    (setq-local forward-sexp-function #'neocaml--forward-sexp-hybrid)

    ;; Workaround for treesit-transpose-sexps being broken on Emacs 30
    ;; (bug#60655).  Emacs 31 rewrites the function to work correctly.
    (when (and (fboundp 'transpose-sexps-default-function)
               (< emacs-major-version 31))
      (setq-local transpose-sexps-function
                  #'transpose-sexps-default-function))))

(defun neocaml--register-with-eglot ()
  "Register neocaml modes with eglot if loaded."
  (when (boundp 'eglot-server-programs)
    (add-to-list 'eglot-server-programs
                 '(((neocaml-mode :language-id "ocaml")
                    (neocaml-interface-mode
                     :language-id "ocaml.interface"))
                   "ocamllsp"))))

(defun neocaml--register-with-dape ()
  "Register neocaml modes with dape's ocamlearlybird config if loaded."
  (when (boundp 'dape-configs)
    (when-let* ((cfg (alist-get 'ocamlearlybird dape-configs)))
      (let ((modes (plist-get cfg 'modes)))
        (unless (memq 'neocaml-mode modes)
          (plist-put cfg 'modes
                     (append '(neocaml-mode neocaml-interface-mode)
                             modes)))))))

(define-derived-mode neocaml-base-mode prog-mode "OCaml"
  "Base major mode for OCaml files, providing shared setup.
This mode is not intended to be used directly.  Use `neocaml-mode'
for .ml files and `neocaml-interface-mode' for .mli files."
  :syntax-table neocaml-base-mode-syntax-table

  ;; Syntax propertization for char literals and quoted strings, so the
  ;; syntactic layer agrees with the parse tree (see commentary above).
  (setq-local syntax-propertize-function #'neocaml--syntax-propertize)

  ;; comment settings
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function #'neocaml--comment-indent-new-line)

  ;; Electric indentation on delimiters
  (setq-local electric-indent-chars
              (append "{}()" electric-indent-chars))

  ;; Fill paragraph
  (setq-local fill-paragraph-function #'neocaml--fill-paragraph)
  (setq-local adaptive-fill-mode t)

  ;; Optional format-on-save via ocamlformat
  (add-hook 'before-save-hook #'neocaml--format-before-save nil t)

  ;; Default to `dune build' for `compile'/`project-compile' in dune projects.
  (when (locate-dominating-file default-directory "dune-project")
    (setq-local compile-command "dune build"))

  ;; Make URLs and bug references in comments clickable.  Set
  ;; `bug-reference-url-format' (e.g. via .dir-locals.el) to resolve refs.
  (goto-address-prog-mode)
  (bug-reference-prog-mode)

  ;; TODO: Make this configurable?
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute builtin constant doc-markup escape-sequence number)
                (operator bracket delimiter variable property label function)))

  (setq-local indent-line-function #'treesit-indent)
  ;; Indent with spaces, as ocamlformat and ocp-indent do.
  (setq-local indent-tabs-mode nil)

  ;; Emacs 29 has no treesit-thing-settings, so treesit-major-mode-setup
  ;; won't configure forward-sexp.  Set the hybrid function directly.
  (unless (boundp 'treesit-thing-settings)
    (setq-local forward-sexp-function #'neocaml--forward-sexp-hybrid))
  (setq-local treesit-defun-type-regexp
              (cons neocaml--defun-type-regexp
                    #'neocaml--defun-valid-p))
  (setq-local treesit-defun-name-function #'neocaml--defun-name)

  ;; which-func-mode / add-log integration
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; outline-minor-mode integration (Emacs 30+)
  (when (boundp 'treesit-outline-predicate)
    (setq-local treesit-outline-predicate
                (cons neocaml--defun-type-regexp
                      #'neocaml--defun-valid-p)))

  ;; ff-find-other-file setup
  (setq-local ff-other-file-alist neocaml-other-file-alist)

  ;; Setup prettify-symbols (users enable prettify-symbols-mode via hooks)
  (setq-local prettify-symbols-alist (neocaml--prettify-symbols-alist))

  ;; Register neocaml modes with eglot so it knows to start ocamllsp.
  (neocaml--register-with-eglot)

  ;; Register neocaml modes with dape's ocamlearlybird config.
  (neocaml--register-with-dape))

;;;###autoload
(define-derived-mode neocaml-mode neocaml-base-mode "OCaml"
  "Major mode for editing OCaml code.

\\{neocaml-base-mode-map}"
  (setq-local treesit-simple-imenu-settings neocaml--imenu-settings)
  (neocaml--setup-mode 'ocaml))

;;;###autoload
(define-derived-mode neocaml-interface-mode neocaml-base-mode "OCaml[Interface]"
  "Major mode for editing OCaml interface (mli) code.

\\{neocaml-base-mode-map}"
  (setq-local treesit-simple-imenu-settings neocaml--interface-imenu-settings)
  (neocaml--setup-mode 'ocaml-interface))

;;;###autoload
(progn
  ;; OCaml source files
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . neocaml-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . neocaml-interface-mode))
  ;; OCaml toplevel init file (plain OCaml code)
  (add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . neocaml-mode))
  ;; OCaml config files (key = value format with # comments)
  (add-to-list 'auto-mode-alist '("/\\.ocamlformat\\'" . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("/\\.ocp-indent\\'" . conf-unix-mode)))

;; Hide OCaml build artifacts from find-file completion
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".annot" ".cmt" ".cmti"))
  (add-to-list 'completion-ignored-extensions ext))

;; Register OCaml compilation error regexp once at load time
(neocaml--setup-compilation)

;; Offer to switch away from _build/ copies
(add-hook 'find-file-hook #'neocaml--check-build-dir)

;; Eglot integration: set the language IDs that ocamllsp expects.
;; These symbol properties are consulted by eglot when it cannot
;; derive the correct language-id from the major-mode name.
(put 'neocaml-mode 'eglot-language-id "ocaml")
(put 'neocaml-interface-mode 'eglot-language-id "ocaml.interface")

(provide 'neocaml)

;;; neocaml.el ends here
