;;; neocaml.el --- Major mode for OCaml code -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml
;; Version: 0.5.0
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

(defconst neocaml-version "0.5.0")

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
  '((ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"
           "v0.24.2"
           "grammars/ocaml/src")
    ;; that's the grammar for mli code
    (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml"
                     "v0.24.2"
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
    (when (treesit-language-available-p 'ocaml)
      (let ((expected (nth 2 (assq 'ocaml neocaml-grammar-recipes))))
        ;; In v0.24.2, module_binding lost its "name" field.
        ;; We parse a snippet and check whether that field still exists.
        ;; treesit-query-compile doesn't validate field names, so we
        ;; must check against an actual parse tree.
        (with-temp-buffer
          (insert "module M = struct end")
          (let* ((parser (treesit-parser-create 'ocaml))
                 (root (treesit-parser-root-node parser))
                 (node (treesit-search-subtree root "module_binding")))
            (when (and node (treesit-node-child-by-field-name node "name"))
              (display-warning
               'neocaml
               (format "The installed tree-sitter OCaml grammar appears older \
than %s.  Run C-u M-x neocaml-install-grammars to reinstall."
                       expected)))))))))

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
    "then" "to" "try" "type" "val" "virtual" "when" "while" "with")
  "OCaml keywords for tree-sitter font-locking.

List taken directly from https://github.com/tree-sitter/tree-sitter-ocaml/blob/master/queries/highlights.scm.")

(defvar neocaml-mode--constants
  '((unit) "true" "false")
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
    "array" "iarray" "list" "option" "int32" "int64" "nativeint" "format6" "lazy_t")
  "OCaml builtin type names for tree-sitter font-locking.
List taken from the upstream highlights.scm.")

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
    '((((comment) @font-lock-doc-face)
       (:match "^(\\*\\*[^*]" @font-lock-doc-face))
      (comment) @font-lock-comment-face
      ;; Preprocessor directives
      (line_number_directive) @font-lock-comment-face
      (directive) @font-lock-comment-face)

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
     ;; patterns containing bound variables
     (value_pattern) @font-lock-variable-name-face
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
     (for_expression ["="] @font-lock-keyword-face))

   :language language
   :feature 'keyword
   `([,@neocaml-mode--keywords] @font-lock-keyword-face
     (fun_expression "->" @font-lock-keyword-face)
     (match_case "->" @font-lock-keyword-face)
     (value_definition [(let_operator) (let_and_operator)] @font-lock-keyword-face)
     (match_expression (match_operator) @font-lock-keyword-face))

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
   `(((value_path :anchor (value_name) @font-lock-builtin-face)
      (:match ,(regexp-opt neocaml-mode--builtin-ids 'symbols) @font-lock-builtin-face))
     ((constructor_path :anchor (constructor_name) @font-lock-builtin-face)
      (:match ,(regexp-opt neocaml-mode--builtin-ids 'symbols) @font-lock-builtin-face))
     ;; Builtin types (int, string, bool, etc.)
     ((type_constructor) @font-lock-builtin-face
      (:match ,(regexp-opt neocaml-mode--builtin-types 'symbols) @font-lock-builtin-face)))

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
     [(type_constructor) (type_variable) (hash_type)
      (class_name) (class_type_name)] @font-lock-type-face
      (function_type "->" @font-lock-type-face)
      (tuple_type "*" @font-lock-type-face)
      (polymorphic_variant_type ["[>" "[<" ">" "|" "[" "]"] @font-lock-type-face)
      (object_type ["<" ">" ";" ".."] @font-lock-type-face)
      (constructor_declaration ["->" "*"] @font-lock-type-face)
      (record_declaration ["{" "}" ";"] @font-lock-type-face)
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
   '((["(" ")" "[" "]" "{" "}" "[|" "|]" "[<" "[>"]) @font-lock-bracket-face)

   :language language
   :feature 'delimiter
   '((["," "." ";" ":" ";;"]) @font-lock-delimiter-face)

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

(defvar neocaml--indent-body-tokens
  '("=" "->" "then" "else" "do" "struct" "sig"
    "begin" "object" "with" "fun" "function" "try")
  "Tokens that expect a body on the next line.
Used by `neocaml--empty-line-offset' to decide whether an empty line
should be indented relative to the previous line.")

(defun neocaml--empty-line-offset (_node _parent bol)
  "Compute extra indentation offset for an empty line at BOL.
If the last token on the previous line expects a body (e.g., `=',
`->', `then'), return `neocaml-indent-offset'.  Otherwise return 0,
which preserves the previous line's indentation level."
  (save-excursion
    (goto-char bol)
    (if (and (zerop (forward-line -1))
             (progn
               (end-of-line)
               (skip-chars-backward " \t")
               (> (point) (line-beginning-position)))
             (let ((node (treesit-node-at (1- (point)))))
               (and node
                    (member (treesit-node-type node)
                            neocaml--indent-body-tokens))))
        neocaml-indent-offset
      0)))

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

     ;; Top-level definitions: column 0
     ((parent-is "compilation_unit") column-0 0)

     ;; Closing delimiters align with the opening construct
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "done") parent-bol 0)
     ((node-is "end") parent-bol 0)
     ((node-is ";;") parent-bol 0)

     ;; "with" keyword aligns with match/try
     ((node-is "with") parent-bol 0)

     ;; then/else clauses align with their enclosing if
     ((node-is "then_clause") parent-bol 0)
     ((node-is "else_clause") parent-bol 0)

     ;; | pipe in match/try aligns with the keyword
     ((match "^|$" "match_expression") parent-bol 0)
     ((match "^|$" "try_expression") parent-bol 0)

     ;; Match cases: match_case node aligns with match/try keyword
     ((node-is "match_case") parent-bol 0)

     ;; Bodies inside then/else are indented
     ((parent-is "then_clause") parent-bol neocaml-indent-offset)
     ((parent-is "else_clause") parent-bol neocaml-indent-offset)

     ;; Match case bodies (after ->) are indented from |
     ((parent-is "match_case") parent-bol neocaml-indent-offset)

     ;; let...in: body after "in" aligns with "let" (no accumulation)
     ((parent-is "let_expression") parent-bol 0)

     ;; Binding operators: and* / and+ align with let* / let+
     ((node-is "let_and_operator") parent-bol 0)

     ;; Let/type/external bindings: body after = is indented
     ((parent-is "let_binding") parent-bol neocaml-indent-offset)
     ((parent-is "type_binding") parent-bol neocaml-indent-offset)
     ((parent-is "external") parent-bol neocaml-indent-offset)
     ((parent-is "value_specification") parent-bol neocaml-indent-offset)

     ;; Type definition components — use grand-parent-bol to avoid
     ;; shifting when the declaration starts on the same line as
     ;; its first child
     ((parent-is "record_declaration") parent-bol neocaml-indent-offset)
     ((parent-is "variant_declaration") neocaml--grand-parent-bol neocaml-indent-offset)

     ;; Module structures and signatures
     ((parent-is "structure") parent-bol neocaml-indent-offset)
     ((parent-is "signature") parent-bol neocaml-indent-offset)

     ;; Loop bodies
     ((parent-is "do_clause") parent-bol neocaml-indent-offset)

     ;; fun/function expressions
     ((parent-is "fun_expression") parent-bol neocaml-indent-offset)
     ((parent-is "function_expression") parent-bol neocaml-indent-offset)

     ;; try/with
     ((parent-is "try_expression") parent-bol neocaml-indent-offset)

     ;; Compound expressions
     ((parent-is "parenthesized_expression") parent-bol neocaml-indent-offset)
     ((parent-is "record_expression") parent-bol neocaml-indent-offset)
     ((parent-is "list_expression") parent-bol neocaml-indent-offset)
     ((parent-is "array_expression") parent-bol neocaml-indent-offset)

     ;; Application and field access
     ((parent-is "application_expression") parent-bol neocaml-indent-offset)
     ((parent-is "field_expression") parent-bol neocaml-indent-offset)

     ;; Sequences (expr1; expr2) — keep aligned
     ((parent-is "sequence_expression") parent-bol 0)

     ;; Object-oriented features
     ((parent-is "object_expression") parent-bol neocaml-indent-offset)
     ((parent-is "class_body_type") parent-bol neocaml-indent-offset)

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
                "class_type_binding"))
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
     (neocaml--subtree-text node "instance_variable_name"))))


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
                "coercion_expression"
                "array_expression"
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

This function is used on Emacs 29 and 30.  Emacs 31+ handles
this natively via the `list' thing in `treesit-thing-settings'.

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
                         "<-" "=" "|" ".."))))
     (list ,(regexp-opt '("parenthesized_expression"
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
                          "class_body_type")
                        'symbols))
     (sentence ,(regexp-opt '("value_definition"
                              "type_definition"
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

(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)

(defun neocaml--setup-compilation ()
  "Register OCaml compilation error regexp with compile.el.
The regexp and associated column functions are installed globally
in `compilation-error-regexp-alist-alist' because `*compilation*'
buffers are not in any language-specific mode.  All active entries
are tried against every line of compilation output."
  (require 'compile)
  (setq compilation-error-regexp-alist-alist
        (assq-delete-all 'ocaml compilation-error-regexp-alist-alist))
  (push `(ocaml
          ,neocaml--compilation-error-regexp
          3 (4 . 5) (neocaml--compilation-begin-column . neocaml--compilation-end-column) (8 . 9) 1
          (8 font-lock-function-name-face))
        compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist
        (delq 'ocaml compilation-error-regexp-alist))
  (push 'ocaml compilation-error-regexp-alist))

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
    (when (and (derived-mode-p 'neocaml-base-mode)
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
      (let ((recipe (assq language neocaml-grammar-recipes)))
        (format "%s (expected: %s)" language (or (nth 2 recipe) "unknown")))
    (format "%s (not installed)" language)))

(defun neocaml-bug-report-info ()
  "Display debug information for bug reports.
The information is also copied to the kill ring."
  (interactive)
  (let* ((info (format (concat "Emacs: %s\n"
                                "System: %s\n"
                                "neocaml: %s\n"
                                "Grammars: %s, %s\n"
                                "Eglot: %s")
                       emacs-version
                       system-type
                       neocaml-version
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
    (easy-menu-define neocaml-mode-menu map "Neocaml Mode Menu"
      '("OCaml"
        ("Navigate"
         ["Beginning of Definition" beginning-of-defun]
         ["End of Definition" end-of-defun]
         ["Forward Expression" forward-sexp]
         ["Backward Expression" backward-sexp]
         ["Forward Statement" forward-sentence]
         ["Backward Statement" backward-sentence])
        ("Find..."
         ["Find Interface/Implementation" ff-find-other-file]
         ["Find Interface/Implementation in other window" ff-find-other-file-other-window])
        "--"
        ["Mark Definition" mark-defun]
        ["Mark Expression" mark-sexp]
        ["Mark Statement" neocaml-mark-sentence]
        "--"
        ["Transpose Expression" transpose-sexps]
        ["Transpose Statement" transpose-sentences]
        "--"
        ["Compile..." compile]
        ["Cycle indent function" neocaml-cycle-indent-function]
        ["Install tree-sitter grammars" neocaml-install-grammars]
        ("Documentation"
         ["Browse OCaml Docs" neocaml-browse-ocaml-docs])
        "--"
        ["Report a neocaml bug" neocaml-report-bug]
        ["Show bug report info" neocaml-bug-report-info]
        ["neocaml version" neocaml-version]))
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

    ;; On Emacs 30, treesit-major-mode-setup sets forward-sexp-function
    ;; to treesit-forward-sexp, which doesn't fall back to scan-sexps
    ;; for delimiter characters.  This breaks commands like delete-pair.
    ;; Use a hybrid function that delegates to scan-sexps on delimiters.
    ;; Emacs 31+ handles this natively via the `list' thing.
    (unless (fboundp 'treesit-forward-sexp-list)
      (setq-local forward-sexp-function #'neocaml--forward-sexp-hybrid))

    ;; Workaround for treesit-transpose-sexps being broken on Emacs 30
    ;; (bug#60655).  Emacs 31 rewrites the function to work correctly.
    (when (and (fboundp 'transpose-sexps-default-function)
               (< emacs-major-version 31))
      (setq-local transpose-sexps-function
                  #'transpose-sexps-default-function))))

(define-derived-mode neocaml-base-mode prog-mode "OCaml"
  "Base major mode for OCaml files, providing shared setup.
This mode is not intended to be used directly.  Use `neocaml-mode'
for .ml files and `neocaml-interface-mode' for .mli files."
  :syntax-table neocaml-base-mode-syntax-table

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

  ;; TODO: Make this configurable?
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute builtin constant escape-sequence number)
                (operator bracket delimiter variable property label function)))

  (setq-local indent-line-function #'treesit-indent)

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
  (setq-local prettify-symbols-alist (neocaml--prettify-symbols-alist)))

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
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . neocaml-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . neocaml-interface-mode)))

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
