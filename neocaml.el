;;; neocaml.el --- Major mode for OCaml code -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml
;; Version: 0.2.0
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

(defconst neocaml-version "0.2.0")

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
           "v0.24.0"
           "grammars/ocaml/src")
    ;; that's the grammar for mli code
    (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml"
                     "v0.24.0"
                     "grammars/interface/src"))
  "Tree-sitter grammar recipes for OCaml and OCaml Interface.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-install-grammars ()
  "Install required language grammars if not already available."
  (interactive)
  (dolist (recipe neocaml-grammar-recipes)
    (let ((grammar (car recipe)))
      (unless (treesit-language-available-p grammar nil)
        (message "Installing %s tree-sitter grammar..." grammar)
        ;; `treesit-language-source-alist' is dynamically scoped.
        ;; Binding it in this let expression allows
        ;; `treesit-install-language-grammar' to pick up the grammar recipes
        ;; without modifying what the user has configured themselves.
        (let ((treesit-language-source-alist neocaml-grammar-recipes))
          (treesit-install-language-grammar grammar))))))

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
    "array" "list" "option" "int32" "int64" "nativeint" "format6" "lazy_t")
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
     (match_case "->" @font-lock-keyword-face))

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
   '((value_name) @font-lock-variable-use-face
     (field_name) @font-lock-variable-use-face
     ;; Labeled arguments: ~label, ?label
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

;; The `ocaml-interface' grammar shares all node types with `ocaml',
;; so a single set of indentation rules works for both languages.

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
     ;; Empty lines: use previous line's indentation, adding offset
     ;; when the previous line ends with a body-expecting token.
     ;; Must come first because Emacs sets node=nil, parent=compilation_unit
     ;; for empty lines, which would otherwise match the top-level rule.
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

     ;; Comments and strings preserve previous indentation
     ((node-is "comment") prev-line 0)
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

(defun neocaml--defun-valid-p (node)
  "Return non-nil if NODE is a top-level definition.
Filters out nodes nested inside `let_expression',
`parenthesized_module_expression', or `package_expression'."
  (and (treesit-node-check node 'named)
       (not (treesit-node-top-level
             node (regexp-opt '("let_expression"
                                "parenthesized_module_expression"
                                "package_expression")
                              'symbols)))))

(defun neocaml--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "type_binding"
         "method_definition"
         "instance_variable_definition"
         "module_binding"
         "module_type_definition"
         "class_binding"
         "class_type_binding")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("exception_definition"
     (treesit-node-text
      (treesit-search-subtree node "constructor_name" nil nil 2) t))
    ("external"
     (treesit-node-text
      (treesit-search-subtree node "value_name" nil nil 1) t))
    ("let_binding"
     (treesit-node-text
      (treesit-node-child-by-field-name node "pattern") t))
    ("value_specification"
     (treesit-node-text
      (treesit-search-subtree node "value_name" nil nil 1) t))
    ("method_specification"
     (treesit-node-text
      (treesit-search-subtree node "method_name" nil nil 1) t))
    ("instance_variable_specification"
     (treesit-node-text
      (treesit-search-subtree node "instance_variable_name" nil nil 1) t))))


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

(defun neocaml--thing-settings (language)
  "Return `treesit-thing-settings' definitions for LANGUAGE.
Configures sexp, sentence, text, and comment navigation."
  `((,language
     (sexp (not ,(rx (or "{" "}" "(" ")" "[" "]" "[|" "|]"
                         "," "." ";" ";;" ":" "::" ":>" "->"
                         "<-" "=" "|" ".."))))
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

;;;; Compilation support

(defconst neocaml--compilation-error-regexp
  (eval-when-compile
    (rx bol
        ;; 0 or 7 leading spaces. 7 spaces = ancillary location (info level).
        ;; Requiring exactly 7 avoids false matches on Python tracebacks.
        (? (group-n 9 "       "))
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

(defun neocaml--compilation-end-column ()
  "Return the end-column from an OCaml compilation message.
OCaml uses exclusive end-columns but Emacs expects inclusive ones."
  (when (match-beginning 7)
    (+ (string-to-number (match-string 7))
       (if (>= emacs-major-version 28) -1 0))))

(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)

(defun neocaml--setup-compilation ()
  "Register OCaml compilation error regexp with compile.el."
  (require 'compile)
  ;; OCaml uses 0-indexed character positions in error messages.
  (setq-local compilation-first-column 0)
  (setq compilation-error-regexp-alist-alist
        (assq-delete-all 'ocaml compilation-error-regexp-alist-alist))
  (push `(ocaml
          ,neocaml--compilation-error-regexp
          3 (4 . 5) (6 . neocaml--compilation-end-column) (8 . 9) 1
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
    (when (and (string-match-p "/_build/" file)
               (derived-mode-p 'neocaml-base-mode))
      (if-let* ((source (neocaml--resolve-build-path file)))
          (when (y-or-n-p (format "This file is under _build.  Switch to %s? " source))
            (find-alternate-file source))
        (message "Note: this file is under _build/ (no source found)")))))

;;;; Fill paragraph

(defun neocaml--fill-paragraph (&optional _justify)
  "Fill the OCaml comment at point.
Uses tree-sitter to find comment boundaries, then narrows to the
comment body (excluding delimiters) and fills.  Returns t if point
was in a comment, nil otherwise to let the default handler run."
  (let* ((node (treesit-node-at (point)))
         (comment (treesit-parent-until
                   node
                   (lambda (n) (equal (treesit-node-type n) "comment"))
                   t)))
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
            (when (looking-back "[ \t]*\\*+)" nil)
              (setq end (match-beginning 0)))
            (narrow-to-region start end)
            (let ((fill-paragraph-function nil))
              (fill-paragraph nil))
            t))))))

;;;; Utility commands

(defconst neocaml-report-bug-url "https://github.com/bbatsov/neocaml/issues/new"
  "The URL to report a `neocaml' issue.")

(defun neocaml-report-bug ()
  "Report a bug in your default browser."
  (interactive)
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
        ("Find..."
         ["Find Interface/Implementation" ff-find-other-file]
         ["Find Interface/Implementation in other window" ff-find-other-file-other-window])
        "--"
        ["Compile..." compile]
        ["Cycle indent function" neocaml-cycle-indent-function]
        ["Install tree-sitter grammars" neocaml-install-grammars]
        ("Documentation"
         ["Browse OCaml Docs" neocaml-browse-ocaml-docs])
        "--"
        ["Report a neocaml bug" neocaml-report-bug]
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

  (when (treesit-ready-p language)
    (treesit-parser-create language)

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

    (treesit-major-mode-setup)))

(define-derived-mode neocaml-base-mode prog-mode "OCaml"
  "Base major mode for OCaml files, providing shared setup.
This mode is not intended to be used directly.  Use `neocaml-mode'
for .ml files and `neocaml-interface-mode' for .mli files."
  :syntax-table neocaml-base-mode-syntax-table

  ;; comment settings
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\*+)")

  ;; Electric indentation on delimiters
  (setq-local electric-indent-chars
              (append "{}()" electric-indent-chars))

  ;; Fill paragraph
  (setq-local fill-paragraph-function #'neocaml--fill-paragraph)
  (setq-local adaptive-fill-mode t)

  ;; TODO: Make this configurable?
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string number)
                (attribute builtin constant type)
                (operator bracket delimiter variable function)))

  (setq-local indent-line-function #'treesit-indent)

  ;; Fallback for Emacs 29 (no treesit-thing-settings)
  (unless (boundp 'treesit-thing-settings)
    (setq-local forward-sexp-function #'neocaml-forward-sexp))
  (setq-local treesit-defun-type-regexp
              (cons neocaml--defun-type-regexp
                    #'neocaml--defun-valid-p))
  (setq-local treesit-defun-name-function #'neocaml--defun-name)

  ;; ff-find-other-file setup
  (setq-local ff-other-file-alist neocaml-other-file-alist)

  ;; Setup prettify-symbols (users enable prettify-symbols-mode via hooks)
  (setq-local prettify-symbols-alist
              (if neocaml-prettify-symbols-full
                  (append neocaml-prettify-symbols-alist
                          neocaml-prettify-symbols-extra-alist)
                neocaml-prettify-symbols-alist))

  (neocaml--setup-compilation))

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

;; Offer to switch away from _build/ copies
(add-hook 'find-file-hook #'neocaml--check-build-dir)

;; Eglot integration: set the language IDs that ocamllsp expects.
;; These symbol properties are consulted by eglot when it cannot
;; derive the correct language-id from the major-mode name.
(put 'neocaml-mode 'eglot-language-id "ocaml")
(put 'neocaml-interface-mode 'eglot-language-id "ocaml.interface")

(provide 'neocaml)

;;; neocaml.el ends here
