;;; neocaml.el --- Major mode for OCaml code -*- lexical-binding: t; -*-

;; Copyright © 2025 Bozhidar Batsov
;;
;; Authors: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml ml
;; Version: 0.0.1
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

(defcustom neocaml-ensure-grammars t
  "When non-nil, ensure required tree-sitter grammars are installed."
  :safe #'booleanp
  :type 'boolean
  :package-version '(neocaml . "0.0.1"))

(defvar neocaml--debug 'font-lock
  "Enables debugging messages, shows current node in mode-line.
Only intended for use at development time.")

(defconst neocaml-version "0.0.1")

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
  "Intended to be used as the value for `treesit-language-source-alist'.")

(defun neocaml--ensure-grammars ()
  "Install required language grammars if not already available."
  (when neocaml-ensure-grammars
    (dolist (recipe neocaml-grammar-recipes)
      (let ((grammar (car recipe)))
        (unless (treesit-language-available-p grammar nil)
          (message "Installing %s tree-sitter grammar" grammar)
          ;; `treesit-language-source-alist' is dynamically scoped.
          ;; Binding it in this let expression allows
          ;; `treesit-install-language-gramamr' to pick up the grammar recipes
          ;; without modifying what the user has configured themselves.
          (let ((treesit-language-source-alist neocaml-grammar-recipes))
            (treesit-install-language-grammar grammar)))))))

;; adapted from tuareg-mode
(defvar neocaml-mode-syntax-table
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

(defvar neocaml-mode--keywords
  (let ((infix-operators '("asr" "land" "lor" "lsl" "lsr" "lxor" "or" "mod")))
    (seq-remove (lambda (k) (seq-position infix-operators k))
                (string-split "
  and         as          assert      asr         begin       class
  constraint  do          done        downto      else        end
  exception   external    false       for         fun         function
  functor     if          in          include     inherit     initializer
  land        lazy        let         lor         lsl         lsr
  lxor        match       method      mod         module      mutable
  new         nonrec      object      of          open        or
  private     rec         sig         struct      then        to
  true        try         type        val         virtual     when
  while       with")))
  "OCaml keywords for tree-sitter font-locking.
List taken directly from https://v2.ocaml.org/manual/lex.html.
Infix operators are parsed and fontified separately.")

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

(defun neocaml-mode--font-lock-settings (language)
  "Tree-sitter font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   '((((comment) @font-lock-doc-face)
      (:match "^(\\*\\*[^*]" @font-lock-doc-face))
     (comment) @font-lock-comment-face)

   :language language
   :feature 'definition
   '(;; let-bound functions and variables, methods
     (let_binding pattern: (value_name) @font-lock-variable-name-face (":" (_)) :? (":>" (_)) :? :anchor body: (_))
     (let_binding pattern: (value_name) @font-lock-function-name-face (parameter)+)
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

   :language language
   :feature 'attribute
   '((attribute) @font-lock-preprocessor-face
     (item_attribute) @font-lock-preprocessor-face
     (floating_attribute) @font-lock-preprocessor-face)

   :language language
   :feature 'string
   :override t
   '([(string) (quoted_string) (character)] @font-lock-string-face)

   :language language
   :feature 'number
   :override t
   '((number) @font-lock-number-face)

   :language language
   :feature 'builtin
   `([";;"] @font-lock-preprocessor-face
     ((value_path :anchor (value_name) @font-lock-builtin-face)
      (:match ,(regexp-opt neocaml-mode--builtin-ids 'symbols) @font-lock-builtin-face))
     ((constructor_path :anchor (constructor_name) @font-lock-builtin-face)
      (:match ,(regexp-opt neocaml-mode--builtin-ids 'symbols) @font-lock-builtin-face)))

   :language language
   :feature 'constant
   `(;; some literals TODO: any more?
     [,@neocaml-mode--constants] @font-lock-constant-face
     ;; doesn't look great
     ;; (constructor_name) @font-lock-constant-face
     ;; (method_invocation (method_name) @font-lock-constant-face)
     ;; TODO: highlight just alpha infix at lvl 3 to match tuareg?
     (method_invocation "#" @font-lock-operator-face)
     (infix_expression operator: _  @font-lock-operator-face)
     (prefix_expression operator: _ @font-lock-operator-face))

   ;; :language language
   ;; :feature 'type
   ;; :override t
   ;; '((_expression) @custom-invalid)

   :language language
   :feature 'type
   '([(type_constructor) (type_variable) (hash_type)
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
      (module_type_name) @font-lock-type-face)))

(defvar neocaml-mode--defun-type-regexp
  (regexp-opt '("type_binding"
                "exception_definition"
                "external"
                "let_binding"
                "value_specification"
                "method_definition"
                "method_specification"
                "instance_variable_definition"
                "instance_variable_specification"
                "module_binding"
                "module_type_definition"
                "class_binding"
                "class_type_binding"))
  "Regex used to find defun-like nodes.")

(defun neocaml-mode--defun-valid-p (node)
  "Predicate to check if NODE is really defun-like."
  (and (treesit-node-check node 'named)
       (not (treesit-node-top-level
             node (regexp-opt '("let_expression"
                                "parenthesized_module_expression"
                                "package_expression")
                              'symbols)))))

(defun neocaml-mode--defun-name (node)
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

(defun neocaml-mode--imenu-name (node)
  "Return qualified defun name of NODE."
  (let ((name nil))
    (while node
      (when-let ((new-name (treesit-defun-name node)))
        (if name
            (setq name (concat new-name
                               treesit-add-log-defun-delimiter
                               name))
          (setq name new-name)))
      (setq node (treesit-node-parent node)))
    name))

;; TODO: could add constructors / fields
(defvar neocaml-mode--imenu-settings
  `(("Type" "\\`type_binding\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name)
    ("Spec" "\\`\\(value_specification\\|method_specification\\)\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name)
    ("Exception" "\\`exception_definition\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name)
    ("Value" "\\`\\(let_binding\\|external\\)\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name)
    ("Method" "\\`\\(method_definition\\)\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name)
    ;; grouping module/class types under Type causes some weird nesting
    ("Module" "\\`\\(module_binding\\|module_type_definition\\)\\'"
     neocaml-mode--defun-valid-p nil)
    ("Class" "\\`\\(class_binding\\|class_type_binding\\)\\'"
     neocaml-mode--defun-valid-p neocaml-mode--imenu-name))
  "Settings for `treesit-simple-imenu'.")

(defvar neocaml-mode--block-regex
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
              'symbols))

(defun neocaml-mode-forward-sexp (arg)
  "Implement `forward-sexp-function'.ARG is passed to `treesit-end-of-thing'."
  (if (< arg 0)
      (treesit-beginning-of-thing neocaml-mode--block-regex (- arg))
    (treesit-end-of-thing neocaml-mode--block-regex arg)))

(defconst neocaml-report-bug-url "https://github.com/bbatsov/neocaml/issues/new"
  "The URL to report a `neocaml' issue.")

(defun neocaml-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url neocaml-report-bug-url))

(defconst neocaml-ocaml-docs-base-url "https://ocaml.org/docs/"
  "The base URL for official OCaml guides.")

(defun neocaml-browse-ocaml-docs ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url neocaml-ocaml-docs-base-url))

(defvar neocaml-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (easy-menu-define neocaml-mode-menu map "Neocaml Mode Menu"
      '("OCaml"
        ("Documentation"
         ["Browse OCaml Docs" neocaml-browse-ocaml-docs])
        "--"
        ["Report a neocaml bug" neocaml-report-bug]
        ["neocaml version" neocaml-version]))
    map))

(defun neocaml--setup-mode (language)
  (neocaml--ensure-grammars)

  (when (treesit-ready-p language)
    (treesit-parser-create language)

    (when neocaml--debug
      (setq-local treesit--indent-verbose t)

      (when (eq neocaml--debug 'font-lock)
        (setq-local treesit--font-lock-verbose t))

      ;; show the node at point in the minibuffer
      (treesit-inspect-mode))

    ;; comment settings
    (setq-local comment-start "(* ")
    (setq-local comment-end " *)")
    (setq-local comment-start-skip "(\\*+[ \t]*")

    ;; font-lock settings
    (setq-local treesit-font-lock-settings
                (neocaml-mode--font-lock-settings language))

    ;; TODO: Level 4 font-locking is currently missing
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string number)
                  (attribute builtin constant type)))

    ;; TODO: add indentation, which-func, etc

    (treesit-major-mode-setup)))

;;;###autoload
(define-derived-mode neocaml-mode prog-mode "OCaml"
  "Major mode for editing OCaml code.

\\{neocaml-mode-map}"
  :syntax-table neocaml-mode-syntax-table
  (neocaml--setup-mode 'ocaml))

;;;###autoload
(define-derived-mode neocamli-mode prog-mode "OCaml[Interface]"
  "Major mode for editing OCaml interface (mli) code.

\\{neocaml-mode-map}"
  :syntax-table neocaml-mode-syntax-table
  (neocaml--setup-mode 'ocaml-interface))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . neocaml-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . neocamli-mode)))

(provide 'neocaml)

;;; neocaml.el ends here
