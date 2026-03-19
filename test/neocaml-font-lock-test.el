;;; neocaml-font-lock-test.el --- Font-lock tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-mode font-lock rules.
;; Tests are organized by font-lock feature, matching the 4 levels
;; defined in `treesit-font-lock-feature-list'.

;;; Code:

(require 'neocaml-test-helpers)

;;;; Tests

(describe "neocaml font-lock"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  ;; ---- Level 1 features ------------------------------------------------

  (describe "comment feature"
    (when-fontifying-it "fontifies regular comments"
      ("(* a comment *)"
       ("(* a comment *)" font-lock-comment-face)))

    (when-fontifying-it "fontifies doc comments"
      ("(** a doc comment *)"
       ("(** a doc comment *)" font-lock-doc-face)))

    (when-fontifying-it "fontifies nested comments"
      ("(* outer (* inner *) end *)"
       ("(* outer (* inner *) end *)" font-lock-comment-face)))

    (when-fontifying-it "fontifies line number directives"
      ("# 1 \"file.ml\""
       ("# 1 \"file.ml\"" font-lock-comment-face)))

    (when-fontifying-it "fontifies shebang lines"
      ("#!/usr/bin/env ocaml\nlet x = 1"
       ("#!/usr/bin/env ocaml" font-lock-comment-face))))

  (describe "definition feature"
    (when-fontifying-it "fontifies let-bound variables"
      ("let x = 42"
       ("x" font-lock-variable-name-face)))

    (when-fontifying-it "fontifies = in let bindings as keyword"
      ("let x = 42"
       ("=" font-lock-keyword-face)))

    (when-fontifying-it "fontifies let-bound functions"
      ("let f x = x + 1"
       (5 5 font-lock-function-name-face)))

    (when-fontifying-it "fontifies let-bound functions with fun body"
      ("let f = fun x -> x"
       (5 5 font-lock-function-name-face)))

    (when-fontifying-it "fontifies let-bound functions with function body"
      ("let f = function\n| _ -> 0"
       (5 5 font-lock-function-name-face)))

    (when-fontifying-it "fontifies external declarations"
      ("external foo : int -> int = \"c_foo\""
       ("foo" font-lock-variable-name-face)))

    (when-fontifying-it "fontifies type-annotated let binding"
      ("let x : int = 42"
       ("x" font-lock-variable-name-face)))

    (when-fontifying-it "fontifies method definition in object"
      ("object method foo = 42 end"
       ("foo" font-lock-function-name-face)))

    (when-fontifying-it "fontifies value_pattern in match"
      ("match x with\n| y -> y"
       (16 16 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies constructor_pattern binding"
      ("match x with\n| Some v -> v"
       (21 21 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies tuple_pattern binding"
      ("let (a, b) = (1, 2)"
       ("a" font-lock-variable-name-face)
       ("b" font-lock-variable-name-face)))

    (when-fontifying-it "fontifies punned field pattern"
      ("let { x; y } = r"
       ("x" font-lock-variable-name-face)
       ("y" font-lock-variable-name-face)))

    (when-fontifying-it "fontifies value_specification in sig"
      ("module type S = sig\n  val x : int\nend"
       (27 27 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies = in type binding as keyword"
      ("type t = int"
       (8 8 font-lock-keyword-face)))

    (when-fontifying-it "fontifies = in for expression as keyword"
      ("for i = 1 to 10 do () done"
       (7 7 font-lock-keyword-face))))

  ;; ---- Level 2 features ------------------------------------------------

  (describe "keyword feature"
    (when-fontifying-it "fontifies if/then/else"
      ("if true then 1 else 2"
       ("if" font-lock-keyword-face)
       ("then" font-lock-keyword-face)
       ("else" font-lock-keyword-face)))

    (when-fontifying-it "fontifies -> in fun expressions"
      ("fun x -> x"
       ("fun" font-lock-keyword-face)
       ("->" font-lock-keyword-face)))

    (when-fontifying-it "fontifies -> in match cases"
      ("match x with\n| _ -> 1"
       ("match" font-lock-keyword-face)
       ("with" font-lock-keyword-face)
       ("->" font-lock-keyword-face)))

    (when-fontifying-it "fontifies various keywords"
      ("let rec f x = f x"
       ("let" font-lock-keyword-face)
       ("rec" font-lock-keyword-face)))

    (when-fontifying-it "fontifies let* binding operator as keyword"
      ("let* x = Some 1 in x"
       ("let*" font-lock-keyword-face)))

    (when-fontifying-it "fontifies and* binding operator as keyword"
      ("let* x = Some 1 and* y = Some 2 in x + y"
       ("let*" font-lock-keyword-face)
       ("and*" font-lock-keyword-face)))

    (when-fontifying-it "fontifies match+ binding operator as keyword"
      ("match+ x with\n| Some y -> y\n| None -> 0"
       ("match+" font-lock-keyword-face))))

  (describe "string feature"
    (when-fontifying-it "fontifies string literals"
      ("\"hello\""
       ("\"hello\"" font-lock-string-face)))

    (when-fontifying-it "fontifies character literals"
      ("let c = 'a'"
       ("'a'" font-lock-string-face)))

    (when-fontifying-it "fontifies quoted/raw strings"
      ("{|raw string|}"
       ("{|raw string|}" font-lock-string-face)))

    (when-fontifying-it "fontifies empty strings"
      ("\"\""
       ("\"\"" font-lock-string-face))))

  (describe "type feature"
    (when-fontifying-it "fontifies type constructors"
      ("type t = my_type"
       ("t" font-lock-type-face)
       ("my_type" font-lock-type-face)))

    (when-fontifying-it "fontifies type variables"
      ("type 'a t = 'a my_list"
       ("'a" font-lock-type-face)
       ("t" font-lock-type-face)
       ("'a" font-lock-type-face)
       ("my_list" font-lock-type-face)))

    (when-fontifying-it "fontifies module names"
      ("module M = List"
       ("M" font-lock-type-face)
       ("List" font-lock-type-face)))

    (when-fontifying-it "fontifies function type arrow"
      ("module type S = sig\n  val f : int -> int\nend"
       (35 36 font-lock-type-face)))

    (when-fontifying-it "fontifies tuple type star"
      ("type t = int * string"
       (14 14 font-lock-type-face)))

    (when-fontifying-it "fontifies constructor declaration star"
      ("type t =\n  | A of int * string"
       (23 23 font-lock-type-face)))

    (when-fontifying-it "fontifies record declaration delimiters"
      ("type r = { x : int; y : string }"
       (10 10 font-lock-type-face)
       (19 19 font-lock-type-face)
       (32 32 font-lock-type-face)))

    (when-fontifying-it "fontifies module_type_name"
      ("module type S = sig end"
       ("S" font-lock-type-face)))

    (when-fontifying-it "fontifies constructor names in expressions"
      ("let x = Some 1"
       ("Some" font-lock-constant-face)))

    (when-fontifying-it "fontifies constructor names in patterns"
      ("match x with\n| Some v -> v\n| None -> 0"
       ("Some" font-lock-constant-face)
       ("None" font-lock-constant-face)))

    (when-fontifying-it "fontifies constructor names in type declarations"
      ("type t =\n  | Foo\n  | Bar of int"
       ("Foo" font-lock-constant-face)
       ("Bar" font-lock-constant-face)))

    (when-fontifying-it "fontifies polymorphic variant tags"
      ("let x = `Red"
       ("Red" font-lock-constant-face))))

  ;; ---- Level 3 features ------------------------------------------------

  (describe "attribute feature"
    (when-fontifying-it "fontifies inline attributes"
      ("let[@inline] f x = x"
       ("[@inline]" font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies item attributes (@@)"
      ("type t = int [@@deriving show]"
       (14 29 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies floating attributes (@@@)"
      ("[@@@warning \"-32\"]"
       (1 11 font-lock-preprocessor-face)
       (18 18 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies extension nodes ([%...])"
      ("[%test \"name\"]"
       (1 7 font-lock-preprocessor-face)
       (14 14 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies item extension nodes ([%%...])"
      ("[%%test \"name\"]"
       (1 8 font-lock-preprocessor-face)
       (15 15 font-lock-preprocessor-face))))

  (describe "builtin feature"
    (when-fontifying-it "fontifies builtin identifiers"
      ("let _ = __LOC__"
       ("__LOC__" font-lock-builtin-face)))

    (when-fontifying-it "fontifies builtin exceptions"
      ("let _ = Not_found"
       ("Not_found" font-lock-builtin-face)))

    (when-fontifying-it "fontifies builtin types"
      ("type t = int"
       (10 12 font-lock-builtin-face))
      ("type t = string"
       (10 15 font-lock-builtin-face))
      ("type t = bool"
       (10 13 font-lock-builtin-face))
      ("type t = int iarray"
       ("iarray" font-lock-builtin-face)))

    (when-fontifying-it "does not fontify user types as builtin"
      ("type t = my_type"
       ("my_type" font-lock-type-face))))

  (describe "constant feature"
    (when-fontifying-it "fontifies boolean constants"
      ("let _ = true"
       ("true" font-lock-constant-face))
      ("let _ = false"
       ("false" font-lock-constant-face)))

    (when-fontifying-it "fontifies unit"
      ("let _ = ()"
       ("()" font-lock-constant-face))))

  (describe "escape-sequence feature"
    (when-fontifying-it "fontifies escape sequences in strings"
      ("\"line1\\nline2\""
       (1 6 font-lock-string-face)
       (7 8 font-lock-escape-face)
       (9 14 font-lock-string-face)))

    (when-fontifying-it "fontifies multiple escape sequences"
      ("\"a\\t\\nb\""
       (1 2 font-lock-string-face)
       (3 4 font-lock-escape-face)
       (5 6 font-lock-escape-face)
       (7 8 font-lock-string-face)))

    (when-fontifying-it "fontifies conversion specifications in format strings"
      ("let _ = Printf.printf \"%d items: %s\""
       (24 25 font-lock-regexp-face)
       (34 35 font-lock-regexp-face))))

  (describe "number feature"
    (when-fontifying-it "fontifies integers"
      ("let x = 42"
       ("42" font-lock-number-face)))

    (when-fontifying-it "fontifies floats"
      ("let x = 3.14"
       ("3.14" font-lock-number-face)))

    (when-fontifying-it "fontifies hex literals"
      ("let x = 0xFF"
       ("0xFF" font-lock-number-face)))

    (when-fontifying-it "fontifies underscore-separated numbers"
      ("let x = 1_000_000"
       ("1_000_000" font-lock-number-face)))

    (when-fontifying-it "fontifies binary literals"
      ("let x = 0b1010"
       ("0b1010" font-lock-number-face)))

    (when-fontifying-it "fontifies negative float exponents"
      ("let x = 1.5e-3"
       ("1.5e-3" font-lock-number-face)))

    (when-fontifying-it "fontifies signed numbers in patterns"
      ("match x with\n| -1 -> true\n| _ -> false"
       ("-1" font-lock-number-face))))

  ;; ---- Level 4 features ------------------------------------------------

  (describe "operator feature"
    (when-fontifying-it "fontifies infix operators"
      ("let x = 1 + 2"
       ("+" font-lock-operator-face)))

    (when-fontifying-it "fontifies prefix operators"
      ("let x = !r"
       ("!" font-lock-operator-face)))

    (when-fontifying-it "fontifies pipe operator"
      ("let x = 1 |> f"
       ("|>" font-lock-operator-face)))

    (when-fontifying-it "fontifies comparison operators"
      ("let b = x >= y"
       (">=" font-lock-operator-face)))

    (when-fontifying-it "fontifies string concat operator"
      ("let s = \"a\" ^ \"b\""
       ("^" font-lock-operator-face)))

    (when-fontifying-it "fontifies cons operator"
      ("let xs = 1 :: []"
       ("::" font-lock-operator-face)))

    (when-fontifying-it "fontifies mutable assignment operator"
      ("r.x <- 42"
       ("<-" font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-it "fontifies parentheses"
      ("let x = (1 + 2)"
       (9 9 font-lock-bracket-face)
       (15 15 font-lock-bracket-face)))

    (when-fontifying-it "fontifies square brackets"
      ("[1; 2; 3]"
       (1 1 font-lock-bracket-face)
       (9 9 font-lock-bracket-face)))

    (when-fontifying-it "fontifies curly braces"
      ("let r = { x = 1 }"
       (9 9 font-lock-bracket-face)
       (17 17 font-lock-bracket-face)))

    (when-fontifying-it "fontifies array brackets"
      ("[| 1; 2 |]"
       (1 2 font-lock-bracket-face)
       (9 10 font-lock-bracket-face))))

  (describe "delimiter feature"
    (when-fontifying-it "fontifies semicolons"
      ("let _ = (1; 2)"
       (";" font-lock-delimiter-face)))

    (when-fontifying-it "fontifies ;;"
      (";;"
       (";;" font-lock-delimiter-face)))

    (when-fontifying-it "fontifies commas in tuples"
      ("(1, 2, 3)"
       (3 3 font-lock-delimiter-face)
       (6 6 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies colon in type annotation"
      ("let (x : int) = 1"
       (8 8 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies dot in module path"
      ("List.length xs"
       ("." font-lock-delimiter-face))))

  (describe "variable feature"
    (when-fontifying-it "fontifies variable uses"
      ("let _ = x + y"
       ("x" font-lock-variable-use-face)
       ("y" font-lock-variable-use-face))))

  (describe "property feature"
    (when-fontifying-it "fontifies field name use"
      ("r.name"
       ("name" font-lock-property-use-face))))

  (describe "label feature"
    (when-fontifying-it "fontifies labeled argument at call site"
      ("let _ = f ~x:1 ~y:2"
       (12 12 font-lock-property-use-face)
       (17 17 font-lock-property-use-face)))

    (when-fontifying-it "fontifies optional labeled argument at call site"
      ("let _ = f ?x:1"
       (12 12 font-lock-property-use-face))))

  (describe "function feature"
    (when-fontifying-it "fontifies function calls"
      ("print_endline \"hello\""
       ("print_endline" font-lock-function-call-face)))

    (when-fontifying-it "fontifies qualified function calls"
      ("List.map f xs"
       ("List" font-lock-type-face)
       ("map" font-lock-function-call-face)))

    (when-fontifying-it "fontifies builtin used as function call"
      ("raise Not_found"
       ("raise" font-lock-function-call-face)))

    (when-fontifying-it "fontifies nested function calls"
      ("f (g x)"
       (1 1 font-lock-function-call-face)
       (4 4 font-lock-function-call-face)))

    (when-fontifying-it "fontifies function in pipe operator"
      ("x |> f"
       (6 6 font-lock-function-call-face)))

    (when-fontifying-it "fontifies function in apply operator"
      ("f @@ x"
       (1 1 font-lock-function-call-face)))))

(describe "neocaml-interface font-lock"
  (before-all
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available")))

  (describe "comment feature"
    (when-fontifying-interface-it "fontifies regular comments"
      ("(* a comment *)"
       ("(* a comment *)" font-lock-comment-face)))

    (when-fontifying-interface-it "fontifies doc comments"
      ("(** a doc comment *)"
       ("(** a doc comment *)" font-lock-doc-face))))

  (describe "definition feature"
    (when-fontifying-interface-it "fontifies value_name in val specification"
      ("val x : int"
       ("x" font-lock-variable-name-face)))

    (when-fontifying-interface-it "fontifies : in val specification as keyword"
      ("val x : int"
       (7 7 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies external value_name"
      ("external foo : int -> int = \"c_foo\""
       ("foo" font-lock-variable-name-face)))

    (when-fontifying-interface-it "fontifies method_specification"
      ("class type c = object method m : int end"
       ("m" font-lock-function-name-face))))

  (describe "keyword feature"
    (when-fontifying-interface-it "fontifies val keyword"
      ("val x : int"
       ("val" font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies type keyword"
      ("type t = int"
       ("type" font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies module keyword"
      ("module M : sig end"
       ("module" font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies sig and end keywords"
      ("module type S = sig\n  val x : int\nend"
       ("sig" font-lock-keyword-face)
       ("end" font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies external keyword"
      ("external foo : int -> int = \"c_foo\""
       ("external" font-lock-keyword-face))))

  (describe "type feature"
    (when-fontifying-interface-it "fontifies type constructors"
      ("val x : my_type"
       ("my_type" font-lock-type-face)))

    (when-fontifying-interface-it "fontifies type variables"
      ("type 'a t = 'a list"
       ("'a" font-lock-type-face)
       ("'a" font-lock-type-face)))

    (when-fontifying-interface-it "fontifies -> in function types"
      ("val f : int -> int"
       ("->" font-lock-type-face)))

    (when-fontifying-interface-it "fontifies module names"
      ("module M : sig end"
       ("M" font-lock-type-face)))

    (when-fontifying-interface-it "fontifies module_type_name"
      ("module type S = sig end"
       ("S" font-lock-type-face)))

    (when-fontifying-interface-it "fontifies constructor names in type declarations"
      ("type t =\n  | Foo\n  | Bar of int"
       ("Foo" font-lock-constant-face)
       ("Bar" font-lock-constant-face))))

  (describe "attribute feature"
    (when-fontifying-interface-it "fontifies item attributes"
      ("type t = int [@@deriving show]"
       (14 28 font-lock-preprocessor-face))))

  (describe "string feature"
    (when-fontifying-interface-it "fontifies strings in external declarations"
      ("external foo : int -> int = \"c_foo\""
       ("\"c_foo\"" font-lock-string-face)))))

;;; neocaml-font-lock-test.el ends here
