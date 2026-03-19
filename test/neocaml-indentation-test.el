;;; neocaml-indentation-test.el --- Indentation tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-mode indentation rules.

;;; Code:

(require 'neocaml-test-helpers)

(describe "neocaml indentation"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (when-indenting-it "indents a simple let binding"
    "let x =
  42")

  (when-indenting-it "indents a let with function body"
    "let f x y =
  x + y")

  (when-indenting-it "indents let...in chain without accumulation"
    "let x = 1 in
let y = 2 in
x + y")

  (when-indenting-it "indents nested let...in"
    "let r =
  let x = 1 in
  let y = 2 in
  x + y")

  (when-indenting-it "indents let* binding operator chain"
    "let () =
  let* x = foo in
  let* y = bar in
  x + y")

  (when-indenting-it "indents let+ binding operator"
    "let () =
  let+ x = foo in
  x + 1")

  (when-indenting-it "indents let* with multiline body"
    "let main () =
  let* response =
    Http.get url
  in
  let* body =
    Response.body response
  in
  print_endline body")

  (when-indenting-it "indents and* binding operator"
    "let () =
  let* x = foo
  and* y = bar in
  x + y")

  (when-indenting-it "indents a match expression"
    "match x with
| A -> 1
| B -> 2")

  (when-indenting-it "indents a match with multiline case body"
    "match x with
| A ->
  long_expr
| B -> 2")

  (when-indenting-it "indents if/then/else"
    "if cond then
  expr1
else
  expr2")

  (when-indenting-it "indents a type definition with variants"
    "type t =
  | Foo
  | Bar of int")

  (when-indenting-it "indents a record type"
    "type t = {
  x: int;
  y: float;
}")

  (when-indenting-it "indents a record expression"
    "let r = {
  x = 1;
  y = 2;
}")

  (when-indenting-it "indents a module with struct"
    "module M = struct
  let x = 1
end")

  (when-indenting-it "indents a signature"
    "module type S = sig
  val x : int
end")

  (when-indenting-it "indents try/with"
    "try
  expr
with
| E -> handler")

  (when-indenting-it "indents a for loop"
    "for i = 0 to 10 do
  body
done")

  (when-indenting-it "indents a while loop"
    "while cond do
  body
done")

  (when-indenting-it "indents a fun expression"
    "let f = fun x ->
  x + 1")

  (when-indenting-it "indents a list expression"
    "let xs = [
  1;
  2;
]")

  (when-indenting-it "indents a sequence expression"
    "let () =
  print \"a\";
  print \"b\"")

  (when-indenting-it "indents a nested match"
    "match x with
| A ->
  match y with
  | B -> 1")

  (when-indenting-it "indents an external declaration"
    "external foo : int -> int = \"c_foo\"")

  ;; ---- Empty-line indentation (no-node) -----------------------------------

  (describe "empty-line indentation"
    (when-newline-indenting-it "indents after = in let binding"
      ("let x =" 2)
      ("let f x =" 2))

    (when-newline-indenting-it "indents after = in type binding"
      ("type t =" 2))

    (when-newline-indenting-it "indents after -> in fun expression"
      ("let f = fun x ->" 2))

    (when-newline-indenting-it "indents after then/else"
      ("let _ = if true then" 2)
      ("if cond then\n  expr1\nelse" 2))

    (when-newline-indenting-it "indents after match/try with"
      ("let _ = match x with" 2)
      ("try\n  expr\nwith" 2))

    (when-newline-indenting-it "indents after do in loops"
      ("for i = 0 to 10 do" 2)
      ("while cond do" 2))

    (when-newline-indenting-it "indents after struct/sig/begin/object"
      ("module M = struct" 2)
      ("module type S = sig" 2)
      ("let _ = begin" 2)
      ("let _ = object" 2))

    (when-newline-indenting-it "indents after try/fun/function"
      ("try" 2)
      ("let _ = function" 2))

    (when-newline-indenting-it "stays at column 0 after complete top-level expressions"
      ("let x = 42" 0)
      ("let f x = x + 1" 0)
      ("type t = int" 0)
      ("module M = struct\n  let x = 1\nend" 0))

    (when-newline-indenting-it "preserves indentation level inside nested constructs"
      ("module M = struct\n  let x =" 4)
      ("module M = struct\n  let x = 42" 2)
      ("module type S = sig\n  val x : int" 2))

    (when-newline-indenting-it "does not indent after in"
      ("let x = 1 in" 0)))

  ;; ---- Single-line TAB indentation with cursor tracking -------------------

  (describe "single-line TAB indentation"
    (when-indenting-with-point-it "corrects misindented let body"
      "let x =\n|42"
      "let x =\n  |42")

    (when-indenting-with-point-it "corrects over-indented let body"
      "let x =\n      |42"
      "let x =\n  |42")

    (when-indenting-with-point-it "corrects misindented end keyword"
      "module M = struct\n  let x = 1\n  |end"
      "module M = struct\n  let x = 1\n|end")

    (when-indenting-with-point-it "corrects misindented done keyword"
      "for i = 0 to 10 do\n  body\n  |done"
      "for i = 0 to 10 do\n  body\n|done")

    (when-indenting-with-point-it "indents then clause body"
      "if cond then\n|expr"
      "if cond then\n  |expr")

    (when-indenting-with-point-it "corrects over-indented else clause body"
      "if cond then\n  expr1\nelse\n      |expr2"
      "if cond then\n  expr1\nelse\n  |expr2")

    (when-indenting-with-point-it "indents struct body"
      "module M = struct\n|let x = 1"
      "module M = struct\n  |let x = 1")

    (when-indenting-with-point-it "keeps top-level at column 0"
      "let x = 42\n  |let y = 1"
      "let x = 42\n|let y = 1"))

  ;; ---- Comment indentation ------------------------------------------------

  (describe "comment indentation"
    (when-indenting-it "preserves indentation in multiline comment"
      "(** First line.
    Second line. *)")

    (when-indenting-it "preserves indentation after blank line in doc comment"
      "(** First line.

    Second line. *)")

    (when-indenting-it "preserves indentation in indented multiline comment"
      "let x =
  (* First line.
     Second line. *)
  42")

    (when-indenting-with-point-it "indents continuation inside doc comment"
      "(** First line.\n|Second line. *)"
      "(** First line.\n    |Second line. *)")

    (when-indenting-with-point-it "indents after blank line in doc comment"
      "(** First line.\n\n|Second line. *)"
      "(** First line.\n\n    |Second line. *)")))

(describe "neocaml-interface indentation"
  (before-all
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available")))

  (when-indenting-interface-it "indents a simple val specification"
    "val x : int")

  (when-indenting-interface-it "indents module type S = sig ... end"
    "module type S = sig
  val x : int
end")

  (when-indenting-interface-it "indents module M : sig ... end"
    "module M : sig
  val x : int
end")

  (when-indenting-interface-it "indents nested signatures"
    "module type S = sig
  module type T = sig
    val x : int
  end
  val y : int
end")

  (when-indenting-interface-it "indents type definitions with variants"
    "type t =
  | Foo
  | Bar of int")

  (when-indenting-interface-it "indents type definitions with records"
    "type t = {
  x: int;
  y: float;
}")

  (when-indenting-interface-it "indents class types"
    "class type c = object
  method m : int
end")

  (when-indenting-interface-it "indents include module type of"
    "include module type of List")

  (when-indenting-interface-it "indents external declarations"
    "external foo : int -> int = \"c_foo\"")

  (when-indenting-interface-it "indents multiple top-level declarations"
    "type t = int
val x : t
val f : t -> t
exception E of string")

  (describe "empty-line indentation"
    (when-newline-indenting-interface-it "indents after sig"
      ("module type S = sig" 2))

    (when-newline-indenting-interface-it "indents after = in type binding"
      ("type t =" 2))

    (when-newline-indenting-interface-it "stays at column 0 after complete declarations"
      ("val x : int" 0)
      ("type t = int" 0)
      ("exception E" 0))

    (when-newline-indenting-interface-it "preserves indentation inside sig"
      ("module type S = sig\n  val x : int" 2)))

  (describe "single-line TAB indentation"
    (when-indenting-interface-with-point-it "corrects misindented val in sig"
      "module type S = sig\n|val x : int"
      "module type S = sig\n  |val x : int")

    (when-indenting-interface-with-point-it "corrects misindented end keyword"
      "module type S = sig\n  val x : int\n  |end"
      "module type S = sig\n  val x : int\n|end")

    (when-indenting-interface-with-point-it "keeps top-level val at column 0"
      "type t = int\n  |val x : t"
      "type t = int\n|val x : t")))

;;; neocaml-indentation-test.el ends here
