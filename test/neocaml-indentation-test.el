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

  (when-indenting-it "indents with spaces past the tab width"
    "let a =
  let b =
    let c =
      let d =
        1
      in
      d
    in
    c
  in
  b")

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

  (when-indenting-it "indents module definitions broken across lines"
    "module Make_equal
    (M : S)
    (Equal : T) =
struct
  let x = 1
end"
    "module M =
  Cached_value"
    "module Dag =
  Import.Dag.Make
    (struct
      type t = int
    end)"
    "module N
  : S = struct
  let x = 1
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

  (when-indenting-it "indents an external type declaration"
    "type t =
  external \"caml_foo\"")

  (when-indenting-it "puts the body of an OxCaml exclave_ where the exclave_ would go"
    "let f x = exclave_
  let y = x in
  y"
    "let g = fun x -> exclave_
  x"
    "let h x =
  exclave_
  (x, x)")

  (when-indenting-it "indents OxCaml stack_, local_ and borrow_ bodies on the next line"
    "let g x =
  stack_
    (Some x)"
    "let h () =
  local_
    [1; 2]"
    "let k x =
  borrow_
    x")

  (when-indenting-it "indents OxCaml kind definitions and aligns mod with its kind"
    "kind_ k =
  value mod global"
    "kind_ k =
  value
  mod global portable"
    "type t :
  value
  mod portable")

  (when-indenting-it "aligns OxCaml comprehension clauses with their expression"
    "let evens =
  [ x
    for x = 1 to 10
    when x mod 2 = 0 ]"
    "let squares =
  [|
    x * x
    for x in xs
  |]"
    "let pairs =
  [ (x, y)
    for x = 1 to 3
    and y in ys
    when x > y ]")

  (when-indenting-it "aligns the @@ of an OxCaml sig @@ portable with the sig"
    "module type S = sig
@@ portable
  val x : int
end")

  (when-indenting-it "indents an attribute after a body like the body"
    "let f x =
  g x
  [@nontail]"
    "module M = struct
  let f x =
    g x (* tail *)
    [@nontail]
    [@exclave_if_stack a]
  ;;
end"
    "[%%template
  let f x =
    g x
    [@nontail]]")

  (when-indenting-it "indents OxCaml immutable arrays like arrays"
    "let a = [:
  1;
  2;
:]")

  ;; ---- Continuation lines -------------------------------------------------

  (describe "continuation lines"
    (when-indenting-it "aligns operator chains with their start"
      "let f xs =
  xs
  |> List.map g
  |> List.filter h"
      "let ok =
  a
  && b
  && c"
      "let l =
  x
  :: y
  :: zs")

    (when-indenting-it "aligns right-associative chains with their start"
      "let s =
  \"a\"
  ^ \"b\"
  ^ \"c\"")

    (when-indenting-it "puts operators of a parenthesized chain under its first operand"
      "let s =
  f
    (\"a\"
     ^ \"b\")"
      "let ys =
  f
    (xs
     |> g
     |> h)")

    (when-indenting-it "aligns the operand after a trailing operator"
      "let ok =
  a &&
  b")

    (when-indenting-it "indents the right side of an assignment"
      "let () =
  r :=
    compute x"
      "let () =
  r.x <-
    compute x"
      "let () =
  r.x
  <- compute x")

    (when-indenting-it "aligns tuple elements"
      "let t =
  ( 1,
    2,
    3 )"
      "let t =
  ( 1
  , 2
  , 3 )")

    (when-indenting-it "aligns tuple and cons patterns"
      "let f = function
  | ( a,
      b ) -> a"
      "let f = function
  | (x
     :: rest) -> x")

    (when-indenting-it "indents arguments past the function after a parenthesis or a bracket"
      "let r =
  Some
    (Printf.sprintf \"%s\" msg
       (to_string bt))"
      "let r =
  f
    [ g a
        b ]"
      "let r = foo ~f:(g a
                  b)")

    (when-indenting-it "puts the body of a fun starting a let body where the fun is"
      "let f : int -> int =
  fun x ->
  x + 1")

    (when-indenting-it "indents the arguments of a constructor on the next line past it"
      "let f = function
  | Texp_record
      { representation = r } -> r
  | PStr
      (x, y) -> x"
      "type error =
  | Non_regular of
      { definition : Path.t }
  | With_error_handler :
      (Exn.t -> Nothing.t) * (unit -> eff)
      -> eff")

    (when-indenting-it "indents the type or value of a record field on the next line past it"
      "type 'state t = {
  menu : Menu.t;
  handle_request :
    Menu.t -> 'state -> Response.t;
}"
      "let g = function
  | { pstr_desc =
        Pstr_eval x } -> x")

    (when-indenting-it "lines up a tuple type and a leading comma in type parameters"
      "type watermark =
  string
  * int"
      "type u =
  #(int
    * string)"
      "type t =
  ( int
  , string ) result"
      "type t =
  ( 'key,
    'value )
    Hashtbl.t"
      "type t = ('key,
          'value) Hashtbl.t")

    (when-indenting-it "indents a module type on the line after its ="
      "module type S =
  T with type t = int"
      "module type S =
sig
  val x : int
end")

    (when-indenting-it "indents the rest of an include, open, functor, local open or class on the next line"
      "include
  Make (M)"
      "open
  functor
    (A : S)
    ->
    struct
      type t
    end"
      "let l =
  M.
    [ x ]"
      "class virtual c =
  object
    val virtual x : int
  end")

    (when-indenting-it "aligns the and of a nested let rec with its let"
      "let f () =
  let rec a x = b x
  and b x = a x in
  a 1"
      "let fields =
  (let+ a = x
   and+ b = y in a + b)")

    (when-indenting-it "aligns or-patterns with the case"
      "let f x =
  match x with
  | A
  | B -> 1
  | C -> 2")

    (when-indenting-it "puts the | of a parenthesized or-pattern under the parenthesis"
      "let f x =
  match x with
  | Some (A
         | B) -> 1
  | _ -> 0")

    (when-indenting-it "aligns a comment between match cases with the cases"
      "let f x =
  match x with
  (* the first case *)
  | A -> 1
  | B -> 2")

    (when-indenting-it "aligns the cases of a polymorphic variant type"
      "type t =
  [ `A
  | `B ]")

    (when-indenting-it "indents the fields of a record pattern from the brace"
      "let f = function
  | { a;
      b } -> a + b"
      "let f = function
  | { a
    ; b } -> a + b")

    (when-indenting-it "puts a leading ; under the opening bracket"
      "type t =
  { a : int
  ; b : string
  }"
      "let r =
  { a = 1
  ; b = \"x\"
  }"
      "let xs =
  [ 1
  ; 2
  ]"
      "let a =
  [| 1
   ; 2
  |]"
      "let f = function
  | [ a
    ; b ] -> a")

    (when-indenting-it "aligns ;; with the item it ends"
      "module M = struct
  let x = 1
  ;;

  let y = 2 (* two *)
  ;;

  let f x = g x [@inline]
  ;;
end")

    (when-indenting-it "aligns the body of a fun or function after an operator with the chain"
      "let f x =
  x >>= function
  | A -> 1
  (* B *)
  | B -> 2"
      "let f x =
  x
  >>= function
  | A -> 1
  | B -> 2"
      "let f x =
  g (x >>= function
     | A -> 1
     | B -> 2)"
      "let f x =
  x >>= fun y ->
  g y")

    (when-indenting-it "indents the body of a fun after :="
      "let () =
  r := fun x ->
    x + 1")

    (when-indenting-it "aligns item attributes on a line of their own with their item"
      "type t =
  | A
  | B
[@@deriving sexp]"
      "let f x = x
[@@inline]"
      "module M = struct
  type t = { a : int }
  [@@deriving sexp]
end")

    (when-indenting-it "indents attribute and extension payloads on the next line"
      "let x = 1
[@@deriving
  sexp, compare]"
      "[%%template
  let f x = x [@@mode m = (global, local)]]")

    (when-indenting-it "leaves the items of a template with several items unindented"
      "[%%template
[@@@mode.default m = (global, local)]

let f x = x
let g x = x]")

    (when-indenting-it "aligns the cases of a function starting its line with it"
      "let f =
  let open M in
  function
  | A -> 1
  | B -> 2"
      "let f = function
  | A -> 1
  | B -> 2")

    (when-indenting-it "indents a match or if subject on a line of its own"
      "let f x =
  match
    g x
  with
  | A -> 1"
      "let f x =
  if
    a && b
  then 1
  else 2")

    (when-indenting-it "indents a labeled argument's value on the next line"
      "let () =
  List.iter
    ~f:
      (fun x -> print x)
    xs")

    (when-indenting-it "indents lazy and extension payloads on the next line"
      "let x =
  lazy
    (compute ())"
      "let%expect_test _ =
  [%expect
    {| foo |}]"))

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

    (when-newline-indenting-it "indents after OxCaml exclave_, stack_, local_ and borrow_"
      ("let f x = exclave_" 2)
      ("let f x =\n  exclave_" 2)
      ("let f x =\n  let y = x in exclave_" 2)
      ("let f x =\n  stack_" 4)
      ("let f x = local_" 2)
      ("let f x = borrow_" 2))

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
      "(** First line.\n\n    |Second line. *)")

    (when-indenting-it "lines up a comment between definitions with them"
      "type t = A
(* c *)
and u = B"
      "module M = struct
  let rec f x = g x
  (* c *)
  and g x = f x
end")

    (it "moves a line no rule covers along with the line its parent starts on"
      (with-temp-buffer
        (insert "module M = struct\nlet f = function\n  | A x\n    as c -> c\nend")
        (neocaml-mode)
        (dotimes (_ 2)
          (indent-region (point-min) (point-max))
          (expect (buffer-string)
                  :to-equal "module M = struct\n  let f = function\n    | A x\n      as c -> c\nend"))))

    (it "indents a line no rule covers that's left of its parent's line"
      (with-temp-buffer
        (insert "    type t =\n      [ `A\n  (* c *)\n      | `B ]")
        (neocaml-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal "type t =\n  [ `A\n  (* c *)\n  | `B ]")))

    (it "keeps a comment no rule covers in place relative to its parent, however often it's indented"
      (dolist (code '("type t =\n  [ `A\n  (* c *)\n  | `B ]"
                      "type\n  (* c *)\n  t = int"))
        (with-temp-buffer
          (insert code)
          (neocaml-mode)
          (dotimes (_ 3)
            (indent-region (point-min) (point-max)))
          (expect (buffer-string) :to-equal code)))))

  (describe "string indentation"
    (it "leaves the lines inside a multi-line string as they are"
      (dolist (code '("let s = \"usage:\n      prog FILE\""
                      "let s = \"foo\n   \""
                      "let () =\n  print ();\n  [%expect {|\n     foo\n   bar |}]"
                      "let () =\n  [%expect {|\n    foo\n  |}]"
                      "let x = {%sql|\n  SELECT 1\n|}"))
        (with-temp-buffer
          (insert code)
          (neocaml-mode)
          (indent-region (point-min) (point-max))
          (expect (buffer-string) :to-equal code))))))

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

  (when-indenting-interface-it "indents an external type declaration"
    "type t =
  external \"caml_foo\"")

  (when-indenting-interface-it "indents OxCaml kind definitions"
    "kind_ k =
  value mod global")

  (when-indenting-interface-it "aligns the lines of a function type"
    "val f :
  int ->
  string ->
  unit"
    "val f :
  (int ->
   int) ->
  unit")

  (when-indenting-interface-it "puts a leading -> under the colon that starts its line"
    "val f
  :  int
  -> string
  -> unit")

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

(describe "neocaml indentation styles"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (describe "ocp-indent (the default)"
    (when-indenting-it "puts the cases of a match or try right inside parentheses under its keyword"
      "let v =
  f
    (match x with
     | A -> 1
     | B -> 2)"
      "let v =
  (try f () with
   | Not_found -> 0)")

    (when-indenting-it "indents arguments two steps past a line after a body token, one after in"
      "let x = some_function arg1
    arg2"
      "let f x = exclave_ g a
    b"
      "let f x =
  let y = 1 in g y
    z")

    (when-indenting-it "indents arguments past the function after the -> of a match case"
      "let f = function
  | A -> g a
           b")

    (when-indenting-it "indents arguments after an operator, a comma or a semicolon past the chain"
      "let v = x |> g a
          b"
      "let l =
  x
  :: List.map
    (fun y -> y)
    ys"
      "let t = (a, f b
           c)")

    (when-indenting-it "indents typed patterns and expressions, polymorphic types and let%ext bindings"
      "let h (x :
         t) = x"
      "let f (x
       : t) = x"
      "let v = (e
         : t)"
      "let f : type a.
  a t -> a = fun x -> x"
      "let%expect_test
  \"name\" = ()")

    (it "indents a typed pattern starting its own line the same way every time"
      (let ((code "let h (\n  x : t) = x"))
        (with-temp-buffer
          (insert code)
          (neocaml-mode)
          (dotimes (_ 3)
            (indent-region (point-min) (point-max)))
          (expect (buffer-string) :to-equal code))))

    (when-indenting-it "indents parentheses opening a match case's body one step past the body"
      "let f = function
  | A -> begin
      foo ();
      bar ()
    end
  | B -> (
      foo ();
      bar ())")

    (when-indenting-it "puts the with of a match or try right inside parentheses under its keyword"
      "let v =
  (try f ()
   with Not_found -> 0)")

    (when-indenting-it "indents parameters on continuation lines twice as far"
      "let f first second
    third =
  first"
      "let rec f ~first
    ?second (type a) =
  first")

    (when-indenting-it "indents a functor argument or parameter partway along a line as if the functor started it"
      "module M = F (struct
    type t = int
  end)"
      "module F (X : sig
    type t
  end) =
struct
  let x = 1
end")

    (when-indenting-it "indents the signature of other functor parameters from inside their parentheses"
      "module G (X : sig
    type t
  end) (Y : sig
          type u
        end) = struct
  let x = 1
end"
      "module H
    (X : sig
       type t
     end)
    (Y : S) =
struct
  let x = 1
end"
      "module F = functor (X : sig
                      type t
                    end) -> struct
  type t = X.t
end")

    (when-indenting-it "indents the body of a fun opening a line in parentheses past the fun"
      "let a =
  List.map
    (fun x ->
       x + 1)
    l")

    (when-indenting-it "indents the body of a fun further along a line twice as far"
      "let b = f x (fun y ->
    y + 1)"
      "let () =
  List.iter
    ~f:(fun x ->
        print x)
    l"))

  (describe "conventional"
    (when-indenting-in-style-it conventional "indents match case bodies twice as far past the |"
      "let f value =
  match value with
  | Some payload ->
      let r = transform payload in
      finish r
  | None -> default"
      "let f value other =
  match value with
  | First -> (
      match other with
      | Alpha -> 1
      | Beta -> 2)
  | Second -> 3")

    (when-indenting-in-style-it conventional "indents a guard or -> starting a line, and the body of a case after with, once"
      "let f = function
  | Some l
    when is_long l ->
      l
  | _ -> 0"
      "let f = function
  | Record { fields; representation }
    ->
      fields"
      "let apply f x =
  try f x
  with exn ->
    let e = capture exn in
    reraise e")

    (when-indenting-in-style-it conventional "puts the cases of a match right inside parentheses under the parenthesis"
      "let v =
  f
    (match x with
    | A -> 1
    | B -> 2)"
      "let v = a @ (match x with
            | A -> 1)")

    (when-indenting-in-style-it conventional "puts the with of a match or try right inside parentheses under its keyword"
      "let v =
  (try f ()
   with Not_found -> 0)")

    (when-indenting-in-style-it conventional "puts operators of a parenthesized chain under the parenthesis"
      "let s =
  f
    (\"a\"
    ^ \"b\")"
      "let ys =
  f
    (xs
    |> g
    |> h)"
      "let f = function
  | (x
    :: rest) -> x"
      "type t =
  (int ->
  int) ->
  unit")

    (when-indenting-in-style-it conventional "indents arguments after an operator past the function"
      "let l =
  x
  :: List.map
       (fun y -> y)
       ys"
      "let v = x |> g a
               b")

    (when-indenting-in-style-it conventional "puts a type applied to parameters on a line of its own under them"
      "type t =
  ( 'key,
    'value )
  Hashtbl.t")

    (when-indenting-in-style-it conventional "indents typed patterns and expressions, polymorphic types and let%ext bindings"
      "let h (x :
        t) = x"
      "let v = (e
          : t)"
      "let f : type a.
    a t -> a = fun x -> x"
      "let%expect_test
    \"name\" = ()")

    (when-indenting-in-style-it conventional "indents parameters on continuation lines twice as far"
      "let f first second
    third =
  first")

    (when-indenting-in-style-it conventional "indents a functor argument or parameter like any other structure or signature"
      "module M = F (struct
  type t = int
end)"
      "module F (X : sig
  type t
end) =
struct
  let x = 1
end"
      "module F = functor (X : sig
  type t
end) -> struct
  type t = X.t
end")

    (when-indenting-in-style-it conventional "indents the body of a fun in parentheses from the parenthesis or the function"
      "let a =
  List.map
    (fun x ->
      x + 1)
    l"
      "let b =
  ref (fun x ->
      let y = f x in
      g y)"
      "let () =
  List.iter
    ~f:(fun x ->
      print x)
    l"))

  (describe "janestreet"
    (when-indenting-in-style-it janestreet "indents match case bodies once past the |"
      "let f value =
  match value with
  | Some payload ->
    let r = transform payload in
    finish r
  | None -> default"
      "let v =
  f
    (match x with
     | A -> 1
     | B -> 2)")

    (when-indenting-in-style-it janestreet "indents arguments past the function everywhere"
      "let x = some_function arg1
          arg2"
      "let v = x |> g a
               b")

    (when-indenting-in-style-it janestreet "puts a leading comma in type parameters under the first one"
      "type t =
  ( int
    , string ) result")

    (when-indenting-in-style-it janestreet "indents typed patterns and expressions, polymorphic types and let%ext bindings"
      "let h (x :
        t) = x"
      "let v = (e
         : t)"
      "let f
  :  'a.
     'a -> 'a
  =
  fun x -> x"
      "let%expect_test
    \"name\" = ()")

    (when-indenting-in-style-it janestreet "indents parameters on continuation lines three times as far"
      "let long_parameters
      first
      second
  =
  first")

    (when-indenting-in-style-it janestreet "indents the body of a fun past the fun only when it's passed to a function"
      "let c =
  List.map
    (fun t ->
       body t)
    l"
      "let a =
  Communication
    (fun t ->
      body t)"
      "let z =
  some_function title (fun formatter ->
    body formatter)")

    (when-indenting-in-style-it janestreet "indents a functor argument or parameter partway along a line as if the functor started it"
      "module M = F (struct
    type t = int
  end)"
      "module F (X : sig
    type t
  end) =
struct
  let x = 1
end")))

(defun neocaml-test--write-file (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert content)))

(defun neocaml-test--style-of (file)
  "Return the indentation style of a buffer visiting FILE."
  (with-temp-buffer
    (setq buffer-file-name file)
    (neocaml--indent-style)))

(describe "neocaml-indent-style"
  :var (root)
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))
  (before-each
    (setq root (file-name-as-directory (make-temp-file "neocaml-style" t)))
    (neocaml-test--write-file (expand-file-name "dune-project" root) "(lang dune 3.0)\n"))
  (after-each
    (delete-directory root t))

  (it "picks the janestreet style from the project's .ocamlformat"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root)
                              "version = 0.27.0\nprofile = janestreet\n")
    (expect (neocaml-test--style-of (expand-file-name "lib/a.ml" root))
            :to-be 'janestreet))

  (it "picks the conventional style for the other profiles, or none"
    (dolist (content '("profile=default\n" "profile = ocamlformat\n" "margin = 90\n"))
      (neocaml-test--write-file (expand-file-name ".ocamlformat" root) content)
      (expect (neocaml-test--style-of (expand-file-name "a.ml" root))
              :to-be 'conventional)))

  (it "falls back to ocp-indent without an .ocamlformat or .ocp-indent"
    (expect (neocaml-test--style-of (expand-file-name "a.ml" root))
            :to-be 'ocp-indent))

  (it "lets the nearest .ocamlformat set an option, up to the project root"
    (let ((project (expand-file-name "proj/" root)))
      (neocaml-test--write-file (expand-file-name ".ocamlformat" root) "profile=janestreet\n")
      (neocaml-test--write-file (expand-file-name "dune-project" project) "(lang dune 3.0)\n")
      (expect (neocaml-test--style-of (expand-file-name "a.ml" project))
              :to-be 'ocp-indent)
      (neocaml-test--write-file (expand-file-name ".ocamlformat" project) "profile=janestreet\n")
      (neocaml-test--write-file (expand-file-name "lib/.ocamlformat" project) "profile=conventional\n")
      (expect (neocaml-test--style-of (expand-file-name "lib/a.ml" project))
              :to-be 'conventional)
      (expect (neocaml-test--style-of (expand-file-name "bin/a.ml" project))
              :to-be 'janestreet)))

  (it "takes the offset of case bodies from cases-exp-indent"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root) "profile=janestreet\n")
    (neocaml-test--write-file (expand-file-name "lib/.ocamlformat" root) "cases-exp-indent = 4\n")
    (let ((code "let f value =
  match value with
  | Some payload ->
      let r = transform payload in
      finish r
  | None -> default"))
      (expect (neocaml-test--reindent #'neocaml-mode code (expand-file-name "lib/a.ml" root))
              :to-equal code)))

  (it "resets the offset of case bodies to the profile's when a profile follows cases-exp-indent"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root) "cases-exp-indent=4\n")
    (neocaml-test--write-file (expand-file-name "lib/.ocamlformat" root) "profile=janestreet\n")
    (neocaml-test--write-file (expand-file-name "bin/.ocamlformat" root)
                              "cases-exp-indent=4\nprofile=janestreet\n")
    (let ((code "let f value =
  match value with
  | Some payload ->
    finish payload
  | None -> default"))
      (expect (neocaml-test--reindent #'neocaml-mode code (expand-file-name "lib/a.ml" root))
              :to-equal code)
      (expect (neocaml-test--reindent #'neocaml-mode code (expand-file-name "bin/a.ml" root))
              :to-equal code)))

  (it "picks the style and the case offset from the .ocp-indent without an .ocamlformat"
    (pcase-dolist (`(,content . ,expected)
                   '(("# Jane Street's\nJaneStreet\n" janestreet)
                     ("normal\nstrict_with=auto\n" ocp-indent)
                     ("match_clause=4\nstrict_with=auto\n" ocp-indent . 4)
                     ("match_clause = 4, JaneStreet" janestreet)))
      (neocaml-test--write-file (expand-file-name ".ocp-indent" root) content)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "lib/a.ml" root))
        (expect (neocaml--project-indentation) :to-equal expected))))

  (it "doesn't count an .ocamlformat for the files ocamlformat doesn't format"
    (neocaml-test--write-file (expand-file-name ".ocp-indent" root) "match_clause=4\n")
    (neocaml-test--write-file (expand-file-name "lambda/.ocamlformat" root)
                              "profile=conventional\ndisable=true\n")
    (neocaml-test--write-file (expand-file-name "lambda/.ocamlformat-enable" root) "matching.ml\n")
    (neocaml-test--write-file (expand-file-name "lib/.ocamlformat" root) "profile=janestreet\n")
    (neocaml-test--write-file (expand-file-name "lib/.ocamlformat-ignore" root)
                              "# vendored\nvendor/*\n")
    (pcase-dolist (`(,file . ,expected)
                   '(("lambda/matching.ml" conventional)
                     ("lambda/translcore.ml" ocp-indent . 4)
                     ("lib/a.ml" janestreet)
                     ("lib/vendor/b.ml" ocp-indent . 4)
                     ("lib/vendor/sub/c.ml" janestreet)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name file root))
        (expect (neocaml--project-indentation) :to-equal expected))))

  (it "prefers the .ocamlformat to the .ocp-indent"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root) "profile=janestreet\n")
    (neocaml-test--write-file (expand-file-name ".ocp-indent" root) "match_clause=4\n")
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "a.ml" root))
      (expect (neocaml--project-indentation) :to-equal '(janestreet))))

  (it "prefers an explicit style to the .ocamlformat's"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root)
                              "profile=janestreet\ncases-exp-indent=4\n")
    (let ((neocaml-indent-style 'ocp-indent)
          (code "let f value =
  match value with
  | Some payload ->
    finish payload
  | None -> default"))
      (expect (neocaml-test--style-of (expand-file-name "a.ml" root)) :to-be 'ocp-indent)
      (expect (neocaml-test--reindent #'neocaml-mode code (expand-file-name "a.ml" root))
              :to-equal code)))

  (it "can be set for the current buffer, and back to the .ocamlformat's"
    (neocaml-test--write-file (expand-file-name ".ocamlformat" root) "profile=janestreet\n")
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "a.ml" root))
      (neocaml-set-indent-style 'conventional)
      (expect (local-variable-p 'neocaml-indent-style) :to-be-truthy)
      (expect (neocaml--indent-style) :to-be 'conventional)
      (neocaml-set-indent-style nil)
      (expect (neocaml--indent-style) :to-be 'janestreet))))

;;; neocaml-indentation-test.el ends here
