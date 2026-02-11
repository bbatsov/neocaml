;;; neocaml-indentation-test.el --- Indentation tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-mode indentation rules.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(defun neocaml-test--strip-indentation (code)
  "Remove all leading whitespace from each line of CODE."
  (mapconcat
   (lambda (line) (string-trim-left line))
   (split-string code "\n")
   "\n"))

(defmacro when-indenting-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Each element of CODE-STRINGS is a
properly-indented OCaml code string.  The macro strips indentation,
re-indents via `neocaml-mode', and asserts the result matches the original."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

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
    "external foo : int -> int = \"c_foo\""))

;;; neocaml-indentation-test.el ends here
