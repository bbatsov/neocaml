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

(defun neocaml-test--reindent (code)
  "Strip indentation from CODE, re-indent with `neocaml-mode', return result.
CODE should be a properly-indented OCaml string.  This function
strips all leading whitespace, inserts the result into a temp
buffer with `neocaml-mode', re-indents the whole buffer, and
returns the buffer contents."
  (with-temp-buffer
    (insert (neocaml-test--strip-indentation code))
    (neocaml-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(describe "neocaml indentation"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "indents a simple let binding"
    (let ((code "let x =\n  42"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a let with function body"
    (let ((code "let f x y =\n  x + y"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents let...in chain without accumulation"
    (let ((code "let x = 1 in\nlet y = 2 in\nx + y"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents nested let...in"
    (let ((code "let r =\n  let x = 1 in\n  let y = 2 in\n  x + y"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a match expression"
    (let ((code "match x with\n| A -> 1\n| B -> 2"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a match with multiline case body"
    (let ((code "match x with\n| A ->\n  long_expr\n| B -> 2"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents if/then/else"
    (let ((code "if cond then\n  expr1\nelse\n  expr2"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a type definition with variants"
    (let ((code "type t =\n  | Foo\n  | Bar of int"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a record type"
    (let ((code "type t = {\n  x: int;\n  y: float;\n}"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a record expression"
    (let ((code "let r = {\n  x = 1;\n  y = 2;\n}"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a module with struct"
    (let ((code "module M = struct\n  let x = 1\nend"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a signature"
    (let ((code "module type S = sig\n  val x : int\nend"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents try/with"
    (let ((code "try\n  expr\nwith\n| E -> handler"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a for loop"
    (let ((code "for i = 0 to 10 do\n  body\ndone"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a while loop"
    (let ((code "while cond do\n  body\ndone"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a fun expression"
    (let ((code "let f = fun x ->\n  x + 1"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a list expression"
    (let ((code "let xs = [\n  1;\n  2;\n]"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a sequence expression"
    (let ((code "let () =\n  print \"a\";\n  print \"b\""))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents a nested match"
    (let ((code "match x with\n| A ->\n  match y with\n  | B -> 1"))
      (expect (neocaml-test--reindent code) :to-equal code)))

  (it "indents an external declaration"
    (let ((code "external foo : int -> int = \"c_foo\""))
      (expect (neocaml-test--reindent code) :to-equal code))))

;;; neocaml-indentation-test.el ends here
