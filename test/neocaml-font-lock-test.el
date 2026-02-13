;;; neocaml-font-lock-test.el --- Font-lock tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-mode font-lock rules.
;; Tests are organized by font-lock feature, matching the 4 levels
;; defined in `treesit-font-lock-feature-list'.

;;; Code:

(require 'buttercup)
(require 'neocaml)

;;;; Test helpers

(defmacro with-fontified-neocaml-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT, apply neocaml-mode font-lock, run BODY.
All four font-lock levels are activated so every feature is testable."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((treesit-font-lock-level 4))
       (neocaml-mode))
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun neocaml-test-face-at (start end)
  "Return the face at range [START, END] in the current buffer.
If all positions in the range share the same face, return it.
Otherwise return the symbol `various-faces'."
  (let ((face (get-text-property start 'face)))
    (if (= start end)
        face
      (let ((pos (1+ start))
            (consistent t))
        (while (and consistent (<= pos end))
          (unless (equal (get-text-property pos 'face) face)
            (setq consistent nil))
          (setq pos (1+ pos)))
        (if consistent face 'various-faces)))))

(defun neocaml-test-expect-faces-at (content &rest face-specs)
  "Fontify CONTENT with `neocaml-mode' and assert FACE-SPECS.
Each element of FACE-SPECS is a list (START END EXPECTED-FACE)
where START and END are buffer positions (1-indexed, inclusive)."
  (with-fontified-neocaml-buffer content
    (dolist (spec face-specs)
      (let* ((start (nth 0 spec))
             (end (nth 1 spec))
             (expected (nth 2 spec))
             (actual (neocaml-test-face-at start end))
             (snippet (buffer-substring-no-properties start (min (1+ end) (point-max)))))
        (expect actual :to-equal expected)))))

(defmacro when-fontifying-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in OCaml code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE (START END FACE) ...)
where CODE is an OCaml source string and each (START END FACE)
triple asserts that positions START through END have FACE."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (apply #'neocaml-test-expect-faces-at test))))

(defmacro with-fontified-neocaml-interface-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT, apply neocaml-interface-mode font-lock, run BODY.
All four font-lock levels are activated so every feature is testable."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((treesit-font-lock-level 4))
       (neocaml-interface-mode))
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun neocaml-test-expect-interface-faces-at (content &rest face-specs)
  "Fontify CONTENT with `neocaml-interface-mode' and assert FACE-SPECS.
Each element of FACE-SPECS is a list (START END EXPECTED-FACE)
where START and END are buffer positions (1-indexed, inclusive)."
  (with-fontified-neocaml-interface-buffer content
    (dolist (spec face-specs)
      (let* ((start (nth 0 spec))
             (end (nth 1 spec))
             (expected (nth 2 spec))
             (actual (neocaml-test-face-at start end))
             (snippet (buffer-substring-no-properties start (min (1+ end) (point-max)))))
        (expect actual :to-equal expected)))))

(defmacro when-fontifying-interface-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in OCaml interface code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE (START END FACE) ...)
where CODE is an OCaml interface source string and each (START END FACE)
triple asserts that positions START through END have FACE."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (apply #'neocaml-test-expect-interface-faces-at test))))

;;;; Tests

(describe "neocaml font-lock"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  ;; ---- Level 1 features ------------------------------------------------

  (describe "comment feature"
    (when-fontifying-it "fontifies regular comments"
      ("(* a comment *)"
       (1 15 font-lock-comment-face)))

    (when-fontifying-it "fontifies doc comments"
      ("(** a doc comment *)"
       (1 20 font-lock-doc-face)))

    (when-fontifying-it "fontifies nested comments"
      ("(* outer (* inner *) end *)"
       (1 27 font-lock-comment-face))))

  (describe "definition feature"
    (when-fontifying-it "fontifies let-bound variables"
      ("let x = 42"
       (5 5 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies = in let bindings as keyword"
      ("let x = 42"
       (7 7 font-lock-keyword-face)))

    (when-fontifying-it "fontifies let-bound functions"
      ("let f x = x + 1"
       (5 5 font-lock-function-name-face)))

    (when-fontifying-it "fontifies external declarations"
      ;; external foo : int -> int = "c_foo"
      ;; 123456789...
      ("external foo : int -> int = \"c_foo\""
       (10 12 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies type-annotated let binding"
      ;; let x : int = 42
      ;; 12345678901234567
      ("let x : int = 42"
       (5 5 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies method definition in object"
      ;; object method foo = 42 end
      ;; 123456789012345678901234567
      ("object method foo = 42 end"
       (15 17 font-lock-function-name-face)))

    (when-fontifying-it "fontifies value_pattern in match"
      ;; match x with\n| y -> y
      ("match x with\n| y -> y"
       (16 16 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies constructor_pattern binding"
      ;; match x with\n| Some v -> v
      ("match x with\n| Some v -> v"
       (21 21 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies tuple_pattern binding"
      ;; let (a, b) = (1, 2)
      ;; 1234567890123456789
      ("let (a, b) = (1, 2)"
       (6 6 font-lock-variable-name-face)
       (9 9 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies punned field pattern"
      ;; let { x; y } = r
      ;; 12345678901234567
      ("let { x; y } = r"
       (7 7 font-lock-variable-name-face)
       (10 10 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies value_specification in sig"
      ;; module type S = sig\n  val x : int\nend
      ("module type S = sig\n  val x : int\nend"
       (27 27 font-lock-variable-name-face)))

    (when-fontifying-it "fontifies = in type binding as keyword"
      ;; type t = int
      ;; 123456789012
      ("type t = int"
       (8 8 font-lock-keyword-face)))

    (when-fontifying-it "fontifies = in for expression as keyword"
      ;; for i = 1 to 10 do () done
      ;; 1234567890123456789012345678
      ("for i = 1 to 10 do () done"
       (7 7 font-lock-keyword-face))))

  ;; ---- Level 2 features ------------------------------------------------

  (describe "keyword feature"
    (when-fontifying-it "fontifies if/then/else"
      ("if true then 1 else 2"
       (1 2 font-lock-keyword-face)
       (9 12 font-lock-keyword-face)
       (16 19 font-lock-keyword-face)))

    (when-fontifying-it "fontifies -> in fun expressions"
      ("fun x -> x"
       (1 3 font-lock-keyword-face)
       (7 8 font-lock-keyword-face)))

    (when-fontifying-it "fontifies -> in match cases"
      ("match x with\n| _ -> 1"
       (1 5 font-lock-keyword-face)
       (9 12 font-lock-keyword-face)
       (18 19 font-lock-keyword-face)))

    (when-fontifying-it "fontifies various keywords"
      ("let rec f x = f x"
       (1 3 font-lock-keyword-face)
       (5 7 font-lock-keyword-face))))

  (describe "string feature"
    (when-fontifying-it "fontifies string literals"
      ("\"hello\""
       (1 7 font-lock-string-face)))

    (when-fontifying-it "fontifies character literals"
      ("let c = 'a'"
       (9 11 font-lock-string-face)))

    (when-fontifying-it "fontifies quoted/raw strings"
      ("{|raw string|}"
       (1 14 font-lock-string-face)))

    (when-fontifying-it "fontifies empty strings"
      ("\"\""
       (1 2 font-lock-string-face)))

    (when-fontifying-it "fontifies strings with escape sequences"
      ("\"line1\\nline2\""
       (1 14 font-lock-string-face))))

  (describe "number feature"
    (when-fontifying-it "fontifies integers"
      ("let x = 42"
       (9 10 font-lock-number-face)))

    (when-fontifying-it "fontifies floats"
      ("let x = 3.14"
       (9 12 font-lock-number-face)))

    (when-fontifying-it "fontifies hex literals"
      ("let x = 0xFF"
       (9 12 font-lock-number-face)))

    (when-fontifying-it "fontifies underscore-separated numbers"
      ("let x = 1_000_000"
       (9 17 font-lock-number-face)))

    (when-fontifying-it "fontifies binary literals"
      ("let x = 0b1010"
       (9 14 font-lock-number-face)))

    (when-fontifying-it "fontifies negative float exponents"
      ("let x = 1.5e-3"
       (9 14 font-lock-number-face))))

  ;; ---- Level 3 features ------------------------------------------------

  (describe "attribute feature"
    (when-fontifying-it "fontifies inline attributes"
      ;; let[@inline] f x = x
      ;; 1234567890123456...
      ("let[@inline] f x = x"
       (4 12 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies item attributes (@@)"
      ;; type t = int [@@deriving show]
      ;; 123456789012345678901234567890
      ("type t = int [@@deriving show]"
       (14 29 font-lock-preprocessor-face)))

    (when-fontifying-it "fontifies floating attributes (@@@)"
      ;; [@@@warning "-32"]
      ;; 123456789012345678
      ("[@@@warning \"-32\"]"
       (1 11 font-lock-preprocessor-face)
       (18 18 font-lock-preprocessor-face))))

  (describe "builtin feature"
    (when-fontifying-it "fontifies builtin identifiers"
      ;; __LOC__ is not used as a function call, so keeps builtin face
      ("let _ = __LOC__"
       (9 15 font-lock-builtin-face)))

    (when-fontifying-it "fontifies builtin exceptions"
      ("let _ = Not_found"
       (9 17 font-lock-builtin-face))))

  (describe "constant feature"
    (when-fontifying-it "fontifies boolean constants"
      ("let _ = true"
       (9 12 font-lock-constant-face))
      ("let _ = false"
       (9 13 font-lock-constant-face)))

    (when-fontifying-it "fontifies unit"
      ("let _ = ()"
       (9 10 font-lock-constant-face))))

  (describe "type feature"
    (when-fontifying-it "fontifies type constructors"
      ("type t = int"
       (6 6 font-lock-type-face)
       (10 12 font-lock-type-face)))

    (when-fontifying-it "fontifies type variables"
      ;; type 'a t = 'a list
      ;; 1234567890123456789
      ("type 'a t = 'a list"
       (6 7 font-lock-type-face)
       (9 9 font-lock-type-face)
       (13 14 font-lock-type-face)
       (16 19 font-lock-type-face)))

    (when-fontifying-it "fontifies module names"
      ;; module M = List
      ;; 123456789012345
      ("module M = List"
       (8 8 font-lock-type-face)
       (12 15 font-lock-type-face)))

    (when-fontifying-it "fontifies function type arrow"
      ;; module type S = sig\n  val f : int -> int\nend
      ("module type S = sig\n  val f : int -> int\nend"
       (35 36 font-lock-type-face)))

    (when-fontifying-it "fontifies tuple type star"
      ;; type t = int * string
      ;; 123456789012345678901
      ("type t = int * string"
       (14 14 font-lock-type-face)))

    (when-fontifying-it "fontifies constructor declaration star"
      ;; type t =\n  | A of int * string
      ("type t =\n  | A of int * string"
       (23 23 font-lock-type-face)))

    (when-fontifying-it "fontifies record declaration delimiters"
      ;; type r = { x : int; y : string }
      ;; 12345678901234567890123456789012
      ("type r = { x : int; y : string }"
       (10 10 font-lock-type-face)
       (19 19 font-lock-type-face)
       (32 32 font-lock-type-face)))

    (when-fontifying-it "fontifies module_type_name"
      ;; module type S = sig end
      ;; 12345678901234567890123
      ("module type S = sig end"
       (13 13 font-lock-type-face)))

    (when-fontifying-it "fontifies constructor names in expressions"
      ;; let x = Some 1
      ;; 123456789012345
      ("let x = Some 1"
       (9 12 font-lock-constant-face)))

    (when-fontifying-it "fontifies constructor names in patterns"
      ;; match x with\n| Some v -> v\n| None -> 0
      ;; S(16)o(17)m(18)e(19); N(30)o(31)n(32)e(33)
      ("match x with\n| Some v -> v\n| None -> 0"
       (16 19 font-lock-constant-face)
       (30 33 font-lock-constant-face)))

    (when-fontifying-it "fontifies constructor names in type declarations"
      ;; type t =\n  | Foo\n  | Bar of int
      ;; \n pos 14 = F, 16 = o; 22 = B, 24 = r
      ("type t =\n  | Foo\n  | Bar of int"
       (14 16 font-lock-constant-face)
       (22 24 font-lock-constant-face)))

    (when-fontifying-it "fontifies polymorphic variant tags"
      ;; `Red
      ;; 12345
      ("let x = `Red"
       (10 12 font-lock-constant-face))))

  ;; ---- Level 4 features ------------------------------------------------

  (describe "operator feature"
    (when-fontifying-it "fontifies infix operators"
      ("let x = 1 + 2"
       (11 11 font-lock-operator-face)))

    (when-fontifying-it "fontifies prefix operators"
      ("let x = !r"
       (9 9 font-lock-operator-face)))

    (when-fontifying-it "fontifies pipe operator"
      ;; let x = 1 |> f
      ;; 123456789012345
      ("let x = 1 |> f"
       (11 12 font-lock-operator-face)))

    (when-fontifying-it "fontifies comparison operators"
      ;; let b = x >= y
      ;; 12345678901234
      ("let b = x >= y"
       (11 12 font-lock-operator-face)))

    (when-fontifying-it "fontifies string concat operator"
      ;; let s = "a" ^ "b"
      ;; 12345678901234567
      ("let s = \"a\" ^ \"b\""
       (13 13 font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-it "fontifies parentheses"
      ("let x = (1 + 2)"
       (9 9 font-lock-bracket-face)
       (15 15 font-lock-bracket-face)))

    (when-fontifying-it "fontifies square brackets"
      ;; [1; 2; 3]
      ;; 123456789
      ("[1; 2; 3]"
       (1 1 font-lock-bracket-face)
       (9 9 font-lock-bracket-face)))

    (when-fontifying-it "fontifies curly braces"
      ;; let r = { x = 1 }
      ;; 12345678901234567
      ("let r = { x = 1 }"
       (9 9 font-lock-bracket-face)
       (17 17 font-lock-bracket-face))))

  (describe "delimiter feature"
    (when-fontifying-it "fontifies semicolons"
      ("let _ = (1; 2)"
       (11 11 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies ;;"
      (";;"
       (1 2 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies commas in tuples"
      ;; (1, 2, 3)
      ;; 123456789
      ("(1, 2, 3)"
       (3 3 font-lock-delimiter-face)
       (6 6 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies colon in type annotation"
      ;; let (x : int) = 1
      ;; 123456789012345678
      ("let (x : int) = 1"
       (8 8 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies dot in module path"
      ;; List.length xs
      ;; 12345678901234
      ("List.length xs"
       (5 5 font-lock-delimiter-face))))

  (describe "variable feature"
    (when-fontifying-it "fontifies variable uses"
      ;; In `x + y`, x and y are plain value_name uses
      ("let _ = x + y"
       (9 9 font-lock-variable-use-face)
       (13 13 font-lock-variable-use-face)))

    (when-fontifying-it "fontifies field name use"
      ;; r.name
      ;; 123456
      ("r.name"
       (3 6 font-lock-variable-use-face))))

  (describe "function feature"
    (when-fontifying-it "fontifies function calls"
      ("print_endline \"hello\""
       (1 13 font-lock-function-call-face)))

    (when-fontifying-it "fontifies qualified function calls"
      ;; List.map f xs
      ;; 1234567890123
      ("List.map f xs"
       (1 4 font-lock-type-face)
       (6 8 font-lock-function-call-face)))

    (when-fontifying-it "fontifies builtin used as function call"
      ;; raise Not_found
      ;; 123456789012345
      ("raise Not_found"
       (1 5 font-lock-function-call-face)))

    (when-fontifying-it "fontifies nested function calls"
      ;; f (g x)
      ;; 1234567
      ("f (g x)"
       (1 1 font-lock-function-call-face)
       (4 4 font-lock-function-call-face)))))

(describe "neocaml-interface font-lock"
  (before-all
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available")))

  (describe "comment feature"
    (when-fontifying-interface-it "fontifies regular comments"
      ("(* a comment *)"
       (1 15 font-lock-comment-face)))

    (when-fontifying-interface-it "fontifies doc comments"
      ("(** a doc comment *)"
       (1 20 font-lock-doc-face))))

  (describe "definition feature"
    (when-fontifying-interface-it "fontifies value_name in val specification"
      ;; val x : int
      ;; 12345678901
      ("val x : int"
       (5 5 font-lock-variable-name-face)))

    (when-fontifying-interface-it "fontifies : in val specification as keyword"
      ;; val x : int
      ;; 12345678901
      ("val x : int"
       (7 7 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies external value_name"
      ;; external foo : int -> int = "c_foo"
      ;; 123456789...
      ("external foo : int -> int = \"c_foo\""
       (10 12 font-lock-variable-name-face)))

    (when-fontifying-interface-it "fontifies method_specification"
      ;; class type c = object method m : int end
      ;; 1234567890123456789012345678901234567890
      ("class type c = object method m : int end"
       (30 30 font-lock-function-name-face))))

  (describe "keyword feature"
    (when-fontifying-interface-it "fontifies val keyword"
      ("val x : int"
       (1 3 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies type keyword"
      ("type t = int"
       (1 4 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies module keyword"
      ("module M : sig end"
       (1 6 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies sig and end keywords"
      ;; module type S = sig\n  val x : int\nend
      ("module type S = sig\n  val x : int\nend"
       (17 19 font-lock-keyword-face)
       (35 37 font-lock-keyword-face)))

    (when-fontifying-interface-it "fontifies external keyword"
      ("external foo : int -> int = \"c_foo\""
       (1 8 font-lock-keyword-face))))

  (describe "type feature"
    (when-fontifying-interface-it "fontifies type constructors"
      ;; val x : int
      ;; 12345678901
      ("val x : int"
       (9 11 font-lock-type-face)))

    (when-fontifying-interface-it "fontifies type variables"
      ;; type 'a t = 'a list
      ;; 1234567890123456789
      ("type 'a t = 'a list"
       (6 7 font-lock-type-face)
       (13 14 font-lock-type-face)))

    (when-fontifying-interface-it "fontifies -> in function types"
      ;; val f : int -> int
      ;; 123456789012345678
      ("val f : int -> int"
       (13 14 font-lock-type-face)))

    (when-fontifying-interface-it "fontifies module names"
      ;; module M : sig end
      ;; 123456789012345678
      ("module M : sig end"
       (8 8 font-lock-type-face)))

    (when-fontifying-interface-it "fontifies module_type_name"
      ;; module type S = sig end
      ;; 12345678901234567890123
      ("module type S = sig end"
       (13 13 font-lock-type-face)))

    (when-fontifying-interface-it "fontifies constructor names in type declarations"
      ;; type t =\n  | Foo\n  | Bar of int
      ("type t =\n  | Foo\n  | Bar of int"
       (14 16 font-lock-constant-face)
       (22 24 font-lock-constant-face))))

  (describe "attribute feature"
    (when-fontifying-interface-it "fontifies item attributes"
      ;; type t = int [@@deriving show]
      ;; 123456789012345678901234567890
      ("type t = int [@@deriving show]"
       (14 28 font-lock-preprocessor-face))))

  (describe "string feature"
    (when-fontifying-interface-it "fontifies strings in external declarations"
      ;; external foo : int -> int = "c_foo"
      ;; positions:                  29-35
      ("external foo : int -> int = \"c_foo\""
       (29 35 font-lock-string-face)))))

;;; neocaml-font-lock-test.el ends here
