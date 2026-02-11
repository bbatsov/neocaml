;;; neocaml-font-lock-test.el --- Font-lock tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025 Bozhidar Batsov

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
       (10 12 font-lock-variable-name-face))))

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
       (9 11 font-lock-string-face))))

  (describe "number feature"
    (when-fontifying-it "fontifies integers"
      ("let x = 42"
       (9 10 font-lock-number-face)))

    (when-fontifying-it "fontifies floats"
      ("let x = 3.14"
       (9 12 font-lock-number-face)))

    (when-fontifying-it "fontifies hex literals"
      ("let x = 0xFF"
       (9 12 font-lock-number-face))))

  ;; ---- Level 3 features ------------------------------------------------

  (describe "attribute feature"
    (when-fontifying-it "fontifies inline attributes"
      ;; let[@inline] f x = x
      ;; 1234567890123456...
      ("let[@inline] f x = x"
       (4 12 font-lock-preprocessor-face))))

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
       (12 15 font-lock-type-face))))

  ;; ---- Level 4 features ------------------------------------------------

  (describe "operator feature"
    (when-fontifying-it "fontifies infix operators"
      ("let x = 1 + 2"
       (11 11 font-lock-operator-face)))

    (when-fontifying-it "fontifies prefix operators"
      ("let x = !r"
       (9 9 font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-it "fontifies parentheses"
      ("let x = (1 + 2)"
       (9 9 font-lock-bracket-face)
       (15 15 font-lock-bracket-face))))

  (describe "delimiter feature"
    (when-fontifying-it "fontifies semicolons"
      ("let _ = (1; 2)"
       (11 11 font-lock-delimiter-face)))

    (when-fontifying-it "fontifies ;;"
      (";;"
       (1 2 font-lock-delimiter-face))))

  (describe "variable feature"
    (when-fontifying-it "fontifies variable uses"
      ;; In `x + y`, x and y are plain value_name uses
      ("let _ = x + y"
       (9 9 font-lock-variable-use-face)
       (13 13 font-lock-variable-use-face))))

  (describe "function feature"
    (when-fontifying-it "fontifies function calls"
      ("print_endline \"hello\""
       (1 13 font-lock-function-call-face)))

    (when-fontifying-it "fontifies qualified function calls"
      ;; List.map f xs
      ;; 1234567890123
      ("List.map f xs"
       (1 4 font-lock-type-face)
       (6 8 font-lock-function-call-face)))))

;;; neocaml-font-lock-test.el ends here
