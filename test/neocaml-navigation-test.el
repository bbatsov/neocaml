;;; neocaml-navigation-test.el --- Navigation tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-mode navigation: defun movement,
;; forward-sexp, defun-name, and sentence navigation.

;;; Code:

(require 'buttercup)
(require 'neocaml)

;;;; Test helpers

(defmacro with-neocaml-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT in `neocaml-mode', run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (neocaml-mode)
     (goto-char (point-min))
     ,@body))

(defmacro with-neocaml-interface-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT in `neocaml-interface-mode', run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (neocaml-interface-mode)
     (goto-char (point-min))
     ,@body))

;;;; beginning-of-defun / end-of-defun

(describe "navigation: beginning-of-defun"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "moves to the start of the current let binding"
    (with-neocaml-buffer "let x = 1\n\nlet y = 2\n"
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "let y") :to-be-truthy)))

  (it "moves past multiple defuns"
    (with-neocaml-buffer "let a = 1\n\nlet b = 2\n\nlet c = 3\n"
      (goto-char (point-max))
      (beginning-of-defun 2)
      (expect (looking-at "let b") :to-be-truthy)))

  (it "skips nested let-in expressions"
    (with-neocaml-buffer "let outer =\n  let inner = 1 in\n  inner + 1\n\nlet next = 42\n"
      (search-forward "next")
      (beginning-of-defun)
      (expect (looking-at "let next") :to-be-truthy)
      (beginning-of-defun)
      (expect (looking-at "let outer") :to-be-truthy))))

(describe "navigation: end-of-defun"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "moves to the end of the current let binding"
    (with-neocaml-buffer "let x = 1\n\nlet y = 2\n"
      (end-of-defun)
      (expect (looking-at "\n\\(let y\\|\n\\)") :to-be-truthy)))

  (it "moves to the end of a multi-line definition"
    (with-neocaml-buffer "let area = function\n  | Circle r -> r\n  | Rect h -> h\n\nlet next = 1\n"
      (end-of-defun)
      ;; Should be past the function definition
      (expect (>= (point) (save-excursion (goto-char (point-min)) (search-forward "Rect h -> h") (line-end-position)))
              :to-be-truthy))))

;;;; forward-sexp

(describe "navigation: forward-sexp"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "moves over a parenthesized expression"
    (with-neocaml-buffer "let x = (1 + 2)\n"
      (search-forward "= ")
      (let ((start (point)))
        (forward-sexp)
        ;; Should move past the closing paren
        (expect (char-before) :to-equal ?\))
        (expect (> (point) start) :to-be-truthy))))

  (it "moves over an identifier"
    (with-neocaml-buffer "let foo = bar\n"
      (search-forward "= ")
      (forward-sexp)
      (expect (looking-back "bar" (line-beginning-position)) :to-be-truthy)))

  (it "moves over a string"
    (with-neocaml-buffer "let x = \"hello\"\n"
      (search-forward "= ")
      (forward-sexp)
      (expect (looking-back "\"hello\"" (line-beginning-position)) :to-be-truthy))))


;;;; defun-name

(describe "navigation: defun-name"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "returns the name of a let binding"
    (with-neocaml-buffer "let factorial n = n\n"
      (search-forward "factorial")
      (let ((node (treesit-node-at (1- (point)))))
        ;; Navigate up to the let_binding node
        (while (and node (not (string= (treesit-node-type node) "let_binding")))
          (setq node (treesit-node-parent node)))
        (expect (neocaml--defun-name node) :to-equal "factorial"))))

  (it "returns the name of a type binding"
    (with-neocaml-buffer "type shape = Circle | Rect\n"
      (search-forward "shape")
      (let ((node (treesit-node-at (1- (point)))))
        (while (and node (not (string= (treesit-node-type node) "type_binding")))
          (setq node (treesit-node-parent node)))
        (expect (neocaml--defun-name node) :to-equal "shape"))))

  (it "returns the name of a module binding"
    (with-neocaml-buffer "module Foo = struct end\n"
      (search-forward "Foo")
      (let ((node (treesit-node-at (1- (point)))))
        (while (and node (not (string= (treesit-node-type node) "module_binding")))
          (setq node (treesit-node-parent node)))
        (expect (neocaml--defun-name node) :to-equal "Foo"))))

  (it "returns the name of an exception definition"
    (with-neocaml-buffer "exception Not_found\n"
      (search-forward "Not_found")
      (let ((node (treesit-node-at (1- (point)))))
        (while (and node (not (string= (treesit-node-type node) "exception_definition")))
          (setq node (treesit-node-parent node)))
        (expect (neocaml--defun-name node) :to-equal "Not_found"))))

  (it "returns the name of a value specification"
    (with-neocaml-interface-buffer "val area : float -> float\n"
      (search-forward "area")
      (let ((node (treesit-node-at (1- (point)))))
        (while (and node (not (string= (treesit-node-type node) "value_specification")))
          (setq node (treesit-node-parent node)))
        (expect (neocaml--defun-name node) :to-equal "area")))))

;;;; sentence navigation (Emacs 30+ only)

(describe "navigation: sentence (Emacs 30+)"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
    (unless (boundp 'treesit-thing-settings)
      (signal 'buttercup-pending "treesit-thing-settings not available (requires Emacs 30+)")))

  (it "forward-sentence moves to the next top-level definition"
    (with-neocaml-buffer "let x = 1\n\nlet y = 2\n\nlet z = 3\n"
      (forward-sentence)
      ;; After forward-sentence from point-min, we should be past the first definition
      (let ((pos (point)))
        (forward-sentence)
        (expect (> (point) pos) :to-be-truthy))))

  (it "backward-sentence moves to the previous top-level definition"
    (with-neocaml-buffer "let x = 1\n\nlet y = 2\n\nlet z = 3\n"
      (goto-char (point-max))
      (backward-sentence)
      (expect (looking-at "let z") :to-be-truthy)
      (backward-sentence)
      (expect (looking-at "let y") :to-be-truthy)))

  (it "navigates sentences between different definition kinds"
    (with-neocaml-buffer "type t = int\n\nlet x = 1\n\nexception Foo\n"
      (goto-char (point-max))
      (backward-sentence)
      (expect (looking-at "exception") :to-be-truthy)
      (backward-sentence)
      (expect (looking-at "let x") :to-be-truthy)
      (backward-sentence)
      (expect (looking-at "type t") :to-be-truthy))))

;;;; Interface mode navigation

(describe "navigation: interface mode"
  (before-all
    (unless (treesit-language-available-p 'ocaml-interface)
      (signal 'buttercup-pending "tree-sitter OCaml interface grammar not available")))

  (it "beginning-of-defun works in interface mode"
    (with-neocaml-interface-buffer "val x : int\n\nval y : string\n"
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "val y") :to-be-truthy)))

  (it "end-of-defun works in interface mode"
    (with-neocaml-interface-buffer "val x : int\n\nval y : string\n"
      (end-of-defun)
      ;; Should be past "val x : int"
      (expect (>= (point) (save-excursion
                             (goto-char (point-min))
                             (search-forward "int")
                             (point)))
              :to-be-truthy)))

  (it "forward-sexp works in interface mode"
    (with-neocaml-interface-buffer "val x : int -> string\n"
      (search-forward ": ")
      (forward-sexp)
      ;; Should move over some part of the type expression
      (expect (> (point) (save-excursion
                           (goto-char (point-min))
                           (search-forward ": ")
                           (point)))
              :to-be-truthy))))

;;;; list navigation (Emacs 30+ only)

(describe "navigation: list (Emacs 30+)"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
    (unless (boundp 'treesit-thing-settings)
      (signal 'buttercup-pending "treesit-thing-settings not available (requires Emacs 30+)")))

  (it "forward-list moves over a parenthesized expression"
    (with-neocaml-buffer "let x = (1 + 2) + 3\n"
      (search-forward "= ")
      (forward-list)
      (expect (char-before) :to-equal ?\))))

  (it "forward-list moves over a list expression"
    (with-neocaml-buffer "let x = [1; 2; 3]\n"
      (search-forward "= ")
      (forward-list)
      (expect (char-before) :to-equal ?\])))

  (it "forward-list moves over a record expression"
    (with-neocaml-buffer "let x = {a = 1; b = 2}\n"
      (search-forward "= ")
      (forward-list)
      (expect (char-before) :to-equal ?})))

  (it "forward-list moves over an array expression"
    (with-neocaml-buffer "let x = [|1; 2; 3|]\n"
      (search-forward "= ")
      (forward-list)
      (expect (looking-back "\\|\\]" (- (point) 2)) :to-be-truthy)))

  (it "up-list moves out of a parenthesized expression"
    (with-neocaml-buffer "let x = (1 + 2)\n"
      (search-forward "1 ")
      (up-list)
      (expect (char-before) :to-equal ?\))))

  (it "down-list moves into a parenthesized expression"
    (with-neocaml-buffer "let x = (1 + 2)\n"
      (search-forward "= ")
      (down-list)
      (expect (char-before) :to-equal ?\()))

  (it "forward-list moves over a polymorphic variant type"
    (with-neocaml-buffer "type t = [ `Foo | `Bar ]\n"
      (search-forward "= ")
      (forward-list)
      (expect (char-before) :to-equal ?\])))

  (it "forward-list moves over a package type"
    (with-neocaml-buffer "type t = (module S)\n"
      (search-forward "= ")
      (forward-list)
      (expect (char-before) :to-equal ?\))))

  (it "delete-pair removes matching parentheses"
    (with-neocaml-buffer "let x = (1 + 2)\n"
      (search-forward "= ")
      (delete-pair)
      (expect (buffer-string) :to-equal "let x = 1 + 2\n")))

  (it "delete-pair removes correct parens in nested code"
    (with-neocaml-buffer "let _ =\n  (let world = \"world\" in\n   Printf.printf \"Hello %s\\n\" world);\n  ()\n"
      (search-forward "  (let")
      (backward-char 4) ;; point on the opening paren before "let"
      (delete-pair)
      ;; The semicolon paren should still be intact, and () at the end too
      (expect (buffer-string) :to-match "let world")
      (expect (buffer-string) :to-match "()"))))

;;;; which-func / add-log integration

(describe "navigation: which-func"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "returns the current defun name via add-log-current-defun"
    (with-neocaml-buffer "let foo x = x + 1\n\nlet bar y = y * 2\n"
      (search-forward "x + ")
      (expect (add-log-current-defun) :to-equal "foo")))

  (it "returns the current type name via add-log-current-defun"
    (with-neocaml-buffer "type color = Red | Green | Blue\n"
      (search-forward "Green")
      (expect (add-log-current-defun) :to-equal "color")))

  (it "returns nil outside any definition"
    (with-neocaml-buffer "let x = 1\n\n\n"
      (goto-char (point-max))
      (expect (add-log-current-defun) :to-be nil))))

;;;; outline integration (Emacs 30+ only)

(describe "navigation: outline (Emacs 30+)"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
    (unless (boundp 'treesit-outline-predicate)
      (signal 'buttercup-pending "treesit-outline-predicate not available (requires Emacs 30+)")))

  (it "sets treesit-outline-predicate"
    (with-neocaml-buffer "let x = 1\n"
      (expect treesit-outline-predicate :not :to-be nil)))

  (it "outline-next-heading moves to the next definition"
    (with-neocaml-buffer "let x = 1\n\nlet y = 2\n\ntype t = int\n"
      (outline-minor-mode 1)
      (outline-next-heading)
      (expect (looking-at "let y") :to-be-truthy)
      (outline-next-heading)
      (expect (looking-at "type t") :to-be-truthy))))

;;; neocaml-navigation-test.el ends here
