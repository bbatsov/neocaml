;;; neocaml-mlx-test.el --- Tests for neocaml-mlx-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-mlx-mode': auto-mode association, mode
;; derivation, and `tsx' language injection for embedded JSX.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neocaml)
(require 'neocaml-mlx)
(require 'neocaml-test-helpers)

(defconst neocaml-mlx-test--react-component
  "\
module App = struct

  let[@react.component] make () =
    <div>
      <h1> (React.string \"Hello, React.ml!\") </h1>
    </div>
  ;;
end"
  "A small OCaml/JSX component used across the specs.")

(defun neocaml-mlx-test--tsx-parser ()
  "Return the first `tsx' parser in the current buffer, or nil."
  (car (neocaml-mlx--tsx-parsers)))

(defun neocaml-mlx-test--tsx-ranges ()
  "Return the included ranges of the first tsx parser."
  (when-let* ((parser (neocaml-mlx-test--tsx-parser)))
    (treesit-parser-included-ranges parser)))

(defun neocaml-mlx-test--jsx-text ()
  "Return the text from the start to the end of the tsx parser's ranges."
  (when-let* ((ranges (neocaml-mlx-test--tsx-ranges)))
    (buffer-substring-no-properties (caar ranges) (cdar (last ranges)))))

(defun neocaml-mlx-test--top-level-types ()
  "Return the types of the top-level nodes of the OCaml parse."
  (mapcar #'treesit-node-type
          (treesit-node-children (treesit-buffer-root-node 'ocaml) t)))

(describe "neocaml-mlx-mode"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not installed")))

  (it "is associated with .mlx files"
    (expect (assoc "\\.mlx\\'" auto-mode-alist)
            :to-equal '("\\.mlx\\'" . neocaml-mlx-mode)))

  (it "derives from `neocaml-base-mode'"
    (with-temp-buffer
      (neocaml-mlx-mode)
      (expect (provided-mode-derived-p major-mode 'neocaml-base-mode)
              :to-be-truthy)
      (expect (provided-mode-derived-p major-mode 'prog-mode)
              :to-be-truthy)))

  (it "indents OCaml in the indentation style of neocaml-mode"
    (let ((neocaml-indent-style 'conventional)
          (code "let f x =\n  match x with\n  | A ->\n      x\n  | B -> 0"))
      (expect (neocaml-test--reindent #'neocaml-mlx-mode code) :to-equal code)))

  (it "parses and indents the code after JSX elements"
    (let ((code "module App = struct
  let[@react.component] make () = <div>(React.string \"a\")</div>
end

let[@react.component] other () = <span />

let after = 1"))
      (expect (neocaml-test--reindent #'neocaml-mlx-mode code) :to-equal code)
      (with-temp-buffer
        (insert code)
        (neocaml-mlx-mode)
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--top-level-types)
                :to-equal '("module_definition" "value_definition"
                            "value_definition")))))

  (it "keeps the OCaml parse right after edits, before the ranges are updated"
    (with-temp-buffer
      (insert "let[@react.component] a () = <p>x</p>\n\nlet b = 1\n")
      (neocaml-mlx-mode)
      (treesit-update-ranges)
      (goto-char (point-min))
      (search-forward "<p>")
      (insert "abc")
      (expect (neocaml-mlx-test--top-level-types)
              :to-equal '("value_definition" "value_definition"))))

  (it "includes the JSX around the end of a recovered node"
    (with-temp-buffer
      (insert "let make () = <div></div>\n;;")
      (let ((node-start (point-min)))
        ;; The OCaml parser doesn't see the closing tag, and the node
        ;; ends before the `>' of the opening one.
        (search-backward "></")
        (let ((node-end (point)))
          (cl-letf (((symbol-function 'treesit-node-start)
                     (lambda (_node) node-start))
                    ((symbol-function 'treesit-node-end)
                     (lambda (_node) node-end)))
            (let* ((range (car (neocaml-mlx--jsx-range 'node nil)))
                   (text (buffer-substring (car range) (cdr range))))
              (expect text :to-equal "<div></div>")))))))

  (describe "when the tsx grammar is available"
    (before-all
      (unless (neocaml-mlx--injection-available-p)
        (signal 'buttercup-pending
                "tsx tree-sitter grammar or Emacs 30+ not available")))

    (it "configures `treesit-range-settings'"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect treesit-range-settings :to-be-truthy)))

    (it "enables the `jsx' font-lock feature"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect (memq 'jsx (mapcan #'identity treesit-font-lock-feature-list))
                :to-be-truthy)))

    (it "preserves OCaml levels and enables JSX at level 3"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (expect (memq 'escape-sequence
                      (nth 2 treesit-font-lock-feature-list))
                :to-be-truthy)
        (expect (memq 'property
                      (nth 3 treesit-font-lock-feature-list))
                :to-be-truthy)
        (expect (memq 'jsx
                      (nth 2 treesit-font-lock-feature-list))
                :to-be-truthy)))

    (it "injects tsx ranges covering the JSX element"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (treesit-update-ranges)
        (let ((text (neocaml-mlx-test--jsx-text)))
          (expect (string-prefix-p "<div>" text) :to-be t)
          (expect (string-suffix-p "</div>" text) :to-be t)
          (expect (string-search "<h1>" text) :to-be-truthy)
          (expect (string-search "</h1>" text) :to-be-truthy))))

    (it "takes `<>' for a fragment, but not the operator"
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] make () = <><p>x</p></>"
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--jsx-text) :to-equal "<><p>x</p></>"))
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] make ~a ~b () = if a <> b then <p /> else <br />"
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--jsx-text) :to-equal "<p /> else <br />")))

    (it "computes injection ranges from the widened buffer"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (goto-char (point-min))
        (search-forward "<h1>")
        (let ((narrow-start (match-beginning 0))
              (narrow-end (match-end 0)))
          (narrow-to-region narrow-start narrow-end)
          (treesit-update-ranges)
          (expect (cons (point-min) (point-max))
                  :to-equal (cons narrow-start narrow-end))
          (widen)
          (expect (neocaml-mlx-test--jsx-text) :to-equal
                  (concat "<div>\n"
                          "      <h1> (React.string \"Hello, React.ml!\") "
                          "</h1>\n"
                          "    </div>")))))

    (it "does not inject into bindings without JSX"
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] make () = print_endline \"plain\""
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--tsx-ranges) :to-equal nil)))

    (it "fontifies JSX tag names with `typescript-ts-jsx-tag-face'"
      (with-temp-buffer
        (insert neocaml-mlx-test--react-component)
        (let ((treesit-font-lock-level 3))
          (neocaml-mlx-mode))
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "<div>")
        ;; The tag name sits between the delimiters.
        (expect (1+ (match-beginning 0))
                :to-have-face 'typescript-ts-jsx-tag-face)))

    (it "fontifies the JSX of every component"
      (neocaml-test--check-face-specs
       #'neocaml-mlx-mode
       "let[@react.component] a () = <div>(React.string \"a\")</div>

let[@react.component] b () = <span>(React.string \"b\")</span>"
       '(("div" typescript-ts-jsx-tag-face)
         ("span" typescript-ts-jsx-tag-face))))

    (it "parses the JSX of each component on its own"
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] a () = <div />\n\nlet[@react.component] b () = <p>x</p>"
        (treesit-update-ranges)
        (let ((parsers (neocaml-mlx--tsx-parsers)))
          (expect (length parsers) :to-equal 2)
          (dolist (parser parsers)
            (expect (treesit-node-check (treesit-parser-root-node parser) 'has-error)
                    :not :to-be-truthy)))))))

(provide 'neocaml-mlx-test)

;;; neocaml-mlx-test.el ends here
