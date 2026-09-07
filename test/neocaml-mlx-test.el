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
  "Return the `tsx' parser active in the current buffer, or nil."
  (car (cl-remove-if-not
        (lambda (p) (eq (treesit-parser-language p) 'tsx))
        (treesit-parser-list))))

(defun neocaml-mlx-test--real-ranges ()
  "Return the non-degenerate included ranges of the current tsx parser."
  (when-let* ((parser (neocaml-mlx-test--tsx-parser))
              (ranges (treesit-parser-included-ranges parser)))
    (cl-remove-if (lambda (r) (= (car r) (cdr r))) ranges)))

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

  (it "includes a closing delimiter at the recovered node boundary"
    (with-temp-buffer
      (insert "let make () = <div></div>\n;;")
      (let ((node-start (point-min)))
        (search-backward ">")
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

    (it "injects a tsx range covering the JSX element"
      (with-neocaml-test-buffer neocaml-mlx-mode neocaml-mlx-test--react-component
        (treesit-update-ranges)
        (let ((ranges (neocaml-mlx-test--real-ranges)))
          (expect ranges :to-be-truthy)
          (expect (length ranges) :to-equal 1)
          (let* ((r (car ranges))
                 (text (buffer-substring (car r) (cdr r))))
            (expect (string-prefix-p "<div>" text) :to-be t)
            (expect (string-suffix-p "</div>" text) :to-be t)
            (expect (string-search "<h1>" text) :to-be-truthy)
            (expect (string-search "</h1>" text) :to-be-truthy)))))

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
          (let* ((range (car (neocaml-mlx-test--real-ranges)))
                 (text (buffer-substring (car range) (cdr range))))
            (expect text :to-equal
                    (concat "<div>\n"
                            "      <h1> (React.string \"Hello, React.ml!\") "
                            "</h1>\n"
                            "    </div>"))))))

    (it "does not inject into bindings without JSX"
      (with-neocaml-test-buffer neocaml-mlx-mode
          "let[@react.component] make () = print_endline \"plain\""
        (treesit-update-ranges)
        (expect (neocaml-mlx-test--real-ranges) :to-equal nil)))

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
                :to-have-face 'typescript-ts-jsx-tag-face)))))

(provide 'neocaml-mlx-test)

;;; neocaml-mlx-test.el ends here
