;;; neocaml-build-test.el --- Tests for _build directory resolution -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml--resolve-build-path'.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "neocaml--resolve-build-path"
  (it "resolves dune-style _build/default/lib/foo.ml"
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (f) (string= f "/project/lib/foo.ml"))))
      (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
              :to-equal "/project/lib/foo.ml")))

  (it "resolves ocamlbuild-style _build/lib/foo.ml"
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (f) (string= f "/project/lib/foo.ml"))))
      (expect (neocaml--resolve-build-path "/project/_build/lib/foo.ml")
              :to-equal "/project/lib/foo.ml")))

  (it "returns nil for non-_build paths"
    (expect (neocaml--resolve-build-path "/project/lib/foo.ml")
            :to-be nil))

  (it "returns nil when no source file exists"
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_f) nil)))
      (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
              :to-be nil)))

  (it "prefers dune-style over ocamlbuild-style when both exist"
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_f) t)))
      (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
              :to-equal "/project/lib/foo.ml"))))

;;; neocaml-build-test.el ends here
