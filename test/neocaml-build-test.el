;;; neocaml-build-test.el --- Tests for _build directory resolution -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml--resolve-build-path'.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "neocaml--resolve-build-path"
  (it "resolves dune-style _build/default/lib/foo.ml"
    (spy-on 'file-readable-p :and-call-fake
            (lambda (f) (string= f "/project/lib/foo.ml")))
    (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
            :to-equal "/project/lib/foo.ml"))

  (it "resolves ocamlbuild-style _build/lib/foo.ml"
    (spy-on 'file-readable-p :and-call-fake
            (lambda (f) (string= f "/project/lib/foo.ml")))
    (expect (neocaml--resolve-build-path "/project/_build/lib/foo.ml")
            :to-equal "/project/lib/foo.ml"))

  (it "returns nil for non-_build paths"
    (expect (neocaml--resolve-build-path "/project/lib/foo.ml")
            :to-be nil))

  (it "returns nil when no source file exists"
    (spy-on 'file-readable-p :and-return-value nil)
    (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
            :to-be nil))

  (it "prefers dune-style over ocamlbuild-style when both exist"
    (spy-on 'file-readable-p :and-return-value t)
    (expect (neocaml--resolve-build-path "/project/_build/default/lib/foo.ml")
            :to-equal "/project/lib/foo.ml")))

(describe "neocaml-redirect-build-files"
  (it "defaults to t"
    (expect (default-value 'neocaml-redirect-build-files) :to-be t))

  (it "is declared safe for directory-local use"
    (expect (get 'neocaml-redirect-build-files 'safe-local-variable)
            :to-equal #'booleanp)))

(describe "neocaml--check-build-dir"
  (before-each
    (spy-on 'derived-mode-p :and-return-value t)
    (spy-on 'y-or-n-p :and-return-value nil))

  (it "prompts to switch when visiting a _build/ file"
    (spy-on 'buffer-file-name :and-return-value "/project/_build/default/lib/foo.ml")
    (spy-on 'file-readable-p :and-call-fake
            (lambda (f) (string= f "/project/lib/foo.ml")))
    (let ((neocaml-redirect-build-files t))
      (neocaml--check-build-dir)
      (expect 'y-or-n-p :to-have-been-called)))

  (it "does not prompt when neocaml-redirect-build-files is nil"
    (spy-on 'buffer-file-name :and-return-value "/project/_build/default/lib/foo.ml")
    (let ((neocaml-redirect-build-files nil))
      (neocaml--check-build-dir)
      (expect 'y-or-n-p :not :to-have-been-called)))

  (it "does not prompt for non-_build/ files"
    (spy-on 'buffer-file-name :and-return-value "/project/lib/foo.ml")
    (let ((neocaml-redirect-build-files t))
      (neocaml--check-build-dir)
      (expect 'y-or-n-p :not :to-have-been-called))))

;;; neocaml-build-test.el ends here
