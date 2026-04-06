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

(describe "neocaml-redirect-build-files"
  (it "defaults to t"
    (expect (default-value 'neocaml-redirect-build-files) :to-be t))

  (it "is declared safe for directory-local use"
    (expect (get 'neocaml-redirect-build-files 'safe-local-variable)
            :to-equal #'booleanp)))

(describe "neocaml--check-build-dir"
  (it "prompts to switch when visiting a _build/ file"
    (let ((prompted nil))
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda () "/project/_build/default/lib/foo.ml"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'file-readable-p)
                 (lambda (f) (string= f "/project/lib/foo.ml")))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) nil)))
        (let ((neocaml-redirect-build-files t))
          (neocaml--check-build-dir)
          (expect prompted :to-be t)))))

  (it "does not prompt when neocaml-redirect-build-files is nil"
    (let ((prompted nil))
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda () "/project/_build/default/lib/foo.ml"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) nil)))
        (let ((neocaml-redirect-build-files nil))
          (neocaml--check-build-dir)
          (expect prompted :to-be nil)))))

  (it "does not prompt for non-_build/ files"
    (let ((prompted nil))
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda () "/project/lib/foo.ml"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) nil)))
        (let ((neocaml-redirect-build-files t))
          (neocaml--check-build-dir)
          (expect prompted :to-be nil))))))

;;; neocaml-build-test.el ends here
