;;; neocaml-grammar-test.el --- Tests for grammar installation -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml-install-grammars' and the grammar-missing prompt
;; in `neocaml--setup-mode'.  These use mocks to avoid actually
;; downloading grammars.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "neocaml-install-grammars"
  (it "skips installation when all grammars are available"
    (spy-on 'treesit-language-available-p :and-return-value t)
    (spy-on 'treesit-install-language-grammar)
    (neocaml-install-grammars)
    (expect 'treesit-install-language-grammar :not :to-have-been-called))

  (it "installs only missing grammars"
    (spy-on 'treesit-language-available-p :and-call-fake
            (lambda (grammar &optional _)
              (eq grammar 'ocaml)))
    (spy-on 'treesit-install-language-grammar)
    (neocaml-install-grammars)
    (expect 'treesit-install-language-grammar :to-have-been-called-times 1)
    (expect 'treesit-install-language-grammar
            :to-have-been-called-with 'ocaml-interface))

  (it "installs all grammars when none are available"
    (spy-on 'treesit-language-available-p :and-return-value nil)
    (spy-on 'treesit-install-language-grammar)
    (neocaml-install-grammars)
    (expect 'treesit-install-language-grammar :to-have-been-called-times 2))

  (it "binds treesit-language-source-alist to neocaml-grammar-recipes"
    (let (captured-alist)
      (spy-on 'treesit-language-available-p :and-return-value nil)
      (spy-on 'treesit-install-language-grammar :and-call-fake
              (lambda (_grammar)
                (setq captured-alist treesit-language-source-alist)))
      (neocaml-install-grammars)
      (expect captured-alist :to-equal neocaml-grammar-recipes))))

(describe "neocaml--setup-mode grammar prompt"
  (it "prompts the user when grammars are missing"
    (spy-on 'treesit-language-available-p :and-return-value nil)
    (spy-on 'y-or-n-p :and-return-value nil)
    (spy-on 'treesit-ready-p :and-return-value nil)
    (neocaml--setup-mode 'ocaml)
    (expect 'y-or-n-p :to-have-been-called))

  (it "calls neocaml-install-grammars when user accepts the prompt"
    (spy-on 'treesit-language-available-p :and-return-value nil)
    (spy-on 'y-or-n-p :and-return-value t)
    (spy-on 'neocaml-install-grammars)
    (spy-on 'treesit-ready-p :and-return-value nil)
    (neocaml--setup-mode 'ocaml)
    (expect 'neocaml-install-grammars :to-have-been-called))

  (it "does not install when user declines the prompt"
    (spy-on 'treesit-language-available-p :and-return-value nil)
    (spy-on 'y-or-n-p :and-return-value nil)
    (spy-on 'neocaml-install-grammars)
    (spy-on 'treesit-ready-p :and-return-value nil)
    (neocaml--setup-mode 'ocaml)
    (expect 'neocaml-install-grammars :not :to-have-been-called))

  (it "does not prompt when grammars are already installed"
    (spy-on 'treesit-language-available-p :and-return-value t)
    (spy-on 'y-or-n-p)
    (spy-on 'treesit-ready-p :and-return-value nil)
    (neocaml--setup-mode 'ocaml)
    (expect 'y-or-n-p :not :to-have-been-called)))

;;; neocaml-grammar-test.el ends here
