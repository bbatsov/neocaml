# Installation

## MELPA

`neocaml` is available on [MELPA](https://melpa.org/#/neocaml). If you have
MELPA in your `package-archives`, install it with:

    M-x package-install <RET> neocaml <RET>

Or with `use-package`:

```emacs-lisp
(use-package neocaml
  :ensure t
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))
```

## From GitHub

You can install directly from the repository:

    M-x package-vc-install <RET> https://github.com/bbatsov/neocaml <RET>

Or with `use-package` on Emacs 30+:

```emacs-lisp
(use-package neocaml
  :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest)
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))
```

!!! note
    If the required tree-sitter grammars are not installed, run
    `M-x neocaml-install-grammars` to install them.

!!! tip
    If you have another OCaml major mode installed (e.g. `tuareg` or `caml-mode`),
    consider removing it to avoid conflicts over `.ml` and `.mli` file
    associations. See [Migrating from tuareg / caml-mode](migration.md)
    for details.
