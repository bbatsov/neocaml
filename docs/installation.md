# Installation

## MELPA

`neocaml` is available on [MELPA](https://melpa.org/#/neocaml). If you have
MELPA in your `package-archives`, install it with:

    M-x package-install <RET> neocaml <RET>

Or with `use-package`:

```emacs-lisp
(use-package neocaml
  :ensure t)
```

## From GitHub

You can install directly from the repository:

    M-x package-vc-install <RET> https://github.com/bbatsov/neocaml <RET>

Or with `use-package` on Emacs 30+:

```emacs-lisp
(use-package neocaml
  :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest))
```

!!! note
    If the required tree-sitter grammars are not installed, run
    `M-x neocaml-install-grammars` to install the OCaml and
    OCaml-interface grammars. If grammar installation fails, see
    [Troubleshooting](troubleshooting.md#tree-sitter-abi-version-mismatch).

    The modes for dune, opam, OCamllex, and Menhir files each use
    their own grammar. These are installed automatically when you
    first open a file of that type, or you can install them manually
    with `M-x neocaml-dune-install-grammar`,
    `M-x neocaml-opam-install-grammar`,
    `M-x neocaml-ocamllex-install-grammar`, and
    `M-x neocaml-menhir-install-grammar`.

!!! tip
    If you have another OCaml major mode installed (e.g. `tuareg` or `caml-mode`),
    consider removing it to avoid conflicts over `.ml` and `.mli` file
    associations. See [Migrating from tuareg / caml-mode](migration.md)
    for details.

## What's Next?

Head to [Getting Started](usage.md) to see what works out of the box,
set up Eglot, and learn the basic workflow. Then check
[Configuration](configuration.md) to customize font-lock levels,
indentation, and more.
