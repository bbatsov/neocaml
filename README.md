# neocaml

[![MELPA](https://melpa.org/packages/neocaml-badge.svg)](https://melpa.org/#/neocaml)
[![MELPA Stable](https://stable.melpa.org/packages/neocaml-badge.svg)](https://stable.melpa.org/#/neocaml)
[![CI](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml)
[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=github)](https://github.com/sponsors/bbatsov)

`neocaml` is a **n**ew **E**macs package for programming in
[OCaml](https://ocaml.org).  Built on
[Tree-sitter](https://tree-sitter.github.io/tree-sitter/), it provides major
modes for editing OCaml (`.ml`) and OCaml Interface (`.mli`) files with
font-locking, indentation, navigation, and toplevel (REPL) integration.

Beyond OCaml source code, neocaml also supports key parts of the OCaml
ecosystem: [dune](https://dune.build) build files,
[opam](https://opam.ocaml.org) package definitions,
[OCamllex](https://v2.ocaml.org/manual/lexyacc.html) lexer definitions (`.mll`),
[Menhir](http://gallium.inria.fr/~fpottier/menhir/) parser definitions (`.mly`),
and [cram](https://dune.readthedocs.io/en/stable/tests.html#cram-tests) test
files (`.t`) -- each with its own tree-sitter-powered major mode.

It's also as cool as Neo from "The Matrix". ;-)

> [!TIP]
> For detailed configuration, usage guides, and migration help, see the
> [full documentation](https://bbatsov.github.io/neocaml).

## Features

- Tree-sitter based font-locking (4 levels) for `.ml` and `.mli` files
- Tree-sitter based indentation with cycle-indent support
- Navigation (`beginning-of-defun`, `end-of-defun`, `forward-sexp`, sentence movement with `M-a`/`M-e`)
- Imenu with language-specific categories for `.ml` and `.mli`
- Toggling between implementation and interface via `ff-find-other-file` (`C-c C-a`)
- OCaml toplevel (REPL) integration (`neocaml-repl`)
- Comment support: `fill-paragraph` (`M-q`), comment continuation (`M-j`), and `comment-dwim` (`M-;`)
- Electric indentation on delimiter characters
- opam file editing (`neocaml-opam-mode`) with font-lock, indentation, imenu, and `opam lint` integration (flymake and [flycheck](https://github.com/flycheck/flycheck))
- dune file editing (`neocaml-dune-mode`) for dune, dune-project, and dune-workspace files
- dune build commands (`neocaml-dune-interaction-mode`) -- build, test, clean, promote, fmt, exec (with watch mode via prefix arg)
- OCamllex file editing (`neocaml-ocamllex-mode`) with font-lock, indentation, imenu, and OCaml language injection (Emacs 30+)
- Menhir file editing (`neocaml-menhir-mode`) with font-lock, indentation, imenu, and OCaml language injection (Emacs 30+)
- Cram test file editing (`neocaml-cram-mode`) with font-lock for commands, output, modifiers, and prose
- Easy installation of `ocaml` and `ocaml-interface` tree-sitter grammars via `M-x neocaml-install-grammars`
- Compilation error regexp for `M-x compile` (errors, warnings, alerts, backtraces)
- `_build` directory awareness (offers to switch to source when opening build artifacts)
- Eglot integration (with [ocaml-eglot](https://github.com/tarides/ocaml-eglot) support)
- Debugging via [dape](https://github.com/svaante/dape) + [ocamlearlybird](https://github.com/hackwaly/ocamlearlybird) (bytecode)
- Prettify-symbols for common OCaml operators

## Quick Start

`neocaml` is available on [MELPA](https://melpa.org/#/neocaml):

```emacs-lisp
(use-package neocaml
  :ensure t
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))
```

If the required tree-sitter grammars are not installed, run
`M-x neocaml-install-grammars`.

See the [documentation](https://bbatsov.github.io/neocaml/installation/) for
alternative installation methods and detailed configuration.

## Funding

While neocaml is free software and will always be, the project would benefit
from some funding. Consider supporting its ongoing development if you find it
useful.

You can support the development of neocaml via:

- [GitHub Sponsors](https://github.com/sponsors/bbatsov)
- [Patreon](https://www.patreon.com/bbatsov)
- [PayPal](https://www.paypal.me/bbatsov)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, debugging, and
how to run tests. Architecture and design notes live in [doc/DESIGN.md](doc/DESIGN.md).

## License

Copyright (c) 2025-2026 Bozhidar Batsov and [contributors](https://github.com/bbatsov/neocaml/graphs/contributors).

Distributed under the GNU General Public License, version 3 or later. See [LICENSE](LICENSE) for details.
