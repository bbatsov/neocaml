# neocaml

[![MELPA](https://melpa.org/packages/neocaml-badge.svg)](https://melpa.org/#/neocaml)
[![MELPA Stable](https://stable.melpa.org/packages/neocaml-badge.svg)](https://stable.melpa.org/#/neocaml)
[![CI](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml)

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

## Why?

Because `caml-mode` is ancient, and `tuareg`, while very powerful, has
grown complex over the years. The time seems ripe for a modern, leaner,
tree-sitter-powered mode for OCaml.

There have been two earlier attempts at tree-sitter OCaml modes for Emacs:

- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- [ocaml-ts-mode](https://github.com/terrateamio/ocaml-ts-mode)

Both provided useful early exploration of what a tree-sitter OCaml mode could
look like, and helped inspire this project. neocaml aims to take the idea
further with a more complete feature set and active maintenance.

They say that third time's the charm, right?

One last thing - we really need more Emacs packages with fun names! :D

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
- `_build` directory awareness (offers to switch to source when opening build artifacts, configurable via `neocaml-redirect-build-files`)
- Eglot integration (with [ocaml-eglot](https://github.com/tarides/ocaml-eglot) support)
- Debugging via [dape](https://github.com/svaante/dape) + [ocamlearlybird](https://github.com/hackwaly/ocamlearlybird) (bytecode)
- Prettify-symbols for common OCaml operators

## Comparison with Other Modes

| Feature                    | neocaml                    | caml-mode     | tuareg       |
|----------------------------|----------------------------|---------------|--------------|
| Required Emacs version     | 29.1+ (30+ recommended)    | 24+           | 26+          |
| Font-lock                  | Tree-sitter (4 levels)     | Regex         | Regex        |
| Indentation                | Tree-sitter + cycle-indent | Custom engine | SMIE         |
| REPL integration           | Yes                        | Yes           | Yes          |
| Navigation (defun, sexp)   | Yes                        | Yes           | Yes          |
| Imenu                      | Yes (.ml and .mli)         | Yes           | Yes          |
| .ml/.mli toggle            | Yes                        | Yes           | Yes          |
| LSP (Eglot) integration    | Yes (auto-configured)      | Yes (manual)  | Yes (manual) |
| Debugger                   | Yes (dape + ocamlearlybird) | Yes (ocamldebug) | Yes (ocamldebug) |
| Compilation commands       | Error regexp + C-c C-c     | Yes           | Yes          |
| `_build` directory aware   | Yes                        | No            | Yes          |
| opam file support          | Yes                        | No            | Yes          |
| dune file support          | Yes                        | No            | No           |
| OCamllex support           | Yes (with OCaml injection) | No            | Yes          |
| Menhir support             | Yes (with OCaml injection) | No            | Yes          |
| Cram test support          | Yes                        | No            | No           |
| Code templates / skeletons | No                         | Yes           | Yes          |

Keep in mind also that `tuareg` uses `caml-mode` internally for some functionality.
I think both modes will probably be folded into one down the road.

### The impact of LSP on major modes

Historically, caml-mode and tuareg bundled features like type display,
completion, jump-to-definition, error checking, and refactoring support
-- all driven by Merlin.  Today, `ocamllsp` provides all of these through
the standard LSP protocol, and Eglot (built into Emacs 29+) acts as the
client.  There is no reason for a major mode to reimplement any of this.

For OCaml-specific LSP extensions that go beyond the standard protocol
-- type enclosing, case analysis (destruct), hole navigation --
[ocaml-eglot](https://github.com/tarides/ocaml-eglot) is the recommended
companion package.

### Why Tree-sitter?

Tree-sitter provides incremental, error-tolerant parsing that is
significantly faster and more accurate than regex-based font-lock and
SMIE-based indentation.  It parses the full syntax tree, so fontification
and indentation rules can reference actual language constructs rather
than fragile regular expressions.  This results in fewer edge-case bugs
and simpler, more maintainable code.

## Funding

While neocaml is free software and will always be, the project would benefit
from some funding. Consider supporting its ongoing development if you find it
useful.

You can support the development of neocaml via:

- [GitHub Sponsors](https://github.com/sponsors/bbatsov)
- [Patreon](https://www.patreon.com/bbatsov)
- [PayPal](https://www.paypal.me/bbatsov)

## License

Copyright (c) 2025-2026 Bozhidar Batsov and [contributors](https://github.com/bbatsov/neocaml/graphs/contributors).

Distributed under the GNU General Public License, version 3 or later. See [LICENSE](https://github.com/bbatsov/neocaml/blob/main/LICENSE) for details.
