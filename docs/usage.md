# Getting Started

## Prerequisites

neocaml provides the editing experience, but you'll want OCaml tooling
installed for building and LSP support:

- **OCaml** (via [opam](https://opam.ocaml.org/doc/Install.html), the OCaml package manager)
- **ocaml-lsp-server** for Eglot integration: `opam install ocaml-lsp-server`
- **dune** for building projects: `opam install dune`

!!! tip
    If you launch Emacs from a desktop shortcut (e.g. Emacs.app on macOS)
    rather than a terminal, your shell's `PATH` may not be inherited, so
    `ocamllsp` and other tools won't be found. See
    [Troubleshooting](troubleshooting.md#ocamllsp-not-found-macos-gui-emacs)
    for the fix.

## What works out of the box

Once neocaml is [installed](installation.md), the following works
automatically with no configuration:

- **Font-lock** (syntax highlighting) for `.ml` and `.mli` files,
  with 4 configurable levels
- **Indentation** via tree-sitter
- **Navigation** - `beginning-of-defun`, `end-of-defun`, `forward-sexp`,
  and more (see [Navigation](navigation.md))
- **Imenu** - jump to definitions with `M-x imenu`
- **File toggle** - switch between `.ml` and `.mli` with `C-c C-a`
- **Comment commands** - `M-;`, `M-j`, `M-q` all work with OCaml's
  `(* ... *)` comments
- **Eglot registration** - `M-x eglot` starts `ocamllsp` automatically
- **Compilation** - `C-c C-c` runs `compile` with OCaml error navigation
- **Additional file types** - opam, dune, OCamllex, Menhir, and cram
  files get their own modes automatically

## Features that need opt-in

Some features require a line or two of configuration to enable:

| Feature | How to enable |
|---|---|
| REPL integration | `(add-hook 'neocaml-base-mode-hook #'neocaml-repl-minor-mode)` - see [REPL](repl.md) |
| dune build commands | `(add-hook 'neocaml-base-mode-hook #'neocaml-dune-interaction-mode)` - see [dune](opam_and_dune.md#dune-commands) |
| Prettify symbols | `(add-hook 'neocaml-base-mode-hook #'prettify-symbols-mode)` - see [Configuration](configuration.md#prettify-symbols) |
| Code folding | `(add-hook 'neocaml-base-mode-hook #'outline-minor-mode)` (Emacs 30+) |
| opam lint | `(add-hook 'neocaml-opam-mode-hook #'flymake-mode)` - see [opam](opam_and_dune.md#opam-lint) |

All of these use Emacs hooks - a way to run code when a mode is
activated. You add them to your Emacs configuration (typically
`init.el`).

## Eglot (LSP)

neocaml auto-registers both modes with Eglot, so `M-x eglot` will
start `ocamllsp` with the correct language IDs automatically. To start
Eglot whenever you open an OCaml file:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'eglot-ensure)
```

!!! note
    neocaml sets the `eglot-language-id` symbol property on both modes
    (`"ocaml"` for `.ml` and `"ocaml.interface"` for `.mli`), so the correct
    language IDs are sent to the server automatically.

### ocaml-eglot

[ocaml-eglot](https://github.com/tarides/ocaml-eglot) is a lightweight
package that enhances Eglot for OCaml by exposing custom LSP requests
from `ocamllsp` - type enclosing, case analysis, hole navigation, and
more. It works with neocaml out of the box:

```emacs-lisp
(use-package ocaml-eglot
  :ensure t
  :hook
  (neocaml-base-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))
```

## Compilation

`C-c C-c` runs `M-x compile`, and neocaml registers an OCaml-specific error
regexp so that `next-error` (`M-g n`) and `previous-error` (`M-g p`) jump
directly to the source locations reported by the OCaml compiler, including
errors, warnings, alerts, and exception backtraces.

For a richer build workflow with dune (build, test, clean, fmt, watch mode),
see [dune commands](opam_and_dune.md#dune-commands).

## Other OCaml-related Files

neocaml also registers sensible modes for a few other OCaml-related files:

- `.ocamlinit` opens in `neocaml-mode` (it's OCaml toplevel startup code)
- `.ocamlformat` and `.ocp-indent` open in `conf-unix-mode` (key = value config files with `#` comments)
- `.eliom` / `.eliomi` (Ocsigen) are handled by `ff-find-other-file` for toggling between implementation and interface

## Useful Commands

| Command | Description |
|---|---|
| `M-x neocaml-version` | Show the installed neocaml version |
| `M-x neocaml-bug-report-info` | Copy debug info (versions, grammar status) to the kill ring |
| `M-x neocaml-report-bug` | Open the neocaml issue tracker in your browser |
| `M-x neocaml-browse-ocaml-docs` | Open the OCaml documentation in your browser |
| `M-x neocaml-cycle-indent-function` | Toggle between tree-sitter and `indent-relative` indentation |

These are also available from the OCaml menu.
