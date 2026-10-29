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
  and more (see [Code Navigation](navigation.md))
- **Imenu** - jump to definitions with `M-x imenu`
- **File toggle** - switch between `.ml` and `.mli` with `C-c C-a`
- **Comment commands** - `M-;`, `M-j`, `M-q` all work with OCaml's
  `(* ... *)` comments
- **Clickable comments** - URLs in comments are clickable, and issue
  references become clickable once `bug-reference-url-format` is set
  (see [Configuration](configuration.md#clickable-links-in-comments))
- **Eglot registration** - `M-x eglot` starts `ocamllsp` automatically
- **Compilation** - `C-c C-c` runs `compile` with OCaml error navigation
- **Additional file types** - opam, dune, OCamllex, Menhir, and cram
  files get their own modes automatically
- **dune & opam completion** - `completion-at-point` (`C-M-i`) offers
  stanza/field names and library/package names in dune and opam files
  (see [opam, dune & Cram](opam_and_dune.md#completion))

## Features that need opt-in

Some features require a line or two of configuration to enable:

| Feature | How to enable |
|---|---|
| REPL integration | `(add-hook 'neocaml-base-mode-hook #'neocaml-repl-minor-mode)` - see [REPL](repl.md) |
| dune build commands | `(add-hook 'neocaml-base-mode-hook #'neocaml-dune-interaction-mode)` - see [dune](opam_and_dune.md#dune-commands) |
| Prettify symbols | `(add-hook 'neocaml-base-mode-hook #'prettify-symbols-mode)` - see [Configuration](configuration.md#prettify-symbols) |
| Code folding | `(add-hook 'neocaml-base-mode-hook #'outline-minor-mode)` (Emacs 30+) |
| opam lint | `(add-hook 'neocaml-opam-mode-hook #'flymake-mode)` - see [opam](opam_and_dune.md#opam-lint) |
| Format on save | `(setq neocaml-format-on-save t)` - see [Configuration](configuration.md#formatting) |

All of these use Emacs hooks - a way to run code when a mode is
activated. You add them to your Emacs configuration (typically
`init.el`).

!!! note
    The examples use `neocaml-base-mode-hook` rather than
    `neocaml-mode-hook`. That's because `neocaml-base-mode` is the
    shared parent of both `neocaml-mode` (`.ml`) and
    `neocaml-interface-mode` (`.mli`), so hooks on it apply to both
    file types at once. Use `neocaml-mode-hook` or
    `neocaml-interface-mode-hook` if you want something only for one.

## Typical Workflow

A typical session looks something like this: open an OCaml file and
you get syntax highlighting and indentation right away. Start Eglot
with `M-x eglot` for completion, type information, and
jump-to-definition. Navigate between definitions with `C-M-a` /
`C-M-e`, jump to a specific one with `M-x imenu`, and switch between
`.ml` and `.mli` with `C-c C-a`. Run `C-c C-c` to compile and
`M-g n` to jump to the first error.

For interactive development, enable the REPL minor mode and press
`C-c C-z` to start a toplevel. Evaluate the current definition with
`C-c C-c` or a region with `C-c C-r`, and switch back to your source
with `C-c C-z` again.

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

### ocamllsp settings

`ocamllsp` takes its settings from Eglot's `eglot-workspace-configuration`.
A few useful ones are off by default:

- `inlayHints` - besides parameter names, show the inferred types of `let`
  bindings (`hintLetBindings`) and pattern variables
  (`hintPatternVariables`) inline.
- `syntaxDocumentation` - explain the syntax under point on hover, with a
  link to the relevant part of the manual.
- `merlinJumpCodeActions` - code actions that jump to the enclosing `fun`,
  `let`, `match`, module or module type, and between match cases.

Set them per project in `.dir-locals.el`:

```emacs-lisp
((nil . ((eglot-workspace-configuration
          . (:inlayHints (:hintLetBindings t :hintPatternVariables t)
             :syntaxDocumentation (:enable t)
             :merlinJumpCodeActions (:enable t))))))
```

The settings go at the top level, not under an `:ocamllsp` key. Inlay
hints are shown by `eglot-inlay-hints-mode`, which Eglot enables on its
own. ocamllsp can also show the type of each binding as a code lens, but
Eglot has no support for code lenses, so inlay hints are the closest
thing.

### Semantic highlighting

On Emacs 31, Eglot enables `eglot-semantic-tokens-mode` whenever the
server supports semantic tokens, and `ocamllsp` does. The tokens are
painted on top of neocaml's tree-sitter font-lock, so some faces change
once Eglot is running - module names, for instance, end up in
`font-lock-keyword-face`, because that's what Eglot maps the LSP
`namespace` token type to.

If you prefer neocaml's own highlighting, tell Eglot to ignore the
capability:

```emacs-lisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-ignored-server-capabilities :semanticTokensProvider))
```

Alternatively, keep semantic tokens and trim what they paint with
`eglot-semantic-token-types` and `eglot-semantic-token-modifiers`.

## Compilation

`C-c C-c` runs `M-x compile`, and neocaml registers an OCaml-specific error
regexp so that `next-error` (`M-g n`) and `previous-error` (`M-g p`) jump
directly to the source locations reported by the OCaml compiler, including
errors, warnings, alerts, and exception backtraces.

For a richer build workflow with dune (build, test, clean, fmt, watch mode),
see [dune commands](opam_and_dune.md#dune-commands).

In a dune project, `compile-command` defaults to `dune build`, so `C-c C-c`
and `M-x project-compile` build the project out of the box.

## Project Integration

neocaml teaches the built-in `project.el` that a directory containing a
`dune-project` file is a project root. That makes the project-wide commands
work for OCaml projects - `project-find-file` (`C-x p f`),
`project-find-regexp` (`C-x p g`), `project-compile` (`C-x p c`), and the
rest - rooted at the dune project, with `_build/` and `_opam/` ignored.

Version-control detection still takes precedence, so a project under git
roots at the repository as usual; the dune-project detection mainly helps
projects that aren't under version control.

## Other OCaml-related Files

neocaml also registers sensible modes for a few other OCaml-related files:

- `.ocamlinit` opens in `neocaml-mode` (it's OCaml toplevel startup code)
- `.ocamlformat` and `.ocp-indent` open in `conf-unix-mode` (key = value config files with `#` comments)
- `.eliom` / `.eliomi` (Ocsigen) are handled by `ff-find-other-file` for toggling between implementation and interface

## OxCaml

neocaml understands the syntax of [OxCaml](https://oxcaml.org/), Jane
Street's extensions to OCaml: modes and modalities (`x @ local`,
`@@ portable`), kinds (`type t : immediate`, `kind_` definitions, `mod`
bounds), the `stack_`, `exclave_`, `local_`, `global_` and `borrow_`
keywords, unboxed types and literals (`float#`, `#1.0`, `#(...)`,
`#{...}`), immutable arrays (`[: ... :]`) and comprehensions. They all
get font-lock and indentation, and kind definitions show up in imenu and
defun navigation. The extensions only appear in code that uses them, so
plain OCaml looks the same as before.

This needs tree-sitter-ocaml 0.26 or newer, which is what
`M-x neocaml-install-grammars` installs. If you installed the grammars
with an older neocaml, upgrade them with `C-u M-x neocaml-install-grammars`.

There's nothing to turn on in neocaml itself. The tooling comes from an
OxCaml switch, created as the
[OxCaml installation guide](https://oxcaml.org/get-oxcaml/) describes:

```shell
opam switch create 5.2.0+ox \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
eval $(opam env --switch 5.2.0+ox)
opam install ocaml-lsp-server merlin ocamlformat
```

With that switch on Emacs's `PATH`, `M-x eglot` starts its `ocamllsp`
and `M-x neocaml-format-buffer` runs its `ocamlformat`.

## Useful Commands

| Command | Description |
|---|---|
| `M-x neocaml-version` | Show the installed neocaml version |
| `M-x neocaml-bug-report-info` | Copy debug info (versions, grammar status) to the kill ring |
| `M-x neocaml-report-bug` | Open the neocaml issue tracker in your browser |
| `M-x neocaml-browse-ocaml-docs` | Open the OCaml documentation in your browser |
| `M-x neocaml-cycle-indent-function` | Toggle between tree-sitter and `indent-relative` indentation |
| `C-c C-f` (`neocaml-format-buffer`) | Format the buffer with `ocamlformat` (see [Configuration](configuration.md#formatting)) |

These are also available from the OCaml menu.

## Companion Packages

These packages work well with neocaml:

- [ocaml-eglot](https://github.com/tarides/ocaml-eglot) - OCaml-specific LSP extensions (type enclosing, case analysis, etc.)
- [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) - inherit shell PATH in GUI Emacs (needed on macOS)
- [expreg](https://github.com/casouri/expreg) - tree-sitter-aware expand-region (see [Configuration](configuration.md#structural-selection))
- [combobulate](https://github.com/mickeynp/combobulate) - structural editing and navigation (see [Configuration](configuration.md#structural-editing))
- [treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold) - tree-sitter-aware code folding
- [dape](https://github.com/svaante/dape) - debugging via Debug Adapter Protocol (see [Debugging](debugging.md))

## What's Next?

- [Code Navigation](navigation.md) - learn the structural navigation commands
- [Configuration](configuration.md) - customize font-lock levels, indentation, prettify-symbols, and more
- [REPL Integration](repl.md) - evaluate OCaml code interactively
- [opam, dune & Cram](opam_and_dune.md) - build commands, opam lint, dune formatting
