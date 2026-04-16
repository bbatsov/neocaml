# Getting Started

The `neocaml` package bundles two major modes - one for OCaml code
and one for OCaml interfaces (`.mli`). Both modes will be auto-enabled
when you open the respective type of files.

You can use `C-c C-a` to toggle between implementation and interface files.

neocaml auto-registers both modes with Eglot, so `M-x eglot` will
start `ocamllsp` with the correct language IDs automatically.

!!! note
    neocaml sets the `eglot-language-id` symbol property on both modes
    (`"ocaml"` for `.ml` and `"ocaml.interface"` for `.mli`), so the correct
    language IDs are sent to the server automatically.

## ocaml-eglot

[ocaml-eglot](https://github.com/tarides/ocaml-eglot) is a lightweight minor
mode that enhances the Eglot experience for OCaml by exposing custom LSP
requests from `ocamllsp` -- type enclosing, case analysis, hole navigation, and
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
