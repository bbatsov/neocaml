# OCamllex & Menhir Support

neocaml includes dedicated tree-sitter modes for OCamllex and Menhir
files. `neocaml-ocamllex-mode` activates automatically for `.mll`
files; `neocaml-menhir-mode` activates for `.mly` files. Both provide
font-lock, indentation, and imenu.

## Grammar Installation

Each mode requires its own tree-sitter grammar, separate from the main
OCaml grammars. The modes will prompt you to install the grammar on
first use, or you can install them manually:

    M-x neocaml-ocamllex-install-grammar
    M-x neocaml-menhir-install-grammar

With a prefix argument (`C-u`), these commands reinstall even if the
grammar is already present.

!!! note
    The OCamllex grammar requires tree-sitter ABI version 15+
    (tree-sitter >= 0.25.0). If your Emacs was built against an
    older tree-sitter, you may need to update it. See
    [Troubleshooting](troubleshooting.md) for details.

## Language Injection

On Emacs 30+, embedded OCaml code inside `{ }` action blocks gets
full OCaml syntax highlighting via language injection. This applies to
both OCamllex action blocks and Menhir action/header blocks (`%{ %}`).

Language injection activates automatically when both the mode's grammar
and the OCaml grammar are installed. No configuration is needed.

## Imenu

Both modes provide imenu entries for quick navigation:

- **OCamllex**: indexes rules (`lexer_entry` nodes) and named regexps
- **Menhir**: indexes rules (both old-style and new-style)

## Configuration

Both modes have a single indentation customization variable:

```emacs-lisp
;; OCamllex indentation (default: 2)
(setq neocaml-ocamllex-indent-offset 4)

;; Menhir indentation (default: 2)
(setq neocaml-menhir-indent-offset 4)
```
