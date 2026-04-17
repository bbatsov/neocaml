# FAQ

## Which Emacs version should I use?

neocaml requires Emacs 29.1+ but Emacs 30+ is recommended. Some features
are only available on newer versions:

- **Emacs 30+**: language injection for OCamllex/Menhir, sentence
  navigation (`M-a`/`M-e`), `outline-minor-mode` integration, list
  navigation (`C-M-u`, `C-M-d`, `C-M-n`, `C-M-p`)
- **Emacs 31+**: improved `backward-up-list` support via the native
  `list` thing

## How do I install grammars for dune, opam, OCamllex, or Menhir?

`M-x neocaml-install-grammars` only installs the OCaml and
OCaml-interface grammars. The other modes have their own grammar
install commands: `M-x neocaml-dune-install-grammar`,
`M-x neocaml-opam-install-grammar`,
`M-x neocaml-ocamllex-install-grammar`, and
`M-x neocaml-menhir-install-grammar`. Each mode will also prompt
you to install its grammar on first use.

## Can I use Merlin instead of Eglot?

neocaml auto-configures Eglot for `ocamllsp`, but you can use Merlin
instead. Install [merlin-mode](https://ocaml.github.io/merlin/) and
enable it via a hook:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'merlin-mode)
```

Merlin provides its own completion, type display, and error checking
independently of Eglot.

## How do I change the indentation width?

Set `neocaml-indent-offset` (default 2):

```emacs-lisp
(setq neocaml-indent-offset 4)
```

See [Configuration](configuration.md#indentation) for alternative
indentation engines (ocp-indent, tuareg SMIE, indent-relative).

## Do I need to remove tuareg or caml-mode?

Not strictly, but it's the simplest path. If both are installed, they
may fight over `.ml`/`.mli` file associations. See
[Migration](migration.md) for details on running them side by side.

## Can I use lsp-mode instead of Eglot?

neocaml auto-configures Eglot, but it doesn't depend on it. If you
prefer [lsp-mode](https://emacs-lsp.github.io/lsp-mode/), install it
and configure it for `ocamllsp` as you would for any language. neocaml's
font-lock, indentation, and navigation all work independently of which
LSP client you use.

## Why doesn't `(` automatically insert `(* *)` inside comments?

neocaml does not implement electric comment delimiters (tuareg does, but the
logic is quite complex). Instead, use `M-;` (`comment-dwim`) to insert comment
delimiters -- it will insert `(* *)` with point positioned between them,
properly indented. This is simpler and more predictable.
