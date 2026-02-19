# Contributing to neocaml

## Getting started

Install the required `ocaml` and `ocaml-interface` grammars by running
`M-x neocaml-install-grammars`.

## Debugging

Set `neocaml--debug` to get more debug information from TreeSitter:

- `t` — output indentation debug data and enable `treesit-inspect-mode`
  (shows the current node in the modeline).
- `'font-lock` — additionally output font-lock debug info (can get very noisy).

```emacs-lisp
;; enable all TreeSitter debug information
(setq neocaml--debug 'font-lock)
```

## Testing tree-sitter queries

As [combobulate](https://github.com/mickeynp/combobulate) doesn't support OCaml yet, the best way to test TS queries is:

> If you don't want to use Combobulate to help you, the builtin method – the
> only method – is to call treesit-query-capture with a starting node (often the
> one from treesit-buffer-root-node or treesit-parser-root-node) and the query
> and then manually inspect the output to see if it's right. Ugh. It's messy,
> and it's hard work. Trust me, I know. I recommend you learn how to use IELM if
> you decide to go this route.

## Architecture

See [doc/DESIGN.md](doc/DESIGN.md) for internal architecture documentation
covering `.mli` support, font-locking levels, and indentation rules.
