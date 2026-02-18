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

## General notes

Emacs doesn't support directly using `.scm` (TreeSitter queries) files, so we currently need
to manually code both the font-locking and indentation queries.

Emacs 31 will introduce `define-treesit-generic-mode` that will make it possible to
use `.scm` for font-locking.

- <https://github.com/emacs-mirror/emacs/blob/master/lisp/treesit-x.el#L47>

## Architecture

See [doc/DESIGN.md](doc/DESIGN.md) for internal architecture documentation
covering `.mli` support, font-locking levels, and indentation rules.

## Sources of inspiration

Based on ideas and code from:

- [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode)
- [tuareg](https://github.com/ocaml/tuareg)
- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- [nvim-treesitter's OCaml TreeSitter queries](https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries/ocaml)
- <https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode>

## References

- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html#index-treesit_002dsimple_002dindent_002dpresets> (indentation)
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html>
- <https://www.jonashietala.se/blog/2024/03/19/lets_create_a_tree-sitter_grammar/>
- <https://archive.casouri.cc/note/2024/emacs-30-tree-sitter/>
