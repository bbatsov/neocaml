# neocaml

`neocaml` is **n**ew **E**macs mode for programming in OCaml.

## Goal

Build a modern Emacs major mode for OCaml powered by TreeSitter
for font-locking and indentation.

## Status

The project is just an experiment at this point and only adventurous people
are encouraged to play with it.

## Installation

Right now it's manual only, unless you're on Emacs 30 where you can do this:

``` emacs-lisp
(use-package neocaml
    :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest))
```

## Progress

### Things that work

- Auto-installation of `ocaml` and `ocaml-interface` grammars
- Font-locking

### Things that don't work

Everything else. :-)

### Planned features

- Indentation
- Smart expression navigation
- Basic integration with dune
- Basic integration with a top-level

## Development notes

Based on ideas and code from:

- [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode)
- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- <https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode>
