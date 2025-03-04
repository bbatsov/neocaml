# neocaml

`neocaml` is **n**ew **E**macs mode for programming in OCaml.

## Why?

Because `caml-mode` is ancient, and `tuareg-mode` is a beast.
The time seems ripe for a modern, leaner, TreeSitter-powered mode for
OCaml.

There have been two other attempts to create TreeSitter-powered
major modes for Emacs, but they didn't get very far:

- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode) (first one, available in MELPA)
- [ocaml-ts-mode](https://github.com/terrateamio/ocaml-ts-mode) (second one)

Looking at the code of both modes, I inferred that the authors were probably knowledgable in
OCaml, but not very familiar with Emacs Lisp and Emacs major modes in general.
For me it's the other way around, and that's what makes this a fun and interesting project for me:

- I enjoy working on Emacs packages
- I want to do more work TreeSitter, how that it's getting more traction
- I really like OCaml and it's one of my favorite "hobby" languages

They say that third time's the charm, and I hope that `neocaml` will get farther than
them. Time will tell!

One last thing - we really need more Emacs packages with fun names! :D

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
