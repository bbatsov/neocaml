# neocaml

`neocaml` is **n**ew **E**macs mode for programming in OCaml.

## Why?

Because `caml-mode` is ancient, and `tuareg-mode` is a beast. (it's very powerful, but also very complex)
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

Build a modern Emacs major mode for OCaml, powered by TreeSitter
for font-locking and indentation.

Secondary goal - port this functionality to Tuareg, if feasible.

## Status

The project is just an experiment at this point and only adventurous people
are encouraged to play with it.

## Installation

Right now it's manual only, unless you're on Emacs 29+ where you can do this:

    M-x package-vc-install <RET> https://github.com/bbatsov/neocaml <RET>

In Emacs 30 you can you `use-package` to both install the package from GitHub
and configure it:

``` emacs-lisp
(use-package neocaml
  :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest))
```

## Usage

The `neocaml` package bundled two major modes - one for OCaml code
and one for OCaml interfaces (`.mli`). Both modes will be auto-enabled
when you open the respective type of files.

## Configuration

The modes provide 4 levels of font-locking, as is the standard for TreeSitter-powered
modes. The default font-locking level is Emacs is 3, and you can change like this:

```emacs-lisp
;; this font-lock everything neocaml supports
(setq treesit-font-lock-level 4)
```

See the documention for `treesit-font-lock-level` and `treesit-font-lock-features` for more details.

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

The mode will install the `ocaml` and `ocaml-interface` grammars automatically when
needed. (e.g. when you visit an `.ml` the `ocaml` grammar will be downloaded and
installed)

You can you `neocaml--debug` to get more debug information from TreeSitter:

- When you set this to it will output indentation debug data and enable
`treesitter-inspect-mode` (this shows the current node in the modeline)
- When you set this to `'font-lock` it will also output some font-lock debug info.

As [combobulate](https://github.com/mickeynp/combobulate) doesn't support OCaml yet, it seems the best way to test TS queries is the following:

> If you don’t want to use Combobulate to help you, the builtin method – the
> only method – is to call treesit-query-capture with a starting node (often the
> one from treesit-buffer-root-node or treesit-parser-root-node) and the query
> and then manually inspect the output to see if it’s right. Ugh. It’s messy,
> and it’s hard work. Trust me, I know. I recommend you learn how to use IELM if
> you decide to go this route.

### Font-locking

   You can control the amount of fontification applied by Font Lock mode
of major modes based on tree-sitter by customizing the variable
‘treesit-font-lock-level’.  Its value is a number between 1 and 4:

- Level 1: This level usually fontifies only comments and function names in
     function definitions.
- Level 2 This level adds fontification of keywords,
     strings, and data types.
- Level 3 This is the default level; it adds
     fontification of assignments, numbers, etc.
- Level 4 This level adds everything else that can be fontified: operators,
     delimiters, brackets, other punctuation, function names in function calls,
     property look ups, variables, etc.

Note that the 4 levels are defined by each major-mode and the above are just
recommendations.

### Source of inspiration

Based on ideas and code from:

- [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode)
- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- [nvim-treesitter's OCaml TreeSitter queries](https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries/ocaml)
- <https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode>

### References

- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html>
- <https://www.jonashietala.se/blog/2024/03/19/lets_create_a_tree-sitter_grammar/>
