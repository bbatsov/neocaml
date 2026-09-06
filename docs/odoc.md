# odoc Support

neocaml includes a dedicated tree-sitter mode for
[odoc](https://ocaml.github.io/odoc/) documentation pages.
`neocaml-odoc-mode` activates automatically for `.mld` files and
provides font-lock, indentation, imenu, and language injection into
code blocks.

## Grammar Installation

The mode requires its own tree-sitter grammar, separate from the main
OCaml grammars. It will prompt you to install the grammar on first
use, or you can install it manually:

    M-x neocaml-odoc-install-grammar

With a prefix argument (`C-u`), the command reinstalls even if the
grammar is already present.

!!! note
    The odoc grammar requires tree-sitter ABI version 14+
    (tree-sitter >= 0.24). If your Emacs was built against an
    older tree-sitter, you may need to update it. See
    [Troubleshooting](troubleshooting.md) for details.

## Font-lock

Every odoc construct gets its own face, so you can remap any of them
without disturbing the others:

| Face | Markup |
|------|--------|
| `neocaml-odoc-heading-face` | `{0 Title}` through `{5 ...}` |
| `neocaml-odoc-bold-face` | `{b ...}` |
| `neocaml-odoc-italic-face` | `{i ...}` |
| `neocaml-odoc-emphasis-face` | `{e ...}` |
| `neocaml-odoc-tag-face` | `@param`, `@return`, `@since`, ... |
| `neocaml-odoc-tag-name-face` | the name in `@param name` and `@raise Exn` |
| `neocaml-odoc-tag-value-face` | the version in `@before 1.0` |
| `neocaml-odoc-code-face` | `[inline code]` and code blocks |
| `neocaml-odoc-verbatim-face` | `{v ... v}` |
| `neocaml-odoc-language-face` | the language tag of `{@ocaml[...]}` |
| `neocaml-odoc-math-face` | `{m ...}` and `{math ...}` |
| `neocaml-odoc-reference-face` | `{!Module.value}` |
| `neocaml-odoc-link-face` | `{:https://...}` and media targets |
| `neocaml-odoc-escape-face` | `\{`, `\}`, `\[`, `\]`, `\@` |
| `neocaml-odoc-markup-face` | `{^ ...}`, `{_ ...}` |
| `neocaml-odoc-raw-markup-face` | `{%html: ...%}` |
| `neocaml-odoc-list-face` | `{ul ...}`, `{ol ...}` |
| `neocaml-odoc-bracket-face` | the delimiters themselves |

Bracket highlighting is a level-4 feature, so raise
`treesit-font-lock-level` to 4 if you want the delimiters coloured.

## Language Injection

On Emacs 30+, code inside a `{@lang[...]}` block gets full syntax
highlighting for that language:

    {@ocaml[
    let greet name = Printf.printf "Hello, %s!\n" name
    ]}

OCaml, dune, and opam are injected, each when its grammar is
installed; delimited blocks such as `{delim@ocaml[...]delim}` are
injected too. A block in any other language still reads as code, via
`neocaml-odoc-code-face`.

Language injection activates automatically. No configuration is
needed.

## Imenu

Imenu indexes the page under two categories:

- **Heading**: every `{0 ...}` through `{5 ...}`, named by its title
- **Tag**: `@param`, `@return`, `@raise`, `@deprecated`, `@since`,
  `@version` and `@author`

Headings also act as defuns, so `C-M-a` and `C-M-e` move between
sections.

## Configuration

```emacs-lisp
;; odoc indentation (default: 2)
(setq neocaml-odoc-indent-offset 4)
```
