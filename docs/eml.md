# Dream eml templates

`neocaml-eml-mode` is a major mode for [Dream](https://github.com/camlworks/dream)'s
Embedded ML templates — the `.eml.ml`, `.eml.html` and `.eml.re` files that
`dream_eml` compiles into ordinary OCaml or Reason.

```ocaml
let render tasks =
  <html>
  <body>
%   tasks |> List.iter begin fun (name, complete) ->
      <p>Task <%s name %>: <%B complete %></p>
%   end;
  </body>
  </html>
```

## Which way the injection goes

Unlike [`neocaml-mlx-mode`](usage.md), where the host grammar is `ocaml` and
another language is injected into it, here the host grammar is `eml` and OCaml
is the *injected* language.

The format forces this. A template body is not an OCaml expression, a `%` code
line is a bare fragment such as `% end;`, and `<%s x %>` has no OCaml node to
anchor to — so an OCaml host parse would have to recover the template regions
out of its own error recovery, which is neither stable nor version-independent.
The `eml` grammar recognises the template structure and leaves the code opaque
instead.

The OCaml ranges deliberately share **one** parser. With the template text
removed, a code block and the `%` lines below it are a single statement stream:
`let render tasks =` opens a binding that `% tasks |> List.iter begin fun _ ->`
continues and `% end;` closes. They only parse as a unit if one parser sees all
of them. (This is the opposite of `neocaml-ocamllex-mode`, which gives each
`{ ... }` action its own parser, because there each action is independent.)

## Setup

The mode needs the `eml` grammar:

```
M-x neocaml-eml-install-grammar
```

Opening a `.eml.ml` file without it offers to install it for you.

Two grammars are optional but worth having:

- `ocaml`, installed by `M-x neocaml-install-grammars`, highlights the code
  block, the `%` code lines and the `<% ... %>` directive bodies.
- `html`, the one `html-ts-mode` uses, highlights the template text.

Without either, the mode still highlights the template skeleton — the `<%` and
`%>` delimiters, the `%` of a code line, the `%%` option and terminator lines,
and the Printf conversion of an output directive.

Injection requires Emacs 30 or newer. On Emacs 29 you get the skeleton only.

## Faces

| Face | What it marks |
| --- | --- |
| `neocaml-eml-delimiter-face` | `<%`, `%>`, and the `%` of a code line |
| `neocaml-eml-format-face` | the Printf conversion, the `s` of `<%s x %>` |
| `neocaml-eml-raw-face` | the `!` of `<%s! x %>` |

The `!` gets its own, deliberately loud, face because it suppresses
`Dream.html_escape`: `<%s x %>` escapes its output and `<%s! x %>` does not.

## `.eml.html` is not always OCaml

`dream_eml` chooses between OCaml and Reason from the file extension, so
`.eml.ml` is OCaml and `.eml.re` is Reason. `.eml.html` is the awkward one: the
extension is `.html`, which falls through to OCaml, *unless* the dune rule
passes `--emit-reason`. Dream ships one of each under the same
`template.eml.html` name — `example/w-template-files` is OCaml and
`example/r-template-files` is Reason.

The extension therefore cannot decide it. `neocaml-eml-embedded-language`
defaults to `ocaml`; set it as a file-local or directory-local variable for a
`.eml.html` that is really Reason:

```elisp
;; .dir-locals.el
((neocaml-eml-mode . ((neocaml-eml-embedded-language . reason))))
```

Note that Emacs ships no `reason` tree-sitter grammar, so unless you have
installed one the code regions in a Reason template are simply left
unhighlighted; the template skeleton is highlighted either way.

Set `neocaml-eml-inject-html` to `nil` to turn off the HTML injection.

## Indentation

Deliberately conservative: the indent rules preserve indentation rather than
compute it, so reindenting a region is close to a no-op.

That is not laziness. eml is layout-sensitive in a way the grammar cannot
repair. A `%` that drifts off column 0 stops being a code line and becomes
template text. A template line that drifts left of the column its template
opened at ends the template, and everything below it becomes OCaml. Only spaces
count as indentation — a tab-indented `<html>` has indent 0 — so
`indent-tabs-mode` is forced off.

## LSP

`neocaml-eml-mode` is deliberately **not** registered with Eglot. A `.eml.ml`
file is not OCaml, and `ocamllsp` would report the whole template as a syntax
error. (This differs from `.mlx`, where `:language-id "ocaml"` is correct
because Merlin reads it through a PPX.)

Run the language server on the *generated* file instead. A dune rule like

```
(rule
 (targets template.ml)
 (deps template.eml.ml)
 (action (run dream_eml %{deps} --workspace %{workspace_root})))
```

puts it at `_build/default/<dir>/template.ml`.

## Known limitations

- No imenu. The definitions in a template are `let` bindings inside the
  injected OCaml, which needs the embedded parser to enumerate; likewise
  `treesit-defun-type-regexp`.
- Comment syntax is OCaml's (`(* ... *)`), because the code regions are what
  people edit in a `.eml.ml`. In a `.eml.html`, where template text dominates,
  set `comment-start` and friends from a file-local variable.

## The grammar

`neocaml-eml-mode` is built on
[tree-sitter-eml](https://github.com/tmcgilchrist/tree-sitter-eml), which
follows the tokenizer in `src/eml/eml.ml` in `camlworks/dream` rather than the
prose in Dream's documentation, and ports its expect tests.
