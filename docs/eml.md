# Dream eml templates

`neocaml-eml-mode` is a major mode for [Dream](https://github.com/camlworks/dream)'s
Embedded ML templates: the `.eml.ml`, `.eml.html` and `.eml.re` files that
`dream_eml` compiles into ordinary source.

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
anchor to. An OCaml host parse would have to recover the template regions out
of its own error recovery, which is neither stable nor version-independent. The
`eml` grammar recognises the template structure and leaves the code opaque.

The OCaml ranges share one parser. With the template text
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

Two more are optional. `ocaml`, installed by `M-x neocaml-install-grammars`,
highlights the code block, the `%` code lines and the `<% ... %>` directive
bodies. `html`, the one `html-ts-mode` uses, highlights the template text.

Without either, the mode still highlights the template skeleton: the `<%` and
`%>` delimiters, the `%` of a code line, the `%%` option and terminator lines,
and the Printf conversion of an output directive.

Injection requires Emacs 30 or newer. On Emacs 29 you get the skeleton only.

## Faces

| Face | What it marks |
| --- | --- |
| `neocaml-eml-delimiter-face` | `<%`, `%>`, and the `%` of a code line |
| `neocaml-eml-format-face` | the Printf conversion, the `s` of `<%s x %>` |
| `neocaml-eml-raw-face` | the `!` of `<%s! x %>` |

The `!` gets its own, loud, face because it suppresses `Dream.html_escape`:
`<%s x %>` escapes its output and `<%s! x %>` does not.

## The embedded language

OCaml is the only embedded language supported out of the box, and
`neocaml-eml-embedded-language` names it.

A template's code regions hold whatever `dream_eml` was told to emit, and the
dune rule can tell it to emit something else. That makes `.eml.html`
ambiguous: the extension is `.html`, which falls through to OCaml unless the
rule says otherwise, and Dream ships one of each under the same
`template.eml.html` name (`example/w-template-files` and
`example/r-template-files`). A `.eml.re` is never OCaml, so the mode injects
nothing into one rather than injecting the wrong thing; the template skeleton
still gets its faces.

Set the variable per file or per directory where the default is wrong:

```elisp
;; .dir-locals.el -- inject nothing into the code regions
((neocaml-eml-mode . ((neocaml-eml-embedded-language . nil))))
```

Any language symbol works, so if you install a grammar for another one,
setting this is all that is needed for injection to start.

Set `neocaml-eml-inject-html` to `nil` to turn off the HTML injection.

## Indentation

The indent rules preserve indentation rather than compute it, so reindenting a
region is close to a no-op.

eml is layout-sensitive in a way the grammar cannot repair. A `%` that drifts
off column 0 stops being a code line and becomes template text. A template line
that drifts left of the column its template opened at ends the template, and
everything below it becomes OCaml. Only spaces count as indentation (a
tab-indented `<html>` has indent 0), so `indent-tabs-mode` is forced off.

## LSP

`neocaml-eml-mode` is not registered with Eglot. A `.eml.ml`
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
