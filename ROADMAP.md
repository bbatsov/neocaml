# neocaml Roadmap

This document collects ideas for where neocaml could go next. It's a wish list and
a thinking tool, not a set of promises or a strict ordering. Items will move in and
out as priorities shift and as the surrounding ecosystem (Emacs, ocaml-lsp-server,
the tree-sitter grammars) evolves. Contributions and discussion on any of these are
very welcome.

## Guiding principles

A few things that shape what belongs here and what doesn't:

- **tree-sitter first.** Every mode is built on a grammar and on the built-in
  `treesit` machinery. We prefer fixing things at that layer over bolting on
  heuristics.
- **Lean on the LSP stack, don't reinvent it.** For `.ml`/`.mli`, `ocaml-lsp-server`
  plus [`ocaml-eglot`](https://github.com/tarides/ocaml-eglot) already provide
  completion, type-on-hover, navigation, type-aware refactoring, and diagnostics.
  neocaml's job is to be a great editing substrate underneath that, not to
  duplicate it. Refactorings that only need the syntax tree are a different story:
  they work without a running server, and they're exactly what tree-sitter is good
  at.
- **The auxiliary modes are ours to own.** dune, opam, ocamllex, menhir, cram, and
  the REPL have no language server. That's where neocaml-specific intelligence pays
  off the most.
- **The build and the environment are ours too.** Running the right tests,
  promoting their output, and picking the right opam switch are things no language
  server does for you.
- **Stay consistent across modes.** Whatever we add (a menu, flymake, a face naming
  scheme) should look and behave the same everywhere.

## Language support

### 1. OxCaml polish

neocaml handles the OxCaml syntax that tree-sitter-ocaml v0.26.0 parses: font-lock,
indentation and navigation for modes, kinds, stack allocation, unboxed types,
immutable arrays and comprehensions. What's left is smaller:

- Modes broken onto lines of their own after `mod`, and `with` bounds on a line of
  their own, are left as typed. ocamlformat's layout for them isn't one worth
  copying, but a sensible default would be nice.
- So are attributes on lines of their own in module types and functor parameters,
  which ppx_template code is full of (`module%template.portable` followed by
  `[@modality p] Make (...)`). ocamlformat doesn't place them consistently, so
  there's no one layout to follow.

**Priority: low.**

### 2. OCaml 5.x font-lock audit

The OCaml 5.5 grammar has shipped. What's left is an audit of the constructs that
arrived across 5.x (effect handlers, binding operators, external types, modular
explicits, polymorphic parameters), making sure each gets a sensible face and
indents well. While at it, drop the node names our rules mention that the
grammar doesn't have (`infix_operator` and the `*_binding_pattern` types).
**Priority: medium.**

## Refactoring

### 3. Syntactic refactorings

clojure-mode and clj-refactor show how far you can get with rewrites that only need
the syntax tree. Structural editing in general (wrapping code in a form, dragging
parameters and match arms around) is already covered by
[combobulate](https://github.com/mickeynp/combobulate), which supports our modes.
What's left are rewrites specific to OCaml, and ocamllsp offers none of these:

- Thread and unwind pipelines: `List.map f (List.filter p xs)` ↔
  `xs |> List.filter p |> List.map f`.
- Convert `let f x = match x with ...` ↔ `let f = function ...`.
- Switch between `|>` and `@@`.
- Convert binding operators: `let* x = e in body` ↔ `e >>= fun x -> body`.
- Eta reduce and expand: `List.map (fun x -> f x) l` ↔ `List.map f l`.
- Flip an `if`: negate the condition and swap the branches.
- Convert `let open M in e` ↔ `M.(e)`.
- Pun and unpun record fields and labels: `{ x = x }` ↔ `{ x }`, `~x:x` ↔ `~x`.
- Turn a string into a quoted string (`"a\"b"` ↔ `{|a"b|}`) and back.
- Comment out the node at point. OCaml comments nest, so this toggles cleanly.
- Convert between lists and arrays, and toggle `mutable` on a record field.

Some reach into a second file, but are still purely syntactic:

- Add a `[@@deriving ...]` attribute together with the matching ppx in the
  stanza's `(preprocess (pps ...))`.
- Jump to (or create) the doc comment of the definition at point, in the `.ml` or
  in the `.mli`, like `go-ts-mode-docstring`.
- Generate stubs for the `.mli` declarations the `.ml` doesn't implement yet, and a
  stub for a function called at point that doesn't exist yet.

clojure-ts-mode is a good template for building these: find the node with
`treesit-thing-at-point`, edit through markers from the end backwards, reindent the
result, signal a `user-error` for shapes we don't handle, and test every refactoring
with before/after specs. They'd get their own prefix, `C-c C-m` as in clj-refactor
(`C-c C-r` is taken by the REPL's send-region). Before building the punning toggle,
check what ocamlformat does with puns. If it normalizes them anyway, the toggle is
pointless for most people. **Priority: high for the pipeline and
`match`/`function` conversions, medium for the rest.**

### 4. Make ocamllsp's code actions easier to reach

ocamllsp already provides extracting a local or a function, inlining, adding and
removing type annotations, combining match cases, qualifying an `open`, adding a
missing `rec`, removing unused code, and updating the `.mli`. ocaml-eglot has
commands only for destruct, construct and inferring an interface, so the rest are
reachable only through `eglot-code-actions`, with no keys, and extract leaves a
placeholder name behind that you rename afterwards. Named commands (with a prompt
for the new name, which ocamllsp's `ocamllsp/refactorExtract` request accepts)
would fix that. They belong in ocaml-eglot rather than here, so step one is
proposing them there. **Priority: medium (upstream).**

## Tests, builds and the environment

### 5. A test and promotion workflow

Expect tests and cram tests both work by promoting dune's output into the source.
neocaml can run the current file's tests, show its pending promotion as a diff,
and promote just that file. What's still missing:

- A list of the files with pending promotions (`dune promotion list`), to review
  and promote them one by one.
- Rerun the last test command from anywhere in the project.
- Run the single `let%expect_test` or `let%test` at point. dune doesn't pass a
  location through to the test runner, so this needs the inline test runner's
  `-only-test` flag and some investigation.
- A `transient` menu over the dune commands.

**Priority: high.**

### 6. Use the project's opam switch automatically

Getting Emacs to use the right opam switch for a project comes up again and again
on discuss.ocaml.org. VS Code and Zed detect a dune-managed toolchain (dune package
management), a local `_opam` switch, or the switch opam itself picks for the
directory, and run ocamllsp, ocamlformat and the toplevel from it. Emacs users get
by with the global `opam-switch-mode` or with envrc. neocaml could set
`process-environment` and `exec-path` buffer-locally per project, so Eglot,
`compile`, the REPLs and the formatter all agree on the switch, and react to
`opam-switch-mode` by restarting the REPLs and flushing the library and package
caches. The completion code already detects a local `_opam` switch, so there's a
starting point. **Priority: high.**

### 7. A setup checker

Bug reports about a broken setup tend to need the same answers. A `neocaml-doctor`
command could collect them up front: whether the grammars are installed and
loadable, which `ocamllsp`, `ocamlformat`, `dune` and `utop` are on the `PATH` and
which switch they come from, whether Eglot knows about our modes, whether there are
stale `.elc` files around, and what merlin configuration dune hands out for the
current file (`dune ocaml merlin dump-config`). It grows naturally out of
`neocaml-bug-report-info`. **Priority: medium-high.**

### 8. Add a missing library

On "Unbound module Yojson", offer to add `yojson` to the `libraries` of the nearest
`dune` stanza, add the package to the `depends` in `dune-project` (or the `.opam`
file), and install it with opam if it's not there. I don't know of an editor that
does this today, and neocaml already has the pieces: the library list from the dune
completion, the dune grammar, and the opam package list. **Priority: medium.**

### 9. See what the preprocessor did

ppx-heavy code is hard to follow when you can't see the expansion. `dune describe
pp FILE` prints a file after preprocessing, and `ocamlc -dparsetree` shows the
parsetree. Commands that show either in a read-only buffer, with locations you can
jump back from, would help a lot. VS Code has both. **Priority: medium.**

### 10. Documentation for the symbol at point

`neocaml-browse-ocaml-docs` opens the ocaml.org docs, but not for the thing at
point. Knowing the module path (from the parse tree, or from ocamllsp), we can
build the URL with an anchor (`/manual/5.4/api/List.html#VALmap` for the stdlib,
`/p/PKG/latest/doc/...` for packages), fall back to `odig` for offline docs, and
wrap `dune ocaml doc` for the project's own. **Priority: medium.**

### 11. dune conveniences

Smaller things around `neocaml-dune-interaction-mode`:

- `dune exec` with completion of the project's executables (via `dune describe`),
  run in a comint buffer so programs that read from stdin work.
- Completion of aliases for `dune build @...`, and a `dune init` wrapper.
- `dune diagnostics` against a running watch build, and a watch buffer that keeps
  only the output of the latest build.
- flymake for `dune` files, and basic structural checks for cram tests, the way
  `neocaml-opam-mode` does it with `opam lint`.

**Priority: medium.**

## REPL

### 12. More REPL helpers

The REPLs cover a lot by now (two backends, per-project sessions, completion,
restart, send-and-step, `#require` with package completion). Room to grow:

- Load the current module into the plain toplevel with `dune ocaml top-module`,
  which prints the directives that load a module without its `.mli` hiding
  anything. That would be a big upgrade for the `ocaml` flavor.
- Map errors in `neocaml-repl` that refer to a sent phrase (`Line 1, characters
  4-7`) back to the source, as `neocaml-utop` already does.
- Insert the result of an evaluation as a comment below the form
  (`(* val x : int = 3 *)`).
- `#trace` and `#show` the identifier at point, and a scratch buffer.

**Priority: medium.**

## Editing ergonomics

### 13. Electric delimiters and keyword reindentation

- When `electric-pair-mode` is on, close `[|` and `{<` with `|]` and `>}`.
  (Comments stay with `M-;`, see the FAQ.)
- Reindent the current line on typing `|`, `end`, `done`, `with` or `in`, like
  tuareg does.

**Priority: medium.**

### 14. Skeletons and templates

tuareg ships insert-helpers for `match`, `let ... in`, `module`, `try ... with`, and
friends, and dune's own Emacs mode has skeletons for most stanzas. neocaml has
neither. Skeletons for the common OCaml forms, for dune stanzas and for an opam
file, plus `auto-insert` templates for empty files (a new `dune` file could start
out as a `library` stanza named after its directory), would cover it.
**Priority: medium.**

### 15. Jump between code and its tests

`C-c C-a` already toggles between `.ml` and `.mli`. `find-sibling-rules` (Emacs 29)
could handle `lib/foo.ml` ↔ `test/test_foo.ml` the same way. **Priority: low.**

### 16. Indentation styles, starting with Jane Street's

neocaml's indentation sits between ocamlformat's two main profiles. On about
130,000 lines of OCaml from opam packages, formatted with each profile, it puts 90%
of the lines where the conventional profile does and 82% where the janestreet one
does. Part of the gap needs no option, because the other profile never uses the
layout in question:

- the janestreet profile's leading `;` in records, lists and arrays (about 6,500
  lines of the gap)
- a `;;` after a definition inside a module, which should line up with the
  definition
- the cases of a `function` that follows an operator (`x >>= function`), which
  both profiles line up with the start of the line

The rest are real conflicts:

- match case bodies: conventional indents them 4 columns from the `|`, janestreet
  2, as neocaml does
- an operator starting a line inside parentheses: conventional puts it under the
  `(`, janestreet a column right of it
- the `|` of a parenthesized `match`, which janestreet puts under the `match`
- parameters on continuation lines, which janestreet indents by 6

`c-ts-mode` shows the shape: a `c-ts-mode-indent-style` defcustom of named styles
and a command to switch them live. A `neocaml-indent-style` with `conventional` and
`janestreet` presets would keep the conflicting rules as overrides in front of the
shared ones, so each preset stays small. Indentation can only follow ocamlformat's
line breaks, not choose them, so this is for people who indent as they type and
format later; with format on save, ocamlformat has the last word anyway.
**Priority: medium, after the fixes that need no option.**

### 17. Prettify and font-lock refinements

Two smaller polish items inspired by sibling modes:

- Add a `prettify-symbols-compose-predicate` so symbols aren't composed inside
  strings, comments, or partial tokens (as `rust-ts-mode` does).
- Consider `haskell-ts-mode`'s approach of highlighting only the pattern variables
  that are actually bindable/usable, rather than every name in a pattern.

**Priority: medium / low.**

## Adopting newer Emacs features (29 / 30 / 31)

Emacs keeps moving capabilities into core. Several map onto things neocaml either
hand-rolls or doesn't expose yet.

### 18. Grammar install via the Emacs 31 built-ins

Emacs 31 adds `treesit-auto-install-grammar` (offer to fetch and build a missing
grammar instead of erroring) and `treesit-enabled-modes`. We can lean on these to
slim down `neocaml-install-grammars` and the "grammar not installed" prompt on 31+,
while keeping the current path for 29/30. **Priority: medium.**

### 19. Evaluate `define-treesit-generic-mode` and `.scm` font-lock

Emacs 31 introduces `define-treesit-generic-mode` and the ability to drive
font-locking from `.scm` query files. Worth evaluating whether the simpler auxiliary
modes (cram, objinfo) could be expressed more compactly this way, and whether parts
of our font-lock could move closer to the upstream `highlights.scm`.
**Priority: later, evaluate.**

### 20. Opt into the newer built-in minor modes

- **`completion-preview-mode`** (Emacs 30): inline preview of the top completion.
  It's `capf`-driven, so it works with Eglot and with the completion in the dune
  and opam modes.
- **`flymake-show-diagnostics-at-end-of-line`** (Emacs 30): nothing to implement,
  but worth recommending in the docs.
- **Multiple eldoc backends** via `eldoc-documentation-strategy`: if we ever add a
  neocaml-owned eldoc source (say, doc-comment or REPL-backed), it should compose
  with Eglot's rather than replace it.

**Priority: low, mostly docs.**

### 21. Track upstream tree-sitter fixes and injection improvements

Retire neocaml's `treesit-forward-comment` off-by-one workaround once the upstream
fix is in a release we depend on, and revisit the language-injection setup (used by
ocamllex, menhir, odoc and mlx) against the newer multi-language support, including
the `markdown-ts-mode` approach of injecting real language modes into code blocks.
**Priority: ongoing.**

## Smaller items and nice-to-haves

- **Doc comments in hovers**: apply the odoc grammar to LSP hovers and eldoc, now
  that doc comments in the source are rendered.
- **Face audit**: make sure every mode exposes customizable, per-feature faces with
  reasonable `font-lock-*` defaults.
- **More file coverage**: a mode or sensible defaults for `META` files and other
  stragglers in the ecosystem.
- **menhir**: support for `.messages` files and for the `.conflicts` file that
  `--explain` writes.
- **Performance and robustness**: behavior on very large files, and good defaults
  for `treesit-font-lock-level`.

## Not planned

- **flymake for ocamllex and menhir.** Running the tools on the buffer only catches
  structural errors, since the OCaml in the actions never gets type-checked. dune
  does the real checking on build, and `compile` already runs it in dune projects.
- **Coverage overlays, profiler integration, a package explorer.** All doable, but
  few people would reach for them from the editor.

## Lessons from sibling modes

A few patterns worth stealing, noted next to the items they informed:

- **clojure-ts-mode and clj-refactor**: refactoring commands built on the parse
  tree, and a test style for them (item 3).
- **`go-ts-mode`**: creating and jumping to the doc comment of a definition
  (item 3).
- **VS Code's OCaml Platform and Zed**: automatic switch detection (item 6), and
  viewers for the preprocessed source and the parsetree (item 9).
- **`rust-ts-mode`**: a `prettify-symbols-compose-predicate` (item 17).
- **`c-ts-mode`**: selectable indentation styles via a defcustom and a live switch
  command (item 16).
- **`haskell-ts-mode`**: highlighting only the pattern variables that are actually
  usable (item 17).

## References

- [Tree-sitter Major Modes (Emacs Lisp manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html)
- [ocaml-eglot](https://github.com/tarides/ocaml-eglot)
- [OxCaml documentation](https://oxcaml.org/documentation/)
- [tree-sitter-ocaml v0.26.0 release notes](https://github.com/tree-sitter/tree-sitter-ocaml/releases/tag/v0.26.0)
- [combobulate](https://github.com/mickeynp/combobulate)
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el)
- [haskell-ts-mode](https://codeberg.org/pranshu/haskell-ts-mode)
- [What's New in Emacs 30.1 (Mastering Emacs)](https://www.masteringemacs.org/article/whats-new-in-emacs-301)
- [Building Emacs Major Modes with Tree-sitter: Lessons Learned](https://batsov.com/articles/2026/02/27/building-emacs-major-modes-with-treesitter-lessons-learned/)
