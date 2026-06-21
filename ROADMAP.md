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
  completion, type-on-hover, navigation, refactoring, and diagnostics. neocaml's job
  is to be a great editing substrate underneath that, not to duplicate it.
- **The auxiliary modes are ours to own.** dune, opam, ocamllex, menhir, cram, and
  the REPL have no language server. That's where neocaml-specific intelligence pays
  off the most.
- **Stay consistent across modes.** Whatever we add (a menu, flymake, a face naming
  scheme) should look and behave the same everywhere.

## Language support

### 1. Finish OCaml 5.x coverage (in flight)

PR #62 moves the grammars to tree-sitter-ocaml v0.25.0, which brings the OCaml 5.5
syntax. Once that lands (currently parked on the upstream ABI-14 tag), audit
font-lock for the newer constructs that arrived across 5.x: effect handlers, the
`effect`/`perform` surface, binding operators, and the 5.5 additions (external
types, modular explicits, polymorphic parameters). Make sure each gets a sensible
face and that indentation handles them. **Priority: high (mostly in progress).**

### 2. OxCaml support (blocked on grammar)

Jane Street's OxCaml adds a substantial set of extensions (modes, kinds, unboxed
and layout-annotated types, locality, and more). Supporting it well needs the
upstream grammar to grow those constructs first. Track the grammar work, and once
it's available, extend font-lock and indentation, ideally gated so the extra rules
only kick in when the OxCaml-aware grammar is installed. **Priority: later (blocked
upstream).**

## Diagnostics and editing intelligence

### 3. flymake backends for the tool modes

Today only `neocaml-opam-mode` has a flymake backend (it shells out to `opam lint`).
Extend that same pattern to the other modes that have no language server behind them:

- **dune**: validate via `dune build` output or a dune-file check.
- **ocamllex / menhir**: run the respective tool and surface parse/grammar errors.
- **cram**: basic structural checks on `.t` files.

There's good precedent for shipping a built-in backend rather than leaving it to the
user: `rust-ts-mode` defines `rust-ts-flymake-command` (defaulting to
`clippy-driver`). For `.ml`/`.mli`, diagnostics already come from eglot + ocamllsp,
so the only thing worth considering there is an optional non-LSP fallback (for
example via `ocamlc -i` or merlin) for users who don't run a language server.
**Priority: high.**

### 4. ocamlformat integration

There's a `neocaml-dune-format-buffer` for dune files, but nothing for OCaml source.
A `neocaml-format-buffer` that drives `ocamlformat` (respecting `.ocamlformat`), with
an opt-in format-on-save mirroring the dune mode, would be one of the highest-value
practical additions. Following `haskell-ts-mode`'s `C-c C-f` (which formats the
active region or the current function with ormolu), we should support formatting a
region or single definition, not just the whole buffer. It should work whether or
not a language server is running, and play nicely with `apheleia`/`format-all` for
people who prefer those. **Priority: high.**

### 5. completion-at-point for the auxiliary modes

The LSP stack handles completion for OCaml source. What it doesn't cover is the
surrounding files: dune stanza and field names, opam fields, and findlib package
names. A small set of `capf` functions for `neocaml-dune-mode`, `neocaml-opam-mode`,
and the dune `(libraries ...)` field would make those modes feel much more alive,
and they'd plug straight into `completion-preview-mode` (see item 17).
**Priority: medium.**

### 6. First-class ocaml-eglot interop

`ocaml-eglot` is becoming the standard way to drive ocamllsp from Emacs, and it
exposes OCaml-specific niceties (type-enclosing sessions, construct, jump-to-*). We
already register the modes with `eglot-server-programs`; the next step is to make
sure neocaml and ocaml-eglot compose cleanly out of the box, document the pairing,
and avoid keybinding or menu clashes. **Priority: medium.**

## Font-lock and syntax correctness

### 7. A `syntax-propertize-function` for OCaml's tricky lexemes

OCaml has a few constructs that a static syntax table can't classify correctly,
most notably the clash between a character literal (`'a'`) and a type variable
(`'a`), plus quoted string literals (`{id|...|id}`). The grammar parses these
fine, but everything keyed on the *syntax table* (sexp motion, `electric-pair`,
`delete-pair`, string/comment detection, `forward-word`) can still trip over them.
`rust-ts-mode` solves the analogous char-vs-lifetime and raw-string problem with a
`syntax-propertize-function`; neocaml should do the same so the lower-level
machinery agrees with the parse tree. **Priority: medium-high (correctness).**

### 8. Prettify and font-lock refinements

Two smaller polish items inspired by sibling modes:

- Add a `prettify-symbols-compose-predicate` so symbols aren't composed inside
  strings, comments, or partial tokens (as `rust-ts-mode` does).
- Consider `haskell-ts-mode`'s approach of highlighting only the pattern variables
  that are actually bindable/usable, rather than every name in a pattern.

**Priority: medium / low.**

## REPL

### 9. A smarter REPL

The REPL (`neocaml-repl-mode`) covers the basics today: send region/buffer/phrase/
definition, `#use` a file, history persistence, prompt and error fontification, and
error navigation back to source. Room to grow:

- **Completion** in the REPL buffer (via the toplevel's own completion or merlin).
- **Restart** command, and a cleaner story for switching between the plain toplevel,
  utop, and `dune utop`.
- **Send-and-step** (evaluate the current phrase and advance to the next).
- **Result handling**: optional pretty-printing, folding long output, and quick
  jump from a REPL error straight to the offending source location.
- **Helpers** for `#require`/`#use`/findlib package loading.

**Priority: medium-high.**

## Tooling and project integration

### 10. project.el backend

The dune interaction mode already finds the project root with
`locate-dominating-file`, but neocaml doesn't teach `project.el` about OCaml
projects. A small backend keyed on `dune-project` would give users project-wide
commands (find file, search, compile, test) for free, and a sensible
`compile-command` per project. **Priority: medium.**

### 11. dune-interaction polish

`neocaml-dune-interaction-mode` already wraps build/test/clean/promote/fmt/exec with
watch-mode support. Natural follow-ups: a `transient` menu, a test runner with
rerun-last and failure navigation, and tighter integration with the compilation
buffer's error parsing. **Priority: medium.**

## Editing ergonomics

### 12. Skeletons and templates for common forms

tuareg ships insert-helpers for `match`, `let ... in`, `module`, `try ... with`, and
friends. neocaml has none. A small set of skeletons (or first-class snippet
definitions) for the common OCaml forms would smooth out everyday editing, and they
compose well with the structural commands we already document. **Priority: medium.**

### 13. Structural editing helpers

We lean hard on the built-in structural commands (see the navigation docs), but
there's room for OCaml-aware editing built on the same tree-sitter framework, in the
spirit of combobulate: wrap a region in `begin ... end`, toggle a module between
`struct` and `sig`, move a function argument left/right, or transpose match arms.
These are exactly the operations the parse tree makes reliable. **Priority: medium,
exploratory.**

### 14. Selectable indentation styles

`c-ts-mode` exposes `c-ts-mode-indent-style` (a defcustom of named styles plus a
command to switch them live). OCaml indentation is famously contentious (ocp-indent
vs ocamlformat vs personal taste), so a small set of named neocaml indent presets,
switchable at runtime, could let people get closer to their preferred look without
hand-tuning rules. Worth prototyping before committing to it, since each style is a
maintenance surface. **Priority: later, exploratory.**

## Adopting newer Emacs features (29 / 30 / 31)

Emacs keeps moving capabilities into core. Several map onto things neocaml either
hand-rolls or doesn't expose yet. None of these are tree-sitter specific.

### 15. Grammar install via the Emacs 31 built-ins

Emacs 31 adds `treesit-auto-install-grammar` (offer to fetch and build a missing
grammar instead of erroring) and `treesit-enabled-modes`. We can lean on these to
slim down `neocaml-install-grammars` and the "grammar not installed" prompt on 31+,
while keeping the current path for 29/30. **Priority: medium.**

### 16. Evaluate `define-treesit-generic-mode` and `.scm` font-lock

Emacs 31 introduces `define-treesit-generic-mode` and the ability to drive
font-locking from `.scm` query files. Worth evaluating whether the simpler auxiliary
modes (cram, objinfo) could be expressed more compactly this way, and whether parts
of our font-lock could move closer to the upstream `highlights.scm`.
**Priority: later, evaluate.**

### 17. Opt into the newer built-in minor modes

A cluster of smaller, non-tree-sitter improvements worth wiring up or recommending:

- **`completion-preview-mode`** (Emacs 30): inline preview of the top completion.
  It's `capf`-driven, so it benefits directly from item 5 and from eglot.
- **`flymake-show-diagnostics-at-end-of-line`** (Emacs 30): nothing to implement,
  but worth recommending in the docs alongside the flymake work.
- **`bug-reference-prog-mode` and `goto-address-prog-mode`**: linkify issue
  references and URLs in comments across all our modes. Cheap, broadly useful.
- **Multiple eldoc backends** via `eldoc-documentation-strategy`: if we ever add a
  neocaml-owned eldoc source (say, doc-comment or REPL-backed), it should compose
  with eglot's rather than replace it.

**Priority: medium / low, mostly low-effort.**

### 18. Track upstream tree-sitter fixes and injection improvements

Retire neocaml's `treesit-forward-comment` off-by-one workaround once the upstream
fix is in a release we depend on, and revisit the language-injection setup (used by
ocamllex and menhir, and by the pending odoc mode) against the newer multi-language
support, including the `markdown-ts-mode` approach of injecting real language modes
into code blocks. **Priority: ongoing.**

## Smaller items and nice-to-haves

- **odoc mode** (PR #46): land `.mld` support once the open review points are
  resolved (notably finishing or dropping the sh/bash injection).
- **ocamldoc rendering**: prettier handling of doc comments in source, and applying
  the odoc grammar to code comments and LSP hovers.
- **Face audit**: make sure every mode exposes customizable, per-feature faces with
  reasonable `font-lock-*` defaults (this came up in the odoc review).
- **More file coverage**: a mode or sensible defaults for `META` files and other
  stragglers in the ecosystem.
- **Performance and robustness**: behavior on very large files, and good defaults
  for `treesit-font-lock-level`.

## Lessons from sibling tree-sitter modes

A few patterns worth stealing, noted next to the items they informed:

- **`rust-ts-mode`**: ships its own flymake backend (`clippy-driver`, item 3), uses a
  `syntax-propertize-function` for char/string/lifetime edge cases (item 7), and a
  `prettify-symbols-compose-predicate` (item 8).
- **`c-ts-mode`**: offers selectable indentation styles via a defcustom and a live
  switch command (item 14), and factors comment filling/indentation into the shared
  `c-ts-common` library.
- **`haskell-ts-mode`**: formats the active region or current function with one key
  (item 4), and highlights only the pattern variables that are actually usable
  (item 8).

## References

- [Tree-sitter Major Modes (Emacs Lisp manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html)
- [ocaml-eglot](https://github.com/tarides/ocaml-eglot)
- [haskell-ts-mode](https://codeberg.org/pranshu/haskell-ts-mode)
- [What's New in Emacs 30.1 (Mastering Emacs)](https://www.masteringemacs.org/article/whats-new-in-emacs-301)
- [Building Emacs Major Modes with Tree-sitter: Lessons Learned](https://batsov.com/articles/2026/02/27/building-emacs-major-modes-with-treesitter-lessons-learned/)
