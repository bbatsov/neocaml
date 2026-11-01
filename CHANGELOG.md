# Changelog

<!-- Entries are one line per item and section headings repeat for every release,
     so the line-length and duplicate-heading rules don't fit this file. -->
<!-- markdownlint-disable MD013 MD024 -->

## main (unreleased)

### New features

- [#60](https://github.com/bbatsov/neocaml/issues/60): Render ocamldoc markup in doc comments - `{b bold}`, `{i italic}`, `[code]`, `{[...]}` code blocks, `{!references}` and `@tags` are now displayed with real faces via the new `doc-markup` font-lock feature (enabled at the default font-lock level).

- [#46](https://github.com/bbatsov/neocaml/pull/46): New `neocaml-odoc-mode` for [odoc](https://ocaml.github.io/odoc/) documentation pages (`.mld`), with font-lock for headings, inline markup, code spans, references, links, tags, lists and tables, indentation, imenu, and language injection into `{@lang[...]}` code blocks for OCaml, dune and opam (Emacs 30+). Install its grammar with `M-x neocaml-odoc-install-grammar`.
- [#77](https://github.com/bbatsov/neocaml/pull/77): Add `neocaml-mlx-mode`, a major mode for `.mlx` files (OCaml with embedded JSX, as used by Melange/React), with JSX highlighting via `tsx` grammar injection (Emacs 30+).
- [#95](https://github.com/bbatsov/neocaml/pull/95): Complete findlib package names in `neocaml-repl-require` and `neocaml-utop-require`, using the packages `ocamlfind list` reports for the project's opam switch.
- [#96](https://github.com/bbatsov/neocaml/pull/96): Add commands for working with the current file's tests:
  - `neocaml-dune-test-file` (`C-c C-o T`) runs a cram test on its own, or the tests of the file's directory for other files.
  - `neocaml-dune-promotion-diff` (`C-c C-o =`) shows the file's pending promotion in a `diff-mode` buffer (with a prefix argument, every pending promotion).
  - `neocaml-dune-promote-file` (`C-c C-o P`) promotes only that file and reverts its buffer.
  - In `neocaml-cram-mode`, `C-c C-c` runs the test, `C-c C-r` shows its correction and `C-c C-p` promotes it.
- [#102](https://github.com/bbatsov/neocaml/pull/102): Fontify [OxCaml](https://oxcaml.org/) syntax: the `stack_`, `exclave_`, `local_`, `global_`, `borrow_`, `kind_` and `kind_of_` keywords, modes and modalities, kinds, immutable arrays, unboxed types and literals (`float#`, `#1.0`, `#true`, `#(...)`, `#{...}`, `.#field`), and the new builtin types (`float32`, `int8`, `int16`, `or_null` and the unboxed `#` variants).
- [#102](https://github.com/bbatsov/neocaml/pull/102): Indent OxCaml code: the bodies of `exclave_`, `stack_`, `local_` and `borrow_` on the next line, kind definitions, `mod` bounds, and the `for` and `when` clauses of comprehensions.
- [#102](https://github.com/bbatsov/neocaml/pull/102): Treat OxCaml kind definitions as definitions for imenu (under "Kind"), defun navigation, `which-function` and sentence motion, immutable arrays as lists, and the `@`/`@@` of modes and modalities as punctuation in sexp motion.
- [#107](https://github.com/bbatsov/neocaml/pull/107): Fontify OxCaml's builtin kinds (`value`, `immediate`, `float64`, etc.) and SIMD vector types (`int8x16`, `float64x2#`, etc.) as builtins, and the predefined `floatarray`, `extension_constructor`, `eff`, `continuation`, `atomic_loc` and `lexing_position` types too.
- [#107](https://github.com/bbatsov/neocaml/pull/107): Indent the `@@` of an OxCaml `sig @@ portable` starting a line like `end`.
- [#107](https://github.com/bbatsov/neocaml/pull/107): Indent an attribute on a line of its own after a body, like the `[@nontail]` and `[@exclave_if_stack a]` of Jane Street code, like the body.
- [#108](https://github.com/bbatsov/neocaml/pull/108): Fontify the toplevel's responses in `neocaml-repl` and `neocaml-utop` with tree-sitter, the signatures as in `.mli` buffers and the values as in `.ml` buffers (toggle with `neocaml-repl-fontify-output` and `neocaml-utop-fontify-output`).
- [#110](https://github.com/bbatsov/neocaml/pull/110): Add indentation styles: `neocaml-indent-style` selects `ocp-indent` (the default), or `conventional` and `janestreet` after ocamlformat's profiles, and when it's `nil` the style comes from the `profile` and `cases-exp-indent` of the project's `.ocamlformat`. `M-x neocaml-set-indent-style` and the OCaml menu switch the style of the current buffer.
- [#112](https://github.com/bbatsov/neocaml/pull/112): Take the indentation style from the project's `.ocp-indent` when it has no `.ocamlformat`: its `JaneStreet` preset selects the `janestreet` style, and its `match_clause` how far the bodies of match cases go.

### Changes

- [#88](https://github.com/bbatsov/neocaml/pull/88): Switch the `ocaml` and `ocaml-interface` grammars to tree-sitter-ocaml v0.26.0 (the `v0.26.0-abi14` tag), which parses the OxCaml extensions. Reinstall them with `C-u M-x neocaml-install-grammars`.
- [#99](https://github.com/bbatsov/neocaml/pull/99): Move the keys that clashed with ocaml-eglot and merlin:
  - In `neocaml-repl-minor-mode` and `neocaml-utop-minor-mode`, sending the phrase at point is now `C-c C-e` (was `C-c C-p`), sending it and moving to the next one `C-c C-s` (was `C-c C-n`), loading the file `C-c C-u` (was `C-c C-l`) and interrupting the toplevel `C-c C-q` (was `C-c C-i`).
  - The dune commands move from the `C-c C-d` prefix to `C-c C-o`, and live in `neocaml-dune-command-map`, so they can be bound under another prefix.
- [#80](https://github.com/bbatsov/neocaml/pull/80): Use the hybrid forward-sexp function on every Emacs version, so `C-M-f` moves over whole keyword-led forms (`fun`, `match`, `if`, etc.) on Emacs 31+ instead of stopping after the keyword.
- [#100](https://github.com/bbatsov/neocaml/pull/100): Give each dune project in a monorepo its own REPL session, named with its path below the `project.el` root (e.g. `*OCaml: mono/pkgs/core*`), so `dune utop` loads the libraries of the project you're working in.
- [#111](https://github.com/bbatsov/neocaml/pull/111): Indent the signature of a functor's parameter as ocp-indent and ocamlformat's janestreet profile do, in the `ocp-indent` and `janestreet` styles: on a module definition's first line (`module F (X : sig`) its body goes 4 columns in and its `end` 2, like a structure passed to a functor, and elsewhere from inside the parameter's parentheses.
- [#110](https://github.com/bbatsov/neocaml/pull/110): Indent as ocp-indent does by default, which moves a few layouts:
  - The cases and the `with` of a `(match ...)` or `(try ...)` go under the `match`, and an operator starting a line inside parentheses under the first operand.
  - Parameters on continuation lines go 4 columns in, arguments after `=`, `then` and the like 4 past the start of the line, and arguments after an operator past the start of the chain.
  - The body of a structure passed to a functor (`F (struct`) goes 4 columns in and its `end` 2, and the body of a `begin` or `(` opening a match case's body 2 past where the case's body goes.
  - The body of a `(fun x ->` starting a line goes past the `fun`, and that of one further along a line 4 past the start of the line.

### Bug fixes

- [#111](https://github.com/bbatsov/neocaml/pull/111): Stop moving a comment that no indentation rule covers further right every time it's indented.
- [#111](https://github.com/bbatsov/neocaml/pull/111): Leave the lines inside a multi-line string, `{|...|}` ones (as in `[%expect]` blocks) included, as they are when indenting, instead of indenting them like code, which changed the string.
- [#111](https://github.com/bbatsov/neocaml/pull/111): Indent the continuation lines no rule covered, which neocaml left where they were typed:
  - The arguments of a constructor on the line after it, in patterns and declarations, and the type or value of a record field after its `:` or `=`.
  - The type of a typed pattern, the `:` of a typed expression, the type after a polymorphic annotation, and a binding on the line after a `let%ext`, as each indentation style lays them out.
  - The elements of a tuple type, a leading comma in type parameters, and the rest of an `include`, `open`, `functor`, local open or class binding.
  - Comments between the definitions of a `let rec ... and` or a `type ... and`, which line up with the definitions.
- [#111](https://github.com/bbatsov/neocaml/pull/111): Indent module definitions broken across lines, as ocp-indent and ocamlformat do:
  - Functor parameters on continuation lines go 4 columns in, and a `struct` or `sig` starting a line under the `module`.
  - A module type or body after `:` or `=` goes 2 columns in, and the arguments of a functor on lines of their own 2 past the functor.
  - The body and `end` of a module whose header spans lines line up with the `module`.
- [#110](https://github.com/bbatsov/neocaml/pull/110): Indent the arguments of a function that comes after a parenthesis or a bracket past the function, as ocp-indent and ocamlformat do, instead of past the start of the line.
- [#110](https://github.com/bbatsov/neocaml/pull/110): Indent the body of a `fun` that starts the body of a `let` on a line of its own (`let f : t =` followed by `fun x ->`) where the `fun` is, as ocp-indent and ocamlformat do.
- [#103](https://github.com/bbatsov/neocaml/pull/103): Indent with spaces in `neocaml-mode` and `neocaml-interface-mode`, as ocamlformat does, instead of a mix of tabs and spaces past column 8.
- [#104](https://github.com/bbatsov/neocaml/pull/104): Indent the lines that no indentation rule covered, which `TAB` and `indent-region` used to leave where they were. They now follow ocamlformat's layout:
  - continuation lines of operator chains (`|>`, `&&`, `^`, `::`, `:=`, `<-`) and of multi-line function types
  - tuple elements, and the fields of record patterns
  - `and` in a nested `let rec` (and `and+` of a `let+` inside parentheses), or-patterns, and polymorphic variant types broken before a `|`
  - a `match` or `if` subject on a line of its own, and a labeled argument's value after `~label:`
  - the leading `,`, `;`, `->` and `<-` of ocamlformat's janestreet profile
- [#106](https://github.com/bbatsov/neocaml/pull/106): Follow more of ocamlformat's layout when indenting:
  - a leading `;` in records, lists and arrays goes under the opening bracket
  - `;;` inside a module aligns with the definition it ends
  - the body of `x >>= fun y ->` and the cases of `x >>= function` line up with `x`, and the cases of a `function` starting its line with the `function`
  - item attributes (`[@@deriving ...]`) on a line of their own align with their item
  - attribute and extension payloads on the next line are indented, except the items of a ppx_template `[%%template ...]` holding several of them
- [#102](https://github.com/bbatsov/neocaml/pull/102): Fontify the `_` wildcard in patterns and types again, which tree-sitter-ocaml v0.26.0 parses as nodes of its own, and warn when the installed OCaml grammars are older than that.
- [#78](https://github.com/bbatsov/neocaml/pull/78): Don't treat `"` and `'` delimiter nodes as sexps, so `treesit-thing-at` before a string or char literal returns the whole literal instead of the bare quote.
- [#79](https://github.com/bbatsov/neocaml/pull/79): Fix a segfault on Emacs 31.1 when font-locking a buffer containing a builtin type or value (`let x : int = 42` was enough to trigger it).
- [#94](https://github.com/bbatsov/neocaml/pull/94): Make `neocaml-format-buffer` and `neocaml-dune-format-buffer` replace only the text that changed, so point, markers and window positions stay put and an already formatted buffer isn't marked modified.
- [#91](https://github.com/bbatsov/neocaml/issues/91): Fix the `dune-utop` flavor of `neocaml-utop` failing to start - it passed the project root to `dune utop` as an absolute path, which dune rejects, so it now runs `dune utop .` from the project root instead.
- [#100](https://github.com/bbatsov/neocaml/pull/100): Make the `dune-utop` flavor of both REPL backends run `dune utop` with `neocaml-dune-program` and `neocaml-dune-use-opam-exec`, and make `neocaml-repl-flavor` set to `dune-utop` (e.g. from `.dir-locals.el`) run `dune utop` from the project root instead of the plain toplevel.
- [#100](https://github.com/bbatsov/neocaml/pull/100): Make `neocaml-dune-project-root-files` pick the nearest directory containing any of the listed files, as documented, instead of trying the files in order and taking the first match at any depth, and honor it in library and package completion too.
- [#108](https://github.com/bbatsov/neocaml/pull/108): Fix font-lock in `neocaml-repl` buffers failing on every fontification, which kept prompts from being highlighted and the input from being fontified with tree-sitter.
- [#108](https://github.com/bbatsov/neocaml/pull/108): Keep the errors in the `neocaml-utop` transcript highlighted, instead of font-lock clearing their face as soon as it fontified them.
- [#108](https://github.com/bbatsov/neocaml/pull/108): Highlight the toplevel's errors and uncaught exceptions in `neocaml-repl` buffers, where errors got the function name face of `compilation-shell-minor-mode` instead, and its warnings and alerts in their current form (`Warning 26 [unused-var]:`).
- [#109](https://github.com/bbatsov/neocaml/pull/109): Don't take `//toplevel//`, the toplevel's name for its input, for a file in OCaml compilation messages, so the backtrace frames that mention it aren't listed as errors.
- [#109](https://github.com/bbatsov/neocaml/pull/109): Make the file locations in the toplevel's errors, warnings and backtraces clickable in `neocaml-repl` buffers, using the regexp of compilation buffers instead of the REPL's own, which matched nothing the toplevel prints.

## 0.10.0 (2026-07-10)

### New features

- Add `neocaml-utop`, an alternative toplevel backend that drives utop through its native editor protocol (`utop -emacs`). It runs in a comint-based transcript with input history and completion-at-point backed by utop's own engine, separates output/results/errors/warnings, and underlines the exact sub-expression a parse or type error points at in the source buffer (with the error message as the overlay's tooltip; the underline clears when you edit the code). Source evaluations cover every `;;`-separated phrase in the region, and the result is shown both in the minibuffer (SLIME/CIDER style; toggle with `neocaml-utop-echo-eval-result`) and inline in the source buffer as a `=> ...` overlay (toggle with `neocaml-utop-inline-eval-result`). In the transcript, `RET` submits a complete phrase and otherwise inserts a newline, so multi-line phrases can be entered naturally, and typed input is fontified with tree-sitter (toggle with `neocaml-utop-fontify-input`). Enable `neocaml-utop-minor-mode` in OCaml buffers to use it; the keybindings mirror `neocaml-repl-minor-mode`.

## 0.9.0 (2026-06-24)

### New features

- [#63](https://github.com/bbatsov/neocaml/pull/63): Flesh out the mode and REPL menus:
  - Add an "Edit" submenu (comment/uncomment region, fill comment paragraph, indent region).
  - Add an "OCaml REPL" menu to the REPL buffer (`neocaml-repl-mode`) for switching back to the source, interrupting, and clearing the buffer.
  - Add a "Toggle" submenu (prettify symbols, subword mode, outline minor mode) and a "Start/Switch to REPL" entry to the mode menu.
  - Add help strings (tooltips) and "Customize" entries to the menus, and disable REPL commands that need an active region or a running process when those aren't available.
- Add `neocaml-set-font-lock-level` and a "Font-Lock Level" submenu for switching the tree-sitter fontification level (1-4) in the current buffer.
- [#65](https://github.com/bbatsov/neocaml/pull/65): Add `neocaml-format-buffer` (bound to `C-c C-f`) to format OCaml source with `ocamlformat`, plus an opt-in `neocaml-format-on-save`.
- [#65](https://github.com/bbatsov/neocaml/pull/65): Make URLs and bug references in comments clickable across the OCaml source and tool modes via `goto-address-prog-mode` and `bug-reference-prog-mode`. Set `bug-reference-url-format` (e.g. via `.dir-locals.el`) to resolve issue references.
- [#65](https://github.com/bbatsov/neocaml/pull/65): Add `neocaml-repl-restart` to kill and restart the OCaml toplevel, with a matching entry in the REPL menu.
- [#68](https://github.com/bbatsov/neocaml/pull/68): Add `neocaml-repl-send-phrase-and-step` (bound to `C-c C-n`), which sends the phrase at point to the REPL and advances to the next one, and `neocaml-repl-require` for loading a findlib package via `#require`.
- [#69](https://github.com/bbatsov/neocaml/pull/69): Give each project its own dedicated REPL. The REPL buffer is now named per project (e.g. `*OCaml: myproject*`, derived from `neocaml-repl-buffer-name`), and the send commands route to the current buffer's project REPL, so multiple projects can run side by side. This also fixes `neocaml-dune-utop` so the send commands reach its toplevel.
- [#70](https://github.com/bbatsov/neocaml/pull/70): Add `neocaml-repl-flavor` for choosing which toplevel the REPL runs (`ocaml`, `utop`, or `dune-utop`); set it globally or per project via `.dir-locals.el`. The active flavor is shown in the REPL's mode line.
- [#71](https://github.com/bbatsov/neocaml/pull/71): Integrate with `project.el`: a directory containing a `dune-project` file is recognized as a project root (for non-VC projects), `_build/` and `_opam/` are ignored, and `compile-command` defaults to `dune build` in dune projects.
- [#72](https://github.com/bbatsov/neocaml/pull/72): Add `completion-at-point` support to `neocaml-dune-mode` and `neocaml-opam-mode`. dune files complete stanza names, the field names valid for the enclosing stanza, and library names inside `libraries`/`pps` fields (both the project's own libraries and the installed findlib libraries); opam files complete field and section names and, inside `depends`/`depopts`/`conflicts`, package names (via `opam list`). The external candidate sources are gathered from the project root, so a project-local opam switch (`_opam/`) is detected and used automatically (via `opam exec --`); they are cached per project and can be toggled with `neocaml-dune-complete-libraries` and `neocaml-opam-complete-packages`.

### Bug fixes

- [#66](https://github.com/bbatsov/neocaml/pull/66): Correctly handle character literals and quoted strings at the syntactic layer via a `syntax-propertize-function`. Characters like `'"'`, `'('`, and `')'`, and the contents of `{|...|}` / `{id|...|id}` quoted strings, no longer confuse sexp motion, `electric-pair-mode`, `delete-pair`, or `syntax-ppss`.

### Changes

- Switch the `ocaml` and `ocaml-interface` grammars to tree-sitter-ocaml v0.25.0, pinned to the upstream `v0.25.0-abi14` tag (an ABI 14 regeneration of the same grammar, since v0.25.0 itself generates an ABI 15 parser most Emacs builds can't load yet). This brings in the OCaml 5.5 grammar support.
- [#60](https://github.com/bbatsov/neocaml/issues/60): Highlight function-typed `val` specifications in `.mli` files with the function face, matching how function `let` bindings are highlighted in implementations.
- Handle OCaml 5.5 external type declarations (`type t = external "caml_foo"`) in sexp navigation, so `C-M-f`/`C-M-b` step over the whole declaration body instead of just the `external` keyword.

## 0.8.1 (2026-05-13)

### Bug fixes

- [#57](https://github.com/bbatsov/neocaml/issues/57): Stop passing `-emacs` to utop in `neocaml-dune-utop` (and drop it from the recommended config in the docs). That flag activates utop's structured protocol intended for the old `utop.el`, which `neocaml-repl` doesn't implement, so the protocol output was leaking into the REPL buffer.
- [#58](https://github.com/bbatsov/neocaml/issues/58): `neocaml-backward-up-list` now jumps out of optional parameters with defaults (e.g., `?(foo = 123)`) instead of erroring with "At top level".
- [#53](https://github.com/bbatsov/neocaml/issues/53): Capture stderr separately when running `dune format-dune-file`, so the `Entering directory` / `Leaving directory` markers newer dune versions emit on stderr no longer wrap the formatted buffer.

### Changes

- [#42](https://github.com/bbatsov/neocaml/issues/42): Lower the tree-sitter ABI requirement from 15 to 14 across `neocaml-opam-mode`, `neocaml-dune-mode`, and `neocaml-ocamllex-mode`, so they work on Emacs built against tree-sitter 0.24 (e.g., the homebrew Emacs 30 on macOS). Switch the menhir recipe to [tmcgilchrist/tree-sitter-menhir](https://github.com/tmcgilchrist/tree-sitter-menhir) and pin the ocamllex recipe back to v0.24.0, both of which target ABI 14.

## 0.8.0 (2026-04-10)

### New features

- [#47](https://github.com/bbatsov/neocaml/issues/47): Add `neocaml-backward-up-list`, bound to `C-M-u`, for jumping out of the enclosing OCaml block (`struct`/`sig`/`object`, records, arrays, etc.). The built-in `backward-up-list` doesn't understand keyword-delimited constructs on Emacs 29/30.
- [#41](https://github.com/bbatsov/neocaml/issues/41): `neocaml-dune-mode` now activates for `dune-workspace` file variants like `dune-workspace.ci` and `dune-workspace.5.3`.
- Add `neocaml-cram-mode` for editing cram test (`.t`) files with tree-sitter font-lock, indentation, and imenu.
- Add `neocaml-dune-format-buffer` for formatting dune files via `dune format-dune-file`.
- Register neocaml modes with `dape` for `ocamlearlybird` debugging support.
- Register file associations for `.ocamlinit`, `.ocamlformat`, and `.ocp-indent` files.
- Extend `neocaml-opam-mode` to activate for `.opam.template` files.
- Include per-grammar ABI version in `neocaml-bug-report-info` output.

### Changes

- The `_build/` directory redirect is now optional (controlled by `neocaml-redirect-build-files`).

## 0.7.1 (2026-03-31)

### Bug fixes

- Fix malformed `eglot-server-programs` entry that prevented `eglot-ensure` from starting `ocamllsp` for neocaml modes.

## 0.7.0 (2026-03-31)

### New features

- [#36](https://github.com/bbatsov/neocaml/issues/36): Add `neocaml-ocamllex-mode` for editing OCamllex (`.mll`) files with tree-sitter font-lock, indentation, imenu, and defun navigation. Embedded OCaml code inside `{ }` blocks gets full syntax highlighting via language injection when the OCaml grammar is installed. Based on the [tree-sitter-ocamllex](https://github.com/314eter/tree-sitter-ocamllex) grammar.
- [#36](https://github.com/bbatsov/neocaml/issues/36): Add `neocaml-menhir-mode` for editing Menhir (`.mly`) files with tree-sitter font-lock, indentation, imenu, and defun navigation. Embedded OCaml code inside `{ }` and `%{ %}` blocks gets full syntax highlighting via language injection. Based on the [tree-sitter-menhir](https://github.com/Kerl13/tree-sitter-menhir) grammar.
- Register `neocaml-mode` and `neocaml-interface-mode` with `eglot-server-programs` so `eglot-ensure` works out of the box with `ocamllsp`.

### Bug fixes

- [#37](https://github.com/bbatsov/neocaml/issues/37): Guard ABI 15 grammars (ocamllex, menhir) on Emacs 30+ and include ABI version in `neocaml-bug-report-info`.
- Language injection in ocamllex and menhir modes now requires Emacs 30+ (injection queries are not supported on Emacs 29).

## 0.6.0 (2026-03-25)

### Bug fixes

- [#34](https://github.com/bbatsov/neocaml/issues/34): Fix indentation of continuation lines inside multi-line comments. Lines now align with the body text after the opening delimiter.

### New features

- Add `neocaml-dune-mode` for editing dune, dune-project, and dune-workspace files with tree-sitter font-lock, indentation, imenu, and defun navigation. Based on the [tree-sitter-dune](https://github.com/tmcgilchrist/tree-sitter-dune) grammar.
- Add `neocaml-opam-mode` for editing opam package files with tree-sitter font-lock, indentation, and imenu. Based on the [tree-sitter-opam](https://github.com/tmcgilchrist/tree-sitter-opam) grammar.
- Add `neocaml-dune-interaction-mode`, a minor mode for running dune commands (build, test, clean, promote, fmt, utop, exec) from any neocaml buffer via `compile`. Includes watch mode support via prefix argument and a Dune menu.
- Add flymake backend for `opam lint` in `neocaml-opam-mode`. Enabled by default when the `opam` executable is found.
- Add tree-sitter font-locking for REPL input via `comint-fontify-input-mode`. Code typed in the REPL now gets the same syntax highlighting as regular `.ml` buffers. Controlled by `neocaml-repl-fontify-input` (default `t`).

## 0.5.0 (2026-03-16)

### Bug fixes

- [#26](https://github.com/bbatsov/neocaml/issues/26): Preserve list items and odoc tags as paragraph boundaries when filling comments.
- [#27](https://github.com/bbatsov/neocaml/issues/27): `neocaml-install-grammars` now accepts a prefix argument (`C-u`) to force reinstallation of grammars, even if they are already installed.
- Avoid the superfluous spaces after the prompt of the REPL when sending code to
  the REPL via the commands `neocaml-repl-send-*`.
- [#28](https://github.com/bbatsov/neocaml/issues/28): Fix `delete-pair` deleting the wrong closing delimiter. Add a `list` thing to `treesit-thing-settings` and a hybrid `forward-sexp` that falls back to syntax-table matching on delimiter characters.

### New features

- Support `outline-minor-mode` for folding top-level definitions (Emacs 30+).
- Support `which-func-mode` for displaying the current definition name in the mode line.
- Add `neocaml-objinfo-mode` for viewing OCaml compiled artifacts (`.cmi`, `.cmo`, `.cmx`, `.cma`, `.cmxa`, `.cmxs`, `.cmt`, `.cmti`) via `ocamlobjinfo`. Includes font-lock, imenu navigation, and revert support.
- Set `treesit-primary-parser` for Emacs 31+ compatibility.

## 0.4.1 (2026-03-10)

### Bug fixes

- [#24](https://github.com/bbatsov/neocaml/issues/24): Fix grammar compatibility check always warning even with up-to-date grammars. `treesit-query-compile` doesn't validate field names, so the check now uses `treesit-node-child-by-field-name` on an actual parse tree instead.

## 0.4.0 (2026-03-10)

### Bug fixes

- [#20](https://github.com/bbatsov/neocaml/issues/20): Work around broken `transpose-sexps` on Emacs 30 (bug#60655). Falls back to default transpose behavior; Emacs 31 has a proper fix.
- [#22](https://github.com/bbatsov/neocaml/issues/22): Fix compilation regexp to handle arbitrary leading whitespace in OCaml error messages.
- [#22](https://github.com/bbatsov/neocaml/issues/22): Fix off-by-one in compilation column positions. OCaml uses 0-indexed columns; the begin-column is now correctly converted to Emacs's 1-indexed columns.

### Changes

- Bump required tree-sitter-ocaml grammar from v0.24.0 to v0.24.2. **Users must reinstall their grammars** via `M-x neocaml-install-grammars`. The upstream release includes breaking changes to the parse tree structure (see [tree-sitter-ocaml#126](https://github.com/tree-sitter/tree-sitter-ocaml/issues/126)).
- neocaml now warns at startup if the installed grammar is older than expected.
- Reorganize font-lock feature levels to align with Emacs conventions: `type` moved to level 2, `number` moved to level 3, `escape-sequence` split into its own feature at level 3, and `property` and `label` split into their own features at level 4.

### New features

- Add `neocaml-mark-sentence` command to mark the current statement around point.
- Add `neocaml-bug-report-info` command for collecting debug information in bug reports.
- Add "Navigate" submenu to the OCaml menu with structural navigation commands.
- Add mark and transpose commands to the OCaml menu.
- Highlight escape sequences (`\n`, `\t`, etc.) in strings with `font-lock-escape-face`.
- Highlight conversion specifications (`%d`, `%s`, etc.) in format strings with `font-lock-regexp-face`.
- Highlight `match+` and similar binding operators as keywords in match expressions.
- [#23](https://github.com/bbatsov/neocaml/issues/23): Add `iarray` to the list of builtin types.

## 0.3.0 (2026-02-26)

### Bug fixes

- Fix `M-q` (`fill-paragraph`) not indenting continuation lines in comments.
- Fix `M-;` (`comment-dwim`) failing to remove `*)` when uncommenting a region.

### New features

- Add `comment-indent-new-line` support: `M-j` inside comments continues the comment with proper indentation.
- Highlight binding operators (`let*`, `let+`, `and*`, `and+`) as keywords.
- Add `electric-indent-chars` for `{}()` so `electric-indent-mode` reindents after typing delimiters.
- Add `fill-paragraph` support for OCaml `(* ... *)` comments via tree-sitter.
- Document `outline-minor-mode` and `treesit-fold` for code folding in README.

## 0.2.0 (2026-02-17)

### Bug fixes

- Fix `compile-goto-error` landing one column before the actual error position.  OCaml uses 0-indexed columns; `compilation-first-column` is now set to 0 accordingly.
- Fix `neocaml-repl-send-definition` signaling an error when point is not inside a definition.
- Fix `;;` terminator detection: only check whether input ends with `;;` instead of searching anywhere in the string, avoiding false positives from `;;` inside strings or comments.
- Fix `neocaml-repl-send-phrase` to skip `;;` inside strings and comments when locating phrase boundaries.

### New features

- Add `neocaml-repl-load-file` (`C-c C-l`): load the current file into the REPL via the `#use` directive.
- Add REPL input history persistence across sessions via `neocaml-repl-history-file` and `neocaml-repl-history-size`.
- Flash the sent region when evaluating code in the REPL (`send-region`, `send-definition`, `send-phrase`, `send-buffer`).

### Changes

- Introduce `neocaml-base-mode` as the shared parent for `neocaml-mode` and `neocaml-interface-mode`.  Users can hook into `neocaml-base-mode-hook` to configure both modes at once.
- Improve `utop` support: strip ANSI escape sequences and recognize utop's prompt format so point is correctly placed after the prompt.
- Make `C-c C-z` reversible: from a source buffer it switches to the REPL, from the REPL it switches back.
- Add `_build` directory awareness: when opening a file under `_build/`, offer to switch to the source copy (supports dune and ocamlbuild layouts).
- Split `neocaml-prettify-symbols-alist` into a column-width-safe base list and `neocaml-prettify-symbols-extra-alist` (`fun`->λ, `->`->→, `not`->¬).  Control extra symbols with the `neocaml-prettify-symbols-full` toggle.
- Register OCaml build artifact extensions (`.cmo`, `.cmx`, `.cmi`, etc.) in `completion-ignored-extensions` to declutter `find-file` completion.
- Bind `C-c C-c` to `compile` in `neocaml-mode` (shadowed by `neocaml-repl-send-definition` when the REPL minor mode is active).
- Extend `neocaml-other-file-alist` to support `.mll`, `.mly`, and `.eliom`/`.eliomi` file pairs for `ff-find-other-file`.
- Register OCaml compilation error regexp for `M-x compile` support (errors, warnings, alerts, backtraces).
- Add `treesit-thing-settings` for sexp, sentence, text, and comment navigation (Emacs 30+).
- Add sentence navigation (`M-a`/`M-e`) for moving between top-level definitions.
- `transpose-sexps` now works with tree-sitter awareness (Emacs 30+).
- Replace automatic grammar installation with the interactive command `M-x neocaml-install-grammars`.
- Remove `neocaml-ensure-grammars` defcustom.
- Remove `neocaml-use-prettify-symbols` and `neocaml-repl-use-prettify-symbols` defcustoms.  `prettify-symbols-alist` is now always set; users enable `prettify-symbols-mode` via hooks.

## 0.1.0 (2026-02-13)

Initial release.

### Features

- Tree-sitter based font-locking with 4 levels of highlighting for `.ml` and `.mli` files.
- Tree-sitter based indentation with cycle-indent support.
- Imenu integration with language-specific categories for `.ml` and `.mli`.
- Navigation support (`beginning-of-defun`, `end-of-defun`, `forward-sexp`).
- OCaml toplevel (REPL) integration via `neocaml-repl`.
- Automatic grammar installation via `treesit-install-language-grammar`.
- Switch between `.ml` and `.mli` files with `ff-find-other-file`.
- Prettify-symbols support for common OCaml operators.
- Eglot integration for LSP support (e.g. `ocamllsp`).
