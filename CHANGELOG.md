# Changelog

## main (unreleased)

### Bug fixes

- [#34](https://github.com/bbatsov/neocaml/issues/34): Fix indentation of continuation lines inside multi-line comments. Lines now align with the body text after the opening delimiter.

### New features

- Add `neocaml-opam-mode` for editing opam package files with tree-sitter font-lock, indentation, and imenu. Based on the [tree-sitter-opam](https://github.com/tmcgilchrist/tree-sitter-opam) grammar.
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
- Fix `M-;` (`comment-dwim`) failing to remove ` *)` when uncommenting a region.

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
