# Changelog

## main (unreleased)

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
