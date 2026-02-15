# Changelog

## main (unreleased)

### Changes

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
