# Troubleshooting

If you run into issues, `M-x neocaml-bug-report-info` collects useful debug
information (Emacs version, neocaml version, grammar status, Eglot status) and
copies it to the kill ring. Paste this into your bug report to help us
diagnose the problem faster.

## General debugging techniques

A few Emacs built-ins that help diagnose problems:

- **`*Messages*` buffer** - check for warnings and errors (`C-h e` to open it)
- **`M-x toggle-debug-on-error`** - get a backtrace when something goes wrong
- **`M-x treesit-explore-mode`** - inspect the syntax tree at point (useful for font-lock and indentation issues)
- **`M-x treesit-inspect-mode`** - show the node at point in the mode line

## Tree-sitter ABI version mismatch

If you see an error like:

```
Cannot activate tree-sitter, because language grammar for X is unavailable (version-mismatch): 15
```

This means the grammar requires tree-sitter ABI version 15 (tree-sitter
0.25.0+), but your Emacs was compiled against an older tree-sitter library that
only supports ABI 14. You can verify by evaluating `(treesit-library-abi-version)`
-- if it returns 14, that confirms the mismatch.

This isn't specific to neocaml -- any grammar targeting ABI 15 will fail the same
way. The fix is to rebuild Emacs against tree-sitter >= 0.25.0.

### macOS (Homebrew)

```sh
brew update && brew upgrade tree-sitter
brew reinstall emacs-plus  # or however you installed Emacs
```

Make sure the Homebrew tree-sitter is what Emacs links against (check with
`otool -L $(which emacs) | grep tree-sitter`).

### Linux

Distro packages for tree-sitter tend to lag behind. You may need to build
tree-sitter from source:

```sh
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make && sudo make install
sudo ldconfig
```

Then recompile Emacs so it picks up the new library.

## No syntax highlighting or "grammar unavailable" errors

If `.ml` files open without any font-lock, the tree-sitter grammars are
probably not installed. Run `M-x neocaml-install-grammars` to install
the OCaml and OCaml-interface grammars. For dune, opam, OCamllex, and
Menhir files, each mode has its own install command (see
[Installation](installation.md)).

## Conflicting mode from tuareg or caml-mode

If another OCaml major mode is installed, it may claim `.ml`/`.mli`
files before neocaml. The simplest fix is to remove the other package.
If you need to keep it around, make sure neocaml's `auto-mode-alist`
entries take priority by loading neocaml after the other package. See
[Migration](migration.md) for details.

## `ocamllsp` not found (macOS GUI Emacs)

When Emacs is launched from a desktop shortcut (e.g. Emacs.app) rather
than a terminal, it may not inherit your shell's `PATH`. This causes
Eglot to fail with "ocamllsp not found". The
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
package fixes this:

```emacs-lisp
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
```

## Stale byte-compiled files after upgrading

If neocaml behaves oddly after an upgrade (old features, missing
commands), Emacs might be loading stale `.elc` files. Look for the
warning "Source file newer than byte-compiled file" in `*Messages*`.
Force a recompile by reinstalling the package or, if you installed from
source, running `make clean && make compile`.
