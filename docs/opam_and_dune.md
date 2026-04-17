# opam, dune & Cram Support

neocaml includes dedicated tree-sitter modes for opam and dune files,
plus a mode for dune's cram test files.
`neocaml-opam-mode` activates automatically for `.opam`, `.opam.template`,
and bare `opam` files;
`neocaml-dune-mode` activates for `dune`, `dune-project`, and `dune-workspace`
files (including variants like `dune-workspace.ci`). Both provide font-lock,
indentation, and imenu.

## opam lint

`neocaml-opam-mode` registers an `opam lint` flymake backend so you get
inline diagnostics for missing fields, deprecated constructs, and syntax errors.
To enable it, activate `flymake-mode` in opam buffers:

```emacs-lisp
(add-hook 'neocaml-opam-mode-hook #'flymake-mode)
```

[flycheck](https://github.com/flycheck/flycheck) users get `opam lint` support
out of the box via flycheck's built-in `opam` checker.

## dune Commands

`neocaml-dune-interaction-mode` is a minor mode that provides keybindings for
running common dune commands from any neocaml buffer. All commands run via
`compile`, so you get error navigation (`M-g n` / `M-g p`), clickable source
locations, and the full compilation-mode interface.

Enable it in OCaml buffers:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'neocaml-dune-interaction-mode)
```

Or in all neocaml-related buffers (including dune and opam files):

```emacs-lisp
(dolist (hook '(neocaml-base-mode-hook
               neocaml-dune-mode-hook
               neocaml-opam-mode-hook))
  (add-hook hook #'neocaml-dune-interaction-mode))
```

Available commands (all under the `C-c C-d` prefix):

| Keybinding | Command | Description |
|---|---|---|
| `C-c C-d b` | `neocaml-dune-build` | Build the project |
| `C-c C-d t` | `neocaml-dune-test` | Run tests |
| `C-c C-d c` | `neocaml-dune-clean` | Clean build artifacts |
| `C-c C-d p` | `neocaml-dune-promote` | Promote test corrections |
| `C-c C-d f` | `neocaml-dune-fmt` | Format code |
| `C-c C-d u` | `neocaml-dune-utop` | Launch utop with project libraries |
| `C-c C-d r` | `neocaml-dune-exec` | Run an executable (prompts for name) |
| `C-c C-d d` | `neocaml-dune-command` | Run any dune command (with history) |
| `C-c C-d .` | `neocaml-dune-find-dune-file` | Find the nearest dune file |

With a prefix argument (`C-u`), build, test, and fmt commands run in **watch
mode** (`--watch`), rebuilding automatically when files change. For example,
`C-u C-c C-d b` runs `dune build --watch`.

The project root is determined by walking up from the current file to find
`dune-project`.

If Emacs doesn't inherit your opam environment (common when launching
from a desktop shortcut), you can prefix all dune commands with
`opam exec --`:

```emacs-lisp
(setq neocaml-dune-use-opam-exec t)
```

## dune File Formatting

`neocaml-dune-mode` can format the current buffer using `dune format-dune-file`.
Use `C-c C-f` (`neocaml-dune-format-buffer`) to format on demand, or enable
automatic formatting on save:

```emacs-lisp
(setq neocaml-dune-format-on-save t)
```

Note: this formats individual dune files via `dune format-dune-file`, which is
different from `dune fmt` (available via `C-c C-d f`) that formats the entire
project.

## Cram Tests

`neocaml-cram-mode` provides syntax highlighting for cram test (`.t`) files, as
used by dune's expect-test framework. It highlights shell commands, expected
output, output modifiers (`(re)`, `(glob)`, `(no-eol)`, `(esc)`), exit codes,
and prose comments.

The mode activates automatically for `.t` files. Use `dune promote` (available
via `C-c C-d p` with `neocaml-dune-interaction-mode`) to accept corrected test
output.

!!! note
    The `.t` extension is also used by Perl test files. If you work with
    both OCaml and Perl, you may need to override the association for Perl
    projects:

    ```emacs-lisp
    ;; Use cperl-mode for .t files in Perl projects
    (add-to-list 'auto-mode-alist '("/t/.*\\.t\\'" . cperl-mode))
    ```

    Entries added later to `auto-mode-alist` take priority, so the more specific
    pattern above will win for `.t` files under a `t/` directory (Perl convention).
