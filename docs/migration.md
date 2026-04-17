# Migrating from tuareg / caml-mode

If you're switching to neocaml from tuareg or caml-mode, here's what you need
to know.

## File associations

neocaml automatically registers `.ml` and `.mli` files via `auto-mode-alist` --
you don't need to add any file associations yourself. Importantly, neocaml uses
**two separate modes**: `neocaml-mode` for `.ml` files (using the `ocaml`
tree-sitter grammar) and `neocaml-interface-mode` for `.mli` files (using the
`ocaml-interface` grammar). Both must be mapped correctly for font-lock and
indentation to work.

!!! warning
    If you previously used caml-mode or tuareg, **remove any manual
    `auto-mode-alist` entries** for OCaml files from your config. A common
    caml-mode pattern like:

    ```emacs-lisp
    (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
    ```

    maps `.mli` files to a single mode. Replacing `caml-mode` with
    `neocaml-mode` in such an entry will break `.mli` files because they
    need `neocaml-interface-mode`, not `neocaml-mode`. The simplest fix is
    to delete the entry entirely and let neocaml's autoloads handle it.

If you need to keep tuareg or caml-mode installed alongside neocaml,
make sure neocaml loads last so its `auto-mode-alist` entries win:

```emacs-lisp
(use-package neocaml
  :ensure t
  :after tuareg)  ; loads after tuareg, so neocaml's auto-mode-alist entries win
```

### Unsupported file types

caml-mode and tuareg also handle `.mll` (ocamllex), `.mly` (ocamlyacc/menhir),
and `.mlp` (camlp4/camlp5) files. neocaml now supports `.mll` files via
`neocaml-ocamllex-mode` (with full OCaml syntax highlighting inside `{ }`
blocks via language injection), and `.mly` files via `neocaml-menhir-mode`
(also with OCaml injection). The language injection requires Emacs 30+; on
Emacs 29 the modes still work but embedded OCaml code is not fontified.
`.mlp` (camlp4/camlp5) files are not supported.

### Using the legacy dune-mode

neocaml provides its own `neocaml-dune-mode` for editing dune files. If you
prefer to keep using the `dune-mode` from the
[dune](https://melpa.org/#/dune) package, override the `auto-mode-alist`
entries after neocaml loads:

```emacs-lisp
(with-eval-after-load 'neocaml-dune
  (add-to-list 'auto-mode-alist '("/dune\\'" . dune-mode))
  (add-to-list 'auto-mode-alist '("/dune-project\\'" . dune-mode))
  (add-to-list 'auto-mode-alist '("/dune-workspace\\'" . dune-mode)))
```

The `neocaml-dune-interaction-mode` (build/test/clean commands) works with
either major mode -- it doesn't depend on `neocaml-dune-mode`.

## Keybinding differences

Most REPL keybindings (`C-c C-z`, `C-c C-r`, `C-c C-b`, `C-c C-l`)
work the same way across all three modes. The table below lists the
bindings that actually differ.

| Keybinding | neocaml | tuareg / caml-mode |
|---|---|---|
| `C-c C-c` | `compile` (or `neocaml-repl-send-definition` with REPL minor mode) | Eval phrase (caml-mode) |
| `C-c C-a` | Toggle .ml/.mli | Not bound |
| `C-c C-p` | Send phrase to REPL | Not bound |
| `C-c C-i` | Interrupt REPL | Not bound |
| `C-c C-k` | Clear REPL buffer | Not bound |

!!! note
    neocaml's REPL keybindings require enabling
    `neocaml-repl-minor-mode`. See [REPL Integration](repl.md).

## Merlin vs Eglot

Merlin works fine with neocaml -- it's a minor mode that hooks into any major
mode. That said, you might want to consider switching to Eglot + `ocamllsp`,
which provides the same features (completion, type display, jump-to-definition)
via the standard LSP protocol. Eglot is built into Emacs 29+ and neocaml
includes the necessary configuration out of the box. See the
[ocaml-eglot](usage.md#ocaml-eglot) section for OCaml-specific extensions like type
enclosing and case analysis.

## Keeping tuareg's indentation

If you prefer tuareg's SMIE-based indentation over neocaml's tree-sitter
indentation, you can use it directly -- see the
[Indentation](configuration.md#indentation) section for the setup snippet.

## What you gain

- Tree-sitter powered font-locking (more accurate, 4 configurable levels)
- Tree-sitter powered navigation (sexp, sentence, defun)
- Built-in Eglot integration with automatic language ID configuration
- `_build` directory awareness
- Simpler, more maintainable codebase

## What you lose

- `ocamldebug` integration (neocaml does not include a debugger frontend, but you can use [dape](https://github.com/svaante/dape) with [ocamlearlybird](https://github.com/hackwaly/ocamlearlybird) instead - see [Debugging](debugging.md))
- `.mlp` (camlp4/camlp5) file support
- Electric comment delimiters (`(` inserting `(* *)` inside comments)
- Code templates / skeletons
