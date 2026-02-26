# neocaml

[![MELPA](https://melpa.org/packages/neocaml-badge.svg)](https://melpa.org/#/neocaml)
[![MELPA Stable](https://stable.melpa.org/packages/neocaml-badge.svg)](https://stable.melpa.org/#/neocaml)
[![CI](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml)
[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=github)](https://github.com/sponsors/bbatsov)

`neocaml` is a **n**ew **E**macs package for programming in OCaml.  It features
both a couple of major modes (for OCaml and OCaml Interface), using TreeSitter,
and integration with an OCaml toplevel (a.k.a. REPL).

It's also as cool as Neo from "The Matrix". ;-)

## Why?

Because `caml-mode` is ancient, and `tuareg-mode` is a beast. (it's very powerful, but also very complex)
The time seems ripe for a modern, leaner, TreeSitter-powered mode for
OCaml.

There have been two other attempts to create TreeSitter-powered
major modes for Emacs, but they didn't get very far:

- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode) (first one, available in MELPA)
- [ocaml-ts-mode](https://github.com/terrateamio/ocaml-ts-mode) (second one)

Looking at the code of both modes, I inferred that the authors were probably knowledgable in
OCaml, but not very familiar with Emacs Lisp and Emacs major modes in general.
For me it's the other way around, and that's what makes this a fun and interesting project for me:

- I enjoy working on Emacs packages
- I want to do more work with TreeSitter, now that it's getting more traction
- I really like OCaml and it's one of my favorite "hobby" languages

They say that third time's the charm, right?

One last thing - we really need more Emacs packages with fun names! :D

## Features

- Tree-sitter based font-locking (4 levels) for `.ml` and `.mli` files
- Tree-sitter based indentation with cycle-indent support
- Navigation (`beginning-of-defun`, `end-of-defun`, `forward-sexp`, sentence movement with `M-a`/`M-e`)
- Imenu with language-specific categories for `.ml` and `.mli`
- Toggling between implementation and interface via `ff-find-other-file` (`C-c C-a`)
- OCaml toplevel (REPL) integration (`neocaml-repl`)
- Comment support: `fill-paragraph` (`M-q`), comment continuation (`M-j`), and `comment-dwim` (`M-;`)
- Electric indentation on delimiter characters
- Easy installation of `ocaml` and `ocaml-interface` tree-sitter grammars via `M-x neocaml-install-grammars`
- Compilation error regexp for `M-x compile` (errors, warnings, alerts, backtraces)
- `_build` directory awareness (offers to switch to source when opening build artifacts)
- Eglot integration (with [ocaml-eglot](https://github.com/tarides/ocaml-eglot) support)
- Prettify-symbols for common OCaml operators

### Planned

- Integration with dune

## Installation

### MELPA

`neocaml` is available on [MELPA](https://melpa.org/#/neocaml). If you have
MELPA in your `package-archives`, install it with:

    M-x package-install <RET> neocaml <RET>

Or with `use-package`:

```emacs-lisp
(use-package neocaml
  :ensure t
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))
```

### From GitHub

You can install directly from the repository:

    M-x package-vc-install <RET> https://github.com/bbatsov/neocaml <RET>

Or with `use-package` on Emacs 30+:

```emacs-lisp
(use-package neocaml
  :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest)
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))
```

> [!NOTE]
> If the required tree-sitter grammars are not installed, run
> `M-x neocaml-install-grammars` to install them.

## Usage

The `neocaml` package bundles two major modes - one for OCaml code
and one for OCaml interfaces (`.mli`). Both modes will be auto-enabled
when you open the respective type of files.

You can use `C-c C-a` to toggle between implementation and interface files.

To use `neocaml` with Eglot, register the modes with `ocamllsp` as shown
in the installation examples above.

> [!NOTE]
> neocaml sets the `eglot-language-id` symbol property on both modes
> (`"ocaml"` for `.ml` and `"ocaml.interface"` for `.mli`), so the correct
> language IDs are sent to the server automatically.

### ocaml-eglot

[ocaml-eglot](https://github.com/tarides/ocaml-eglot) is a lightweight minor
mode that enhances the Eglot experience for OCaml by exposing custom LSP
requests from `ocamllsp` — type enclosing, case analysis, hole navigation, and
more. It works with neocaml out of the box:

```emacs-lisp
(use-package ocaml-eglot
  :ensure t
  :hook
  (neocaml-base-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))
```

## Configuration

The modes provide 4 levels of font-locking, as is the standard for TreeSitter-powered
modes. The default font-locking level in Emacs is 3, and you can change it like this:

```emacs-lisp
;; this font-locks everything neocaml supports
(setq treesit-font-lock-level 4)
```

See the documentation for `treesit-font-lock-level` and `treesit-font-lock-features` for more details.

You can "prettify" certain symbols (see `neocaml-prettify-symbols-alist`) by
enabling `prettify-symbols-mode` via a hook:

```emacs-lisp
;; Enable for both .ml and .mli files at once
(add-hook 'neocaml-base-mode-hook #'prettify-symbols-mode)
```

When it comes to indentation you've got several options:

- Using the built-in TreeSitter indentation
  - Supports `let` bindings, `let...in` chains, `match`/`try` expressions, `if`/`then`/`else`, variant and record types, modules, signatures, loops, `fun`/`function` expressions, lists, arrays, sequences, and more
  - It still needs some work, so it might not always behave the way you'd like it to
- Use the built-in Emacs function `indent-relative` that simply indents the next line relative to the previous line and allows you manually indent/outdent further. Very simple, but kind of bullet-proof.
- Use the indent function of `ocp-indent.el` (this requires for you to have `ocp-indent.el` and `ocp-indent` installed)
- Use the indent function of Tuareg.

You can change the indentation function used by Neocaml like this:

```emacs-lisp
(defun my-neocaml-mode-setup ()
  "Set up my custom indentation for neocaml-mode."
  (setq-local indent-line-function 'indent-relative))

;; Use neocaml-base-mode-hook to apply to both .ml and .mli files
(add-hook 'neocaml-base-mode-hook 'my-neocaml-mode-setup)
```

### Comments

OCaml uses block comments `(* ... *)` exclusively (no line comments), which
requires some mode-level configuration for Emacs comment commands to work well.
neocaml sets all the necessary variables internally, so everything works out of
the box:

- **`M-;`** (`comment-dwim`) -- comments/uncomments regions, inserts inline
  comments, etc. Works as expected with OCaml's `(* ... *)` delimiters.
- **`M-j`** (`default-indent-new-line`) -- inside a comment, inserts a newline
  and indents the continuation line to align with the comment body text. For
  example, pressing `M-j` inside `(** doc text` produces a new line indented to
  column 4 (after `(** `), keeping the comment open rather than closing and
  reopening it.
- **`M-q`** (`fill-paragraph`) -- refills the current comment, wrapping text at
  `fill-column` with continuation lines properly indented to the body column.

These commands respect both regular comments (`(* ... *)`) and doc comments
(`(** ... *)`), and work correctly for indented comments.

### Code Folding

On Emacs 30+, `outline-minor-mode` works out of the box with neocaml -- it
automatically picks up definition headings from the tree-sitter imenu settings.
Enable it via a hook:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'outline-minor-mode)
```

For tree-sitter-aware code folding (fold any node, not just top-level
definitions), see [treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold).

## Toplevel (REPL) Integration

`neocaml` provides integration with the OCaml toplevel (REPL). This allows you to evaluate OCaml code directly from your source buffer and see the results.

You can also start an OCaml REPL (toplevel) and interact with it using
`neocaml-repl-minor-mode`. You can enable the mode like this:

```emacs-lisp
;; Enable for both .ml and .mli files at once
(add-hook 'neocaml-base-mode-hook #'neocaml-repl-minor-mode)
```

If you're using `use-package` you'd probably do something like:

```emacs-lisp
(use-package neocaml
  :vc (:url "https://github.com/bbatsov/neocaml" :rev :newest)
  :config
  (add-hook 'neocaml-base-mode-hook #'neocaml-repl-minor-mode)
  ;; other config options...
  )
```

The following commands are available for interacting with the OCaml toplevel:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-z` | `neocaml-repl-switch-to-repl` | Start OCaml REPL or switch to it if already running |
| `C-c C-c` | `neocaml-repl-send-definition` | Send the current definition to the REPL |
| `C-c C-r` | `neocaml-repl-send-region` | Send the selected region to the REPL |
| `C-c C-b` | `neocaml-repl-send-buffer` | Send the entire buffer to the REPL |
| `C-c C-l` | `neocaml-repl-load-file` | Load the current file into the REPL via `#use` |
| `C-c C-p` | `neocaml-repl-send-phrase` | Send the current phrase (code up to next `;;`) to the REPL |
| `C-c C-i` | `neocaml-repl-interrupt` | Interrupt the current evaluation in the REPL |
| `C-c C-k` | `neocaml-repl-clear-buffer` | Clear the REPL buffer |

### Configuration

You can customize the OCaml REPL integration with the following variables:

```emacs-lisp
;; Add command-line arguments to the default OCaml toplevel
(setq neocaml-repl-program-args '("-short-paths" "-color=never"))

;; Change the REPL buffer name
(setq neocaml-repl-buffer-name "*OCaml-REPL*")
```

#### Using utop instead of the default OCaml toplevel

[utop](https://github.com/ocaml-community/utop) is an improved toplevel for OCaml with many features like auto-completion, syntax highlighting, and a rich history. To use utop with neocaml-repl:

```emacs-lisp
(setq neocaml-repl-program-name "utop")
(setq neocaml-repl-program-args '("-emacs"))
```

> [!NOTE]
> If you launch Emacs from a desktop shortcut (e.g. Emacs.app on macOS) rather
> than a terminal, your shell's `PATH` may not be inherited. This can cause
> `utop` or `ocaml` to not be found. The
> [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
> package is the usual fix for this.

## Comparison with Other Modes

People love comparisons, so here's a comparison of neocaml with its main historical
alternatives.

### Feature Overview

| Feature                    | neocaml                    | caml-mode     | tuareg-mode  |
|----------------------------|----------------------------|---------------|--------------|
| Required Emacs version     | 29.1+ (30+ recommended)    | 24+           | 26+          |
| Font-lock                  | Tree-sitter (4 levels)     | Regex         | Regex        |
| Indentation                | Tree-sitter + cycle-indent | Custom engine | SMIE         |
| REPL integration           | Yes                        | Yes           | Yes          |
| Navigation (defun, sexp)   | Yes                        | Yes           | Yes          |
| Imenu                      | Yes (.ml and .mli)         | Yes           | Yes          |
| .ml/.mli toggle            | Yes                        | Yes           | Yes          |
| LSP (Eglot) integration    | Yes                        | Manual setup  | Manual setup |
| Debugger (ocamldebug)      | No                         | Yes           | Yes          |
| Compilation commands       | Error regexp + C-c C-c     | Yes           | Yes          |
| `_build` directory aware   | Yes                        | No            | Yes          |
| Menhir / opam support      | No                         | No            | Yes          |
| Code templates / skeletons | No                         | Yes           | Yes          |

Keep in mind also that `tuareg-mode` uses `caml-mode` internally for some functionality.
I think both modes will probably be folded into one down the road.

### The impact of LSP on major modes

Historically, caml-mode and tuareg bundled features like type display,
completion, jump-to-definition, error checking, and refactoring support
-- all driven by Merlin.  Today, `ocamllsp` provides all of these through
the standard LSP protocol, and Eglot (built into Emacs 29+) acts as the
client.  There is no reason for a major mode to reimplement any of this.

For OCaml-specific LSP extensions that go beyond the standard protocol
-- type enclosing, case analysis (destruct), hole navigation --
[ocaml-eglot](https://github.com/tarides/ocaml-eglot) is the recommended
companion package.

### Why TreeSitter?

Tree-sitter provides incremental, error-tolerant parsing that is
significantly faster and more accurate than regex-based font-lock and
SMIE-based indentation.  It parses the full syntax tree, so fontification
and indentation rules can reference actual language constructs rather
than fragile regular expressions.  This results in fewer edge-case bugs
and simpler, more maintainable code.

## FAQ

### Why doesn't `(` automatically insert `(* *)` inside comments?

neocaml does not implement electric comment delimiters (tuareg does, but the
logic is quite complex). Instead, use `M-;` (`comment-dwim`) to insert comment
delimiters -- it will insert `(* *)` with point positioned between them,
properly indented. This is simpler and more predictable.

## Funding

While neocaml is free software and will always be, the project would benefit
from some funding. Consider supporting its ongoing development if you find it
useful.

**Please consider [supporting financially neocaml's development](#funding).**

You can support the development of neocaml via:

- [GitHub Sponsors](https://github.com/sponsors/bbatsov)
- [Patreon](https://www.patreon.com/bbatsov)
- [PayPal](https://www.paypal.me/bbatsov)

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, debugging, and
how to run tests. Architecture and design notes live in [doc/DESIGN.md](doc/DESIGN.md).

## License

Copyright © 2025-2026 Bozhidar Batsov and [contributors](https://github.com/bbatsov/neocaml/graphs/contributors).

Distributed under the GNU General Public License, version 3 or later. See [LICENSE](LICENSE) for details.
