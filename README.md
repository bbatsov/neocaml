# neocaml

[![MELPA](https://melpa.org/packages/neocaml-badge.svg)](https://melpa.org/#/neocaml)
[![MELPA Stable](https://stable.melpa.org/packages/neocaml-badge.svg)](https://stable.melpa.org/#/neocaml)
[![CI](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml)
[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=github)](https://github.com/sponsors/bbatsov)

`neocaml` is a **n**ew **E**macs package for programming in OCaml.  It features
two major modes (for OCaml and OCaml Interface), using Tree-sitter,
and integration with an OCaml toplevel (a.k.a. REPL).

It's also as cool as Neo from "The Matrix". ;-)

## Why?

Because `caml-mode` is ancient, and `tuareg`, while very powerful, has
grown complex over the years. The time seems ripe for a modern, leaner,
tree-sitter-powered mode for OCaml.

There have been two earlier attempts at tree-sitter OCaml modes for Emacs:

- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- [ocaml-ts-mode](https://github.com/terrateamio/ocaml-ts-mode)

Both provided useful early exploration of what a tree-sitter OCaml mode could
look like, and helped inspire this project. neocaml aims to take the idea
further with a more complete feature set and active maintenance.

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

> [!TIP]
> If you have another OCaml major mode installed (e.g. `tuareg` or `caml-mode`),
> consider removing it to avoid conflicts over `.ml` and `.mli` file
> associations. See [Migrating from tuareg / caml-mode](#migrating-from-tuareg--caml-mode)
> for details.

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

### Compilation

`C-c C-c` runs `M-x compile`, and neocaml registers an OCaml-specific error
regexp so that `next-error` (`M-g n`) and `previous-error` (`M-g p`) jump
directly to the source locations reported by the OCaml compiler, including
errors, warnings, alerts, and exception backtraces.

## Navigation

neocaml uses tree-sitter to power all structural navigation commands. These are
standard Emacs keybindings, but backed by the AST rather than heuristics:

| Keybinding | Command | Description |
|---|---|---|
| `C-M-a` | `beginning-of-defun` | Move to the beginning of the current definition |
| `C-M-e` | `end-of-defun` | Move to the end of the current definition |
| `C-M-f` | `forward-sexp` | Move forward over a balanced expression |
| `C-M-b` | `backward-sexp` | Move backward over a balanced expression |
| `M-a` | `backward-sentence` | Move to the beginning of the current statement (Emacs 30+) |
| `M-e` | `forward-sentence` | Move to the end of the current statement (Emacs 30+) |

"Definitions" include `let` bindings, type definitions, module bindings, class
definitions, exceptions, and externals. "Statements" cover the same plus
`open`, `include`, and expression items -- essentially any top-level or
block-level construct.

All navigation commands are also available from the OCaml menu under "Navigate".

## Configuration

### Font-locking

neocaml provides 4 levels of font-locking, as is the standard for Tree-sitter
modes. The default level in Emacs is 3, and you can change it like this:

```emacs-lisp
;; this font-locks everything neocaml supports
(setq treesit-font-lock-level 4)
```

The font-lock features available at each level are:

| Level | Features                                                       | What they cover                                                            |
|-------|----------------------------------------------------------------|----------------------------------------------------------------------------|
| 1     | `comment`, `definition`                                        | Comments, doc comments, let/val/type/method bindings, value patterns       |
| 2     | `keyword`, `string`, `number`                                  | Language keywords, strings, characters, escape sequences, format specs     |
| 3     | `attribute`, `builtin`, `constant`, `type`                     | PPX attributes/extensions, builtin ids/types, `true`/`false`/`()`, type names, modules, constructors |
| 4     | `operator`, `bracket`, `delimiter`, `variable`, `function`     | Operators, brackets, `,`/`;`/`.`, value names, labels, function calls     |

#### Selecting features

You don't have to use the level system. If you want fine-grained control over
what gets highlighted, you can cherry-pick individual features using
`treesit-font-lock-recompute-features`:

```emacs-lisp
(defun my-neocaml-font-lock-setup ()
  (treesit-font-lock-recompute-features
   ;; enable these features
   '(comment definition keyword string number
     attribute builtin constant type operator variable)
   ;; disable these features
   '(bracket delimiter function)))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-font-lock-setup)
```

This gives you operators and variables without bracket and delimiter noise, for
example. You can also call `M-x treesit-font-lock-recompute-features`
interactively to toggle features in the current buffer.

#### Customizing faces

The faces used are standard `font-lock-*-face` faces, so any theme applies
automatically. You can customize individual faces to change how specific
syntactic elements look:

```emacs-lisp
;; Use a custom color for type names
(custom-set-faces
 '(font-lock-type-face ((t (:foreground "DarkSeaGreen4")))))
```

#### Adding custom font-lock rules

For distinctions that neocaml doesn't make by default (e.g. highlighting block
keywords differently from control flow keywords), you can layer additional
Tree-sitter font-lock rules via a hook:

```emacs-lisp
(defface my-ocaml-block-keyword-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for OCaml block-delimiting keywords.")

(defun my-neocaml-block-keywords ()
  (setq treesit-font-lock-settings
        (append treesit-font-lock-settings
                (treesit-font-lock-rules
                 :language (treesit-parser-language
                            (car (treesit-parser-list)))
                 :override t
                 :feature 'keyword
                 '(["begin" "end" "struct" "sig" "object"
                    "do" "done" "fun" "function"]
                   @my-ocaml-block-keyword-face))))
  (treesit-font-lock-recompute-features))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-block-keywords)
```

The rules use standard Tree-sitter query syntax with `:override t` to take
precedence over neocaml's built-in rules. You can target any node type the
grammar produces -- use `M-x treesit-explore-mode` to inspect the syntax tree
and find the right node types to match.

#### Prettify Symbols

You can "prettify" certain symbols (see `neocaml-prettify-symbols-alist`) by
enabling `prettify-symbols-mode` via a hook:

```emacs-lisp
;; Enable for both .ml and .mli files at once
(add-hook 'neocaml-base-mode-hook #'prettify-symbols-mode)
```

### Indentation

When it comes to indentation you've got several options:

- Using the built-in Tree-sitter indentation
  - Supports `let` bindings, `let...in` chains, `match`/`try` expressions, `if`/`then`/`else`, variant and record types, modules, signatures, loops, `fun`/`function` expressions, lists, arrays, sequences, and more
  - It still needs some work, so it might not always behave the way you'd like it to
- Use the built-in Emacs function `indent-relative` that simply indents the next line relative to the previous line and allows you manually indent/outdent further. Very simple, but kind of bullet-proof.
- Use the indent function of [ocp-indent](https://github.com/OCamlPro/ocp-indent) (this requires `ocp-indent.el` and the `ocp-indent` binary)
- Use the indent function of [tuareg](https://github.com/ocaml/tuareg).

You can quickly toggle between tree-sitter indentation and `indent-relative`
using `M-x neocaml-cycle-indent-function` (also available from the OCaml menu).
This is handy when the tree-sitter indentation doesn't do what you want for a
particular piece of code.

You can also permanently change the indentation function like this:

```emacs-lisp
;; Use indent-relative (simple, but bullet-proof)
(defun my-neocaml-mode-setup ()
  (setq-local indent-line-function 'indent-relative))

(add-hook 'neocaml-base-mode-hook 'my-neocaml-mode-setup)
```

To use `ocp-indent` instead:

```emacs-lisp
(defun my-neocaml-mode-setup ()
  (setq-local indent-line-function #'ocp-indent-line)
  (setq-local indent-region-function #'ocp-indent-region))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-mode-setup)
```

To use tuareg's SMIE-based indentation:

```emacs-lisp
(defun my-neocaml-mode-setup ()
  (setq-local indent-line-function #'tuareg-indent-line))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-mode-setup)
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

### Structural Selection

[expreg](https://github.com/casouri/expreg) provides expand-region-style
selection that leverages tree-sitter for language-aware expansion. It walks the
AST to grow/shrink the selection to the next syntactic node, which works
particularly well with OCaml's deeply nested expressions (match arms, let
bindings, module structures, etc.):

```emacs-lisp
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
```

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
  :ensure t
  :config
  (add-hook 'neocaml-base-mode-hook #'neocaml-repl-minor-mode)
  ;; other config options...
  )
```

The following keybindings are available when `neocaml-repl-minor-mode` is active:

> [!NOTE]
> `C-c C-c` is bound to `compile` in the base mode. When
> `neocaml-repl-minor-mode` is enabled, it is rebound to
> `neocaml-repl-send-definition`.

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

| Feature                    | neocaml                    | caml-mode     | tuareg       |
|----------------------------|----------------------------|---------------|--------------|
| Required Emacs version     | 29.1+ (30+ recommended)    | 24+           | 26+          |
| Font-lock                  | Tree-sitter (4 levels)     | Regex         | Regex        |
| Indentation                | Tree-sitter + cycle-indent | Custom engine | SMIE         |
| REPL integration           | Yes                        | Yes           | Yes          |
| Navigation (defun, sexp)   | Yes                        | Yes           | Yes          |
| Imenu                      | Yes (.ml and .mli)         | Yes           | Yes          |
| .ml/.mli toggle            | Yes                        | Yes           | Yes          |
| LSP (Eglot) integration    | Yes (auto-configured)      | Yes (manual)  | Yes (manual) |
| Debugger (ocamldebug)      | No                         | Yes           | Yes          |
| Compilation commands       | Error regexp + C-c C-c     | Yes           | Yes          |
| `_build` directory aware   | Yes                        | No            | Yes          |
| Menhir / opam support      | No                         | No            | Yes          |
| Code templates / skeletons | No                         | Yes           | Yes          |

Keep in mind also that `tuareg` uses `caml-mode` internally for some functionality.
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

### Why Tree-sitter?

Tree-sitter provides incremental, error-tolerant parsing that is
significantly faster and more accurate than regex-based font-lock and
SMIE-based indentation.  It parses the full syntax tree, so fontification
and indentation rules can reference actual language constructs rather
than fragile regular expressions.  This results in fewer edge-case bugs
and simpler, more maintainable code.

## Migrating from tuareg / caml-mode

If you're switching to neocaml from tuareg or caml-mode, here's what you need
to know.

### File associations

Both tuareg and neocaml register themselves for `.ml` and `.mli` files via
`auto-mode-alist`. Whichever loads last wins. The simplest approach is to
uninstall tuareg. If you want to keep both installed, make sure neocaml's
entries come first:

```emacs-lisp
(add-to-list 'auto-mode-alist '("\\.mli?\\'" . neocaml-mode))
```

Or with `use-package`, ensure neocaml loads after tuareg:

```emacs-lisp
(use-package neocaml
  :ensure t
  :after tuareg)  ; loads after tuareg, so neocaml's auto-mode-alist entries win
```

### Keybinding differences

Some keybindings have different meanings across the modes:

| Keybinding | neocaml | tuareg / caml-mode |
|---|---|---|
| `C-c C-c` | `compile` | Eval phrase (caml-mode) |
| `C-c C-z` | Switch to REPL | Switch to REPL |
| `C-c C-a` | Toggle .ml/.mli | Not bound |
| `C-c C-r` | Send region to REPL | Send region to REPL |
| `C-c C-b` | Send buffer to REPL | Send buffer to REPL |

### Merlin vs Eglot

Merlin works fine with neocaml -- it's a minor mode that hooks into any major
mode. That said, you might want to consider switching to Eglot + `ocamllsp`,
which provides the same features (completion, type display, jump-to-definition)
via the standard LSP protocol. Eglot is built into Emacs 29+ and neocaml
includes the necessary configuration out of the box. See the
[ocaml-eglot](#ocaml-eglot) section for OCaml-specific extensions like type
enclosing and case analysis.

### Keeping tuareg's indentation

If you prefer tuareg's SMIE-based indentation over neocaml's tree-sitter
indentation, you can use it directly -- see the
[Indentation](#indentation) section for the setup snippet.

### What you gain

- Tree-sitter powered font-locking (more accurate, 4 configurable levels)
- Tree-sitter powered navigation (sexp, sentence, defun)
- Built-in Eglot integration with automatic language ID configuration
- `_build` directory awareness
- Simpler, more maintainable codebase

### What you lose

- `ocamldebug` integration (neocaml does not include a debugger frontend)
- Menhir / opam support
- Electric comment delimiters (`(` inserting `(* *)` inside comments)
- Code templates / skeletons

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

## Troubleshooting

If you run into issues, `M-x neocaml-bug-report-info` collects useful debug
information (Emacs version, neocaml version, grammar status, Eglot status) and
copies it to the kill ring. Paste this into your bug report to help us
diagnose the problem faster.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, debugging, and
how to run tests. Architecture and design notes live in [doc/DESIGN.md](doc/DESIGN.md).

## License

Copyright © 2025-2026 Bozhidar Batsov and [contributors](https://github.com/bbatsov/neocaml/graphs/contributors).

Distributed under the GNU General Public License, version 3 or later. See [LICENSE](LICENSE) for details.
