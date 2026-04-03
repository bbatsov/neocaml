# neocaml

[![MELPA](https://melpa.org/packages/neocaml-badge.svg)](https://melpa.org/#/neocaml)
[![MELPA Stable](https://stable.melpa.org/packages/neocaml-badge.svg)](https://stable.melpa.org/#/neocaml)
[![CI](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/neocaml/actions/workflows/ci.yml)
[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=github)](https://github.com/sponsors/bbatsov)

`neocaml` is a **n**ew **E**macs package for programming in OCaml.  It features
major modes for OCaml, OCaml Interface, opam, and dune files, using Tree-sitter,
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
- opam file editing (`neocaml-opam-mode`) with font-lock, indentation, imenu, and `opam lint` integration (flymake and [flycheck](https://github.com/flycheck/flycheck))
- dune file editing (`neocaml-dune-mode`) for dune, dune-project, and dune-workspace files
- dune build commands (`neocaml-dune-interaction-mode`) — build, test, clean, promote, fmt, exec (with watch mode via prefix arg)
- OCamllex file editing (`neocaml-ocamllex-mode`) with font-lock, indentation, imenu, and OCaml language injection (Emacs 30+)
- Menhir file editing (`neocaml-menhir-mode`) with font-lock, indentation, imenu, and OCaml language injection (Emacs 30+)
- Cram test file editing (`neocaml-cram-mode`) with font-lock for commands, output, modifiers, and prose
- Easy installation of `ocaml` and `ocaml-interface` tree-sitter grammars via `M-x neocaml-install-grammars`
- Compilation error regexp for `M-x compile` (errors, warnings, alerts, backtraces)
- `_build` directory awareness (offers to switch to source when opening build artifacts)
- Eglot integration (with [ocaml-eglot](https://github.com/tarides/ocaml-eglot) support)
- Prettify-symbols for common OCaml operators

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

**Level 1** (minimal — comments and definitions):

- `comment` — comments and doc comments: `(* ... *)`, `(** ... *)`
- `definition` — let/val/type/method bindings and value patterns: `let f x = ...`, `val x : int`

**Level 2** (add keywords, strings, types):

- `keyword` — language keywords and arrows: `let`, `match`, `fun`, `if`, `->`, ...
- `string` — strings and characters: `"hello"`, `'a'`, `{|raw|}`
- `type` — type names, modules, constructors, type punctuation: `int`, `List`, `Some`, `'a`, `->`/`*` in type expressions

**Level 3** (default — full-blown fontification):

- `attribute` — PPX attributes and extension nodes: `[@inline]`, `[@@deriving show]`, `[%test ...]`
- `builtin` — builtin identifiers, exceptions, and types: `print_endline`, `Not_found`, `int`, `string`
- `constant` — boolean and unit literals: `true`, `false`, `()`
- `escape-sequence` — escape sequences and format specs in strings: `\n`, `\t`, `%d`, `%s`
- `number` — numeric literals: `42`, `3.14`, `0xFF`, `0b1010`

**Level 4** (maximum detail):

Level 4 can get very noisy — rather than enabling it wholesale, consider
cherry-picking individual features from it (see [Selecting features](#selecting-features) below).

- `operator` — operators: `+`, `::`, `<-`, `#` (method invocation)
- `bracket` — brackets: `()`, `[]`, `{}`, `[| |]`
- `delimiter` — delimiters: `,`, `.`, `;`, `:`, `;;`
- `variable` — value names in expressions: `x`, `y` in `x + y`
- `property` — record field access: `name` in `person.name`
- `label` — labeled arguments: `x` in `~x:1`, `y` in `?y:2`
- `function` — function calls: `f` in `f x`, `map` in `List.map`, pipe targets `x |> f`

#### Selecting features

You don't have to use the level system. If you want fine-grained control over
what gets highlighted, you can cherry-pick individual features using
`treesit-font-lock-recompute-features`:

```emacs-lisp
(defun my-neocaml-font-lock-setup ()
  (treesit-font-lock-recompute-features
   ;; enable these features
   '(comment definition keyword string type
     attribute builtin constant escape-sequence number
     operator variable property label)
   ;; disable these features
   '(bracket delimiter function)))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-font-lock-setup)
```

This gives you operators and variables without bracket and delimiter noise, for
example. You can also call `M-x treesit-font-lock-recompute-features`
interactively to toggle features in the current buffer.

#### Customizing faces

The faces used are standard `font-lock-*-face` faces, so any theme applies
automatically. If you want to tweak how specific syntactic elements look, you
have two options:

**Buffer-local remapping** (recommended) -- changes apply only to neocaml
buffers, leaving other modes unaffected:

```emacs-lisp
;; Use a custom color for type names in OCaml buffers only
(add-hook 'neocaml-base-mode-hook
  (lambda ()
    (face-remap-add-relative 'font-lock-type-face
                             :foreground "DarkSeaGreen4")))
```

You can remap multiple faces in the same hook. Each
`face-remap-add-relative` call stacks on top of the face's current
definition, so theme settings are preserved as a base. See
[Face Remapping](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html)
in the Emacs Lisp manual for details.

**Global customization** -- changes apply everywhere the face is used:

```emacs-lisp
;; Change type names globally (affects all modes)
(custom-set-faces
 '(font-lock-type-face ((t (:foreground "DarkSeaGreen4")))))
```

This is simpler but less precise -- since tree-sitter modes share the same
`font-lock-*-face` faces, a global change will affect every tree-sitter mode
(and traditional modes) that uses that face.

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

### Input Syntax Highlighting

By default, code you type in the REPL is fontified using tree-sitter
via `comint-fontify-input-mode`, giving you the same syntax highlighting
as in regular `.ml` buffers. REPL output (errors, warnings, values)
keeps its own highlighting.

To disable this and use only basic REPL font-lock:

```emacs-lisp
(setq neocaml-repl-fontify-input nil)
```

> [!TIP]
> You can also get language-aware indentation for REPL input by
> leveraging `comint-indent-input-line-default`, which delegates
> indentation to the same indirect buffer used for font-lock:
>
> ```emacs-lisp
> (add-hook 'neocaml-repl-mode-hook
>           (lambda ()
>             (setq-local indent-line-function
>                         #'comint-indent-input-line-default)
>             (setq-local indent-region-function
>                         #'comint-indent-input-region-default)))
> ```
>
> This is experimental — tree-sitter parsers see the entire comint
> buffer (prompts, output, and input), so indentation may be
> approximate for complex multi-line input.

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

## Viewing Compiled Artifacts

neocaml can display OCaml compiled artifacts (`.cmi`, `.cmo`, `.cmx`,
`.cma`, `.cmxa`, `.cmxs`, `.cmt`, `.cmti`) in a human-readable form
using `ocamlobjinfo`. Just open any compiled file and neocaml will
automatically show the `ocamlobjinfo` output instead of binary content.

You can also view a file explicitly with `M-x neocaml-objinfo-view-file`.

| Keybinding | Command | Description |
|------------|---------|-------------|
| `g` | `neocaml-objinfo-revert` | Refresh the output (e.g. after recompilation) |
| `n` | `neocaml-objinfo-next-unit` | Jump to the next compilation unit (useful in archives) |
| `p` | `neocaml-objinfo-previous-unit` | Jump to the previous compilation unit |
| `q` | `quit-window` | Close the buffer |

### Configuration

```emacs-lisp
;; Use a wrapper script (e.g. for opam exec)
(setq neocaml-objinfo-program "opam exec -- ocamlobjinfo")

;; Customize the flags passed to ocamlobjinfo
(setq neocaml-objinfo-program-args '("-no-approx" "-no-code"))
```

## opam and dune Support

neocaml includes dedicated tree-sitter modes for opam and dune files.
`neocaml-opam-mode` activates automatically for `.opam` and `opam` files;
`neocaml-dune-mode` activates for `dune`, `dune-project`, and `dune-workspace`
files. Both provide font-lock, indentation, and imenu.

### opam lint

`neocaml-opam-mode` registers an `opam lint` flymake backend so you get
inline diagnostics for missing fields, deprecated constructs, and syntax errors.
To enable it, activate `flymake-mode` in opam buffers:

```emacs-lisp
(add-hook 'neocaml-opam-mode-hook #'flymake-mode)
```

[flycheck](https://github.com/flycheck/flycheck) users get `opam lint` support
out of the box via flycheck's built-in `opam` checker.

### dune Commands

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

### dune File Formatting

`neocaml-dune-mode` can format the current buffer using `dune format-dune-file`.
Use `C-c C-f` (`neocaml-dune-format-buffer`) to format on demand, or enable
automatic formatting on save:

```emacs-lisp
(setq neocaml-dune-format-on-save t)
```

Note: this formats individual dune files via `dune format-dune-file`, which is
different from `dune fmt` (available via `C-c C-d f`) that formats the entire
project.

### Cram Tests

`neocaml-cram-mode` provides syntax highlighting for cram test (`.t`) files, as
used by dune's expect-test framework. It highlights shell commands, expected
output, output modifiers (`(re)`, `(glob)`, `(no-eol)`, `(esc)`), exit codes,
and prose comments.

The mode activates automatically for `.t` files. Use `dune promote` (available
via `C-c C-d p` with `neocaml-dune-interaction-mode`) to accept corrected test
output.

> [!NOTE]
> The `.t` extension is also used by Perl test files. If you work with
> both OCaml and Perl, you may need to override the association for Perl
> projects:
>
> ```emacs-lisp
> ;; Use cperl-mode for .t files in Perl projects
> (add-to-list 'auto-mode-alist '("/t/.*\\.t\\'" . cperl-mode))
> ```
>
> Entries added later to `auto-mode-alist` take priority, so the more specific
> pattern above will win for `.t` files under a `t/` directory (Perl convention).

### Other OCaml-related Files

neocaml also registers sensible modes for a few other OCaml-related files:

- `.ocamlinit` opens in `neocaml-mode` (it's OCaml toplevel startup code)
- `.ocamlformat` and `.ocp-indent` open in `conf-unix-mode` (key = value config files with `#` comments)

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
| opam file support          | Yes                        | No            | Yes          |
| dune file support          | Yes                        | No            | No           |
| OCamllex support           | Yes (with OCaml injection) | No            | Yes          |
| Menhir support             | Yes (with OCaml injection) | No            | Yes          |
| Cram test support          | Yes                        | No            | No           |
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

neocaml automatically registers `.ml` and `.mli` files via `auto-mode-alist` --
you don't need to add any file associations yourself. Importantly, neocaml uses
**two separate modes**: `neocaml-mode` for `.ml` files (using the `ocaml`
tree-sitter grammar) and `neocaml-interface-mode` for `.mli` files (using the
`ocaml-interface` grammar). Both must be mapped correctly for font-lock and
indentation to work.

> [!CAUTION]
> If you previously used caml-mode or tuareg, **remove any manual
> `auto-mode-alist` entries** for OCaml files from your config. A common
> caml-mode pattern like:
>
> ```emacs-lisp
> (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
> ```
>
> maps `.mli` files to a single mode. Replacing `caml-mode` with
> `neocaml-mode` in such an entry will break `.mli` files because they
> need `neocaml-interface-mode`, not `neocaml-mode`. The simplest fix is
> to delete the entry entirely and let neocaml's autoloads handle it.

If you need to keep tuareg or caml-mode installed alongside neocaml,
make sure neocaml loads last so its `auto-mode-alist` entries win:

```emacs-lisp
(use-package neocaml
  :ensure t
  :after tuareg)  ; loads after tuareg, so neocaml's auto-mode-alist entries win
```

#### Unsupported file types

caml-mode and tuareg also handle `.mll` (ocamllex), `.mly` (ocamlyacc/menhir),
and `.mlp` (camlp4/camlp5) files. neocaml now supports `.mll` files via
`neocaml-ocamllex-mode` (with full OCaml syntax highlighting inside `{ }`
blocks via language injection), and `.mly` files via `neocaml-menhir-mode`
(also with OCaml injection). The language injection requires Emacs 30+; on
Emacs 29 the modes still work but embedded OCaml code is not fontified.
`.mlp` (camlp4/camlp5) files are not supported.

#### Using the legacy dune-mode

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
- `.mlp` (camlp4/camlp5) file support
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

### Tree-sitter ABI version mismatch

If you see an error like:

```
Cannot activate tree-sitter, because language grammar for X is unavailable (version-mismatch): 15
```

This means the grammar requires tree-sitter ABI version 15 (tree-sitter
0.25.0+), but your Emacs was compiled against an older tree-sitter library that
only supports ABI 14. You can verify by evaluating `(treesit-library-abi-version)`
— if it returns 14, that confirms the mismatch.

This isn't specific to neocaml — any grammar targeting ABI 15 will fail the same
way. The fix is to rebuild Emacs against tree-sitter >= 0.25.0.

**macOS (Homebrew):**

```sh
brew update && brew upgrade tree-sitter
brew reinstall emacs-plus  # or however you installed Emacs
```

Make sure the Homebrew tree-sitter is what Emacs links against (check with
`otool -L $(which emacs) | grep tree-sitter`).

**Linux:**

Distro packages for tree-sitter tend to lag behind. You may need to build
tree-sitter from source:

```sh
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make && sudo make install
sudo ldconfig
```

Then recompile Emacs so it picks up the new library.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, debugging, and
how to run tests. Architecture and design notes live in [doc/DESIGN.md](doc/DESIGN.md).

## License

Copyright © 2025-2026 Bozhidar Batsov and [contributors](https://github.com/bbatsov/neocaml/graphs/contributors).

Distributed under the GNU General Public License, version 3 or later. See [LICENSE](LICENSE) for details.
