# Configuration

## Font-locking

neocaml provides 4 levels of font-locking, as is the standard for Tree-sitter
modes. The default level in Emacs is 3, and you can change it like this:

```emacs-lisp
;; this font-locks everything neocaml supports
(setq treesit-font-lock-level 4)
```

The font-lock features available at each level are:

**Level 1** (minimal -- comments and definitions):

- `comment` -- comments and doc comments: `(* ... *)`, `(** ... *)`
- `definition` -- let/val/type/method bindings and value patterns: `let f x = ...`, `val x : int`

**Level 2** (add keywords, strings, types):

- `keyword` -- language keywords and arrows: `let`, `match`, `fun`, `if`, `->`, ...
- `string` -- strings and characters: `"hello"`, `'a'`, `{|raw|}`
- `type` -- type names, modules, constructors, type punctuation: `int`, `List`, `Some`, `'a`, `->`/`*` in type expressions

**Level 3** (default -- full-blown fontification):

- `attribute` -- PPX attributes and extension nodes: `[@inline]`, `[@@deriving show]`, `[%test ...]`
- `builtin` -- builtin identifiers, exceptions, and types: `print_endline`, `Not_found`, `int`, `string`
- `constant` -- boolean and unit literals: `true`, `false`, `()`
- `escape-sequence` -- escape sequences and format specs in strings: `\n`, `\t`, `%d`, `%s`
- `number` -- numeric literals: `42`, `3.14`, `0xFF`, `0b1010`

**Level 4** (maximum detail):

Level 4 can get very noisy -- rather than enabling it wholesale, consider
cherry-picking individual features from it (see [Selecting features](#selecting-features) below).

- `operator` -- operators: `+`, `::`, `<-`, `#` (method invocation)
- `bracket` -- brackets: `()`, `[]`, `{}`, `[| |]`
- `delimiter` -- delimiters: `,`, `.`, `;`, `:`, `;;`
- `variable` -- value names in expressions: `x`, `y` in `x + y`
- `property` -- record field access: `name` in `person.name`
- `label` -- labeled arguments: `x` in `~x:1`, `y` in `?y:2`
- `function` -- function calls: `f` in `f x`, `map` in `List.map`, pipe targets `x |> f`

### Selecting features

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

### Customizing faces

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

### Adding custom font-lock rules

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

### Prettify Symbols

You can "prettify" certain symbols by enabling `prettify-symbols-mode`
via a hook:

```emacs-lisp
;; Enable for both .ml and .mli files at once
(add-hook 'neocaml-base-mode-hook #'prettify-symbols-mode)
```

By default, neocaml replaces operators that have obvious Unicode
equivalents and preserve column width (`<=` to `≤`, `&&` to `∧`,
etc.). The full set is in `neocaml-prettify-symbols-alist`.

There's also an extra set of replacements (`fun` to `λ`, `->` to `→`,
`not` to `¬`) that can break column alignment because the replacement
is a different width. To enable these:

```emacs-lisp
(setq neocaml-prettify-symbols-full t)
```

You can customize both `neocaml-prettify-symbols-alist` and
`neocaml-prettify-symbols-extra-alist` to add or remove replacements.

## Indentation

The base indentation offset is controlled by `neocaml-indent-offset` (default 2):

```emacs-lisp
(setq neocaml-indent-offset 4)
```

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

## Comments

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

## Code Folding

On Emacs 30+, `outline-minor-mode` works out of the box with neocaml -- it
automatically picks up definition headings from the tree-sitter imenu settings.
Enable it via a hook:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'outline-minor-mode)
```

For tree-sitter-aware code folding (fold any node, not just top-level
definitions), see [treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold).

## Structural Selection

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

## Build Directory Redirect

When you open a file under `_build/` (dune's build output directory),
neocaml offers to switch to the corresponding source file instead.
This is controlled by `neocaml-redirect-build-files` (default `t`):

```emacs-lisp
;; Disable if you work with build artifacts directly
(setq neocaml-redirect-build-files nil)
```
