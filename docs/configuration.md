# Configuration

!!! tip
    You can browse all of neocaml's customization variables interactively
    with `M-x customize-group RET neocaml`. Sub-groups like `neocaml-repl`
    and `neocaml-dune-interaction` have their own entries.

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

### Indentation Styles

OCaml doesn't have one indentation style that everyone follows. ocamlformat
ships several profiles, Jane Street's code doesn't look like the compiler's,
and plenty of projects never run a formatter at all. So neocaml's indentation
comes in three styles, which agree on most things and differ on a handful of
layouts:

- `ocp-indent` (the default) indents the way
  [ocp-indent](https://github.com/OCamlPro/ocp-indent) does out of the box.
- `conventional` indents the way ocamlformat's `conventional` profile (its
  default one) does. ocaml.org, ppxlib and Irmin are formatted with it.
- `janestreet` indents the way ocamlformat's `janestreet` profile does. Jane
  Street's libraries, dune and ocaml-lsp are formatted with it.

Here's the same code indented in each style:

=== "ocp-indent"

    ```ocaml
    let classify value =
      match value with
      | Some payload ->
        let result = transform payload in
        finish result
      | None ->
        (match fallback () with
         | [] -> 0
         | _ -> 1)

    let print_all first_list
        second_list =
      List.iter
        (fun x ->
           print x)
        (first_list
         @ second_list);
      List.iter first_list (fun x ->
          print x)

    module Int_set = Set.Make (struct
        type t = int
        let compare = compare
      end)
    ```

=== "conventional"

    ```ocaml
    let classify value =
      match value with
      | Some payload ->
          let result = transform payload in
          finish result
      | None ->
          (match fallback () with
          | [] -> 0
          | _ -> 1)

    let print_all first_list
        second_list =
      List.iter
        (fun x ->
          print x)
        (first_list
        @ second_list);
      List.iter first_list (fun x ->
          print x)

    module Int_set = Set.Make (struct
      type t = int
      let compare = compare
    end)
    ```

=== "janestreet"

    ```ocaml
    let classify value =
      match value with
      | Some payload ->
        let result = transform payload in
        finish result
      | None ->
        (match fallback () with
         | [] -> 0
         | _ -> 1)

    let print_all first_list
          second_list =
      List.iter
        (fun x ->
           print x)
        (first_list
         @ second_list);
      List.iter first_list (fun x ->
        print x)

    module Int_set = Set.Make (struct
        type t = int
        let compare = compare
      end)
    ```

The differences, with the columns counted for the default
`neocaml-indent-offset` of 2:

| Layout | `ocp-indent` | `conventional` | `janestreet` |
|--------|--------------|----------------|--------------|
| Body of a match case on a line of its own, from the bar | 2 | 4 | 2 |
| Cases of a `(match ...)` or `(try ...)` | under the `match` | under the parenthesis | under the `match` |
| Parameters on continuation lines | 4 | 4 | 6 |
| Arguments on continuation lines, after `=`, `then` and the like | 4 | 4 | 2 past the function |
| Arguments on continuation lines, after an operator, a comma or a semicolon | 2 past the start of the chain | 2 past the function | 2 past the function |
| An operator starting a line inside parentheses | under the first operand | under the parenthesis | under the first operand |
| A structure passed to a functor, `F (struct ... end)`, or the signature of a functor's parameter, `module F (X : sig ... end)` | body 4, `end` 2 | body 2, `end` 0 | body 4, `end` 2 |
| Body of a `(fun x ->` starting a line | 2 past the `fun` | 2 past the parenthesis | 2 past the `fun` when passed to a function, 2 past the parenthesis otherwise |
| Body of a `(fun x ->` further along a line | 4 past the start of the line | 4 past the function it's passed to | 2 past the start of the line |

A few rarer layouts differ by a column or two as well: the type of a
typed pattern or the `:` of a typed expression on a line of its own, the
type after a `type a.` annotation, a binding on the line after a
`let%ext`, a leading comma in type parameters, and the signatures of
other functor parameters than the first one of a module definition.

Everything else is the same in all three: the bar of a case goes under its
`match`, nothing is indented after `in`, `then` and `else` go under their
`if`, and so on.

Why `ocp-indent` by default? If you use ocamlformat, the default doesn't
matter much, because neocaml picks the style that matches your
`.ocamlformat` (see below). For code that ocamlformat never touches,
ocp-indent is what most projects indent with: opam, Rocq, Frama-C and Eio do,
and so does the compiler (with deeper match cases). It's also close to the
`janestreet` profile, and it keeps match case bodies one
`neocaml-indent-offset` past the bar, as neocaml always has.

Keep in mind that indentation only places the lines you've broken, it doesn't
break them for you. If ocamlformat formats your code on save, it gets the last
word on the layout either way. Picking the matching style just means less of
what you type moves around when you save.

#### Switching Styles

By default (`neocaml-indent-style` set to `nil`) neocaml looks for an
`.ocamlformat` in the file's directory and its parents, up to the project
root, the same way ocamlformat does:

- `profile = janestreet` selects the `janestreet` style, and any other profile
  (or none, since `conventional` is ocamlformat's default) the `conventional`
  one.
- A `cases-exp-indent` option sets how far the bodies of match cases go,
  whatever the profile.
- An `.ocamlformat` doesn't count for the files ocamlformat leaves alone, i.e.
  when it says `disable = true` and no `.ocamlformat-enable` lists the file, or
  when an `.ocamlformat-ignore` lists it.

Without an `.ocamlformat`, neocaml looks for the nearest `.ocp-indent`, as
ocp-indent does. Its `JaneStreet` preset selects the `janestreet` style, and
anything else the `ocp-indent` one. Its `match_clause` sets how far the bodies
of match cases go. That's how the compiler's own sources get their deeper
match cases. Without either file, you get the `ocp-indent` style.

neocaml reads these files once per buffer, so after you change one, revert
the buffers it applies to (`M-x revert-buffer`).

To use one style everywhere, whatever the project's files say:

```emacs-lisp
(setq neocaml-indent-style 'janestreet)
```

To pick a style for one project without such a file, set it in a
`.dir-locals.el` at the project root:

```emacs-lisp
((neocaml-base-mode . ((neocaml-indent-style . conventional))))
```

To switch the current buffer to another style, use `M-x
neocaml-set-indent-style` or the "Indentation Style" submenu of the OCaml
menu. That only changes how lines get indented from then on, and only in
that buffer. To reindent the
whole buffer, select it with `C-x h` and press `TAB`.

### Alternative Indentation Functions

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

## Formatting

neocaml can format OCaml source with
[ocamlformat](https://github.com/ocaml-ppx/ocamlformat). `C-c C-f`
(`neocaml-format-buffer`) pipes the buffer through `ocamlformat` and
replaces only the parts that changed, so point, markers and window
positions stay where they were. The buffer's file name is
passed to ocamlformat, so it picks up the right `.ocamlformat` profile and
applies the correct implementation vs. interface rules.

To format automatically on save:

```emacs-lisp
(setq neocaml-format-on-save t)
```

The program is configurable via `neocaml-ocamlformat-program` (default
`"ocamlformat"`). ocamlformat formats whole compilation units, so there is
no region- or definition-level variant.

!!! note
    ocamlformat only formats files inside a project that has a
    `.ocamlformat` file; outside such a project it reports an error, which
    neocaml surfaces in the echo area.

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

## Clickable Links in Comments

neocaml enables `goto-address-prog-mode` and `bug-reference-prog-mode` in
the OCaml source modes (and in the dune, opam, OCamllex, and Menhir modes).
URLs in comments and strings become clickable out of the box.

Issue references (like `#123`) become clickable once you tell Emacs how to
resolve them by setting `bug-reference-url-format`, usually per-project via
a `.dir-locals.el` at the project root:

```emacs-lisp
((prog-mode
  . ((bug-reference-url-format
      . "https://github.com/me/myproject/issues/%s"))))
```

## Current Definition in Mode Line

`which-func-mode` displays the name of the current definition (let
binding, type, module, etc.) in the mode line. neocaml supports it out
of the box:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'which-function-mode)
```

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

## Structural Editing

[combobulate](https://github.com/mickeynp/combobulate) supports
`neocaml-mode` and `neocaml-interface-mode` out of the box. On top of
moving around by syntax node, it can wrap the code at point in `let`,
`match`, `if`, `try ... with`, `begin ... end`, `fun` and other forms,
drag parameters, match cases and list elements past their neighbors, and
splice or clone them. It isn't on an ELPA, so install it from its
repository:

```emacs-lisp
;; Emacs 30+; on Emacs 29, install it with `package-vc-install' instead.
(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :hook (neocaml-base-mode . combobulate-mode))
```

## Eglot (LSP)

Eglot is auto-configured - neocaml registers both modes with the correct
language IDs so `M-x eglot` just works. See
[Getting Started](usage.md#eglot-lsp) for setup details and
[ocaml-eglot](usage.md#ocaml-eglot) for OCaml-specific extensions.

## Build Directory Redirect

When you open a file under `_build/` (dune's build output directory),
neocaml offers to switch to the corresponding source file instead.
This is controlled by `neocaml-redirect-build-files` (default `t`):

```emacs-lisp
;; Disable if you work with build artifacts directly
(setq neocaml-redirect-build-files nil)
```
