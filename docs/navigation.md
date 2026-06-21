# Code Navigation

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
| `C-M-u` | `neocaml-backward-up-list` | Move out to the enclosing `struct`/`sig`/`object`, record, array, or paren group |
| `C-M-d` | `down-list` | Move into a parenthesized/bracketed expression (Emacs 30+) |
| `C-M-n` | `forward-list` | Move forward over a parenthesized/bracketed group (Emacs 30+) |
| `C-M-p` | `backward-list` | Move backward over a parenthesized/bracketed group (Emacs 30+) |
| `C-c C-a` | `ff-find-other-file` | Toggle between `.ml` and `.mli` (also works for `.mll`, `.mly`, `.eliom`/`.eliomi`) |
| `C-c 4 C-a` | `ff-find-other-file-other-window` | Same, but in another window |

!!! note
    The built-in `backward-up-list` only understands syntax-table parens, so on
    Emacs 29/30 it can't walk out of a `module Foo = struct ... end` block.
    neocaml ships `neocaml-backward-up-list` as a tree-sitter-aware replacement
    bound to `C-M-u`. On Emacs 31+ the built-in already consults
    `treesit-thing-settings`, so the command simply delegates to it.

"Definitions" include `let` bindings, type definitions, module bindings, class
definitions, exceptions, and externals. "Statements" cover the same plus
`open`, `include`, and expression items -- essentially any top-level or
block-level construct.

## Worked examples

In every example below, `█` marks the position of point (the cursor). The first
block is the state before the command, the second the state after.

### Moving over expressions

`forward-sexp` (`C-M-f`) moves over the *largest* balanced expression that
starts at point. At the head of a function application that's the whole call,
not just the function name:

```ocaml
let r = █List.map (fun x -> x + 1) numbers
```

```ocaml
let r = List.map (fun x -> x + 1) numbers█
```

Inside a collection (list, array, tuple, record) it steps over one element at a
time, which is what you usually want when editing the elements:

```ocaml
let xs = [ █aa; bb; cc ]
```

```ocaml
let xs = [ aa█; bb; cc ]
```

`backward-sexp` (`C-M-b`) is the mirror image: from the end of the application
above it lands back before `List.map`.

### Diving in and climbing out

`down-list` (`C-M-d`) moves point just inside the next bracketed group -- handy
for jumping into a record or argument list before editing its contents:

```ocaml
let c = █{ name = "t"; size = 3 }
```

```ocaml
let c = {█ name = "t"; size = 3 }
```

`neocaml-backward-up-list` (`C-M-u`) does the opposite, and understands
keyword-delimited blocks, not just parens. From inside a `struct` body it climbs
out to the enclosing module:

```ocaml
module M = struct
  let x = 1
  let y = █2
end
```

```ocaml
module M = █struct
  let x = 1
  let y = 2
end
```

It works the same way for collections -- from inside a list it lands just before
the opening bracket:

```ocaml
let xs = [ 1; █2; 3 ]
```

```ocaml
let xs = █[ 1; 2; 3 ]
```

### Jumping between definitions

`beginning-of-defun` (`C-M-a`) and `end-of-defun` (`C-M-e`) move by whole
definitions. From anywhere inside a binding, `C-M-a` lands on its first column:

```ocaml
let greet name =
  Printf.printf "hi %s" na█me
```

```ocaml
█let greet name =
  Printf.printf "hi %s" name
```

## Editing with structure

The same tree-sitter backing makes the standard structural *editing* commands
reliable. They aren't OCaml-specific, but they stop misbehaving once the AST,
rather than the syntax table, decides where an expression ends.

| Keybinding | Command | Description |
|---|---|---|
| `C-M-h` | `mark-defun` | Mark the current definition |
| `C-M-SPC` | `mark-sexp` | Mark the next balanced expression |
| `M-x neocaml-mark-sentence` | `neocaml-mark-sentence` | Mark the current statement (Emacs 30+) |
| `C-M-k` | `kill-sexp` | Kill the balanced expression after point |
| `C-M-<backspace>` | `backward-kill-sexp` | Kill the balanced expression before point |
| `C-M-t` | `transpose-sexps` | Swap the two expressions around point |
| `M-x delete-pair` | `delete-pair` | Remove a matched delimiter pair (unwrap) |
| `M-x raise-sexp` | `raise-sexp` | Replace the enclosing expression with the one at point |

`kill-sexp` (`C-M-k`) removes the expression ahead of point in one stroke:

```ocaml
let xs = █(List.rev ys) @@ tl
```

```ocaml
let xs = █ @@ tl
```

`transpose-sexps` (`C-M-t`) swaps the expressions on either side of point -- a
quick way to reorder function arguments or operands:

```ocaml
let d = sub minuend █subtrahend
```

```ocaml
let d = sub subtrahend minuend█
```

`delete-pair` unwraps a bracketed group. Put point on the opening delimiter and
it deletes both it and its match (neocaml's tree-sitter-aware `forward-sexp`
finds the correct closing delimiter, even across keyword blocks):

```ocaml
let area = pi *. █(r *. r)
```

```ocaml
let area = pi *. █r *. r
```

`raise-sexp` replaces the *enclosing* expression with the sub-expression at
point. It's the fastest way to strip a wrapper -- here, dropping a `Some` around
a call:

```ocaml
let result = process (Some █(find_opt key tbl))
```

```ocaml
let result = process █(find_opt key tbl)
```

!!! note
    `raise-sexp` needs an enclosing bracketed group to raise into. Invoked at the
    top level of a binding -- where there's nothing surrounding the expression --
    it errors with "Unbalanced parentheses". Reach for it inside a call, list, or
    parenthesized expression.

!!! tip
    These commands stay correct around OCaml's trickier lexemes. Character
    literals like `'"'`, `'('`, and `')'`, and the contents of `{|...|}` /
    `{id|...|id}` quoted strings, used to confuse the syntax table (a `"`
    opening a string, a `(` opening a list); neocaml now propertizes them so
    sexp motion, `delete-pair`, and `electric-pair-mode` behave.

## The commands worth memorizing

There are a lot of structural commands; in day-to-day OCaml editing you'll keep
coming back to a small subset:

- **`C-M-f` / `C-M-b`** -- step over an expression. The everyday workhorses for
  moving across arguments, list elements, and whole sub-expressions.
- **`C-M-u` / `C-M-d`** -- climb out of / dive into the enclosing block. Pair
  them to reposition quickly: `C-M-u` to the top of a `struct` or record, then
  `C-M-d` back into a sibling.
- **`C-M-SPC`** (`mark-sexp`) -- select exactly one expression, then act on it.
  Combined with `C-M-u` first, it selects a whole bracketed group.
- **`C-M-k`** (`kill-sexp`) -- delete an expression without fiddling with the
  closing delimiter. With `mark-sexp` and `C-M-t` it covers most cut/move edits.
- **`C-M-a` / `C-M-e`** -- jump to the start/end of the current definition; great
  before `mark-defun` (`C-M-h`) to grab the whole thing.

!!! tip
    `delete-pair` and `raise-sexp` have no default key bindings but are some of
    the handiest refactoring commands. If you unwrap expressions often, bind them
    in `neocaml-mode-map`, for example:

    ```elisp
    (with-eval-after-load 'neocaml
      (define-key neocaml-mode-map (kbd "C-c r") #'raise-sexp)
      (define-key neocaml-mode-map (kbd "C-c d") #'delete-pair))
    ```

All of these commands are also available from the OCaml menu.
