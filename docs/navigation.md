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

### Marking and transposing

Standard Emacs commands for marking and transposing work with
tree-sitter-aware granularity:

| Keybinding | Command | Description |
|---|---|---|
| `C-M-h` | `mark-defun` | Mark the current definition |
| `C-M-SPC` | `mark-sexp` | Mark the next balanced expression |
| `M-x neocaml-mark-sentence` | `neocaml-mark-sentence` | Mark the current statement (Emacs 30+) |
| `C-M-t` | `transpose-sexps` | Transpose two balanced expressions |

All navigation commands are also available from the OCaml menu.
