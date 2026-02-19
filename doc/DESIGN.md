# neocaml Design Notes

## OCaml Interface (.mli) support

The `ocaml-interface` tree-sitter grammar inherits **all** rules from the base
`ocaml` grammar and only overrides `compilation_unit` (accepting `_signature_item`
instead of `_structure_item`).  Both grammars expose the same set of named node
types, which means:

- Font-lock queries that reference `.ml`-only constructs (e.g. `application_expression`,
  `let_binding`) simply produce no matches in `.mli` files — they are harmless no-ops.
- Indentation rules work identically because the node types used for anchoring
  (`structure`, `signature`, `value_specification`, `type_binding`, etc.) exist
  in both grammars.

This lets `neocaml` use a **single set of font-lock and indentation rules** for both
`neocaml-mode` and `neocaml-interface-mode`, keeping the code simple and maintainable.
The only place where the two modes diverge is imenu, which uses tailored categories
for each grammar (e.g. "Val" and "External" for `.mli` vs "Value" for `.ml`).

## Font-locking

You can control the amount of fontification applied by Font Lock mode of
major modes based on tree-sitter by customizing the variable
`treesit-font-lock-level`.  Its value is a number between 1 and 4:

- Level 1: This level usually fontifies only comments and function names in
     function definitions.
- Level 2: This level adds fontification of keywords,
     strings, and data types.
- Level 3: This is the default level; it adds
     fontification of assignments, numbers, etc.
- Level 4: This level adds everything else that can be fontified: operators,
     delimiters, brackets, other punctuation, function names in function calls,
     property look ups, variables, etc.

Note that the 4 levels are defined by each major-mode and the above are just
recommendations.

## Indentation

Tree-sitter indentation in Emacs is driven by `treesit-simple-indent-rules` — a
list of `(MATCHER ANCHOR OFFSET)` triples tried in order. The first matching rule
wins. MATCHER decides *if* a rule applies, ANCHOR provides a reference position,
and OFFSET is added to that position's column to produce the final indentation.

The rules in `neocaml--indent-rules` are roughly ordered as follows:

1. **Empty-line handling** — `(no-node ...)` must come first (see below).
2. **Top-level** — `(parent-is "compilation_unit")` pins everything at column 0.
   `compilation_unit` is tree-sitter's root node representing the entire source file.
3. **Closing delimiters** — `)`, `]`, `}`, `done`, `end` align with the opening construct via `parent-bol 0`.
4. **Keyword alignment** — `with`, `then_clause`, `else_clause`, match `|` align with their enclosing keyword.
5. **Body indentation** — children of `let_binding`, `match_case`, `structure`, `do_clause`, etc. are indented by `neocaml-indent-offset`.
6. **Error recovery** — `(parent-is "ERROR")` indents by offset so that typing inside incomplete code gets reasonable indentation.

### The `no-node` problem

When the cursor is on an empty line, tree-sitter has no node at point.  In
Emacs 30 the indentation engine (`treesit--indent-1`) sets `node=nil` and
resolves `parent` via `treesit-node-on`, which returns `compilation_unit` — the
only node spanning the empty position.  This means the
`(parent-is "compilation_unit")` rule would fire first, always giving column 0
— even inside incomplete constructs like `let x =`.

We solve this with a single `no-node` rule placed **before** all other rules:

```elisp
(no-node prev-line neocaml--empty-line-offset)
```

- **`prev-line`** anchors to the previous line's indentation (first
  non-whitespace column).
- **`neocaml--empty-line-offset`** is a custom offset function that inspects the
  last token on the previous line.  If it's a "body-expecting" token (listed in
  `neocaml--indent-body-tokens`: `=`, `->`, `then`, `else`, `do`, `struct`,
  `sig`, `begin`, `object`, `in`, `with`, `fun`, `function`, `try`), the offset
  is `neocaml-indent-offset`; otherwise it's 0.

This gives the right result in all common cases:

| Previous line ends with | Anchor (prev-line) | Offset | Result |
|---|---|---|---|
| `let x =` | col 0 | +2 | col 2 |
| `let x = 42` | col 0 | 0 | col 0 |
| `module M = struct` | col 0 | +2 | col 2 |
| `  let x =` (inside struct) | col 2 | +2 | col 4 |
| `  let x = 42` (inside struct) | col 2 | 0 | col 2 |

### Tips for adding new indentation rules

- Use `treesit-explore-mode` and `treesit-inspect-mode` to see node types at
  point. Set `neocaml--debug` to `t` to enable verbose indentation logging.
- Order matters: more specific rules must come before general ones.
- The `parent-bol` anchor resolves to the first non-whitespace column on the
  parent node's starting line. This is almost always what you want.
- When a parent node starts on the same line as its first child (common with
  variant declarations), `parent-bol` shifts unexpectedly after the child is
  indented. Use `neocaml--grand-parent-bol` to go one level up instead.
- Test new rules with `eldev test` — the indentation test suite uses
  `when-indenting-it` specs that assert exact indentation for multi-line OCaml
  snippets.

## General notes

Emacs doesn't support directly using `.scm` (TreeSitter queries) files, so we currently need
to manually code both the font-locking and indentation queries.

Emacs 31 will introduce `define-treesit-generic-mode` that will make it possible to
use `.scm` for font-locking.

- <https://github.com/emacs-mirror/emacs/blob/master/lisp/treesit-x.el#L47>

## Sources of inspiration

Based on ideas and code from:

- [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode)
- [tuareg](https://github.com/ocaml/tuareg)
- [ocaml-ts-mode](https://github.com/dmitrig/ocaml-ts-mode)
- [nvim-treesitter's OCaml TreeSitter queries](https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries/ocaml)
- <https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode>

## References

- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Tree_002dsitter-Major-Modes.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html#index-treesit_002dsimple_002dindent_002dpresets> (indentation)
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html>
- <https://www.jonashietala.se/blog/2024/03/19/lets_create_a_tree-sitter_grammar/>
- <https://archive.casouri.cc/note/2024/emacs-30-tree-sitter/>
