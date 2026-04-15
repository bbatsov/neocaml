# Viewing Compiled Artifacts

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

## Configuration

```emacs-lisp
;; Use a wrapper script (e.g. for opam exec)
(setq neocaml-objinfo-program "opam exec -- ocamlobjinfo")

;; Customize the flags passed to ocamlobjinfo
(setq neocaml-objinfo-program-args '("-no-approx" "-no-code"))
```
