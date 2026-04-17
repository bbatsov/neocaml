# Toplevel (REPL) Integration

neocaml provides integration with the OCaml toplevel (REPL), allowing
you to evaluate OCaml code directly from your source buffer.

To get started, enable `neocaml-repl-minor-mode` (which adds REPL
keybindings to your OCaml buffers), then press `C-c C-z` to start
a REPL session:


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

!!! note
    `C-c C-c` is bound to `compile` in the base mode. When
    `neocaml-repl-minor-mode` is enabled, it is rebound to
    `neocaml-repl-send-definition`.

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

!!! tip
    In the REPL buffer itself, `C-c C-z` switches back to the source
    buffer you came from, so you can quickly bounce between source and REPL.

The REPL buffer also enables `compilation-shell-minor-mode`, so
error locations in REPL output are clickable and navigable with
`next-error` / `previous-error`.

## Input Syntax Highlighting

By default, code you type in the REPL is fontified using tree-sitter
via `comint-fontify-input-mode`, giving you the same syntax highlighting
as in regular `.ml` buffers. REPL output (errors, warnings, values)
keeps its own highlighting.

To disable this and use only basic REPL font-lock:

```emacs-lisp
(setq neocaml-repl-fontify-input nil)
```

!!! tip
    You can also get language-aware indentation for REPL input by
    leveraging `comint-indent-input-line-default`, which delegates
    indentation to the same indirect buffer used for font-lock:

    ```emacs-lisp
    (add-hook 'neocaml-repl-mode-hook
              (lambda ()
                (setq-local indent-line-function
                            #'comint-indent-input-line-default)
                (setq-local indent-region-function
                            #'comint-indent-input-region-default)))
    ```

    This is experimental -- tree-sitter parsers see the entire comint
    buffer (prompts, output, and input), so indentation may be
    approximate for complex multi-line input.

## Configuration

You can customize the OCaml REPL integration with the following variables:

```emacs-lisp
;; Add extra command-line arguments to the default OCaml toplevel.
;; The default is '("-nopromptcont"), which disables continuation
;; prompts for cleaner multi-line input in comint.  Make sure to
;; preserve it when adding your own flags:
(setq neocaml-repl-program-args '("-nopromptcont" "-short-paths" "-color=never"))

;; Change the REPL buffer name (default: "*OCaml*")
(setq neocaml-repl-buffer-name "*OCaml-REPL*")
```

REPL input history is persisted across sessions automatically.
You can configure this with `neocaml-repl-history-file` (set to
`nil` to disable) and `neocaml-repl-history-size` (default 1000).

### Using utop instead of the default OCaml toplevel

[utop](https://github.com/ocaml-community/utop) is an improved toplevel for OCaml with many features like auto-completion, syntax highlighting, and a rich history. To use utop with neocaml-repl:

```emacs-lisp
(setq neocaml-repl-program-name "utop")
(setq neocaml-repl-program-args '("-emacs"))
```

!!! note
    If Emacs can't find `utop` or `ocaml`, your shell `PATH` may not be
    inherited. See
    [Troubleshooting](troubleshooting.md#ocamllsp-not-found-macos-gui-emacs)
    for the fix.
