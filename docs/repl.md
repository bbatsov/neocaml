# Toplevel (REPL) Integration

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
;; Add command-line arguments to the default OCaml toplevel
(setq neocaml-repl-program-args '("-short-paths" "-color=never"))

;; Change the REPL buffer name
(setq neocaml-repl-buffer-name "*OCaml-REPL*")
```

### Using utop instead of the default OCaml toplevel

[utop](https://github.com/ocaml-community/utop) is an improved toplevel for OCaml with many features like auto-completion, syntax highlighting, and a rich history. To use utop with neocaml-repl:

```emacs-lisp
(setq neocaml-repl-program-name "utop")
(setq neocaml-repl-program-args '("-emacs"))
```

!!! note
    If you launch Emacs from a desktop shortcut (e.g. Emacs.app on macOS) rather
    than a terminal, your shell's `PATH` may not be inherited. This can cause
    `utop` or `ocaml` to not be found. The
    [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
    package is the usual fix for this.
