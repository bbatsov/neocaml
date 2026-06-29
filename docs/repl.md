# Toplevel (REPL) Integration

neocaml provides integration with the OCaml toplevel (REPL), allowing
you to evaluate OCaml code directly from your source buffer. The REPL
features:

- Tree-sitter syntax highlighting for input (via `comint-fontify-input-mode`)
- Persistent input history across sessions
- Clickable error locations (via `compilation-shell-minor-mode`)
- Quick switching between source and REPL with `C-c C-z`
- Support for both the standard `ocaml` toplevel and [utop](#using-utop-instead-of-the-default-ocaml-toplevel)

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
| `C-c C-n` | `neocaml-repl-send-phrase-and-step` | Send the current phrase, then move to the next one |
| `C-c C-i` | `neocaml-repl-interrupt` | Interrupt the current evaluation in the REPL |
| `C-c C-k` | `neocaml-repl-clear-buffer` | Clear the REPL buffer |

!!! tip
    In the REPL buffer itself, `C-c C-z` switches back to the source
    buffer you came from, so you can quickly bounce between source and REPL.

`C-c C-n` (`neocaml-repl-send-phrase-and-step`) is handy for working
through a buffer: it sends the phrase at point and then advances to the
start of the next one, so you can evaluate a file phrase by phrase with
repeated presses.

`M-x neocaml-repl-require` loads a findlib package into the REPL via
`#require` (the toplevel needs findlib, e.g. via `topfind` or utop).

`M-x neocaml-repl-restart` kills the running toplevel and starts a fresh
one in the same buffer. These, along with the commands above, are also
available from the "OCaml REPL" menu.

The REPL buffer also enables `compilation-shell-minor-mode`, so
error locations in REPL output are clickable and navigable with
`next-error` / `previous-error`.

## Per-project REPLs

Each project gets its own dedicated REPL. When you start or send to a
REPL from a source file, neocaml derives the project (via `project.el`,
falling back to the directory containing a `dune-project`) and routes to
a per-project buffer named after `neocaml-repl-buffer-name` plus the
project, e.g. `*OCaml: myproject*`. Files outside any project share the
base `*OCaml*` buffer.

This means you can have several OCaml projects open at once, each with
its own toplevel, and the send commands always reach the right one. It
also means `M-x neocaml-dune-utop` (see below) and the regular send
commands target the same per-project REPL.

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

;; Change the base REPL buffer name (default: "*OCaml*").  Per-project
;; REPLs derive their name from this, e.g. "*OCaml: myproject*".
(setq neocaml-repl-buffer-name "*OCaml-REPL*")
```

REPL input history is persisted across sessions automatically.
You can configure this with `neocaml-repl-history-file` (set to
`nil` to disable) and `neocaml-repl-history-size` (default 1000).

### Choosing a toplevel

`neocaml-repl-flavor` selects which toplevel to launch:

- `ocaml` (the default) - the standard ocaml toplevel, using
  `neocaml-repl-program-name` and `neocaml-repl-program-args`.
- `utop` - [utop](https://github.com/ocaml-community/utop), an improved
  toplevel with auto-completion, syntax highlighting, and a rich history.
- `dune-utop` - `dune utop` for the current project, launched via
  `M-x neocaml-dune-utop`, so the project's libraries are available.

Set it globally or per project via `.dir-locals.el`; the REPL reads it
when it starts (restart an existing one with `M-x neocaml-repl-restart`
to apply a change). The active flavor is shown in the REPL's mode line,
e.g. `OCaml-REPL[utop]`.

```emacs-lisp
(setq neocaml-repl-flavor 'utop)
```

!!! note
    Don't pass utop's `-emacs` flag here. It activates a structured
    protocol that the comint-based REPL doesn't speak; plain `utop`
    works fine with `neocaml-repl`. If you want that protocol, use the
    dedicated [`neocaml-utop`](#the-utop-protocol-backend) backend
    described below.

!!! note
    If Emacs can't find `utop` or `ocaml`, your shell `PATH` may not be
    inherited. See
    [Troubleshooting](troubleshooting.md#ocamllsp-not-found-macos-gui-emacs)
    for the fix.

## The utop protocol backend

`neocaml-utop` is an alternative toplevel integration that drives utop
through its native editor protocol (`utop -emacs`) instead of treating
it as a plain comint stream. It's a separate module from `neocaml-repl`;
use whichever you prefer. `neocaml-repl` remains the right choice for the
standard `ocaml` toplevel, which has no protocol of its own.

In protocol mode utop does no rendering itself and reports structured
information that a raw stream can't, which lets `neocaml-utop`:

- separate output, results, errors, and warnings without guessing from
  the text;
- underline the **exact** sub-expression a parse or type error points at,
  back in your source buffer (see the `neocaml-utop-error-face` face),
  rather than just the line;
- answer completion from utop's own engine, wired into
  `completion-at-point` in the transcript.

The transcript is a comint derivative, so input editing, history (with
optional persistence via `neocaml-utop-history-file`), and read-only
prompts work as usual.

Enable the source-buffer minor mode to get the keybindings, then press
`C-c C-z` to start a session:

```emacs-lisp
(add-hook 'neocaml-base-mode-hook #'neocaml-utop-minor-mode)
```

The keybindings mirror `neocaml-repl-minor-mode`:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-z` | `neocaml-utop-switch-to-utop` | Start utop or switch to a running session |
| `C-c C-c` | `neocaml-utop-send-definition` | Send the definition at point |
| `C-c C-r` | `neocaml-utop-send-region` | Send the active region |
| `C-c C-b` | `neocaml-utop-send-buffer` | Send the whole buffer |
| `C-c C-p` | `neocaml-utop-send-phrase` | Send the phrase at point (code up to `;;`) |
| `C-c C-n` | `neocaml-utop-send-phrase-and-step` | Send the phrase, then move to the next one |
| `C-c C-l` | `neocaml-utop-load-file` | Load the current file via `#use` |
| `C-c C-i` | `neocaml-utop-interrupt` | Interrupt the utop process |
| `C-c C-k` | `neocaml-utop-clear-buffer` | Clear the transcript buffer |

Choose between plain utop and `dune utop` with `neocaml-utop-flavor`
(`utop` or `dune-utop`), set globally or per project via `.dir-locals.el`;
`dune-utop` makes the project's own libraries available in the toplevel.
`M-x neocaml-utop-require` loads a findlib package via `#require`.

Sending a phrase or definition from a source buffer also echoes its
result (or error) in the minibuffer, SLIME/CIDER style, while the full
output still goes to the transcript. Disable this with:

```emacs-lisp
(setq neocaml-utop-echo-eval-result nil)
```

!!! note
    `neocaml-utop` needs utop (it relies on the `-emacs` protocol), so it
    doesn't apply to the standard `ocaml` toplevel. Pick one backend per
    buffer: enable either `neocaml-repl-minor-mode` or
    `neocaml-utop-minor-mode`, since they share the same keybindings.
