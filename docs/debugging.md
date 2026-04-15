# Debugging

neocaml integrates with [dape](https://github.com/svaante/dape) (a DAP client,
available from GNU ELPA) and
[ocamlearlybird](https://github.com/hackwaly/ocamlearlybird) for OCaml
debugging. When dape is loaded, neocaml automatically registers its modes with
dape's built-in `ocamlearlybird` configuration.

## Setup

1. Install ocamlearlybird:

   ```
   opam install earlybird
   ```

2. Install dape from GNU ELPA (`M-x package-install RET dape RET`).

3. Compile your program as bytecode. In your `dune` file:

   ```
   (executable
    (name main)
    (modes byte exe))
   ```

4. Build with `dune build`.

## Usage

Set breakpoints with `M-x dape-breakpoint-toggle` (or `C-x C-a b`) on the
lines where you want execution to pause. Then start a debug session with `M-x
dape` -- it will offer the `ocamlearlybird` config and guess the program path
based on your current buffer name (e.g., `_build/default/bin/main.bc`). You can
edit the path in the minibuffer if needed.

Once the session starts, dape provides the standard debugging commands:

| Command | Keybinding | Description |
|---|---|---|
| `dape-next` | `C-x C-a n` | Step over |
| `dape-step-in` | `C-x C-a i` | Step into |
| `dape-step-out` | `C-x C-a o` | Step out |
| `dape-continue` | `C-x C-a c` | Continue execution |
| `dape-breakpoint-toggle` | `C-x C-a b` | Toggle breakpoint at point |
| `dape-info` | | Show debugger info (variables, stack, breakpoints) |
| `dape-repl` | | Open the debug REPL |
| `dape-quit` | `C-x C-a q` | Stop the debug session |

## Caveats

- ocamlearlybird only works with **bytecode** (`.bc`) executables, not native code.
- For dune >= 3.0, you may need to add `(map_workspace_root false)` to your
  `dune-project` for breakpoints to resolve correctly.
- See the [ocamlearlybird documentation](https://github.com/hackwaly/ocamlearlybird)
  for troubleshooting and known limitations.
- neocaml normally offers to redirect you away from `_build/` files. If this
  interferes with your debugging workflow, disable it:

  ```emacs-lisp
  (setq neocaml-redirect-build-files nil)
  ```
