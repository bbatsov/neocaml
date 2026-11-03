# Debugging

neocaml integrates with [dape](https://github.com/svaante/dape) (a
Debug Adapter Protocol client, available from GNU ELPA) and
[ocamlearlybird](https://github.com/hackwaly/ocamlearlybird) for OCaml
debugging. When dape is loaded, neocaml automatically registers its modes with
dape's built-in `ocamlearlybird` configuration.

ocamlearlybird debugs **bytecode**. For **native** executables built by
`ocamlopt`, see [Native debugging](#native-debugging-lldb-gdb) below.

## Setup

1. Install ocamlearlybird:

   ```
   opam install earlybird
   ```

2. Install dape from GNU ELPA (`M-x package-install RET dape RET`).

3. Compile your program as bytecode. In your
   [`dune` file](https://dune.readthedocs.io/en/stable/reference/dune/executable.html):

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

## Native debugging (lldb, gdb)

ocamlearlybird cannot attach to native executables. For those, use a native
debugger through dape's `lldb-dap` configuration (macOS, Linux) or `gdb`
(Linux; dape requires gdb >= 14.1). neocaml registers its modes with these
configurations too, so they appear at the `M-x dape` prompt in OCaml buffers.

Compile with debug information -- in your `dune` file:

```
(executable
 (name main)
 (ocamlopt_flags (:standard -g)))
```

Then `dune build`, and start a session with `M-x dape`, choosing `lldb-dap`
(or `gdb`) and pointing `:program` at `_build/default/bin/main.exe`.

Everything past that -- symbol mangling, setting breakpoints, reading
backtraces, and inspecting OCaml values with the `tools/gdb.py` and
`tools/lldb.py` helper scripts -- is covered by the
[Native debugging chapter](https://ocaml.org/manual/5.5/native-debugger.html)
of the OCaml manual. Two settings from it are worth knowing up front, because
dune's build layout makes them likely:

- if the debugger cannot find your sources, set `target.source-map` (lldb) or
  use `directory` (gdb) to redirect the recorded paths
- build dependencies with `OPAMKEEPBUILDDIR=1` so their sources remain
  available for source-level debugging

## Caveats

- ocamlearlybird only works with **bytecode** (`.bc`) executables, not native code.
- For dune >= 3.0, you may need to add `(map_workspace_root false)` to your
  `dune-project` for breakpoints to resolve correctly.
- See the [ocamlearlybird documentation](https://github.com/hackwaly/ocamlearlybird)
  for troubleshooting and known limitations.
- neocaml normally offers to redirect you away from `_build/` files. If this
  interferes with your debugging workflow, disable it with
  `(setq neocaml-redirect-build-files nil)`.
  See [Configuration](configuration.md#build-directory-redirect) for details.
