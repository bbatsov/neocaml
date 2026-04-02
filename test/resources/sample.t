Basic commands and output:

  $ echo hello
  hello

  $ echo "multi-word output"
  multi-word output

  $ cat <<EOF
  > line one
  > line two
  > EOF
  line one
  line two

Multi-line commands with backslash continuation:

  $ echo \
  >   hello \
  >   world
  hello world

Output modifiers:

  $ date
  .* (re)

  $ ls *.ml
  f* (glob)

  $ printf "no newline"
  no newline (no-eol)

  $ printf "\x00binary"
  \x00binary (esc)

Non-zero exit codes:

  $ false
  [1]

  $ exit 42
  [42]

Multiple lines of output:

  $ seq 1 5
  1
  2
  3
  4
  5

Empty command (just a prompt):

  $

Dune-typical patterns:

  $ cat > foo.ml <<EOF
  > let () = print_endline "hello"
  > EOF

  $ ocamlfind ocamlc -package str foo.ml -linkpkg -o foo 2>&1
  File "foo.ml", line 3, characters 5-8:
  Error: Unbound value bar
  [2]

Unreachable commands after early exit:

  $ exit 1
  [1]
  ***** UNREACHABLE *****
  $ echo "this never runs"

A test with mixed prose and commands showing realistic dune workflow:

  $ dune build 2>&1
  $ dune exec ./main.exe
  Hello, world!

  $ dune exec ./main.exe -- --verbose
  [DEBUG] Starting up
  Hello, world!
  [DEBUG] Shutting down
