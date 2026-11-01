#!/bin/sh
# Usage: dev/indent-corpus.sh SOURCE-DIR CORPUS-DIR [MAX-FILES]
#
# Formats up to MAX-FILES (300 by default) of the .ml and .mli files
# under SOURCE-DIR (an opam switch's lib directory, say) with
# ocamlformat's conventional and janestreet profiles, into
# CORPUS-DIR/conventional and CORPUS-DIR/janestreet, to score neocaml's
# indentation against with dev/indent-score.el.  ocamlformat formats
# each file under a name in an empty directory, so no .ocamlformat of
# the sources' projects (or a global one) changes the profiles.  Files
# it can't format are skipped, and so are those that turn it off with
# an [@@@ocamlformat "disable"], as it leaves them as they are.

set -eu

src=$1
out=$2
max=${3:-300}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$out/conventional" "$out/janestreet"
find "$src/" \( -name _build -o -name _opam -o -name .git \) -prune -o \
     -type f \( -name '*.ml' -o -name '*.mli' \) -print |
  sort | head -n "$max" | cat -n |
  while read -r n file; do
    grep -q 'ocamlformat "disable"' "$file" && continue
    name=$(printf '%03d-%s' "$n" "$(basename "$file")")
    for profile in conventional janestreet; do
      XDG_CONFIG_HOME=$tmp ocamlformat --enable-outside-detected-project \
        --profile="$profile" --name="$tmp/$name" - \
        < "$file" > "$out/$profile/$name" 2>/dev/null ||
        rm -f "$out/$profile/$name"
    done
  done
