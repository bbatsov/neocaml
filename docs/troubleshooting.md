# Troubleshooting

If you run into issues, `M-x neocaml-bug-report-info` collects useful debug
information (Emacs version, neocaml version, grammar status, Eglot status) and
copies it to the kill ring. Paste this into your bug report to help us
diagnose the problem faster.

## Tree-sitter ABI version mismatch

If you see an error like:

```
Cannot activate tree-sitter, because language grammar for X is unavailable (version-mismatch): 15
```

This means the grammar requires tree-sitter ABI version 15 (tree-sitter
0.25.0+), but your Emacs was compiled against an older tree-sitter library that
only supports ABI 14. You can verify by evaluating `(treesit-library-abi-version)`
-- if it returns 14, that confirms the mismatch.

This isn't specific to neocaml -- any grammar targeting ABI 15 will fail the same
way. The fix is to rebuild Emacs against tree-sitter >= 0.25.0.

### macOS (Homebrew)

```sh
brew update && brew upgrade tree-sitter
brew reinstall emacs-plus  # or however you installed Emacs
```

Make sure the Homebrew tree-sitter is what Emacs links against (check with
`otool -L $(which emacs) | grep tree-sitter`).

### Linux

Distro packages for tree-sitter tend to lag behind. You may need to build
tree-sitter from source:

```sh
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make && sudo make install
sudo ldconfig
```

Then recompile Emacs so it picks up the new library.
