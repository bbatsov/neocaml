# Contributing to neocaml

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `main`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Include any relevant code to the issue summary.

## Pull requests

* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][2].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Fix indentation for ...`).
* Update the [changelog][3].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (`C-c ? d`).
* [Squash related commits together][4].
* Open a [pull request][5] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

## Getting started

Install the required `ocaml` and `ocaml-interface` grammars by running
`M-x neocaml-install-grammars`.

## Debugging

Set `neocaml--debug` to get more debug information from TreeSitter:

- `t` — output indentation debug data and enable `treesit-inspect-mode`
  (shows the current node in the modeline).
- `'font-lock` — additionally output font-lock debug info (can get very noisy).

```emacs-lisp
;; enable all TreeSitter debug information
(setq neocaml--debug 'font-lock)
```

## Testing tree-sitter queries

As [combobulate](https://github.com/mickeynp/combobulate) doesn't support OCaml yet, the best way to test TS queries is:

> If you don't want to use Combobulate to help you, the builtin method – the
> only method – is to call treesit-query-capture with a starting node (often the
> one from treesit-buffer-root-node or treesit-parser-root-node) and the query
> and then manually inspect the output to see if it's right. Ugh. It's messy,
> and it's hard work. Trust me, I know. I recommend you learn how to use IELM if
> you decide to go this route.

## Architecture

See [doc/DESIGN.md](doc/DESIGN.md) for internal architecture documentation
covering `.mli` support, font-locking levels, and indentation rules.

[1]: https://github.com/bbatsov/neocaml/issues
[2]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[3]: https://github.com/bbatsov/neocaml/blob/main/CHANGELOG.md
[4]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[5]: https://help.github.com/articles/using-pull-requests
