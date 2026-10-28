# eml test fixtures

The `sample*.eml.ml` files here are copied verbatim from
[camlworks/dream](https://github.com/camlworks/dream) at commit `ebb6d57`, under
Dream's MIT licence (Copyright 2021 Anton Bachin). They are used unmodified so
that `neocaml-eml-test.el` tests real templates rather than ones written to fit
the mode.

| Fixture | Copied from | Why this one |
| --- | --- | --- |
| `sample.eml.ml` | `example/w-template-logic/template.eml.ml` | Two templates in one file, interleaved with three code blocks; `%` code lines carrying `begin`/`end;` across the template text |
| `sample-stream.eml.ml` | `example/w-template-stream/template_stream.eml.ml` | Explicit `%% response` options line; a `%` code line containing `let%lwt`, whose inner `%` must not read as a delimiter |
| `sample-dedent.eml.ml` | `example/9-error/error.eml.ml` | The only file in Dream's corpus whose template closes on a *non-zero* dedent: indent 4, wrapped in `Dream.set_body … begin`, closing at `  end;` at indent 2 |
| `sample-text-template.eml.ml` | `example/w-nginx/server.eml.ml` | A template that is the entire body of a `let`, the commonest shape in the corpus: with the template text removed the OCaml reads `let home =` followed by the next binding, so the injected OCaml tree legitimately contains an `ERROR` |
| `sample-no-template.eml.ml` | `example/z-playground/sandbox/ocaml/server.eml.ml` | A `.eml.ml` with no template at all |

To refresh, re-copy from a Dream checkout and update the commit above.
