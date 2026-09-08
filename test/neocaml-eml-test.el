;;; neocaml-eml-test.el --- Tests for neocaml-eml-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-eml-mode: font-lock, injection ranges,
;; indentation, and integration.
;;
;; The `sample*.eml.ml' fixtures are verbatim copies of files from
;; camlworks/dream, so that the awkward shapes are the ones the format
;; actually produces rather than ones written to suit the mode.  See
;; test/resources/eml-fixtures.md for what each one is for.

;;; Code:

(require 'cl-lib)
(require 'neocaml-test-helpers)
(require 'neocaml-eml)

;;;; Helpers (eml-specific)

(defmacro when-fontifying-eml-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in eml templates.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-eml-mode content specs)))))

(defun neocaml-eml-test--parser (language)
  "Return the current buffer's parser for LANGUAGE, if any."
  (car (cl-remove-if-not
        (lambda (p) (eq (treesit-parser-language p) language))
        (treesit-parser-list))))

(defun neocaml-eml-test--real-ranges (language)
  "Return the non-degenerate included ranges of LANGUAGE's parser."
  (when-let* ((parser (neocaml-eml-test--parser language))
              (ranges (treesit-parser-included-ranges parser)))
    (cl-remove-if (lambda (r) (= (car r) (cdr r))) ranges)))

(defun neocaml-eml-test--range-texts (language)
  "Return the buffer text of each included range of LANGUAGE's parser."
  (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r)))
          (neocaml-eml-test--real-ranges language)))

(defun neocaml-eml-test--fixture (name)
  "Return the absolute path of the eml fixture NAME."
  (expand-file-name
   (concat "resources/" name)
   (file-name-directory (locate-library "neocaml-eml-test"))))

(defmacro with-eml-fixture (name &rest body)
  "Visit fixture NAME in `neocaml-eml-mode', fontify it, and run BODY.
The buffer keeps a file name, so that the mode sees the extension."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((file (neocaml-eml-test--fixture ,name)))
       (insert-file-contents file)
       (setq buffer-file-name file))
     (neocaml-eml-mode)
     (font-lock-ensure)
     (treesit-update-ranges)
     (goto-char (point-min))
     (unwind-protect (progn ,@body)
       (set-buffer-modified-p nil)
       (setq buffer-file-name nil))))

(defmacro with-eml-named-buffer (name content &rest body)
  "Run BODY in a `neocaml-eml-mode' buffer holding CONTENT and named NAME.
Only the file name matters; NAME need not exist."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (setq buffer-file-name (expand-file-name ,name temporary-file-directory))
     (neocaml-eml-mode)
     (goto-char (point-min))
     (unwind-protect (progn ,@body)
       (set-buffer-modified-p nil)
       (setq buffer-file-name nil))))

(defun neocaml-eml-test--face-of (text &optional offset)
  "Return the face at TEXT in the current buffer, OFFSET characters in."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (get-text-property (+ (match-beginning 0) (or offset 0)) 'face))))

(defconst neocaml-eml-test--template
  "let render tasks =
  <html>
%   tasks |> List.iter begin fun name ->
      <p>Task <%s name %>:</p>
%   end;
  </html>

let () = Dream.run
"
  "A template exercising a code block, code lines, text and a directive.")

;;;; Tests

(describe "neocaml-eml-mode"
  (before-all
    (unless (treesit-language-available-p 'eml)
      (signal 'buttercup-pending "tree-sitter eml grammar not available")))

  (describe "file associations"
    (it "is associated with .eml.ml files"
      (expect (assoc-default "x.eml.ml" auto-mode-alist #'string-match)
              :to-equal 'neocaml-eml-mode))

    (it "is associated with .eml.html files"
      (expect (assoc-default "x.eml.html" auto-mode-alist #'string-match)
              :to-equal 'neocaml-eml-mode))

    (it "is associated with .eml.re files"
      (expect (assoc-default "x.eml.re" auto-mode-alist #'string-match)
              :to-equal 'neocaml-eml-mode))

    ;; `neocaml-mode' claims "\\.ml\\'", which also matches "x.eml.ml", and
    ;; the earlier entry in `auto-mode-alist' wins.  These two pin the
    ;; ordering in both directions.
    (it "does not steal plain .ml files from `neocaml-mode'"
      (expect (assoc-default "x.ml" auto-mode-alist #'string-match)
              :to-equal 'neocaml-mode)))

  (describe "setup"
    (it "creates an eml parser"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (expect (neocaml-eml-test--parser 'eml) :to-be-truthy)))

    (it "parses the template without errors"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "ERROR")
                :to-be nil)))

    (it "does not use tabs for indentation"
      ;; eml counts only spaces as indentation: a tab-indented line has
      ;; indent 0 and will not open a template.
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (expect indent-tabs-mode :to-be nil))))

  (describe "template boundaries"
    ;; Q1: only spaces count as indentation.  `scan_whitespace' in eml.ml
    ;; never matches a tab, so a tab-indented `<html>' has indent 0, fails
    ;; the `[ ]*<' test, and stays inside the code block.
    (it "does not open a template on a tab-indented line"
      (with-neocaml-test-buffer neocaml-eml-mode "let f =\n\t<html>\n"
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "template")
                :to-be nil)))

    (it "opens a template on a space-indented line"
      (with-neocaml-test-buffer neocaml-eml-mode "let f =\n  <html>\n"
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "template")
                :to-be-truthy)))

    ;; Dream ships one of these, so it is not a hypothetical.
    (it "handles a file with no template at all"
      (with-eml-fixture "sample-no-template.eml.ml"
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "ERROR")
                :to-be nil)
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "template")
                :to-be nil)
        (expect (neocaml-eml-test--face-of "let () =")
                :to-equal 'font-lock-keyword-face))))

  (describe "font-lock"
    (when-fontifying-eml-it "fontifies directive delimiters"
      ("let f x =\n  <p><%s x %></p>\n"
       ("<%" neocaml-eml-delimiter-face)
       ("%>" neocaml-eml-delimiter-face)))

    (when-fontifying-eml-it "fontifies the code line marker"
      ("let f x =\n  <html>\n% ignore x;\n  </html>\n"
       ("%" neocaml-eml-delimiter-face)))

    ;; The conversion character and the `!' are single characters with no
    ;; symbol boundary around them, which the string form of the face
    ;; matcher needs, so these use explicit positions into the content.
    (when-fontifying-eml-it "fontifies the Printf conversion"
      ("let f x =\n  <p><%i x %></p>\n"
       (18 18 neocaml-eml-format-face)))

    (when-fontifying-eml-it "fontifies the raw marker distinctly"
      ("let f x =\n  <p><%s! x %></p>\n"
       (18 18 neocaml-eml-format-face)
       (19 19 neocaml-eml-raw-face)))

    (when-fontifying-eml-it "fontifies the options and terminator lines"
      ("let f response =\n  %% response\n  <p>hi</p>\n  %%\n"
       ("%% response" font-lock-preprocessor-face)))

    ;; `<%B b %>' and `<%02X n %>' both appear in Dream's examples; the
    ;; conversion is not always a single letter.
    (when-fontifying-eml-it "fontifies non-string and multi-character conversions"
      ("let f b =\n  <p><%B b %></p>\n"
       (18 18 neocaml-eml-format-face))
      ("let f n =\n  <p><%02X n %></p>\n"
       (18 20 neocaml-eml-format-face)))

    ;; `% let%lwt () = Dream.flush response in' is a code line whose body
    ;; contains a `%' of its own.  Only the marker in column 0 belongs to
    ;; eml; the rest of the line is OCaml and must keep the injected faces.
    ;; Anchoring the delimiter capture to the `%' child rather than to
    ;; `code_line' is what keeps them apart.
    (it "does not fontify a percent inside a code line body"
      (with-eml-fixture "sample-stream.eml.ml"
        (expect (neocaml-eml-test--face-of "%   let rec paragraphs")
                :to-equal 'neocaml-eml-delimiter-face)
        (expect (neocaml-eml-test--face-of "let%lwt" 3)
                :not :to-equal 'neocaml-eml-delimiter-face)))

    ;; A directive inside an HTML attribute value sits in a gap in the html
    ;; parser's ranges, and the attribute node spans that gap.  The eml
    ;; rules override so the directive keeps its own faces.
    (when-fontifying-eml-it "keeps its faces inside an HTML attribute"
      ("let f request =\n  <input value=\"<%s! token request %>\">\n"
       (33 34 neocaml-eml-delimiter-face)
       (35 35 neocaml-eml-format-face)
       (36 36 neocaml-eml-raw-face))))

  (describe "OCaml injection"
    (before-all
      (unless (treesit-language-available-p 'ocaml)
        (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

    (it "creates a single ocaml parser"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (treesit-update-ranges)
        (expect (length (cl-remove-if-not
                         (lambda (p) (eq (treesit-parser-language p) 'ocaml))
                         (treesit-parser-list)))
                :to-equal 1)))

    (it "covers the code block, the code lines and the directive body"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (treesit-update-ranges)
        (let ((texts (neocaml-eml-test--range-texts 'ocaml)))
          (expect texts :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "let render tasks =" s)) texts)
                  :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "List.iter begin" s)) texts)
                  :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "end;" s)) texts)
                  :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "name" s)) texts)
                  :to-be-truthy))))

    (it "excludes the code line marker and the template text"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (treesit-update-ranges)
        (let ((texts (neocaml-eml-test--range-texts 'ocaml)))
          (expect texts :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "<html>" s)) texts)
                  :to-be nil)
          (expect (cl-some (lambda (s) (string-search "<p>Task" s)) texts)
                  :to-be nil)
          (expect (cl-some (lambda (s) (string-prefix-p "%" s)) texts)
                  :to-be nil))))

    ;; The commonest shape in Dream's corpus: the template is the entire
    ;; body of a `let', so with the text removed the OCaml reads `let home ='
    ;; followed straight by the next binding.  The injected tree therefore
    ;; contains an ERROR -- that is inherent, not a bug -- but tree-sitter
    ;; keeps the tokens inside it and the surrounding OCaml stays highlighted.
    ;; 12 of Dream's 25 .eml.{ml,html} examples have this shape, so a change
    ;; to how the ranges are grouped must not silently degrade it.
    (it "still highlights OCaml around a template that is a whole let body"
      (with-eml-fixture "sample-text-template.eml.ml"
        (expect (neocaml-eml-test--face-of "let home")
                :to-equal 'font-lock-keyword-face)
        (expect (neocaml-eml-test--face-of "Dream.run")
                :to-equal 'font-lock-type-face)))

    (it "handles two templates in one file"
      (with-eml-fixture "sample.eml.ml"
        (expect (length (cl-remove-if-not
                         (lambda (n) (equal (treesit-node-type n) "template"))
                         (treesit-node-children (treesit-buffer-root-node 'eml))))
                :to-equal 2)
        ;; Both bindings, either side of the first template, are highlighted.
        (expect (neocaml-eml-test--face-of "let render_home")
                :to-equal 'font-lock-keyword-face)
        (expect (neocaml-eml-test--face-of "let render_task")
                :to-equal 'font-lock-keyword-face)))

    ;; The `%%' options and terminator lines are eml's own syntax: they are
    ;; neither OCaml nor HTML and must not reach either parser.
    (it "keeps the %% lines out of every injection"
      (with-eml-fixture "sample-stream.eml.ml"
        (dolist (language '(ocaml html))
          (let ((texts (neocaml-eml-test--range-texts language)))
            (expect texts :to-be-truthy)
            (dolist (text texts)
              (expect (string-search "%%" text) :to-be nil))))))

    ;; The only file in Dream's corpus whose template closes on a non-zero
    ;; dedent: indent 4, wrapped in `Dream.set_body ... begin', closing at
    ;; `  end;' at indent 2.  The `end;' has to reach the OCaml parser with
    ;; its leading whitespace or the `begin' never balances.
    (it "carries a non-zero dedent back into the OCaml stream"
      (with-eml-fixture "sample-dedent.eml.ml"
        (let ((stream (apply #'concat (neocaml-eml-test--range-texts 'ocaml))))
          (expect (string-search "begin" stream) :to-be-truthy)
          (expect (string-search "end;" stream) :to-be-truthy)
          (expect (string-search "<html>" stream) :to-be nil))
        (expect (neocaml-eml-test--face-of "begin")
                :to-equal 'font-lock-keyword-face)))

    ;; The point of sharing one parser: a code block and the `%' lines under
    ;; it are one statement stream, so `begin' and its `end' have to be seen
    ;; by the same parser to balance.
    (it "highlights OCaml in both the code block and the code lines"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "let render")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face)
        (goto-char (point-min))
        (search-forward "begin")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-keyword-face))))

  (describe "HTML injection"
    (before-all
      (unless (treesit-language-available-p 'html)
        (signal 'buttercup-pending "tree-sitter HTML grammar not available")))

    (it "covers the template text and nothing else"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (treesit-update-ranges)
        (let ((texts (neocaml-eml-test--range-texts 'html)))
          (expect texts :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "<html>" s)) texts)
                  :to-be-truthy)
          (expect (cl-some (lambda (s) (string-search "List.iter" s)) texts)
                  :to-be nil)
          (expect (cl-some (lambda (s) (string-search "<%s" s)) texts)
                  :to-be nil))))

    (it "can be turned off"
      (let ((neocaml-eml-inject-html nil))
        (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
          (treesit-update-ranges)
          (expect (neocaml-eml-test--parser 'html) :to-be nil)))))

  (describe "embedded language"
    (it "defaults to OCaml"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (expect neocaml-eml-embedded-language :to-equal 'ocaml)))

    (it "is a safe file-local variable"
      ;; `.eml.html' is OCaml or Reason depending on whether the dune rule
      ;; passes --emit-reason, so it has to be settable per file.
      (expect (get 'neocaml-eml-embedded-language 'safe-local-variable)
              :to-be-truthy))

    (it "degrades gracefully when the grammar is missing"
      (let ((neocaml-eml-embedded-language 'no-such-language))
        (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
          (expect (treesit-search-subtree
                   (treesit-buffer-root-node 'eml) "ERROR")
                  :to-be nil))))

    ;; dream_eml reads the syntax off `Filename.extension', so .eml.re is
    ;; Reason.  Emacs ships no `reason' grammar, so this also exercises the
    ;; path where the code regions go unhighlighted.
    (it "selects Reason for a .eml.re file"
      (with-eml-named-buffer "x.eml.re" "let f = x => {\n  <p>hi</p>\n};\n"
        (expect neocaml-eml-embedded-language :to-equal 'reason)
        (expect (treesit-search-subtree
                 (treesit-buffer-root-node 'eml) "ERROR")
                :to-be nil)))

    ;; .eml.html is the ambiguous one: `Filename.extension' gives ".html",
    ;; which falls through to OCaml unless the dune rule passes
    ;; --emit-reason.  Default to OCaml and let the user override.
    (it "defaults a .eml.html file to OCaml"
      (with-eml-named-buffer "x.eml.html" "let f x =\n  <p><%s x %></p>\n"
        (expect neocaml-eml-embedded-language :to-equal 'ocaml))))

  (describe "incremental reparse"
    (before-all
      (unless (and (treesit-language-available-p 'ocaml)
                   (treesit-language-available-p 'html))
        (signal 'buttercup-pending "OCaml or HTML grammar not available")))

    (it "extends the injection ranges after an edit"
      (with-neocaml-test-buffer neocaml-eml-mode "let f x =\n  <p>hi</p>\n"
        (treesit-update-ranges)
        (let ((html-before (length (neocaml-eml-test--real-ranges 'html)))
              (ocaml-before (length (neocaml-eml-test--real-ranges 'ocaml))))
          (goto-char (point-max))
          (insert "  <p><%s x %></p>\n")
          (treesit-update-ranges)
          (font-lock-ensure)
          (expect (length (neocaml-eml-test--real-ranges 'html))
                  :to-be-greater-than html-before)
          (expect (length (neocaml-eml-test--real-ranges 'ocaml))
                  :to-be-greater-than ocaml-before)
          ;; The directive typed in is picked up, not just re-spanned.
          (expect (neocaml-eml-test--face-of "<%")
                  :to-equal 'neocaml-eml-delimiter-face)))))

  (describe "indentation"
    ;; eml is layout-sensitive in a way the grammar cannot repair, so the
    ;; rules preserve indentation rather than compute it.
    (it "keeps the code line marker in column 0"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (goto-char (point-min))
        (search-forward "tasks |> List.iter")
        (indent-according-to-mode)
        (beginning-of-line)
        (expect (char-after) :to-equal ?%)))

    (it "leaves template text where it is"
      (with-neocaml-test-buffer neocaml-eml-mode neocaml-eml-test--template
        (let ((before (buffer-string)))
          (indent-region (point-min) (point-max))
          (expect (buffer-string) :to-equal before)))))

  (describe "integration"
    (it "opens every Dream fixture with no eml parse errors"
      (dolist (fixture '("sample.eml.ml"
                         "sample-stream.eml.ml"
                         "sample-dedent.eml.ml"
                         "sample-text-template.eml.ml"
                         "sample-no-template.eml.ml"))
        (with-eml-fixture fixture
          (expect (treesit-search-subtree
                   (treesit-buffer-root-node 'eml) "ERROR")
                  :to-be nil)
          (expect (treesit-search-subtree
                   (treesit-buffer-root-node 'eml) "MISSING")
                  :to-be nil))))

    (it "creates both injected parsers for a template with code and text"
      (with-eml-fixture "sample.eml.ml"
        (when (treesit-language-available-p 'ocaml)
          (expect (neocaml-eml-test--parser 'ocaml) :to-be-truthy))
        (when (treesit-language-available-p 'html)
          (expect (neocaml-eml-test--parser 'html) :to-be-truthy))))))

(provide 'neocaml-eml-test)

;;; neocaml-eml-test.el ends here
