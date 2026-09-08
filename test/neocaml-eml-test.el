;;; neocaml-eml-test.el --- Tests for neocaml-eml-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-eml-mode: font-lock, injection ranges,
;; indentation, and integration.

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
          (expect (cl-some (lambda (s) (string-search "<html>" s)) texts)
                  :to-be nil)
          (expect (cl-some (lambda (s) (string-search "<p>Task" s)) texts)
                  :to-be nil)
          (expect (cl-some (lambda (s) (string-prefix-p "%" s)) texts)
                  :to-be nil))))

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
                  :to-be nil)))))

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
    (it "opens a real template with both parsers and no errors"
      (let ((file (expand-file-name
                   "resources/sample.eml.ml"
                   (file-name-directory (locate-library "neocaml-eml-test")))))
        (with-temp-buffer
          (insert-file-contents file)
          (neocaml-eml-mode)
          (font-lock-ensure)
          (expect (treesit-search-subtree
                   (treesit-buffer-root-node 'eml) "ERROR")
                  :to-be nil)
          (when (treesit-language-available-p 'ocaml)
            (expect (neocaml-eml-test--parser 'ocaml) :to-be-truthy))
          (when (treesit-language-available-p 'html)
            (expect (neocaml-eml-test--parser 'html) :to-be-truthy)))))))

(provide 'neocaml-eml-test)

;;; neocaml-eml-test.el ends here
