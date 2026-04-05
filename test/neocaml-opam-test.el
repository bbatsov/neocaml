;;; neocaml-opam-test.el --- Tests for neocaml-opam-mode -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-opam-mode: font-lock and indentation.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-opam)

;;;; Font-lock helpers (opam-specific)

(defmacro when-fontifying-opam-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in opam code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-opam-mode content specs)))))

;;;; Indentation helpers (opam-specific)

(defmacro when-indenting-opam-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-opam-mode'."
  (declare (indent 1))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (neocaml-opam-mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

;;;; Tests

(describe "neocaml-opam font-lock"
  (before-all
    (unless (treesit-language-available-p 'opam)
      (signal 'buttercup-pending "tree-sitter opam grammar not available")))

  (describe "comment feature"
    (when-fontifying-opam-it "fontifies line comments"
      ("# a comment"
       ("# a comment" font-lock-comment-face))))

  (describe "keyword feature"
    (when-fontifying-opam-it "fontifies field names"
      ("opam-version: \"2.0\""
       ("opam-version" font-lock-keyword-face)))

    (when-fontifying-opam-it "fontifies section names"
      ("url { src: \"https://example.com\" }"
       ("url" font-lock-keyword-face)))

    (when-fontifying-opam-it "fontifies various fields"
      ("depends: []"
       ("depends" font-lock-keyword-face))
      ("build: []"
       ("build" font-lock-keyword-face))
      ("synopsis: \"test\""
       ("synopsis" font-lock-keyword-face))))

  (describe "string feature"
    (when-fontifying-opam-it "fontifies regular strings"
      ("version: \"1.0.0\""
       ("\"1.0.0\"" font-lock-string-face))))

  (describe "constant feature"
    (when-fontifying-opam-it "fontifies booleans"
      ("available: true"
       ("true" font-lock-constant-face))
      ("available: false"
       ("false" font-lock-constant-face))))

  (describe "number feature"
    (when-fontifying-opam-it "fontifies integers"
      ;; Integers appear rarely standalone but they exist in the grammar
      ("x-priority: 10"
       ("10" font-lock-number-face))))

  (describe "operator feature"
    (when-fontifying-opam-it "fontifies relational operators"
      ("available: os = \"linux\""
       ("=" font-lock-operator-face)))

    (when-fontifying-opam-it "fontifies logical operators"
      ("available: os = \"linux\" & arch = \"x86_64\""
       ("&" font-lock-operator-face))))

  (describe "bracket feature"
    (when-fontifying-opam-it "fontifies brackets"
      ("depends: []"
       (10 10 font-lock-bracket-face)
       (11 11 font-lock-bracket-face))))

  (describe "delimiter feature"
    (when-fontifying-opam-it "fontifies colon"
      ("name: \"test\""
       (":" font-lock-delimiter-face)))))

(describe "neocaml-opam indentation"
  (before-all
    (unless (treesit-language-available-p 'opam)
      (signal 'buttercup-pending "tree-sitter opam grammar not available")))

  (when-indenting-opam-it "indents top-level fields at column 0"
    "opam-version: \"2.0\"
name: \"example\"
version: \"1.0.0\"")

  (when-indenting-opam-it "indents list contents"
    "depends: [
  \"ocaml\"
  \"dune\"
]")

  (when-indenting-opam-it "indents nested lists"
    "build: [
  [\"dune\" \"build\" \"-p\" name]
]")

  (when-indenting-opam-it "indents section contents"
    "url {
  src: \"https://example.com/archive.tar.gz\"
}")

  (when-indenting-opam-it "indents list items with option braces"
    "depends: [
  \"ocaml\" {>= \"4.14.0\"}
  \"dune\" {>= \"3.0\"}
]")

  (when-indenting-opam-it "preserves content inside triple-quoted strings"
    "description: \"\"\"
This is a longer description
of the example package.
\"\"\""))

(describe "neocaml-opam integration"
  (before-all
    (unless (treesit-language-available-p 'opam)
      (signal 'buttercup-pending "tree-sitter opam grammar not available")))

  (it "preserves correct indentation of sample.opam"
    (let* ((file (expand-file-name "test/resources/sample.opam"
                                   (file-name-directory (or load-file-name
                                                            buffer-file-name
                                                            default-directory))))
           (original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
      (with-temp-buffer
        (insert original)
        (neocaml-opam-mode)
        (indent-region (point-min) (point-max))
        (expect (buffer-string) :to-equal original))))

  (it "applies expected font-lock faces to sample.opam"
    (let ((file (expand-file-name "test/resources/sample.opam"
                                  (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory)))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((treesit-font-lock-level 4))
          (neocaml-opam-mode))
        (font-lock-ensure)
        ;; Check that "opam-version" is fontified as keyword
        (goto-char (point-min))
        (expect (get-text-property 1 'face)
                :to-equal 'font-lock-keyword-face)
        ;; Check that a string is fontified
        (search-forward "\"2.0\"")
        (expect (get-text-property (- (point) 3) 'face)
                :to-equal 'font-lock-string-face)
        ;; Check that a comment is fontified
        (search-forward "# This")
        (expect (get-text-property (match-beginning 0) 'face)
                :to-equal 'font-lock-comment-face)))))

(describe "neocaml-opam lint parsing"
  (it "parses error with location"
    (with-temp-buffer
      (insert "opam-version: \"2.0\"\nname: \"test\"\ndepends: [\n  bad\n]\n")
      (let ((diag (neocaml-opam--parse-lint-line
                   "             error  3: File format error in 'depends' at line 5, column 2: expected pkg-formula"
                   (current-buffer))))
        (expect diag :not :to-be nil)
        (expect (flymake-diagnostic-type diag) :to-be :error)
        (expect (flymake-diagnostic-text diag) :to-match "opam lint \\[3\\]"))))

  (it "parses error without location"
    (with-temp-buffer
      (insert "opam-version: \"2.0\"\n")
      (let ((diag (neocaml-opam--parse-lint-line
                   "             error 23: Missing field 'maintainer'"
                   (current-buffer))))
        (expect diag :not :to-be nil)
        (expect (flymake-diagnostic-type diag) :to-be :error)
        (expect (flymake-diagnostic-text diag) :to-match "Missing field")
        ;; File-level diagnostic points to position 1
        (expect (flymake-diagnostic-beg diag) :to-equal 1))))

  (it "parses warning without location"
    (with-temp-buffer
      (insert "opam-version: \"2.0\"\n")
      (let ((diag (neocaml-opam--parse-lint-line
                   "           warning 25: Missing field 'authors'"
                   (current-buffer))))
        (expect diag :not :to-be nil)
        (expect (flymake-diagnostic-type diag) :to-be :warning)
        (expect (flymake-diagnostic-text diag) :to-match "Missing field"))))

  (it "returns nil for non-diagnostic lines"
    (with-temp-buffer
      (insert "opam-version: \"2.0\"\n")
      (expect (neocaml-opam--parse-lint-line "Errors." (current-buffer))
              :to-be nil)
      (expect (neocaml-opam--parse-lint-line "" (current-buffer))
              :to-be nil)
      (expect (neocaml-opam--parse-lint-line "Passed." (current-buffer))
              :to-be nil))))

(describe "neocaml-opam auto-mode"
  (it "matches foo.opam"
    (let ((match (assoc "foo.opam" auto-mode-alist #'string-match-p)))
      (expect match :to-be-truthy)
      (expect (cdr match) :to-equal 'neocaml-opam-mode)))

  (it "matches bare /opam"
    (let ((match (assoc "/opam" auto-mode-alist #'string-match-p)))
      (expect match :to-be-truthy)
      (expect (cdr match) :to-equal 'neocaml-opam-mode)))

  (it "matches foo.opam.template"
    (let ((match (assoc "foo.opam.template" auto-mode-alist #'string-match-p)))
      (expect match :to-be-truthy)
      (expect (cdr match) :to-equal 'neocaml-opam-mode)))

  (it "does not match foo.opam.lock"
    (let ((match (assoc "foo.opam.lock" auto-mode-alist #'string-match-p)))
      (expect (or (null match)
                  (not (eq (cdr match) 'neocaml-opam-mode)))
              :to-be-truthy))))

;;; neocaml-opam-test.el ends here
