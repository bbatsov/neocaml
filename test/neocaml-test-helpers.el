;;; neocaml-test-helpers.el --- Shared test helpers for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Shared macros and functions used across neocaml buttercup test suites.
;; Provides unified buffer setup, font-lock assertion helpers, and
;; indentation test macros that work with both `neocaml-mode' and
;; `neocaml-interface-mode'.

;;; Code:

(require 'buttercup)
(require 'neocaml)

;;;; String helpers

(defun neocaml-test--dedent (string)
  "Remove common leading whitespace from all non-empty lines in STRING.
A single leading newline is stripped (so the string can start on the
line after the opening quote), and a single trailing newline followed
by only whitespace is stripped (so the closing quote can sit on its
own indented line).  Interior blank lines and trailing newlines that
are part of the content are preserved.

  (neocaml-test--dedent \"
    let x = 1
    let y = 2\")

produces \"let x = 1\\nlet y = 2\"."
  (if (not (string-search "\n" string))
      string
    (let* ((str (if (string-prefix-p "\n" string)
                    (substring string 1)
                  string))
           ;; Strip a single trailing newline + optional whitespace
           ;; (the closing quote's indentation), but preserve
           ;; intentional trailing newlines in the content.
           (str (if (string-match "\n[ \t]*\\'" str)
                    (substring str 0 (match-beginning 0))
                  str))
           (lines (split-string str "\n"))
           (non-empty (seq-filter (lambda (l) (not (string-blank-p l))) lines))
           (min-indent (if non-empty
                          (apply #'min (mapcar (lambda (l)
                                                (- (length l) (length (string-trim-left l))))
                                              non-empty))
                        0)))
      (mapconcat (lambda (l)
                   (if (string-blank-p l) ""
                     (substring l min-indent)))
                 lines "\n"))))

;;;; Buffer setup

(defmacro with-neocaml-test-buffer (mode content &rest body)
  "Set up a temporary buffer with CONTENT in MODE, run BODY.
MODE should be a symbol like `neocaml-mode' or `neocaml-interface-mode'.
CONTENT is automatically dedented via `neocaml-test--dedent'."
  (declare (indent 2))
  `(with-temp-buffer
     (insert (neocaml-test--dedent ,content))
     (funcall #',mode)
     (goto-char (point-min))
     ,@body))

(defmacro with-neocaml-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT in `neocaml-mode', run BODY.
CONTENT is automatically dedented via `neocaml-test--dedent'."
  (declare (indent 1))
  `(with-neocaml-test-buffer neocaml-mode ,content ,@body))

(defmacro with-neocaml-interface-buffer (content &rest body)
  "Set up a temporary buffer with CONTENT in `neocaml-interface-mode', run BODY.
CONTENT is automatically dedented via `neocaml-test--dedent'."
  (declare (indent 1))
  `(with-neocaml-test-buffer neocaml-interface-mode ,content ,@body))

;;;; Font-lock helpers

(defun neocaml-test-face-at-range (start end)
  "Return the face at range [START, END] in the current buffer.
If all positions in the range share the same face, return it.
Otherwise return the symbol `various-faces'."
  (let ((face (get-text-property start 'face)))
    (if (= start end)
        face
      (let ((pos (1+ start))
            (consistent t))
        (while (and consistent (<= pos end))
          (unless (equal (get-text-property pos 'face) face)
            (setq consistent nil))
          (setq pos (1+ pos)))
        (if consistent face 'various-faces)))))

(defun neocaml-test--check-face-specs (mode content face-specs)
  "Fontify CONTENT with MODE and assert FACE-SPECS.
Each element of FACE-SPECS is either:
  (\"text\" EXPECTED-FACE) — search for \"text\" sequentially and check its face
  (START END EXPECTED-FACE) — check face at position range [START, END]"
  (with-temp-buffer
    (insert content)
    (let ((treesit-font-lock-level 4))
      (funcall mode))
    (font-lock-ensure)
    (goto-char (point-min))
    (dolist (spec face-specs)
      (if (stringp (car spec))
          ;; Text-based spec: ("text" face)
          (let* ((text (nth 0 spec))
                 (expected (nth 1 spec))
                 (case-fold-search nil)
                 (found (if (string-match-p "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'" text)
                            ;; Identifier-like text: use symbol boundaries
                            ;; to avoid matching inside longer identifiers/keywords
                            (re-search-forward
                             (concat "\\_<" (regexp-quote text) "\\_>") nil t)
                          (search-forward text nil t))))
            (expect found :not :to-be nil)
            (when found
              (let* ((start (match-beginning 0))
                     (end (1- (match-end 0)))
                     (actual (neocaml-test-face-at-range start end)))
                (expect actual :to-equal expected))))
        ;; Position-based spec: (start end face)
        (let* ((start (nth 0 spec))
               (end (nth 1 spec))
               (expected (nth 2 spec))
               (actual (neocaml-test-face-at-range start end)))
          (expect actual :to-equal expected))))))

(defmacro when-fontifying-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in OCaml code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-mode content specs)))))

(defmacro when-fontifying-interface-it (description &rest tests)
  "Create a Buttercup test asserting font-lock faces in OCaml interface code.
DESCRIPTION is the test name.  Each element of TESTS is
  (CODE SPEC ...)
where each SPEC is either (\"text\" FACE) for text-based matching
or (START END FACE) for position-based matching."
  (declare (indent 1))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((content (car test))
             (specs (cdr test)))
         (neocaml-test--check-face-specs #'neocaml-interface-mode content specs)))))

;;;; Indentation helpers

(defun neocaml-test--strip-indentation (code)
  "Remove all leading whitespace from each line of CODE."
  (mapconcat
   (lambda (line) (string-trim-left line))
   (split-string code "\n")
   "\n"))

(defmacro when-indenting--it (mode description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
MODE is the major mode function to use.  DESCRIPTION is the test name.
Each element of CODE-STRINGS is a properly-indented OCaml code string.
The macro strips indentation, re-indents via MODE, and asserts the
result matches the original."
  (declare (indent 2))
  `(it ,description
     ,@(mapcar
        (lambda (code)
          `(let ((expected ,code))
             (expect
              (with-temp-buffer
                (insert (neocaml-test--strip-indentation expected))
                (funcall #',mode)
                (indent-region (point-min) (point-max))
                (buffer-string))
              :to-equal expected)))
        code-strings)))

(defmacro when-indenting-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-mode'."
  (declare (indent 1))
  `(when-indenting--it neocaml-mode ,description ,@code-strings))

(defmacro when-indenting-interface-it (description &rest code-strings)
  "Create a Buttercup test that asserts each CODE-STRING indents correctly.
DESCRIPTION is the test name.  Uses `neocaml-interface-mode'."
  (declare (indent 1))
  `(when-indenting--it neocaml-interface-mode ,description ,@code-strings))

(defmacro when-indenting-with-point--it (mode description before after)
  "Create a Buttercup test asserting single-line indentation with cursor tracking.
MODE is the major mode function.  DESCRIPTION is the test name.
BEFORE is the buffer content with `|' marking cursor position.
AFTER is the expected content after `indent-according-to-mode'
with `|' marking the expected cursor position."
  (declare (indent 2))
  `(it ,description
     (let* ((after-str ,after)
            (expected-cursor-pos (1+ (seq-position after-str ?|)))
            (expected-state (concat (substring after-str 0 (1- expected-cursor-pos))
                                    (substring after-str expected-cursor-pos))))
       (with-temp-buffer
         (insert ,before)
         (funcall #',mode)
         (goto-char (point-min))
         (search-forward "|")
         (delete-char -1)
         (font-lock-ensure)
         (indent-according-to-mode)
         (expect (buffer-string) :to-equal expected-state)
         (expect (point) :to-equal expected-cursor-pos)))))

(defmacro when-indenting-with-point-it (description before after)
  "Create a Buttercup test asserting single-line indentation with cursor tracking.
DESCRIPTION is the test name.  Uses `neocaml-mode'."
  (declare (indent 1))
  `(when-indenting-with-point--it neocaml-mode ,description ,before ,after))

(defmacro when-indenting-interface-with-point-it (description before after)
  "Create a Buttercup test asserting single-line indentation with cursor tracking.
DESCRIPTION is the test name.  Uses `neocaml-interface-mode'."
  (declare (indent 1))
  `(when-indenting-with-point--it neocaml-interface-mode ,description ,before ,after))

(defmacro when-newline-indenting--it (mode description &rest tests)
  "Create a Buttercup test asserting empty-line indentation.
MODE is the major mode function.  DESCRIPTION is the test name.
Each element of TESTS is (CODE EXPECTED-COLUMN) where CODE is a
source string and EXPECTED-COLUMN is the column that
`newline-and-indent' should produce after CODE."
  (declare (indent 2))
  `(it ,description
     (dolist (test (quote ,tests))
       (let ((code (nth 0 test))
             (expected-col (nth 1 test)))
         (with-temp-buffer
           (insert code)
           (let ((treesit-font-lock-level 4))
             (funcall #',mode))
           (goto-char (point-max))
           (newline-and-indent)
           (expect (current-column) :to-equal expected-col))))))

(defmacro when-newline-indenting-it (description &rest tests)
  "Create a Buttercup test asserting empty-line indentation.
DESCRIPTION is the test name.  Uses `neocaml-mode'."
  (declare (indent 1))
  `(when-newline-indenting--it neocaml-mode ,description ,@tests))

(defmacro when-newline-indenting-interface-it (description &rest tests)
  "Create a Buttercup test asserting empty-line indentation.
DESCRIPTION is the test name.  Uses `neocaml-interface-mode'."
  (declare (indent 1))
  `(when-newline-indenting--it neocaml-interface-mode ,description ,@tests))

(provide 'neocaml-test-helpers)

;;; neocaml-test-helpers.el ends here
