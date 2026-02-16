;;; neocaml-compilation-test.el --- Tests for compilation error regexp -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml--compilation-error-regexp' covering all OCaml compiler
;; output formats: errors, warnings, alerts, backtraces, and ancillary locations.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(defun neocaml-test--match-compilation (output)
  "Match OUTPUT against `neocaml--compilation-error-regexp' in a temp buffer.
Returns a plist (:file FILE :line LINE :end-line END-LINE
:col COL :end-col END-COL :severity SEV) where SEV is
2 (error), 1 (warning), or 0 (info), matching compilation convention.
Returns nil if the regexp does not match."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (when (re-search-forward neocaml--compilation-error-regexp nil t)
      (let* ((file (match-string 3))
             (line (string-to-number (match-string 4)))
             (end-line (when (match-string 5)
                         (string-to-number (match-string 5))))
             (col (when (match-string 6)
                    (string-to-number (match-string 6))))
             (end-col (when (match-string 7)
                        (string-to-number (match-string 7))))
             ;; Severity mirrors compilation-error-regexp-alist convention:
             ;; group 8 matched = warning (1), group 9 matched = info (0),
             ;; otherwise error (2).
             ;; N.B. We use :severity instead of :type to avoid a conflict
             ;; with buttercup's oclosure :type tag on Emacs 29.
             (severity (cond ((match-string 8) 1)
                             ((match-string 9) 0)
                             (t 2))))
        (list :file file :line line :end-line end-line
              :col col :end-col end-col :severity severity)))))

(describe "compilation regexp"
  (it "matches a simple error"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 4, characters 6-7:\nError: Unbound value x\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 2)
      (expect (plist-get info :line) :to-equal 4)
      (expect (plist-get info :col) :to-equal 6)))

  (it "matches a warning"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 3, characters 6-7:\nWarning 26 [unused-var]: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 1)
      (expect (plist-get info :line) :to-equal 3)))

  (it "matches a warning (old format without mnemonic)"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 3, characters 6-7:\nWarning 26: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :severity) :to-equal 1)))

  (it "matches an alert"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 5, characters 9-10:\nAlert deprecated: use new_fn instead.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 1)))

  (it "matches a multi-line span"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", lines 5-7, characters 10-20:\nError: Syntax error\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :line) :to-equal 5)
      (expect (plist-get info :end-line) :to-equal 7)))

  (it "matches error with source-code snippet"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 2, characters 0-5:\n2 | let x\n    ^^^^^\nError: Unbound value x\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 2)
      (expect (plist-get info :line) :to-equal 2)
      (expect (plist-get info :col) :to-equal 0)))

  (it "matches warning with source-code snippet"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 2, characters 0-5:\n2 | let x\n    ^^^^^\nWarning 26 [unused-var]: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :severity) :to-equal 1)))

  (it "matches a backtrace with Raised at"
    (let ((info (neocaml-test--match-compilation
                 "Raised at Foo.f in file \"foo.ml\", line 5, characters 4-22\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 2)
      (expect (plist-get info :line) :to-equal 5)
      (expect (plist-get info :col) :to-equal 4)))

  (it "matches a backtrace with Called from"
    (let ((info (neocaml-test--match-compilation
                 "Called from Foo.g in file \"foo.ml\", line 9, characters 2-5\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 2)
      (expect (plist-get info :line) :to-equal 9)))

  (it "matches an ancillary location (7-space indent) as info"
    (let ((info (neocaml-test--match-compilation
                 "       File \"foo.ml\", line 10, characters 2-41:\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :severity) :to-equal 0)
      (expect (plist-get info :line) :to-equal 10)))

  (it "matches a file location with no characters"
    (let ((info (neocaml-test--match-compilation
                 "File \"foo.ml\", line 1:\nError: Syntax error\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :line) :to-equal 1)
      (expect (plist-get info :col) :to-be nil)))

  (it "sets compilation-first-column to 0 for OCaml's 0-indexed columns"
    (with-temp-buffer
      (neocaml-mode)
      (expect compilation-first-column :to-equal 0)))

  (it "computes end-column correctly"
    (with-temp-buffer
      (insert "File \"foo.ml\", line 4, characters 6-20:\nError: type error\n")
      (goto-char (point-min))
      (re-search-forward neocaml--compilation-error-regexp)
      (expect (neocaml--compilation-end-column) :to-equal
              (+ 20 (if (>= emacs-major-version 28) -1 0))))))

;;; neocaml-compilation-test.el ends here
