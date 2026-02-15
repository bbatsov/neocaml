;;; neocaml-compilation-test.el --- Tests for compilation error regexp -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml--compilation-error-regexp' covering all OCaml compiler
;; output formats: errors, warnings, alerts, backtraces, and ancillary locations.

;;; Code:

(require 'buttercup)
(require 'compile)
(require 'neocaml)

(defun neocaml-test--extract-compilation-info (output)
  "Parse OUTPUT as compilation output and return info from the first message.
Returns a plist (:file FILE :type TYPE :line LINE :col COL) or nil
if no message was found."
  (with-temp-buffer
    (insert output)
    (let ((compilation-locs (make-hash-table))
          (compilation-error-regexp-alist '(ocaml)))
      (compilation-parse-errors (point-min) (point-max))
      (goto-char (point-min))
      (let ((msg (get-text-property (point) 'compilation-message)))
        (unless msg
          (setq msg (next-single-property-change (point) 'compilation-message))
          (when msg
            (setq msg (get-text-property msg 'compilation-message))))
        (when msg
          (let* ((loc (compilation--message->loc msg))
                 (type (compilation--message->type msg))
                 (file-struct (compilation--loc->file-struct loc))
                 (file (car (compilation--file-struct->file-spec file-struct)))
                 (line (compilation--loc->line loc))
                 (col (compilation--loc->col loc)))
            (list :file file
                  :type type
                  :line line
                  :col col)))))))

(describe "compilation regexp"
  (it "matches a simple error"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 4, characters 6-7:\nError: Unbound value x\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 2)
      (expect (plist-get info :line) :to-equal 4)
      (expect (plist-get info :col) :to-equal 6)))

  (it "matches a warning"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 3, characters 6-7:\nWarning 26 [unused-var]: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 1)
      (expect (plist-get info :line) :to-equal 3)))

  (it "matches a warning (old format without mnemonic)"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 3, characters 6-7:\nWarning 26: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :type) :to-equal 1)))

  (it "matches an alert"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 5, characters 9-10:\nAlert deprecated: use new_fn instead.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 1)))

  (it "matches a multi-line span"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", lines 5-7, characters 10-20:\nError: Syntax error\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :line) :to-equal 5)))

  (it "matches error with source-code snippet"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 2, characters 0-5:\n2 | let x\n    ^^^^^\nError: Unbound value x\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 2)
      (expect (plist-get info :line) :to-equal 2)
      (expect (plist-get info :col) :to-equal 0)))

  (it "matches warning with source-code snippet"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 2, characters 0-5:\n2 | let x\n    ^^^^^\nWarning 26 [unused-var]: unused variable x.\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :type) :to-equal 1)))

  (it "matches a backtrace with Raised at"
    (let ((info (neocaml-test--extract-compilation-info
                 "Raised at Foo.f in file \"foo.ml\", line 5, characters 4-22\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 2)
      (expect (plist-get info :line) :to-equal 5)
      (expect (plist-get info :col) :to-equal 4)))

  (it "matches a backtrace with Called from"
    (let ((info (neocaml-test--extract-compilation-info
                 "Called from Foo.g in file \"foo.ml\", line 9, characters 2-5\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 2)
      (expect (plist-get info :line) :to-equal 9)))

  (it "matches an ancillary location (7-space indent) as info"
    (let ((info (neocaml-test--extract-compilation-info
                 "       File \"foo.ml\", line 10, characters 2-41:\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :type) :to-equal 0)
      (expect (plist-get info :line) :to-equal 10)))

  (it "matches a file location with no characters"
    (let ((info (neocaml-test--extract-compilation-info
                 "File \"foo.ml\", line 1:\nError: Syntax error\n")))
      (expect info :not :to-be nil)
      (expect (plist-get info :file) :to-equal "foo.ml")
      (expect (plist-get info :line) :to-equal 1)
      (expect (plist-get info :col) :to-be nil))))

;;; neocaml-compilation-test.el ends here
