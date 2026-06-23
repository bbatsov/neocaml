;;; neocaml-dune-completion-test.el --- Tests for dune completion-at-point -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-dune-completion-at-point': stanza-name,
;; field-name, and library-name completion contexts.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-dune)

(defun neocaml-dune-test--capf (content)
  "Run the dune capf on CONTENT, where `|' marks point.
Return the raw capf result, or nil."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (let ((pos (point)))
      (neocaml-dune-mode)
      (goto-char pos))
    (neocaml-dune-completion-at-point)))

(defun neocaml-dune-test--candidates (content)
  "Return the candidate list the dune capf offers for CONTENT."
  (nth 2 (neocaml-dune-test--capf content)))

(describe "neocaml-dune completion-at-point"
  (before-all
    (unless (treesit-language-available-p 'dune)
      (signal 'buttercup-pending "tree-sitter dune grammar not available")))

  (describe "stanza-name completion"
    (it "offers stanza names at the head of a top-level form"
      (let ((cands (neocaml-dune-test--candidates "(lib|")))
        (expect (member "library" cands) :to-be-truthy)
        (expect (member "executable" cands) :to-be-truthy)
        (expect (member "rule" cands) :to-be-truthy)))

    (it "offers stanza names right after the opening paren"
      (expect (member "test" (neocaml-dune-test--candidates "(|"))
              :to-be-truthy))

    (it "spans the partial stanza name being typed"
      (let ((result (neocaml-dune-test--capf "(lib|")))
        ;; "lib" occupies buffer positions 2..4 in "(lib", so the
        ;; completion span is [2, 5).
        (expect (nth 0 result) :to-equal 2)
        (expect (nth 1 result) :to-equal 5))))

  (describe "field-name completion"
    (it "offers fields for the enclosing library stanza"
      (let ((cands (neocaml-dune-test--candidates "(library (lib|")))
        (expect (member "libraries" cands) :to-be-truthy)
        (expect (member "public_name" cands) :to-be-truthy)))

    (it "offers fields right after an inner opening paren"
      (expect (member "name" (neocaml-dune-test--candidates "(executable (|"))
              :to-be-truthy))

    (it "scopes fields to the stanza type"
      ;; `targets' is a rule field, not an executable field
      (let ((exe (neocaml-dune-test--candidates "(executable (|")))
        (expect (member "targets" exe) :to-be nil))
      (let ((rule (neocaml-dune-test--candidates "(rule (|")))
        (expect (member "targets" rule) :to-be-truthy)
        (expect (member "action" rule) :to-be-truthy)))

    (it "falls back to the union of fields for unknown stanzas"
      (let ((cands (neocaml-dune-test--candidates "(mystery_stanza (|")))
        (expect (member "name" cands) :to-be-truthy))))

  (describe "context boundaries"
    (it "offers nothing inside a comment"
      (expect (neocaml-dune-test--capf "; (lib|") :to-be nil))

    (it "offers nothing inside a string"
      (expect (neocaml-dune-test--capf "(name \"foo|") :to-be nil))

    (it "offers nothing at the top level outside any form"
      (expect (neocaml-dune-test--capf "lib|") :to-be nil))

    (it "offers nothing in a stanza body away from a form head"
      (expect (neocaml-dune-test--capf "(library (name foo) |") :to-be nil))))

(provide 'neocaml-dune-completion-test)

;;; neocaml-dune-completion-test.el ends here
