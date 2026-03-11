;;; neocaml-objinfo-test.el --- Tests for neocaml-objinfo -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml-objinfo-mode'.

;;; Code:

(require 'buttercup)
(require 'neocaml-objinfo)

(describe "neocaml-objinfo-mode"
  (describe "font-lock"
    (it "highlights section headers"
      (with-temp-buffer
        (insert "Unit name: Foo\nInterfaces imported:\n")
        (neocaml-objinfo-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (expect (get-text-property (point) 'face)
                :to-equal font-lock-keyword-face)))

    (it "highlights CRC hashes"
      (with-temp-buffer
        (insert "\t8a4c67b2d5e1f093a7b6c4d8e2f10394\tStdlib\n")
        (neocaml-objinfo-mode)
        (font-lock-ensure)
        (expect (get-text-property 2 'face)
                :to-equal font-lock-comment-face)))

    (it "highlights module names after CRC"
      (with-temp-buffer
        (insert "\t8a4c67b2d5e1f093a7b6c4d8e2f10394\tStdlib\n")
        (neocaml-objinfo-mode)
        (font-lock-ensure)
        (let ((pos (1+ (save-excursion
                         (goto-char (point-min))
                         (search-forward "\tS")
                         (match-beginning 0)))))
          (expect (get-text-property pos 'face)
                  :to-equal font-lock-type-face)))))

  (describe "mode setup"
    (it "derives from special-mode"
      (with-temp-buffer
        (neocaml-objinfo-mode)
        (expect (derived-mode-p 'special-mode) :to-be-truthy)))

    (it "sets the buffer to read-only"
      (with-temp-buffer
        (neocaml-objinfo-mode)
        (expect buffer-read-only :to-be-truthy)))

    (it "disables undo"
      (with-temp-buffer
        (neocaml-objinfo-mode)
        (expect buffer-undo-list :to-equal t)))

    (it "sets revert-buffer-function"
      (with-temp-buffer
        (neocaml-objinfo-mode)
        (expect revert-buffer-function :to-equal #'neocaml-objinfo-revert)))

    (it "binds g to revert"
      (expect (lookup-key neocaml-objinfo-mode-map "g")
              :to-equal #'neocaml-objinfo-revert)))

  (describe "neocaml-objinfo--run"
    (it "errors when program is not found"
      (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
        (with-temp-buffer
          (expect (neocaml-objinfo--run "/tmp/test.cmi")
                  :to-throw 'user-error))))

    (it "inserts error header on non-zero exit"
      (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'call-process)
                 (lambda (_program _infile _destination _display &rest _args)
                   (insert "Error: not an OCaml object file\n")
                   1)))
        (with-temp-buffer
          (neocaml-objinfo--run "/tmp/bad.cmi")
          (expect (buffer-string) :to-match "exited with code 1")
          (expect (buffer-string) :to-match "not an OCaml object file"))))

    (it "inserts output on success"
      (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'call-process)
                 (lambda (_program _infile _destination _display &rest _args)
                   (insert "Unit name: Foo\n")
                   0)))
        (with-temp-buffer
          (neocaml-objinfo--run "/tmp/foo.cmi")
          (expect (buffer-string) :to-equal "Unit name: Foo\n")))))

  (describe "revert"
    (it "re-runs ocamlobjinfo on the stored file"
      (let ((run-count 0))
        (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'call-process)
                   (lambda (_program _infile _destination _display &rest _args)
                     (setq run-count (1+ run-count))
                     (insert (format "Run %d\n" run-count))
                     0)))
          (with-temp-buffer
            (setq neocaml-objinfo--file "/tmp/foo.cmi")
            (neocaml-objinfo--run neocaml-objinfo--file)
            (expect run-count :to-equal 1)
            (neocaml-objinfo--run neocaml-objinfo--file)
            (expect run-count :to-equal 2)
            (expect (buffer-string) :to-equal "Run 2\n"))))))

  (describe "auto-mode-alist"
    (dolist (ext '(".cmi" ".cmo" ".cmx" ".cma" ".cmxa" ".cmxs" ".cmt" ".cmti"))
      (it (format "registers %s" ext)
        (let ((match (assoc (format "test%s" ext) auto-mode-alist
                            #'string-match-p)))
          (expect match :to-be-truthy)
          (expect (cdr match) :to-equal 'neocaml-objinfo-mode))))))

;;; neocaml-objinfo-test.el ends here
