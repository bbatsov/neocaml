;;; neocaml-eglot-dape-test.el --- Tests for the Eglot and dape setup -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; neocaml registers its modes with Eglot and dape when those load.
;; Each spec runs in a fresh Emacs, so the order the libraries load in
;; is the one under test, not whatever the rest of the suite left
;; behind.

;;; Code:

(require 'buttercup)

(defun neocaml-test--fresh-emacs-value (form)
  "Return the value of FORM in a fresh Emacs that can load neocaml."
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil '(t nil) nil
                   "-Q" "--batch"
                   "-L" (file-name-directory (locate-library "neocaml"))
                   "--eval" "(setq load-prefer-newer t)"
                   "--eval" (format "(prin1 %S)" form))))
      (unless (eql status 0)
        (error "Emacs exited with status %s" status))
      (read (buffer-string)))))

(describe "neocaml's Eglot and dape setup"
  (it "registers the modes with Eglot whichever loads first"
    (dolist (libraries '((neocaml eglot) (eglot neocaml)))
      (expect (neocaml-test--fresh-emacs-value
               `(progn
                  ,@(mapcar (lambda (library) `(require ',library)) libraries)
                  (car (last eglot-server-programs))))
              :to-equal '(((neocaml-mode :language-id "ocaml")
                           (neocaml-interface-mode
                            :language-id "ocaml.interface"))
                          "ocamllsp"))))

  (it "adds the modes to dape's ocamlearlybird config when dape loads"
    (expect (neocaml-test--fresh-emacs-value
             '(progn
                (require 'neocaml)
                (defvar dape-configs
                  (list (list 'ocamlearlybird 'modes (list 'tuareg-mode))))
                (provide 'dape)
                (plist-get (alist-get 'ocamlearlybird dape-configs) 'modes)))
            :to-equal '(neocaml-mode neocaml-interface-mode tuareg-mode))))

;;; neocaml-eglot-dape-test.el ends here
