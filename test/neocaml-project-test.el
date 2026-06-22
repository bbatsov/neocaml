;;; neocaml-project-test.el --- project.el backend tests -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml's project.el integration: detecting a
;; dune-project root, ignoring build artifacts, and the dune compile-command.

;;; Code:

(require 'neocaml-test-helpers)
(require 'project)

(defmacro neocaml-test--with-dune-project (var &rest body)
  "Create a temp dune project directory, bind its path to VAR, run BODY.
A `dune-project' file and a `lib/' subdirectory are created."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "neocaml-proj" t))))
     (unwind-protect
         (progn
           (write-region "" nil (expand-file-name "dune-project" ,var))
           (make-directory (expand-file-name "lib" ,var) t)
           ,@body)
       (delete-directory ,var t))))

(describe "neocaml project.el integration"
  (describe "neocaml-project-find"
    (it "finds the dune-project root from the project directory"
      (neocaml-test--with-dune-project root
        (expect (neocaml-project-find root)
                :to-equal (list 'neocaml root))))

    (it "finds the root from a subdirectory"
      (neocaml-test--with-dune-project root
        (expect (project-root (neocaml-project-find (expand-file-name "lib" root)))
                :to-equal root)))

    (it "returns nil when there is no dune-project"
      (let ((dir (file-name-as-directory (make-temp-file "neocaml-noproj" t))))
        (unwind-protect
            (expect (neocaml-project-find dir) :to-be nil)
          (delete-directory dir t)))))

  (describe "project-ignores"
    (it "ignores OCaml build and switch artifacts"
      (neocaml-test--with-dune-project root
        (let ((ignores (project-ignores (neocaml-project-find root) root)))
          (expect (member "_build/" ignores) :to-be-truthy)
          (expect (member "_opam/" ignores) :to-be-truthy)))))

  (describe "compile-command"
    (it "defaults to `dune build' inside a dune project"
      (unless (treesit-language-available-p 'ocaml)
        (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
      (neocaml-test--with-dune-project root
        (with-temp-buffer
          (setq default-directory root)
          (neocaml-mode)
          (expect compile-command :to-equal "dune build"))))

    (it "is left at its default outside a dune project"
      (unless (treesit-language-available-p 'ocaml)
        (signal 'buttercup-pending "tree-sitter OCaml grammar not available"))
      (let ((dir (file-name-as-directory (make-temp-file "neocaml-noproj" t))))
        (unwind-protect
            (with-temp-buffer
              (setq default-directory dir)
              (neocaml-mode)
              (expect compile-command :not :to-equal "dune build"))
          (delete-directory dir t))))))

(provide 'neocaml-project-test)

;;; neocaml-project-test.el ends here
