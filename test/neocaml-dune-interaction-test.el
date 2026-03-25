;;; neocaml-dune-interaction-test.el --- Tests for dune interaction -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for neocaml-dune-interaction-mode.

;;; Code:

(require 'buttercup)
(require 'neocaml-dune-interaction)

(describe "neocaml-dune project root"
  (it "finds project root from dune-project"
    (let* ((dir (make-temp-file "neocaml-dune-test" t))
           (subdir (expand-file-name "src" dir)))
      (unwind-protect
          (progn
            (make-directory subdir t)
            (write-region "" nil (expand-file-name "dune-project" dir) nil 'silent)
            (expect (neocaml-dune--locate-project-root subdir)
                    :to-equal (file-name-as-directory dir)))
        (delete-directory dir t))))

  (it "returns nil when no dune-project is found"
    (let ((dir (make-temp-file "neocaml-dune-test" t)))
      (unwind-protect
          (expect (neocaml-dune--locate-project-root dir)
                  :to-be nil)
        (delete-directory dir t)))))

(describe "neocaml-dune find dune file"
  (it "finds nearest dune file in current directory"
    (let* ((dir (make-temp-file "neocaml-dune-test" t))
           (dune-file (expand-file-name "dune" dir)))
      (unwind-protect
          (progn
            (write-region "(library (name test))" nil dune-file nil 'silent)
            (let ((default-directory (file-name-as-directory dir))
                  (buf nil))
              (unwind-protect
                  (progn
                    (neocaml-dune-find-dune-file)
                    (setq buf (current-buffer))
                    (expect (buffer-file-name) :to-equal dune-file))
                (when buf (kill-buffer buf)))))
        (delete-directory dir t))))

  (it "finds nearest dune file in ancestor directory"
    (let* ((dir (make-temp-file "neocaml-dune-test" t))
           (subdir (expand-file-name "lib" dir))
           (dune-file (expand-file-name "dune" dir)))
      (unwind-protect
          (progn
            (make-directory subdir t)
            (write-region "(library (name test))" nil dune-file nil 'silent)
            (let ((default-directory (file-name-as-directory subdir))
                  (buf nil))
              (unwind-protect
                  (progn
                    (neocaml-dune-find-dune-file)
                    (setq buf (current-buffer))
                    (expect (buffer-file-name) :to-equal dune-file))
                (when buf (kill-buffer buf)))))
        (delete-directory dir t)))))

(describe "neocaml-dune-interaction-mode"
  (it "defines the expected keybindings"
    (let ((map neocaml-dune-interaction-mode-map))
      (expect (lookup-key map (kbd "C-c C-d b")) :to-be #'neocaml-dune-build)
      (expect (lookup-key map (kbd "C-c C-d t")) :to-be #'neocaml-dune-test)
      (expect (lookup-key map (kbd "C-c C-d c")) :to-be #'neocaml-dune-clean)
      (expect (lookup-key map (kbd "C-c C-d p")) :to-be #'neocaml-dune-promote)
      (expect (lookup-key map (kbd "C-c C-d f")) :to-be #'neocaml-dune-fmt)
      (expect (lookup-key map (kbd "C-c C-d u")) :to-be #'neocaml-dune-utop)
      (expect (lookup-key map (kbd "C-c C-d r")) :to-be #'neocaml-dune-exec)
      (expect (lookup-key map (kbd "C-c C-d d")) :to-be #'neocaml-dune-command)
      (expect (lookup-key map (kbd "C-c C-d .")) :to-be #'neocaml-dune-find-dune-file)))

  (it "shows lighter in mode line"
    (with-temp-buffer
      (neocaml-dune-interaction-mode 1)
      (expect neocaml-dune-interaction-mode :to-be-truthy)
      (expect (assq 'neocaml-dune-interaction-mode minor-mode-alist)
              :not :to-be nil))))

;;; neocaml-dune-interaction-test.el ends here
