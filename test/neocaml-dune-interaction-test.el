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
            (expect (neocaml-common-dune-project-root subdir)
                    :to-equal (file-name-as-directory dir)))
        (delete-directory dir t))))

  (it "returns nil when no dune-project is found"
    (let ((dir (make-temp-file "neocaml-dune-test" t)))
      (unwind-protect
          (expect (neocaml-common-dune-project-root dir)
                  :to-be nil)
        (delete-directory dir t))))

  (it "picks the nearest directory containing any of the root files"
    ;; A far-away dune-workspace must not win over a nearer dune-project,
    ;; whatever order the files are listed in.
    (let* ((outer (file-name-as-directory (make-temp-file "neocaml-dune-test" t)))
           (inner (expand-file-name "pkg/" outer))
           (lib (expand-file-name "lib/" inner))
           (neocaml-dune-project-root-files '("dune-workspace" "dune-project")))
      (unwind-protect
          (progn
            (make-directory lib t)
            (write-region "" nil (expand-file-name "dune-workspace" outer) nil 'silent)
            (write-region "" nil (expand-file-name "dune-project" inner) nil 'silent)
            (expect (neocaml-common-dune-project-root lib) :to-equal inner)
            (let ((default-directory lib))
              (expect (neocaml-dune--project-root) :to-equal inner))
            (let ((neocaml-dune-project-root-files '("dune-workspace")))
              (expect (neocaml-common-dune-project-root lib) :to-equal outer)))
        (delete-directory outer t)))))

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

(defmacro neocaml-dune-test--with-file (path content &rest body)
  "Run BODY visiting PATH, holding CONTENT, in a temporary dune project.
PATH is relative to the project root.  BODY can refer to the project
root as `root' and to the visited file as `file'."
  (declare (indent 2))
  `(let* ((root (file-name-as-directory (make-temp-file "neocaml-dune-test" t)))
          (file (expand-file-name ,path root)))
     (unwind-protect
         (progn
           (write-region "(lang dune 3.0)\n" nil
                         (expand-file-name "dune-project" root) nil 'silent)
           (make-directory (file-name-directory file) t)
           (write-region ,content nil file nil 'silent)
           (let ((buf (find-file-noselect file)))
             (unwind-protect
                 (with-current-buffer buf ,@body)
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf))))
       (delete-directory root t))))

(describe "neocaml-dune--test-args"
  (it "runs a cram test on its own"
    (expect (neocaml-dune--test-args "test/foo.t")
            :to-equal '("test" "test/foo.t")))

  (it "runs a whole directory test from any file in it"
    (expect (neocaml-dune--test-args "test/foo.t/run.t")
            :to-equal '("test" "test/foo.t")))

  (it "runs the tests of the directory of any other file"
    (expect (neocaml-dune--test-args "lib/foo.ml")
            :to-equal '("build" "@@lib/runtest")))

  (it "runs the tests of the root for a file at the root"
    (expect (neocaml-dune--test-args "foo.ml")
            :to-equal '("build" "@@runtest"))))

(describe "neocaml-dune--call"
  (it "signals dune's output when dune fails"
    (spy-on 'process-file-shell-command :and-call-fake
            (lambda (&rest _) (insert "Error: boom\n") 1))
    (expect (neocaml-dune--call "promote")
            :to-throw 'user-error '("Running dune promote failed: Error: boom"))))

(describe "neocaml-dune-test-file"
  :var (command dir)
  (before-each
    (spy-on 'compile :and-call-fake
            (lambda (cmd &rest _) (setq command cmd dir default-directory))))

  (it "runs the current cram test from the project root"
    (neocaml-dune-test--with-file "test/foo.t" "  $ true\n"
      (neocaml-dune-test-file)
      (expect command :to-equal "dune test test/foo.t")
      (expect dir :to-equal root)))

  (it "runs the tests of the current source file's directory"
    (neocaml-dune-test--with-file "lib/foo.ml" "let x = 1\n"
      (neocaml-dune-test-file)
      (expect command :to-equal
              (concat "dune build " (shell-quote-argument "@@lib/runtest"))))))

(describe "neocaml-dune-promote-file"
  (it "promotes only the current file and reverts its buffer"
    (neocaml-dune-test--with-file "test/foo.t" "  $ echo hi\n  bye\n"
      (spy-on 'neocaml-dune--call :and-call-fake
              (lambda (&rest _)
                (write-region "  $ echo hi\n  hi\n" nil file nil 'silent)
                "Promoting _build/default/test/foo.t.corrected to test/foo.t.\n"))
      (neocaml-dune-promote-file)
      (expect 'neocaml-dune--call
              :to-have-been-called-with "promote" "test/foo.t")
      (expect (buffer-string) :to-equal "  $ echo hi\n  hi\n")))

  (it "saves unsaved changes first when asked to"
    (neocaml-dune-test--with-file "test/foo.t" "  $ echo hi\n"
      (insert "# comment\n")
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'neocaml-dune--call :and-return-value "")
      (neocaml-dune-promote-file)
      (expect (buffer-modified-p) :not :to-be-truthy)
      (expect (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))
              :to-equal "# comment\n  $ echo hi\n")))

  (it "refuses to overwrite unsaved changes"
    (neocaml-dune-test--with-file "test/foo.t" "  $ echo hi\n"
      (insert "# comment\n")
      (spy-on 'y-or-n-p :and-return-value nil)
      (spy-on 'neocaml-dune--call)
      (expect (neocaml-dune-promote-file) :to-throw 'user-error)
      (expect 'neocaml-dune--call :not :to-have-been-called)
      (expect (buffer-string) :to-equal "# comment\n  $ echo hi\n"))))

(describe "neocaml-dune-promotion-diff"
  :var ((diff (concat "File \"test/foo.t\", line 1, characters 0-0:\n"
                      "\e[1m--- a/test/foo.t\e[m\n"
                      "\e[1m+++ b/_build/.promotion-staging/test/foo.t\e[m\n"
                      "\e[36m@@ -1,2 +1,2 @@\e[m\n"
                      "   $ echo hi\n"
                      "\e[31m-  bye\e[m\n"
                      "\e[32m+  hi\e[m\n")))
  (before-each
    (spy-on 'pop-to-buffer))
  (after-each
    (when-let* ((buf (get-buffer neocaml-dune--promotion-diff-buffer)))
      (kill-buffer buf)))

  (it "shows the current file's promotion in diff-mode without colors"
    (neocaml-dune-test--with-file "test/foo.t" ""
      (spy-on 'neocaml-dune--call :and-return-value diff)
      (neocaml-dune-promotion-diff)
      (expect 'neocaml-dune--call
              :to-have-been-called-with "promotion" "diff" "test/foo.t")
      (with-current-buffer neocaml-dune--promotion-diff-buffer
        (expect major-mode :to-be 'diff-mode)
        (expect default-directory :to-equal root)
        ;; The new side is dune's staging copy, so jump to the old one.
        (expect diff-jump-to-old-file :to-be-truthy)
        (expect (buffer-string) :not :to-match "\e")
        (expect (buffer-string) :to-match "^\\+  hi$"))))

  (it "shows every pending promotion with a prefix argument"
    (neocaml-dune-test--with-file "test/foo.t" ""
      (spy-on 'neocaml-dune--call :and-return-value diff)
      (neocaml-dune-promotion-diff t)
      (expect 'neocaml-dune--call :to-have-been-called-with "promotion" "diff")))

  (it "only reports when there is nothing to promote"
    (neocaml-dune-test--with-file "test/foo.t" ""
      (spy-on 'neocaml-dune--call
              :and-return-value "Warning: Nothing to promote for test/foo.t.\n")
      (spy-on 'message)
      (neocaml-dune-promotion-diff)
      (expect 'message :to-have-been-called-with
              "%s" "Warning: Nothing to promote for test/foo.t.")
      (expect (get-buffer neocaml-dune--promotion-diff-buffer) :to-be nil))))

(describe "neocaml-dune per-file test workflow"
  (it "reviews and promotes a failing cram test with dune"
    (unless (executable-find "dune")
      (signal 'buttercup-pending "dune executable not found"))
    (neocaml-dune-test--with-file "test/foo.t" "  $ echo hello\n  goodbye\n"
      ;; Leaves a correction to promote, since the test fails.
      (let ((default-directory root))
        (call-process "dune" nil nil nil "test"))
      (spy-on 'pop-to-buffer)
      (neocaml-dune-promotion-diff)
      (with-current-buffer neocaml-dune--promotion-diff-buffer
        (expect (buffer-string) :to-match "^\\+  hello$")
        (goto-char (point-min))
        (re-search-forward "^\\+  hello$")
        (expect (diff-find-file-name t t) :to-equal "test/foo.t"))
      (kill-buffer neocaml-dune--promotion-diff-buffer)
      (neocaml-dune-promote-file)
      (expect (buffer-string) :to-equal "  $ echo hello\n  hello\n"))))

(describe "neocaml-dune--utop-command"
  :var (root lib)
  (before-each
    (setq root (file-name-as-directory (make-temp-file "neocaml-dune-test" t))
          lib (expand-file-name "lib/" root))
    (write-region "" nil (expand-file-name "dune-project" root) nil 'silent)
    (make-directory lib))
  (after-each (delete-directory root t))

  (it "runs dune utop on . from the project root"
    (let ((default-directory lib)
          (neocaml-dune-program "dune")
          (neocaml-dune-use-opam-exec nil))
      (expect (neocaml-dune--utop-command)
              :to-equal (list root "dune" "utop" "."))))

  (it "follows neocaml-dune-program and neocaml-dune-use-opam-exec"
    (let ((default-directory lib)
          (neocaml-dune-program "/opt/dune")
          (neocaml-dune-use-opam-exec t))
      (expect (neocaml-dune--utop-command)
              :to-equal (list root "opam" "exec" "--" "/opt/dune" "utop" "."))))

  (it "signals an error outside a dune project"
    (let ((default-directory (file-name-as-directory
                              (make-temp-file "neocaml-dune-none" t))))
      (unwind-protect
          (expect (neocaml-dune--utop-command) :to-throw 'error)
        (delete-directory default-directory t)))))

(describe "neocaml-dune-utop"
  (it "switches to a dune-utop REPL"
    (let (flavor)
      (spy-on 'neocaml-repl-switch-to-repl
              :and-call-fake (lambda () (setq flavor neocaml-repl-flavor)))
      (neocaml-dune-utop)
      (expect flavor :to-be 'dune-utop))))

(describe "neocaml-dune-interaction-mode"
  (it "defines the expected keybindings"
    (let ((map neocaml-dune-interaction-mode-map))
      (expect (lookup-key map (kbd "C-c C-o b")) :to-be #'neocaml-dune-build)
      (expect (lookup-key map (kbd "C-c C-o t")) :to-be #'neocaml-dune-test)
      (expect (lookup-key map (kbd "C-c C-o c")) :to-be #'neocaml-dune-clean)
      (expect (lookup-key map (kbd "C-c C-o p")) :to-be #'neocaml-dune-promote)
      (expect (lookup-key map (kbd "C-c C-o T")) :to-be #'neocaml-dune-test-file)
      (expect (lookup-key map (kbd "C-c C-o P")) :to-be #'neocaml-dune-promote-file)
      (expect (lookup-key map (kbd "C-c C-o =")) :to-be #'neocaml-dune-promotion-diff)
      (expect (lookup-key map (kbd "C-c C-o f")) :to-be #'neocaml-dune-fmt)
      (expect (lookup-key map (kbd "C-c C-o u")) :to-be #'neocaml-dune-utop)
      (expect (lookup-key map (kbd "C-c C-o r")) :to-be #'neocaml-dune-exec)
      (expect (lookup-key map (kbd "C-c C-o d")) :to-be #'neocaml-dune-command)
      (expect (lookup-key map (kbd "C-c C-o .")) :to-be #'neocaml-dune-find-dune-file)))

  (it "shows lighter in mode line"
    (with-temp-buffer
      (neocaml-dune-interaction-mode 1)
      (expect neocaml-dune-interaction-mode :to-be-truthy)
      (expect (assq 'neocaml-dune-interaction-mode minor-mode-alist)
              :not :to-be nil))))

;;; neocaml-dune-interaction-test.el ends here
