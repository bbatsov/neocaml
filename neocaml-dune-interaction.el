;;; neocaml-dune-interaction.el --- Dune build system interaction -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml dune

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minor mode for running dune commands from any neocaml buffer.
;; Provides keybindings for common dune operations (build, test,
;; clean, promote, fmt, utop, exec), for running and promoting the
;; tests of the current file, and navigation to dune files.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)

(declare-function neocaml-repl-switch-to-repl "neocaml-repl")
(defvar neocaml-repl-program-name)
(defvar neocaml-repl-program-args)
(defvar neocaml-repl-buffer-name)
(defvar neocaml-repl-flavor)

(defgroup neocaml-dune-interaction nil
  "Dune build system interaction for neocaml."
  :prefix "neocaml-dune-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-dune-program "dune"
  "The dune executable."
  :type 'string
  :safe #'stringp
  :group 'neocaml-dune-interaction
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-dune-use-opam-exec nil
  "When non-nil, prefix dune commands with `opam exec --'.
This is useful when Emacs does not inherit the opam environment,
e.g. when launched from a desktop shortcut on macOS."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-dune-interaction
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-dune-project-root-files '("dune-project")
  "Files that indicate a dune project root.
The first ancestor directory containing any of these files is
used as the project root for dune commands."
  :type '(repeat string)
  :group 'neocaml-dune-interaction
  :package-version '(neocaml . "0.6.0"))

;;; Project root detection

(defun neocaml-dune--project-root ()
  "Find the dune project root by walking up from the current directory.
Returns the directory containing `dune-project', or signals an
error if none is found."
  (or (neocaml-dune--locate-project-root default-directory)
      (error "Not inside a dune project (no dune-project file found)")))

(defun neocaml-dune--locate-project-root (dir)
  "Find the nearest ancestor of DIR containing a dune project file."
  (cl-some (lambda (marker)
             (when-let* ((found (locate-dominating-file dir marker)))
               (file-name-as-directory (expand-file-name found))))
           neocaml-dune-project-root-files))

;;; Running dune commands

(defun neocaml-dune--command-prefix ()
  "Return the command prefix for running dune.
When `neocaml-dune-use-opam-exec' is non-nil, returns
\"opam exec -- dune\", otherwise just \"dune\"."
  (if neocaml-dune-use-opam-exec
      (concat "opam exec -- " (shell-quote-argument neocaml-dune-program))
    (shell-quote-argument neocaml-dune-program)))

(defun neocaml-dune--command-line (command args)
  "Return the shell command line for dune COMMAND with ARGS."
  (concat (neocaml-dune--command-prefix) " "
          (mapconcat #'shell-quote-argument (cons command args) " ")))

(defun neocaml-dune--run (command &rest args)
  "Run a dune COMMAND with ARGS via `compile' in the project root.
When called with a prefix argument, appends `--watch' and runs in
comint mode so the process stays alive and rebuilds on file changes."
  (let ((watch current-prefix-arg)
        (default-directory (neocaml-dune--project-root)))
    (compile (neocaml-dune--command-line
              command (if watch (append args '("--watch")) args))
             (and watch t))))

(defun neocaml-dune--run-no-watch (command &rest args)
  "Run a dune COMMAND with ARGS via `compile' in the project root.
Like `neocaml-dune--run' but ignores the prefix argument."
  (let ((current-prefix-arg nil))
    (apply #'neocaml-dune--run command args)))

(defun neocaml-dune--call (command &rest args)
  "Run a dune COMMAND with ARGS in `default-directory' and return its output.
Standard output and standard error are returned together.  When dune
fails, signal a `user-error' with its output."
  (with-temp-buffer
    (let ((status (process-file-shell-command
                   (neocaml-dune--command-line command args) nil t)))
      (unless (eql status 0)
        (user-error "Running dune %s failed: %s"
                    command (string-trim (buffer-string))))
      (buffer-string))))

(defun neocaml-dune--current-file (root)
  "Return the file the current buffer visits, relative to ROOT."
  (file-relative-name
   (or buffer-file-name (user-error "Buffer is not visiting a file"))
   root))

(defun neocaml-dune--test-args (file)
  "Return the dune command and arguments to test FILE.
dune only takes cram tests by name, so a `.t' file, or any file in a
`.t' directory test, runs that test.  Any other file runs the `runtest'
alias of its own directory, without the directories below it."
  (let ((dir (file-name-directory file)))
    (cond
     ((and dir (string-suffix-p ".t/" dir))
      (list "test" (directory-file-name dir)))
     ((string-suffix-p ".t" file) (list "test" file))
     (t (list "build" (concat "@@" dir "runtest"))))))

;;;###autoload
(defun neocaml-dune-build ()
  "Run `dune build' in the project root.
With prefix argument, run in watch mode."
  (interactive)
  (neocaml-dune--run "build"))

;;;###autoload
(defun neocaml-dune-test ()
  "Run `dune test' in the project root.
With prefix argument, run in watch mode."
  (interactive)
  (neocaml-dune--run "test"))

;;;###autoload
(defun neocaml-dune-clean ()
  "Run `dune clean' in the project root."
  (interactive)
  (neocaml-dune--run-no-watch "clean"))

;;;###autoload
(defun neocaml-dune-promote ()
  "Run `dune promote' in the project root.
Promotes test corrections (replaces expected output with actual)."
  (interactive)
  (neocaml-dune--run-no-watch "promote"))

;;;###autoload
(defun neocaml-dune-test-file ()
  "Run the current file's test suite.
A cram test (`.t' file) runs on its own.  For any other file, run the
tests of its directory, such as the inline and expect tests of the
library defined there.  With prefix argument, run in watch mode."
  (interactive)
  (apply #'neocaml-dune--run
         (neocaml-dune--test-args
          (neocaml-dune--current-file (neocaml-dune--project-root)))))

;;;###autoload
(defun neocaml-dune-promote-file ()
  "Promote the test corrections for the current file only.
Accepts the output of the file's failing expect or cram tests, leaves
the other pending promotions alone, and reverts the buffer to show the
result.  A buffer with unsaved changes has to be saved first."
  (interactive)
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save %s first? " (buffer-name)))
        (save-buffer)
      (user-error "Promoting would overwrite the unsaved changes")))
  (let ((default-directory (neocaml-dune--project-root)))
    (message "%s" (string-trim
                   (neocaml-dune--call "promote" (neocaml-dune--current-file
                                                  default-directory)))))
  (revert-buffer t t t))

(defconst neocaml-dune--promotion-diff-buffer "*dune promotion diff*"
  "Name of the buffer `neocaml-dune-promotion-diff' shows the diff in.")

;;;###autoload
(defun neocaml-dune-promotion-diff (&optional all)
  "Show what promoting the test corrections for the current file would do.
With prefix argument ALL, show every pending promotion in the project.
The diff is shown in `diff-mode', where \\[diff-goto-source] jumps to
the source file rather than to dune's staging copy."
  (interactive "P")
  (let* ((root (neocaml-dune--project-root))
         (default-directory root)
         (output (ansi-color-filter-apply
                  (apply #'neocaml-dune--call "promotion" "diff"
                         (unless all (list (neocaml-dune--current-file root)))))))
    (if (string-match-p "^@@ " output)
        (with-current-buffer (get-buffer-create
                              neocaml-dune--promotion-diff-buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))
            (diff-mode)
            (setq default-directory root
                  buffer-read-only t)
            (setq-local diff-jump-to-old-file t))
          (pop-to-buffer (current-buffer)))
      (let ((msg (string-trim output)))
        (message "%s" (if (equal msg "") "Nothing to promote" msg))))))

;;;###autoload
(defun neocaml-dune-fmt ()
  "Run `dune fmt' in the project root.
With prefix argument, run in watch mode."
  (interactive)
  (neocaml-dune--run "fmt"))

;;;###autoload
(defun neocaml-dune-exec (name)
  "Run `dune exec NAME' in the project root.
Prompts for the executable name.
With prefix argument, run in watch mode."
  (interactive "sExecutable name: ")
  (neocaml-dune--run "exec" name))

;;;###autoload
(defun neocaml-dune-utop ()
  "Run `dune utop' in the project root via `neocaml-repl'.
Launches utop with the project's libraries loaded, using the full
REPL interaction (send region, send definition, etc.)."
  (interactive)
  (require 'neocaml-repl)
  ;; Bind `default-directory' to the project root so the REPL is named (and
  ;; routed) per project, and source files in the project send to it.
  (let* ((default-directory (neocaml-dune--project-root))
         (program (if neocaml-dune-use-opam-exec "opam" neocaml-dune-program))
         (args (if neocaml-dune-use-opam-exec
                   (list "exec" "--" neocaml-dune-program "utop" ".")
                 (list "utop" ".")))
         (neocaml-repl-program-name program)
         (neocaml-repl-program-args args)
         (neocaml-repl-flavor 'dune-utop))
    (neocaml-repl-switch-to-repl)))

(defvar neocaml-dune--command-history nil
  "History for `neocaml-dune-command'.")

;;;###autoload
(defun neocaml-dune-command (command)
  "Run an arbitrary dune COMMAND in the project root.
Prompts for the full command string (without the `dune' prefix).
The command string is passed as-is, not shell-quoted."
  (interactive
   (list (read-string "dune command: " nil 'neocaml-dune--command-history)))
  (let ((default-directory (neocaml-dune--project-root)))
    (compile (concat (neocaml-dune--command-prefix) " " command))))

;;; Navigation

;;;###autoload
(defun neocaml-dune-find-dune-file ()
  "Find the nearest `dune' file governing the current directory.
Walks up from the current file's directory looking for a file
named `dune'."
  (interactive)
  (let* ((dir (or (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  default-directory))
         (found (locate-dominating-file dir "dune")))
    (if found
        (find-file (expand-file-name "dune" found))
      (user-error "No dune file found above %s" dir))))

;;; Minor mode

(defvar neocaml-dune-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d b") #'neocaml-dune-build)
    (define-key map (kbd "C-c C-d t") #'neocaml-dune-test)
    (define-key map (kbd "C-c C-d c") #'neocaml-dune-clean)
    (define-key map (kbd "C-c C-d p") #'neocaml-dune-promote)
    (define-key map (kbd "C-c C-d T") #'neocaml-dune-test-file)
    (define-key map (kbd "C-c C-d P") #'neocaml-dune-promote-file)
    (define-key map (kbd "C-c C-d =") #'neocaml-dune-promotion-diff)
    (define-key map (kbd "C-c C-d f") #'neocaml-dune-fmt)
    (define-key map (kbd "C-c C-d u") #'neocaml-dune-utop)
    (define-key map (kbd "C-c C-d r") #'neocaml-dune-exec)
    (define-key map (kbd "C-c C-d d") #'neocaml-dune-command)
    (define-key map (kbd "C-c C-d .") #'neocaml-dune-find-dune-file)
    (easy-menu-define neocaml-dune-interaction-menu map
      "Dune interaction menu."
      '("Dune Interaction"
        ["Build" neocaml-dune-build]
        ["Test" neocaml-dune-test]
        ["Clean" neocaml-dune-clean]
        ["Promote" neocaml-dune-promote]
        ["Format" neocaml-dune-fmt]
        "--"
        ("Current File"
         ["Test" neocaml-dune-test-file]
         ["Show Promotion Diff" neocaml-dune-promotion-diff]
         ["Promote" neocaml-dune-promote-file])
        "--"
        ("Watch (rebuild on changes)"
         ["Build --watch"
          (let ((current-prefix-arg '(4))) (call-interactively #'neocaml-dune-build))
          :keys "C-u C-c C-d b"]
         ["Test --watch"
          (let ((current-prefix-arg '(4))) (call-interactively #'neocaml-dune-test))
          :keys "C-u C-c C-d t"]
         ["Format --watch"
          (let ((current-prefix-arg '(4))) (call-interactively #'neocaml-dune-fmt))
          :keys "C-u C-c C-d f"])
        "--"
        ["Find dune file" neocaml-dune-find-dune-file]
        "--"
        ["Run utop" neocaml-dune-utop]
        ["Run Executable..." neocaml-dune-exec]
        ["Run Command..." neocaml-dune-command]))
    map)
  "Keymap for `neocaml-dune-interaction-mode'.")

;;;###autoload
(define-minor-mode neocaml-dune-interaction-mode
  "Minor mode for running dune commands from neocaml buffers.

Provides keybindings for common dune operations:

\\{neocaml-dune-interaction-mode-map}"
  :lighter " Dune-Int"
  :keymap neocaml-dune-interaction-mode-map)

(provide 'neocaml-dune-interaction)

;;; neocaml-dune-interaction.el ends here
