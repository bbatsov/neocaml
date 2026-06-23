;;; neocaml-opam.el --- Major mode for opam files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml opam

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing opam package files.
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-opam.

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

(require 'treesit)
(require 'flymake)

(defgroup neocaml-opam nil
  "Major mode for editing opam files with tree-sitter."
  :prefix "neocaml-opam-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-opam-indent-offset 2
  "Number of spaces for each indentation step in `neocaml-opam-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-opam
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-opam-complete-packages t
  "When non-nil, complete opam package names in dependency fields.
Candidates come from `opam list' (see `neocaml-opam-list-program')
and are offered inside the `depends', `depopts', and `conflicts'
fields."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-opam
  :package-version '(neocaml . "0.9.0"))

(defcustom neocaml-opam-list-program "opam"
  "The opam executable used to list available packages."
  :type 'string
  :group 'neocaml-opam
  :package-version '(neocaml . "0.9.0"))

(defcustom neocaml-opam-use-opam-exec nil
  "When non-nil, list packages via `opam exec --'.
This is useful when Emacs does not inherit the opam environment,
e.g. when launched from a desktop shortcut on macOS, so the
project's switch is consulted."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-opam
  :package-version '(neocaml . "0.9.0"))

;;; Grammar installation

(defconst neocaml-opam-grammar-recipes
  '((opam "https://github.com/tmcgilchrist/tree-sitter-opam"
          "master"
          "src"))
  "Tree-sitter grammar recipe for opam files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-opam-install-grammar (&optional force)
  "Install the opam tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'opam nil)))
    (message "Installing opam tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-opam-grammar-recipes))
      (treesit-install-language-grammar 'opam))))

;;; Font-lock

(defvar neocaml-opam--font-lock-settings
  (treesit-font-lock-rules
   :language 'opam
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'opam
   :feature 'keyword
   '((variable name: (ident) @font-lock-keyword-face)
     (section kind: (ident) @font-lock-keyword-face))

   :language 'opam
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'opam
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'opam
   :feature 'number
   '((int) @font-lock-number-face)

   :language 'opam
   :feature 'constant
   '((bool) @font-lock-constant-face)

   :language 'opam
   :feature 'operator
   '((relop) @font-lock-operator-face
     (pfxop) @font-lock-operator-face
     (envop) @font-lock-operator-face
     "&" @font-lock-operator-face
     "|" @font-lock-operator-face)

   :language 'opam
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'opam
   :feature 'delimiter
   '(":" @font-lock-delimiter-face)

   :language 'opam
   :feature 'variable
   '((ident) @font-lock-variable-use-face))
  "Font-lock settings for `neocaml-opam-mode'.")

;;; Indentation

(defvar neocaml-opam--indent-rules
  `((opam
     ((parent-is "source_file") column-0 0)
     ;; Don't reindent inside strings (e.g., triple-quoted descriptions)
     ((parent-is "string") no-indent 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "section") parent-bol neocaml-opam-indent-offset)
     ((parent-is "list") parent-bol neocaml-opam-indent-offset)
     ((parent-is "group") parent-bol neocaml-opam-indent-offset)
     ((parent-is "option_value") parent-bol neocaml-opam-indent-offset)
     (no-node parent-bol neocaml-opam-indent-offset)))
  "Indentation rules for `neocaml-opam-mode'.")

;;; Imenu

(defvar neocaml-opam--imenu-settings
  '(("Variable" "\\`variable\\'" nil nil)
    ("Section" "\\`section\\'" nil nil))
  "Imenu settings for `neocaml-opam-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Prettify symbols

(defvar neocaml-opam-prettify-symbols-alist
  '(("&" . ?∧)
    ("|" . ?∨)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("!=" . ?≠))
  "Alist of symbol prettifications for opam files.
See `prettify-symbols-alist' for more information.")

;;; Completion

;; Field and section names follow the opam manual's package definition
;; reference (https://opam.ocaml.org/doc/Manual.html).

(defvar neocaml-opam--field-names
  '("opam-version" "name" "version" "synopsis" "description" "maintainer"
    "authors" "license" "homepage" "doc" "bug-reports" "dev-repo" "tags"
    "depends" "depopts" "conflicts" "conflict-class" "depexts" "available"
    "flags" "features" "build" "install" "remove" "run-test" "build-doc"
    "build-test" "patches" "substs" "build-env" "setenv" "extra-files"
    "pin-depends" "messages" "post-messages" "x-env-path-rewrite")
  "Top-level field names for `neocaml-opam-mode' completion.")

(defvar neocaml-opam--section-kinds
  '("url" "extra-source")
  "Section kinds for `neocaml-opam-mode' completion.")

(defvar neocaml-opam--dependency-fields
  '("depends" "depopts" "conflicts")
  "Fields whose list values are opam package names.")

(defconst neocaml-opam--ident-chars "[:alnum:]_-"
  "Characters that make up an opam field or package identifier.")

(defvar neocaml-opam--package-cache (make-hash-table :test 'equal)
  "Per-project cache of opam package names, keyed by project root.")

(defun neocaml-opam--ident-bounds ()
  "Return the bounds of the opam identifier at point as (START . END)."
  (cons (save-excursion (skip-chars-backward neocaml-opam--ident-chars) (point))
        (save-excursion (skip-chars-forward neocaml-opam--ident-chars) (point))))

(defun neocaml-opam--field-of-list (open-pos)
  "Return the field name owning the list opened at OPEN-POS, or nil."
  (save-excursion
    (goto-char open-pos)
    (skip-chars-backward " \t\n")
    (when (eq (char-before) ?:)
      (backward-char)
      (let ((end (point)))
        (skip-chars-backward neocaml-opam--ident-chars)
        (buffer-substring-no-properties (point) end)))))

(defun neocaml-opam--project-root ()
  "Return the project root for opam completion.
Uses the nearest ancestor with a `dune-project', else
`default-directory'."
  (if-let* ((root (locate-dominating-file default-directory "dune-project")))
      (expand-file-name root)
    (expand-file-name default-directory)))

(defun neocaml-opam--list-command ()
  "Return the argv used to list available opam packages.
Prefixes with `opam exec --' when `neocaml-opam-use-opam-exec' is set."
  (if neocaml-opam-use-opam-exec
      (list "opam" "exec" "--"
            neocaml-opam-list-program "list" "--all" "--short")
    (list neocaml-opam-list-program "list" "--all" "--short")))

(defun neocaml-opam--opam-packages (root)
  "Return the opam packages available in ROOT's switch.
Run the listing from ROOT so a project-local switch is picked up.
Return nil when opam is unavailable or fails."
  (let* ((command (neocaml-opam--list-command))
         (program (car command)))
    (when (executable-find program)
      (let ((default-directory (or root default-directory)))
        (with-temp-buffer
          (when (zerop (apply #'call-process program nil t nil (cdr command)))
            (split-string (buffer-string) "[ \t\n]+" t)))))))

(defun neocaml-opam--package-candidates ()
  "Return opam package names for completion in the current project.
Caches the result per project root."
  (when neocaml-opam-complete-packages
    (let* ((root (neocaml-opam--project-root))
           (cached (gethash root neocaml-opam--package-cache 'missing)))
      (if (eq cached 'missing)
          (puthash root (neocaml-opam--opam-packages root)
                   neocaml-opam--package-cache)
        cached))))

(defun neocaml-opam-refresh-packages ()
  "Clear the cached opam package names used for completion.
Call this after the set of available packages changes (e.g. a new
switch or `opam update')."
  (interactive)
  (clrhash neocaml-opam--package-cache)
  (message "neocaml-opam: package cache cleared"))

(defun neocaml-opam--capf (start end candidates annotation kind)
  "Build a capf result for CANDIDATES spanning START..END.
ANNOTATION is shown next to each candidate and KIND is the
`:company-kind' symbol."
  (list start end candidates
        :annotation-function (lambda (_) (concat " " annotation))
        :company-kind (lambda (_) kind)
        :exclusive 'no))

(defun neocaml-opam-completion-at-point ()
  "Complete opam field names and dependency package names at point.
Intended for `completion-at-point-functions'."
  (let ((ppss (syntax-ppss)))
    (cond
     ;; Inside a string: offer package names when it is an element of a
     ;; dependency field's list.
     ((nth 3 ppss)
      (when neocaml-opam-complete-packages
        (let ((opens (nth 9 ppss)))
          (when (and opens
                     (member (neocaml-opam--field-of-list (car (last opens)))
                             neocaml-opam--dependency-fields))
            (neocaml-opam--capf
             (1+ (nth 8 ppss)) (point)
             (completion-table-dynamic
              (lambda (_) (neocaml-opam--package-candidates)))
             "package" 'module)))))
     ;; Inside a comment: nothing.
     ((nth 4 ppss) nil)
     ;; A top-level identifier at the start of a line is a field name.
     ((zerop (car ppss))
      (let* ((bounds (neocaml-opam--ident-bounds))
             (start (car bounds)))
        (when (save-excursion (goto-char start) (skip-chars-backward " \t") (bolp))
          (neocaml-opam--capf
           start (cdr bounds)
           (append neocaml-opam--field-names neocaml-opam--section-kinds)
           "field" 'keyword)))))))

;;; Flymake (opam lint)

(defcustom neocaml-opam-lint-program "opam"
  "The opam executable used for linting."
  :type 'string
  :group 'neocaml-opam
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-opam-flymake-backend t
  "When non-nil, register the `opam lint' flymake backend.
The backend is registered in `flymake-diagnostic-functions' but
`flymake-mode' is not enabled automatically.  Enable it via a hook:

  (add-hook \\='neocaml-opam-mode-hook #\\='flymake-mode)"
  :type 'boolean
  :group 'neocaml-opam
  :package-version '(neocaml . "0.6.0"))

(defvar-local neocaml-opam--flymake-proc nil
  "The current opam lint process for flymake.")

(defun neocaml-opam--flymake-lint (report-fn &rest _args)
  "Flymake backend for `opam lint'.
Calls REPORT-FN with a list of diagnostics.
Buffer contents are piped to `opam lint -' via stdin."
  ;; Kill any leftover process
  (when (process-live-p neocaml-opam--flymake-proc)
    (kill-process neocaml-opam--flymake-proc))
  (let ((source (current-buffer)))
    (setq neocaml-opam--flymake-proc
          (make-process
           :name "neocaml-opam-lint"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *neocaml-opam-lint*")
           :command (list neocaml-opam-lint-program "lint" "-")
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (when (and (buffer-live-p source)
                              (with-current-buffer source
                                (eq proc neocaml-opam--flymake-proc)))
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (let ((diags nil))
                         (while (not (eobp))
                           (let ((diag (neocaml-opam--parse-lint-line
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position))
                                        source)))
                             (when diag (push diag diags)))
                           (forward-line 1))
                         (funcall report-fn (nreverse diags)))))
                 (kill-buffer (process-buffer proc)))))))
    ;; Send buffer contents via stdin
    (save-restriction
      (widen)
      (process-send-region neocaml-opam--flymake-proc (point-min) (point-max)))
    (process-send-eof neocaml-opam--flymake-proc)))

(defun neocaml-opam--parse-lint-line (line source-buffer)
  "Parse a single LINE of opam lint output into a flymake diagnostic.
SOURCE-BUFFER is the buffer being checked."
  (cond
   ;; Error/warning with location: "error  3: ... at line 5, column 2: ..."
   ((string-match
     (rx (group (or "error" "warning"))
         (+ space) (group (+ digit)) ": "
         (group (+? anything))
         " at line " (group (+ digit)) ", column " (group (+ digit)) ": "
         (group (+ anything)))
     line)
    (let* ((type (if (string= (match-string 1 line) "error") :error :warning))
           (code (match-string 2 line))
           (context (match-string 3 line))
           (lnum (string-to-number (match-string 4 line)))
           (col (string-to-number (match-string 5 line)))
           (detail (match-string 6 line))
           (msg (format "opam lint [%s]: %s: %s" code context detail))
           (region (with-current-buffer source-buffer
                     (flymake-diag-region source-buffer lnum col))))
      (flymake-make-diagnostic source-buffer
                               (car region) (cdr region) type msg)))
   ;; Error/warning without location: "error 23: Missing field 'maintainer'"
   ((string-match
     (rx (group (or "error" "warning"))
         (+ space) (group (+ digit)) ": "
         (group (+ anything)))
     line)
    (let* ((type (if (string= (match-string 1 line) "error") :error :warning))
           (code (match-string 2 line))
           (msg (format "opam lint [%s]: %s" code (match-string 3 line))))
      ;; File-level diagnostic: point to first character
      (flymake-make-diagnostic source-buffer 1 2 type msg)))))

;;; Mode definition

(defvar neocaml-opam-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `#' starts a line comment ending at the newline.  Brackets and
    ;; double-quoted strings are inherited from the standard table; teaching
    ;; the syntax table about comments keeps `syntax-ppss' (and thus
    ;; completion-at-point) from misreading commented-out text as code.
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `neocaml-opam-mode'.")

;;;###autoload
(define-derived-mode neocaml-opam-mode prog-mode "opam"
  "Major mode for editing opam package files.

\\{neocaml-opam-mode-map}"
  (when (< (treesit-library-abi-version) 14)
    (error "The opam grammar requires tree-sitter ABI version 14+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.24" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'opam)
    (when (y-or-n-p "Opam tree-sitter grammar is not installed.  Install it now?")
      (neocaml-opam-install-grammar))
    (unless (treesit-ready-p 'opam)
      (error "Cannot activate neocaml-opam-mode without the opam grammar")))
  (treesit-parser-create 'opam)

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Font-lock
  (setq-local treesit-font-lock-settings neocaml-opam--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword)
                (string constant)
                (number escape-sequence)
                (operator bracket delimiter variable)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-opam--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-opam--imenu-settings)

  ;; Completion
  (add-hook 'completion-at-point-functions
            #'neocaml-opam-completion-at-point nil t)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              "\\`\\(?:variable\\|section\\)\\'")
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Prettify symbols
  (setq-local prettify-symbols-alist neocaml-opam-prettify-symbols-alist)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  ;; Make URLs and bug references in comments clickable
  (goto-address-prog-mode)
  (bug-reference-prog-mode)

  (treesit-major-mode-setup)

  ;; Flymake via opam lint (users enable flymake-mode via hook)
  (when (and neocaml-opam-flymake-backend
             (executable-find neocaml-opam-lint-program))
    (add-hook 'flymake-diagnostic-functions
              #'neocaml-opam--flymake-lint nil t)))

;;;###autoload
;; Matches bare "opam" files (e.g., repo/opam), named "foo.opam"
;; files — the [./] covers the dot in ".opam".
;; Optionally matches "foo.opam.template" files.
(add-to-list 'auto-mode-alist '("[./]opam\\(?:\\.template\\)?\\'" . neocaml-opam-mode))

(provide 'neocaml-opam)

;;; neocaml-opam.el ends here
