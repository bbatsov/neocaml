;;; neocaml-dune.el --- Major mode for dune files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml dune

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing dune build files,
;; dune-project files, and dune-workspace files.
;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-dune.

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

(defgroup neocaml-dune nil
  "Major mode for editing dune files with tree-sitter."
  :prefix "neocaml-dune-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-dune-indent-offset 1
  "Number of spaces for each indentation step in `neocaml-dune-mode'.
Dune files conventionally use 1-space indentation."
  :type 'natnum
  :safe 'natnump
  :group 'neocaml-dune
  :package-version '(neocaml . "0.6.0"))

(defcustom neocaml-dune-format-on-save nil
  "When non-nil, format the buffer with `dune format-dune-file' before saving."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-dune
  :package-version '(neocaml . "0.8.0"))

(defcustom neocaml-dune-complete-libraries t
  "When non-nil, complete findlib library names in dune files.
Candidates come from `ocamlfind list' (see
`neocaml-dune-ocamlfind-program') and are offered inside fields
such as `libraries' and `pps'."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-dune
  :package-version '(neocaml . "0.9.0"))

(defcustom neocaml-dune-ocamlfind-program "ocamlfind"
  "The ocamlfind executable used to list installed libraries."
  :type 'string
  :group 'neocaml-dune
  :package-version '(neocaml . "0.9.0"))

;;; Grammar installation

(defconst neocaml-dune-grammar-recipes
  '((dune "https://github.com/tmcgilchrist/tree-sitter-dune"
          "master"
          "src"))
  "Tree-sitter grammar recipe for dune files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-dune-install-grammar (&optional force)
  "Install the dune tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'dune nil)))
    (message "Installing dune tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-dune-grammar-recipes))
      (treesit-install-language-grammar 'dune))))

;;; Formatting

(defun neocaml-dune-format-buffer ()
  "Format the current buffer using `dune format-dune-file'.
Pipes the buffer content through the command and replaces the
buffer text with the formatted output, preserving point."
  (interactive)
  (let ((outbuf (generate-new-buffer " *neocaml-dune-format*"))
        ;; Send stderr to its own file so dune's `Entering directory' /
        ;; `Leaving directory' markers (emitted on stderr from dune 3.21+
        ;; when invoked inside a project) don't wrap the formatted output.
        (errfile (make-temp-file "neocaml-dune-format-err"))
        (orig-point (point))
        (orig-window-start (window-start)))
    (unwind-protect
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              "dune" nil
                                              (list outbuf errfile) nil
                                              "format-dune-file")))
          (if (zerop exit-code)
              (progn
                (erase-buffer)
                (insert-buffer-substring outbuf)
                (goto-char (min orig-point (point-max)))
                (set-window-start (selected-window) orig-window-start))
            (user-error "Dune format-dune-file failed: %s"
                        (with-temp-buffer
                          (insert-file-contents errfile)
                          (string-trim (buffer-string))))))
      (kill-buffer outbuf)
      (delete-file errfile))))

(defun neocaml-dune--format-before-save ()
  "Format the buffer before saving if `neocaml-dune-format-on-save' is non-nil."
  (when neocaml-dune-format-on-save
    (neocaml-dune-format-buffer)))

;;; Font-lock

(defvar neocaml-dune--font-lock-settings
  (treesit-font-lock-rules
   :language 'dune
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'dune
   :feature 'keyword
   '((stanza_name) @font-lock-keyword-face
     (action_name) @font-lock-keyword-face)

   :language 'dune
   :feature 'property
   '((field_name) @font-lock-property-name-face)

   :language 'dune
   :feature 'string
   '((quoted_string) @font-lock-string-face
     (multiline_string) @font-lock-string-face)

   :language 'dune
   :feature 'constant
   '(["true" "false"] @font-lock-constant-face)

   :language 'dune
   :feature 'type
   '((module_name) @font-lock-type-face
     (library_name) @font-lock-type-face
     (package_name) @font-lock-type-face
     (public_name) @font-lock-type-face)

   :language 'dune
   :feature 'operator
   '((blang_op) @font-lock-operator-face)

   :language 'dune
   :feature 'bracket
   '(["(" ")"] @font-lock-bracket-face))
  "Font-lock settings for `neocaml-dune-mode'.")

;;; Indentation

;; Known limitation: the grammar flattens field-value pairs into
;; direct stanza children (no wrapper node for e.g. "(action (run ...))").
;; This means field values on continuation lines indent at the same
;; level as the field name rather than one deeper:
;;   (action
;;   (run foo))     ; actual — both at stanza indent + 1
;; instead of the conventional:
;;   (action
;;    (run foo))    ; expected — value at stanza indent + 2
;; See https://github.com/tmcgilchrist/tree-sitter-dune/issues/9

(defvar neocaml-dune--indent-rules
  `((dune
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ;; Don't reindent inside strings
     ((parent-is "quoted_string") no-indent 0)
     ((parent-is "multiline_string") no-indent 0)
     ;; All parenthesized constructs: indent 1 from parent bol
     ((parent-is "stanza") parent-bol neocaml-dune-indent-offset)
     ((parent-is "sexp") parent-bol neocaml-dune-indent-offset)
     ((parent-is "action") parent-bol neocaml-dune-indent-offset)
     ((parent-is "blang") parent-bol neocaml-dune-indent-offset)
     ((parent-is "_list") parent-bol neocaml-dune-indent-offset)
     (no-node parent-bol neocaml-dune-indent-offset)))
  "Indentation rules for `neocaml-dune-mode'.")

;;; Completion

;; Stanza and field names are taken from the dune manual's reference
;; (https://dune.readthedocs.io/en/stable/reference/dune/index.html).

(defvar neocaml-dune--stanza-names
  '("library" "executable" "executables" "test" "tests" "rule" "alias"
    "install" "env" "menhir" "ocamllex" "ocamlyacc" "documentation"
    "cram" "mdx" "foreign_library" "toplevel" "plugin" "cinaps"
    "deprecated_library_name" "generate_sites_module" "library_parameter"
    "rocq.theory" "copy_files" "include" "dynamic_include" "subdir"
    "dirs" "data_only_dirs" "ignored_subdirs" "vendored_dirs"
    "include_subdirs" "files")
  "Top-level stanza names for `neocaml-dune-mode' completion.")

(defvar neocaml-dune--stanza-fields
  '(("library"
     . ("name" "public_name" "package" "synopsis" "modules" "libraries"
        "wrapped" "preprocess" "preprocessor_deps" "optional" "foreign_stubs"
        "foreign_archives" "install_c_headers" "public_headers" "modes"
        "no_dynlink" "kind" "ppx_runtime_libraries" "virtual_deps" "implements"
        "parameters" "js_of_ocaml" "wasm_of_ocaml" "flags" "ocamlc_flags"
        "ocamlopt_flags" "library_flags" "c_library_flags"
        "modules_without_implementation" "private_modules"
        "allow_overlapping_dependencies" "enabled_if" "inline_tests"
        "root_module" "ctypes" "empty_module_interface_if_absent"))
    ("executable"
     . ("name" "public_name" "package" "libraries" "link_flags" "link_deps"
        "modules" "root_module" "modes" "preprocess" "preprocessor_deps"
        "js_of_ocaml" "wasm_of_ocaml" "flags" "ocamlc_flags" "ocamlopt_flags"
        "modules_without_implementation" "allow_overlapping_dependencies"
        "optional" "enabled_if" "promote" "foreign_stubs" "foreign_archives"
        "forbidden_libraries" "embed_in_plugin_libraries" "ctypes"
        "empty_module_interface_if_absent"))
    ("executables"
     . ("names" "public_names" "package" "libraries" "link_flags" "link_deps"
        "modules" "root_module" "modes" "preprocess" "preprocessor_deps"
        "js_of_ocaml" "wasm_of_ocaml" "flags" "ocamlc_flags" "ocamlopt_flags"
        "modules_without_implementation" "allow_overlapping_dependencies"
        "optional" "enabled_if" "promote" "foreign_stubs" "foreign_archives"
        "forbidden_libraries" "embed_in_plugin_libraries" "ctypes"
        "empty_module_interface_if_absent"))
    ("test"
     . ("name" "libraries" "deps" "action" "enabled_if" "build_if" "locks"
        "package" "modules" "modes" "flags" "preprocess" "preprocessor_deps"
        "link_flags" "link_deps" "root_module" "ocamlc_flags" "ocamlopt_flags"
        "forbidden_libraries"))
    ("tests"
     . ("names" "libraries" "deps" "action" "enabled_if" "build_if" "locks"
        "package" "modules" "modes" "flags" "preprocess" "preprocessor_deps"
        "link_flags" "link_deps" "root_module" "ocamlc_flags" "ocamlopt_flags"
        "forbidden_libraries"))
    ("rule"
     . ("action" "target" "targets" "deps" "mode" "fallback" "locks" "alias"
        "aliases" "package" "enabled_if"))
    ("install"
     . ("section" "files" "dirs" "source_trees" "package" "enabled_if"))
    ("alias"
     . ("name" "deps" "enabled_if" "action" "package" "locks"))
    ("env"
     . ("flags" "ocamlc_flags" "ocamlopt_flags" "link_flags" "c_flags"
        "cxx_flags" "env-vars" "menhir_flags" "menhir" "js_of_ocaml"
        "wasm_of_ocaml" "binaries" "inline_tests" "odoc" "rocq" "formatting"
        "bin_annot"))
    ("documentation"
     . ("package" "mld_files" "files"))
    ("menhir"
     . ("modules" "merge_into" "flags" "infer" "explain"))
    ("ocamllex"
     . ("modules" "mode" "enabled_if"))
    ("ocamlyacc"
     . ("modules" "mode" "enabled_if")))
  "Alist mapping a dune stanza name to its valid field names.
Stanzas not listed here fall back to `neocaml-dune--all-fields'.")

(defvar neocaml-dune--all-fields
  (delete-dups (apply #'append (mapcar #'cdr neocaml-dune--stanza-fields)))
  "Union of all known dune field names, used as a fallback.")

(defconst neocaml-dune--atom-chars "[:alnum:]_.+-"
  "Characters that make up a dune atom (stanza, field, or library name).")

(defun neocaml-dune--atom-bounds ()
  "Return the bounds of the dune atom at point as a cons (START . END).
When point is not on an atom, both ends equal point."
  (cons (save-excursion (skip-chars-backward neocaml-dune--atom-chars) (point))
        (save-excursion (skip-chars-forward neocaml-dune--atom-chars) (point))))

(defun neocaml-dune--first-atom-pos (open-pos)
  "Return the position of the first atom following the open paren at OPEN-POS."
  (save-excursion
    (goto-char (1+ open-pos))
    (skip-chars-forward " \t\n")
    (point)))

(defun neocaml-dune--head-after (open-pos)
  "Return the head atom of the form opened at OPEN-POS, or nil."
  (save-excursion
    (goto-char (neocaml-dune--first-atom-pos open-pos))
    (let ((beg (point)))
      (skip-chars-forward neocaml-dune--atom-chars)
      (when (> (point) beg)
        (buffer-substring-no-properties beg (point))))))

(defun neocaml-dune--fields-for (stanza)
  "Return the field names valid inside STANZA, or the union fallback."
  (or (cdr (assoc stanza neocaml-dune--stanza-fields))
      neocaml-dune--all-fields))

(defvar neocaml-dune--library-fields '("libraries" "pps")
  "Field names whose values are findlib library names.")

(defvar neocaml-dune--library-cache nil
  "Cached list of findlib library names, or nil when not yet fetched.")

(defun neocaml-dune--ocamlfind-libraries ()
  "Return the list of installed findlib libraries via ocamlfind.
Return nil when ocamlfind is unavailable or fails."
  (when (executable-find neocaml-dune-ocamlfind-program)
    (with-temp-buffer
      (when (zerop (call-process neocaml-dune-ocamlfind-program nil t nil "list"))
        (goto-char (point-min))
        (let (libs)
          (while (not (eobp))
            (when (looking-at "\\([[:alnum:]_.-]+\\)[ \t]")
              (push (match-string-no-properties 1) libs))
            (forward-line 1))
          (nreverse libs))))))

(defun neocaml-dune--library-candidates ()
  "Return findlib library names for completion, caching the result."
  (or neocaml-dune--library-cache
      (setq neocaml-dune--library-cache (neocaml-dune--ocamlfind-libraries))))

(defun neocaml-dune-refresh-libraries ()
  "Clear the cached findlib library names used for completion.
Call this after installing or removing opam packages so the next
completion reflects the current switch."
  (interactive)
  (setq neocaml-dune--library-cache nil)
  (message "neocaml-dune: findlib library cache cleared"))

(defun neocaml-dune--capf (start end candidates annotation kind)
  "Build a capf result for CANDIDATES spanning START..END.
ANNOTATION is shown next to each candidate and KIND is the
`:company-kind' symbol."
  (list start end candidates
        :annotation-function (lambda (_) (concat " " annotation))
        :company-kind (lambda (_) kind)
        :exclusive 'no))

(defun neocaml-dune-completion-at-point ()
  "Complete dune stanza and field names at point.
Intended for `completion-at-point-functions'."
  (let ((ppss (syntax-ppss)))
    (when-let* (((not (nth 3 ppss)))    ; not inside a string
                ((not (nth 4 ppss)))    ; not inside a comment
                (opens (nth 9 ppss)))   ; enclosing open parens
      (let* ((bounds (neocaml-dune--atom-bounds))
             (start (car bounds))
             (end (cdr bounds))
             (depth (length opens))
             (inner (car (last opens)))
             (outer (car opens))
             (head-p (= start (neocaml-dune--first-atom-pos inner))))
        (cond
         ((and (= depth 1) head-p)
          (neocaml-dune--capf start end neocaml-dune--stanza-names
                              "stanza" 'keyword))
         ((and (>= depth 2) head-p)
          (neocaml-dune--capf start end
                              (neocaml-dune--fields-for
                               (neocaml-dune--head-after outer))
                              "field" 'property))
         ((and neocaml-dune-complete-libraries
               (not head-p)
               (member (neocaml-dune--head-after inner)
                       neocaml-dune--library-fields))
          (neocaml-dune--capf start end
                              (completion-table-dynamic
                               (lambda (_) (neocaml-dune--library-candidates)))
                              "library" 'module)))))))

;;; Imenu

(defvar neocaml-dune--imenu-settings
  '(("Stanza" "\\`stanza\\'" nil nil))
  "Imenu settings for `neocaml-dune-mode'.
See `treesit-simple-imenu-settings' for the format.")

;;; Navigation

(defun neocaml-dune--defun-name (node)
  "Return a name for NODE suitable for imenu and which-func.
For stanzas, returns the stanza type and its name field if present."
  (let* ((first-child (treesit-node-child node 0 t))
         (stanza-name (when (and first-child
                                 (string= (treesit-node-type first-child) "stanza_name"))
                        (treesit-node-text first-child t))))
    (when stanza-name
      ;; Try to find a name-like field for a more descriptive label.
      ;; First check named fields, then scan for a (name ...) field.
      (let ((name-value
             (or (let ((n (treesit-node-child-by-field-name node "project_name")))
                   (when n (treesit-node-text n t)))
                 (let ((n (treesit-node-child-by-field-name node "alias_name")))
                   (when n (treesit-node-text n t)))
                 (let ((child first-child)
                       (result nil))
                   (while (and (not result) (setq child (treesit-node-next-sibling child t)))
                     (when (and (string= (treesit-node-type child) "field_name")
                                (string= (treesit-node-text child t) "name"))
                       (let ((next (treesit-node-next-sibling child t)))
                         (when next
                           (setq result (treesit-node-text next t))))))
                   result))))
        (if name-value
            (format "%s %s" stanza-name name-value)
          stanza-name)))))

;;; Mode definition

(defvar neocaml-dune-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `;' starts a line comment ending at the newline.  Parentheses and
    ;; double-quoted strings are inherited from the standard table; teaching
    ;; the syntax table about comments keeps `syntax-ppss' (and thus
    ;; completion-at-point) from miscounting parens inside comments.
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `neocaml-dune-mode'.")

;;;###autoload
(define-derived-mode neocaml-dune-mode prog-mode "dune"
  "Major mode for editing dune build files.

Supports dune, dune-project, and dune-workspace files.

\\{neocaml-dune-mode-map}"
  (when (< (treesit-library-abi-version) 14)
    (error "The dune grammar requires tree-sitter ABI version 14+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.24" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'dune)
    (when (y-or-n-p "Dune tree-sitter grammar is not installed.  Install it now?")
      (neocaml-dune-install-grammar))
    (unless (treesit-ready-p 'dune)
      (error "Cannot activate neocaml-dune-mode without the dune grammar")))
  (treesit-parser-create 'dune)

  ;; Comments
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")

  ;; Font-lock
  (setq-local treesit-font-lock-settings neocaml-dune--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword)
                (string property)
                (constant type)
                (operator bracket)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules neocaml-dune--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings neocaml-dune--imenu-settings)

  ;; Completion
  (add-hook 'completion-at-point-functions
            #'neocaml-dune-completion-at-point nil t)

  ;; Navigation
  (setq-local treesit-defun-type-regexp "\\`stanza\\'")
  (setq-local treesit-defun-name-function #'neocaml-dune--defun-name)

  ;; which-func-mode / add-log integration
  (setq-local add-log-current-defun-function #'treesit-add-log-current-defun)

  ;; Format on save
  (add-hook 'before-save-hook #'neocaml-dune--format-before-save nil t)

  ;; Final newline
  (setq-local require-final-newline mode-require-final-newline)

  ;; Make URLs and bug references in comments clickable
  (goto-address-prog-mode)
  (bug-reference-prog-mode)

  (treesit-major-mode-setup))

(define-key neocaml-dune-mode-map (kbd "C-c C-f") #'neocaml-dune-format-buffer)

;;;###autoload
(progn
  ;; Matches "dune" files (e.g., src/dune) but not dune-project or dune-workspace
  (add-to-list 'auto-mode-alist '("/dune\\'" . neocaml-dune-mode))
  ;; dune-project and dune-workspace use the same grammar and mode.
  ;; dune-workspace files may have a dot-suffix (e.g., dune-workspace.ci,
  ;; dune-workspace.5.3) used by dune-pkg workflows.
  (add-to-list 'auto-mode-alist '("/dune-project\\'" . neocaml-dune-mode))
  (add-to-list 'auto-mode-alist '("/dune-workspace\\(?:\\..*\\)?\\'" . neocaml-dune-mode)))

(provide 'neocaml-dune)

;;; neocaml-dune.el ends here
