;;; neocaml-eml.el --- Major mode for Dream eml template files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tree-sitter based major mode for editing Dream's Embedded ML
;; templates (`.eml.ml', `.eml.html', `.eml.re').

;; Note which way round the injection goes.  Unlike `neocaml-mlx-mode',
;; where the host grammar is `ocaml' and another language is injected
;; into it, here the host grammar is `eml' and OCaml is the *injected*
;; language.  That is forced by the format: an eml template body is not
;; an OCaml expression, a `% ...' code line is a bare fragment such as
;; `% end;', and `<%s x %>' has no OCaml node to anchor to, so an OCaml
;; host parse would have to compute the template regions out of error
;; recovery.  The eml grammar recognises the template structure and
;; leaves the code opaque, and this mode injects `ocaml' into the code
;; regions and `html' into the template text.

;; The OCaml ranges deliberately share one parser.  With the template
;; text removed, a code block and the `%' lines that follow it are a
;; single statement stream -- `let render tasks =' opens a binding that
;; `% tasks |> List.iter begin fun _ ->' continues and `% end;' closes
;; -- and they only parse as a unit if one parser sees all of them.
;; This differs from `neocaml-ocamllex-mode', which uses `:local t'
;; because each `{ ... }' action there is independent.

;; For the tree-sitter grammar this mode is based on,
;; see https://github.com/tmcgilchrist/tree-sitter-eml.

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
(require 'neocaml)
(require 'neocaml-common)

;; `html-ts-mode' is Emacs 30+, while neocaml supports Emacs 29, so it is
;; named through a variable rather than required outright -- a literal
;; `require' would oblige the package to raise its Emacs dependency for a
;; feature that is optional here.  Its internals are declared, not assumed.
(defconst neocaml-eml--html-feature 'html-ts-mode
  "Feature providing the HTML tree-sitter font-lock rules.")

(defvar html-ts-mode--font-lock-settings)
(defvar html-ts-mode--treesit-font-lock-feature-list)

(defun neocaml-eml--load-html-mode ()
  "Load the HTML tree-sitter mode, returning non-nil on success."
  (require neocaml-eml--html-feature nil t))

(defgroup neocaml-eml nil
  "Major mode for editing Dream eml templates with tree-sitter."
  :prefix "neocaml-eml-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

(defcustom neocaml-eml-embedded-language 'ocaml
  "Language to inject into the code regions of an eml template.

`dream_eml' decides between OCaml and Reason from the file
extension, so `.eml.ml' is OCaml and `.eml.re' is Reason -- but
`.eml.html' falls through to OCaml unless the dune rule passes
`--emit-reason', and Dream ships an example of each under the same
`template.eml.html' name.  The extension therefore cannot decide it
and this defaults to `ocaml'; set it as a file-local or
directory-local variable for a `.eml.html' that is really Reason.

When the grammar for this language is not installed, the code
regions are simply left unhighlighted."
  :type '(choice (const :tag "OCaml" ocaml)
                 (const :tag "Reason" reason)
                 (const :tag "None" nil))
  :safe #'symbolp
  :group 'neocaml-eml
  :package-version '(neocaml . "0.11.0"))

(defcustom neocaml-eml-inject-html t
  "Whether to inject the `html' grammar into eml template text.
Has no effect when the `html' grammar is not installed."
  :type 'boolean
  :safe #'booleanp
  :group 'neocaml-eml
  :package-version '(neocaml . "0.11.0"))

;;; Grammar installation

(defconst neocaml-eml-grammar-recipes
  '((eml "https://github.com/tmcgilchrist/tree-sitter-eml"
         "v0.1.0"
         "src"))
  "Tree-sitter grammar recipe for Dream eml template files.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

(defun neocaml-eml-install-grammar (&optional force)
  "Install the eml tree-sitter grammar if not already available.
With prefix argument FORCE, reinstall even if already installed."
  (interactive "P")
  (when (or force (not (treesit-language-available-p 'eml nil)))
    (message "Installing eml tree-sitter grammar...")
    (let ((treesit-language-source-alist neocaml-eml-grammar-recipes))
      (treesit-install-language-grammar 'eml))))

;;; Faces

(defface neocaml-eml-delimiter-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the `<%' and `%>' directive delimiters and the `%' of a code line."
  :group 'neocaml-eml
  :package-version '(neocaml . "0.11.0"))

(defface neocaml-eml-format-face
  '((t :inherit font-lock-builtin-face))
  "Face for the Printf conversion of an output directive, the `s' of `<%s x %>'."
  :group 'neocaml-eml
  :package-version '(neocaml . "0.11.0"))

(defface neocaml-eml-raw-face
  '((t :inherit font-lock-warning-face))
  "Face for the `!' of `<%s! x %>', which suppresses HTML escaping.
Unescaped output is a possible injection vector, so it is worth
seeing at a glance."
  :group 'neocaml-eml
  :package-version '(neocaml . "0.11.0"))

;;; Injection

(defun neocaml-eml--code-injection-available-p ()
  "Non-nil if the embedded code language can be injected.
Requires Emacs 30+ (for `treesit-range-rules' with `:embed') and
the grammar named by `neocaml-eml-embedded-language'."
  (and (>= emacs-major-version 30)
       neocaml-eml-embedded-language
       (treesit-language-available-p neocaml-eml-embedded-language)))

(defun neocaml-eml--html-injection-available-p ()
  "Non-nil if HTML injection into the template text is available."
  (and (>= emacs-major-version 30)
       neocaml-eml-inject-html
       (treesit-language-available-p 'html)))

(defun neocaml-eml--range-settings ()
  "Return range settings for the languages embedded in an eml template.
Returns nil when neither is available."
  (append
   (when (neocaml-eml--code-injection-available-p)
     (treesit-range-rules
      :embed neocaml-eml-embedded-language
      :host 'eml
      ;; No `:local t': one parser over every range, so that a code
      ;; block and the `%' lines below it parse as one statement
      ;; stream.  A directive body is spliced into that stream too;
      ;; `<% ... %>' is a statement and reads naturally there, and
      ;; while `<%s x %>' is an expression, keeping it in the stream
      ;; costs less than the extra parser per directive would.
      '((ocaml_block) @capture
        (code_line (code) @capture)
        (directive (code) @capture)
        (output_directive (code) @capture))))
   (when (neocaml-eml--html-injection-available-p)
     (treesit-range-rules
      :embed 'html
      :host 'eml
      ;; Likewise combined: the per-line `text' nodes are fragments of
      ;; one page, not pages of their own.
      '((text) @capture)))))

;;; Font-lock

(defvar neocaml-eml--font-lock-settings
  (treesit-font-lock-rules
   :language 'eml
   :feature 'eml-keyword
   :override t
   '([(template_options) (template_end)] @font-lock-preprocessor-face)

   :language 'eml
   :feature 'eml-delimiter
   :override t
   '(["<%" "%>"] @neocaml-eml-delimiter-face
     (code_line "%" @neocaml-eml-delimiter-face))

   :language 'eml
   :feature 'eml-format
   :override t
   '((format) @neocaml-eml-format-face
     (raw) @neocaml-eml-raw-face))
  "Font-lock settings for the eml template skeleton itself.

`:override t' throughout: a node in an injected parser can span a
gap in that parser's own ranges -- an HTML attribute value whose
quotes sit either side of a directive is the common case -- and
would otherwise paint the directive with its own face.")

(defun neocaml-eml--font-lock-settings ()
  "Return font-lock settings for `neocaml-eml-mode'.
The embedded languages' rules come first and the eml rules last, so
that the overriding eml rules have the final say over the template
skeleton -- `html-ts-mode' also overrides, and whichever runs last
wins."
  (append
   (when (neocaml-eml--code-injection-available-p)
     (neocaml-mode--font-lock-settings neocaml-eml-embedded-language))
   (when (and (neocaml-eml--html-injection-available-p)
              (neocaml-eml--load-html-mode)
              (boundp 'html-ts-mode--font-lock-settings))
     html-ts-mode--font-lock-settings)
   neocaml-eml--font-lock-settings))

(defconst neocaml-eml--feature-list
  '((eml-keyword)
    (eml-delimiter eml-format)
    nil
    nil)
  "Font-lock features for the eml skeleton.
Deliberately at levels 1 and 2, so that the template structure is
visible at the default level 3 rather than needing level 4.  The
names are prefixed to keep them distinct from the `delimiter' and
`keyword' features of the embedded languages, which sit at
different levels.")

(defconst neocaml-eml--html-fallback-feature-list
  '((comment keyword definition) (property string) nil nil)
  "Fallback HTML feature list.
Used on Emacs versions where `html-ts-mode' keeps its feature list
inline in the mode body rather than in a variable.")

(defun neocaml-eml--font-lock-feature-list ()
  "Return the feature list merging eml with the embedded languages.
The embedded lists are read from the variables the other modes
define, rather than by instantiating those modes in a temp buffer,
which would run their mode hooks on every file visit."
  (let ((features neocaml-eml--feature-list))
    (when (neocaml-eml--code-injection-available-p)
      (setq features
            (neocaml-common-merge-feature-lists
             features neocaml--font-lock-feature-list)))
    (when (neocaml-eml--html-injection-available-p)
      (neocaml-eml--load-html-mode)
      (setq features
            (neocaml-common-merge-feature-lists
             features
             (if (boundp 'html-ts-mode--treesit-font-lock-feature-list)
                 html-ts-mode--treesit-font-lock-feature-list
               neocaml-eml--html-fallback-feature-list))))
    features))

;;; Indentation

(defvar neocaml-eml--indent-rules
  `((eml
     ;; A `%' code line only counts as one if the `%' is in column 0.
     ((node-is "code_line") column-0 0)
     ((parent-is "code_line") column-0 0)
     ;; The `%%' lines line up with the template they open or close.
     ((node-is "template_options") parent-bol 0)
     ((node-is "template_end") parent-bol 0)
     ;; Template text is layout-significant: never reflow it, just keep
     ;; whatever the previous line had.
     ((parent-is "template") prev-line 0)
     ((node-is "ocaml_block") no-indent 0)
     ((parent-is "ocaml_block") no-indent 0)
     (no-node prev-line 0)))
  "Indentation rules for `neocaml-eml-mode'.

Deliberately conservative.  eml is layout-sensitive in a way the
template grammar cannot repair: a `%' that drifts off column 0
stops being a code line, and a template line that drifts left of
its opening column ends the template.  So these rules preserve
indentation rather than compute it, and reindenting a whole buffer
is a no-op instead of a hazard.")

;;; Mode definition

(defun neocaml-eml--embedded-language-for-file ()
  "Return the embedded language implied by the current file name.
`.eml.re' is Reason; everything else keeps the value of
`neocaml-eml-embedded-language', which defaults to OCaml."
  (if (and buffer-file-name
           (string-suffix-p ".eml.re" buffer-file-name))
      'reason
    neocaml-eml-embedded-language))

;;;###autoload
(define-derived-mode neocaml-eml-mode prog-mode "EML"
  "Major mode for editing Dream Embedded ML template files.

The host grammar is `eml'.  When the grammar for
`neocaml-eml-embedded-language' is installed, the code block, the
`%' code lines and the `<% ... %>' directives are highlighted by
injecting it; when the `html' grammar is installed, the template
text is highlighted by injecting that.  Both degrade gracefully to
plain template highlighting when the grammar is missing.

\\{neocaml-eml-mode-map}"
  (when (< (treesit-library-abi-version) 14)
    (error "The eml grammar requires tree-sitter ABI version 14+, but \
your Emacs was built against ABI version %d; rebuild Emacs with \
tree-sitter >= 0.24" (treesit-library-abi-version)))
  (unless (treesit-ready-p 'eml)
    (when (y-or-n-p "The eml tree-sitter grammar is not installed.  Install it now?")
      (neocaml-eml-install-grammar))
    (unless (treesit-ready-p 'eml)
      (error "Cannot activate neocaml-eml-mode without the eml grammar")))

  (setq-local neocaml-eml-embedded-language
              (neocaml-eml--embedded-language-for-file))

  (treesit-parser-create 'eml)

  ;; OCaml comments, because the code regions are what people edit in a
  ;; `.eml.ml'.  In a `.eml.html' the template text dominates instead;
  ;; set `comment-start' and friends from a file-local variable there.
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+ *")

  ;; Injection, before `treesit-major-mode-setup'.  The embedded parsers
  ;; have to be created by hand: `treesit-update-ranges' only *looks up*
  ;; the parser for a non-local range rule and silently does nothing when
  ;; there isn't one.  (`neocaml-ocamllex-mode' gets away without this
  ;; because `:local t' ranges create their parsers on demand.)
  (let ((range-settings (neocaml-eml--range-settings)))
    (when range-settings
      (setq-local treesit-range-settings range-settings)
      (when (neocaml-eml--code-injection-available-p)
        (treesit-parser-create neocaml-eml-embedded-language))
      (when (neocaml-eml--html-injection-available-p)
        (treesit-parser-create 'html))))

  ;; Font-lock
  (setq-local treesit-font-lock-settings (neocaml-eml--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              (neocaml-eml--font-lock-feature-list))

  ;; Indentation.  Only spaces are indentation as far as eml is
  ;; concerned -- a tab-indented line has indent 0 and will not open a
  ;; template -- so never insert one.
  (setq-local treesit-simple-indent-rules neocaml-eml--indent-rules)
  (setq-local indent-tabs-mode nil)

  (setq-local require-final-newline mode-require-final-newline)

  ;; Make URLs and bug references in comments clickable
  (goto-address-prog-mode)
  (bug-reference-prog-mode)

  (treesit-major-mode-setup))

;; `neocaml-mode' claims "\\.ml\\'", which also matches `foo.eml.ml'.
;; `add-to-list' prepends and the first match in `auto-mode-alist' wins,
;; so these entries have to be added after it; the autoload cookie above
;; `neocaml-mode' comes earlier in the generated autoloads file, which
;; makes that so.  `neocaml-eml-test.el' pins the behaviour.
;;;###autoload
(dolist (pattern '("\\.eml\\.ml\\'" "\\.eml\\.html\\'" "\\.eml\\.re\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'neocaml-eml-mode)))

(provide 'neocaml-eml)

;;; neocaml-eml.el ends here
