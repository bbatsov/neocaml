;;; neocaml-cram.el --- Major mode for cram test files -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml cram

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing cram test (.t) files, as used by dune's
;; expect-test framework.  Cram tests are a simple line-oriented
;; format where shell commands, expected output, and prose are
;; distinguished by their indentation:
;;
;;   Prose/comments (unindented lines)
;;
;;     $ shell-command
;;     expected output
;;     > continuation-of-previous-command
;;
;; This mode provides font-lock highlighting for all line types,
;; output modifiers ((re), (glob), (no-eol), (esc)), and dune's
;; unreachable marker.

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

(defgroup neocaml-cram nil
  "Major mode for editing cram test files."
  :prefix "neocaml-cram-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neocaml"))

;;; Font-lock

(defvar neocaml-cram--font-lock-keywords
  `(;; Command lines: "  $ command..."
    ("^  \\(\\$\\) \\(.*\\)$"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-call-face))
    ;; Continuation lines: "  > command..."
    ("^  \\(>\\) \\(.*\\)$"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-call-face))
    ;; Dune unreachable marker
    ("^  \\(\\*\\*\\*\\*\\* UNREACHABLE \\*\\*\\*\\*\\*\\)$"
     (1 'font-lock-warning-face))
    ;; Output with modifier: "  output (re)" etc.
    ("^  \\(.*\\) \\((\\(?:re\\|glob\\|no-eol\\|esc\\))\\)$"
     (1 'font-lock-string-face)
     (2 'font-lock-type-face))
    ;; Exit code: "  [N]"
    ("^  \\(\\[[0-9]+\\]\\)$"
     (1 'font-lock-constant-face))
    ;; Plain output lines: "  anything"
    ("^  \\(.*\\)$"
     (1 'font-lock-string-face))
    ;; Prose/comment lines (unindented)
    ("^\\([^ \n].*\\)$"
     (1 'font-lock-comment-face)))
  "Font-lock keywords for `neocaml-cram-mode'.")

;;; Imenu

(defvar neocaml-cram--imenu-generic-expression
  '(("Command" "^  \\$ \\(.+\\)$" 1))
  "Imenu generic expression for `neocaml-cram-mode'.
Indexes shell command lines.")

;;; Mode definition

;;;###autoload
(define-derived-mode neocaml-cram-mode text-mode "Cram"
  "Major mode for editing cram test files.

Cram tests use a simple line-oriented format:
- Lines starting with `  $ ' are shell commands
- Lines starting with `  > ' are command continuations
- Lines starting with `  ' (2 spaces) are expected output
- Unindented lines are prose/comments

\\{neocaml-cram-mode-map}"
  (setq-local font-lock-defaults '(neocaml-cram--font-lock-keywords))
  (setq-local font-lock-multiline nil)
  (setq-local comment-start "")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression neocaml-cram--imenu-generic-expression)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local require-final-newline mode-require-final-newline))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.t\\'" . neocaml-cram-mode))

(provide 'neocaml-cram)

;;; neocaml-cram.el ends here
