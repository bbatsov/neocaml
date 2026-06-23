;;; neocaml-common.el --- Shared helpers for neocaml tool modes -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/bbatsov/neocaml
;; Keywords: languages ocaml

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Small, dependency-free helpers shared by neocaml's auxiliary tool
;; modes (dune and opam): completion-at-point result construction,
;; dune project-root detection, opam local-switch detection, a
;; per-key memoization helper, and running an external program from a
;; given directory.

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

(defun neocaml-common-capf (start end candidates annotation kind)
  "Build a `completion-at-point' result for CANDIDATES over START..END.
ANNOTATION is appended after each candidate and KIND is the
`:company-kind' symbol.  The result is non-exclusive so other
completion functions still run."
  (list start end candidates
        :annotation-function (lambda (_) (concat " " annotation))
        :company-kind (lambda (_) kind)
        :exclusive 'no))

(defun neocaml-common-dune-project-root ()
  "Return the dune project root, or nil when not in a dune project.
The root is the nearest ancestor directory containing a
`dune-project' file."
  (when-let* ((root (locate-dominating-file default-directory "dune-project")))
    (expand-file-name root)))

(defun neocaml-common-local-switch-p (dir)
  "Non-nil when DIR is a project with a local opam switch (an `_opam').
DIR may be nil, in which case the result is nil."
  (and dir (file-directory-p (expand-file-name "_opam" dir))))

(defconst neocaml-common--cache-missing (make-symbol "neocaml-common--missing")
  "Sentinel distinguishing an absent cache entry from a cached nil.")

(defun neocaml-common-cache-get (table key compute)
  "Return the value for KEY in hash TABLE, computing it on a miss.
COMPUTE is a function of no arguments called only when KEY is
absent; its result (including nil) is cached and returned."
  (let ((cached (gethash key table neocaml-common--cache-missing)))
    (if (eq cached neocaml-common--cache-missing)
        (puthash key (funcall compute) table)
      cached)))

(defun neocaml-common-run (dir program args)
  "Run PROGRAM with ARGS in DIR and return its standard output as a string.
Standard error is discarded.  Return nil when DIR is remote, PROGRAM
is not found, or the command exits non-zero.  DIR may be nil, meaning
`default-directory'."
  (let ((directory (or dir default-directory)))
    (unless (file-remote-p directory)
      (when (executable-find program)
        (let ((default-directory directory))
          (with-temp-buffer
            (when (zerop (apply #'call-process program nil '(t nil) nil args))
              (buffer-string))))))))

(provide 'neocaml-common)

;;; neocaml-common.el ends here
