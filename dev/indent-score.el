;;; indent-score.el --- Score neocaml's indentation against formatted code -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: `eldev emacs --batch -l dev/indent-score.el STYLE FILE...'
;;
;; For every non-blank line of each FILE outside comments and strings,
;; works out where neocaml would indent the line in STYLE (ocp-indent,
;; conventional or janestreet), given the rest of the file as it is,
;; and compares that with where the line is.  Nothing gets reindented.
;; Prints the lines neocaml would move, and the gaps, the lines only
;; the last resort rule covers (which keeps any line where it is, so it
;; would count them as right), then a summary.  dev/indent-corpus.sh
;; builds a corpus of files formatted with ocamlformat's profiles to
;; run it on.

;;; Code:

(require 'neocaml)

;; Lots of garbage on a big corpus
(setq gc-cons-threshold (* 64 1024 1024))

(defun neocaml-score--line ()
  "Return the status of the line at point, and where it is and would go.
The status is `ok', `wrong', `gap' (no rule but the last resort covers
it), `error' (a rule failed), or nil for a line in a comment or a
string, as it stays there."
  (back-to-indentation)
  (let* ((bol (point))
         (actual (current-column))
         (token (treesit-node-at bol))
         (node (treesit--indent-largest-node-at bol))
         (string (neocaml--string-line node (if node (treesit-node-parent node) token) bol)))
    (unless (if string
                (not (cadr string))
              (or (equal (treesit-node-type token) "comment")
                  (< (treesit-node-start token) bol)))
      (condition-case err
          (let* ((result (treesit--indent-1))
                 (want (and (car result)
                            (+ (save-excursion (goto-char (car result)) (current-column))
                               (cdr result)))))
            (list (cond ((null want) 'gap)
                        ((= want actual) 'ok)
                        (t 'wrong))
                  want actual node))
        (error (list 'error err actual node))))))

(let ((style (intern (pop command-line-args-left)))
      (counts (list (cons 'ok 0) (cons 'wrong 0) (cons 'gap 0) (cons 'error 0))))
  (setq-default neocaml-indent-style style)
  (dolist (file command-line-args-left)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((inhibit-message t))
        (if (string-suffix-p ".mli" file) (neocaml-interface-mode) (neocaml-mode)))
      ;; Without the last resort, a line no rule covers has no anchor
      (setq-local treesit-simple-indent-rules
                  (neocaml--without-last-resort-rule treesit-simple-indent-rules))
      (goto-char (point-min))
      (while (not (eobp))
        (unless (looking-at-p "[ \t]*$")
          (pcase-let ((`(,status ,want ,actual ,node) (save-excursion (neocaml-score--line))))
            (when status
              (cl-incf (alist-get status counts))
              (unless (eq status 'ok)
                (princ (format "%s:%d: %s: %s\n"
                               file (line-number-at-pos)
                               (pcase status
                                 ('wrong (format "%d instead of %d" want actual))
                                 ('error (format "error %S" want))
                                 (_ (format "gap in %s<%s" (treesit-node-type node)
                                            (treesit-node-type (treesit-node-parent node)))))
                               (string-trim (buffer-substring (point) (line-end-position)))))))))
        (forward-line 1))))
  (let ((total (apply #'+ (mapcar #'cdr counts))))
    (princ (format "%d lines in the %s style: %d ok (%.1f%%), %d wrong, %d gaps%s\n"
                   total style (alist-get 'ok counts)
                   (if (zerop total) 0 (/ (* 100.0 (alist-get 'ok counts)) total))
                   (alist-get 'wrong counts) (alist-get 'gap counts)
                   (if (zerop (alist-get 'error counts)) ""
                     (format ", %d errors" (alist-get 'error counts))))))
  (setq command-line-args-left nil))

;;; indent-score.el ends here
