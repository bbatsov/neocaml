;;; neocaml-repl-test.el --- Tests for neocaml-repl -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for `neocaml-repl'.  Covers the pure helpers
;; (prompt regex, input sender, phrase delimiter search) and the
;; buffer-switching plumbing, without spawning an actual OCaml
;; toplevel process.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-repl)
(require 'neocaml-dune)
(require 'neocaml-dune-interaction)
(require 'cl-lib)

(describe "neocaml-repl prompt regex"
  (it "matches the standard ocaml prompt"
    (expect (string-match-p neocaml-repl--prompt-regexp "# ") :to-be-truthy))

  (it "matches the plain utop prompt"
    (expect (string-match-p neocaml-repl--prompt-regexp "utop # ") :to-be-truthy))

  (it "matches numbered utop prompts"
    (expect (string-match-p neocaml-repl--prompt-regexp "utop[0] # ") :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "utop[42] # ") :to-be-truthy))

  (it "anchors at beginning of line"
    (expect (string-match-p neocaml-repl--prompt-regexp "foo # ") :not :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "  # ") :not :to-be-truthy))

  (it "does not match unrelated text"
    (expect (string-match-p neocaml-repl--prompt-regexp "Welcome to OCaml") :not :to-be-truthy)
    (expect (string-match-p neocaml-repl--prompt-regexp "val x : int") :not :to-be-truthy)))

(describe "neocaml-repl font-lock"
  (it "highlights the prompt"
    (let ((neocaml-repl-history-file nil)
          (neocaml-repl-fontify-input nil))
      (with-temp-buffer
        (neocaml-repl-mode)
        (insert (propertize "utop # " 'field 'output))
        (font-lock-ensure)
        (expect '(1 7) :to-have-face 'font-lock-comment-face))))

  (it "highlights errors, uncaught exceptions, warnings and alerts"
    (let ((neocaml-repl-history-file nil)
          (neocaml-repl-fontify-input nil))
      (with-temp-buffer
        (neocaml-repl-mode)
        (insert (propertize "Error: Unbound value x\n# Exception: Not_found.\nWarning 26 [unused-var]: unused variable y.\nAlert deprecated: f\n"
                            'field 'output))
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "Error:" :to-have-face 'font-lock-warning-face)
        (expect "Exception:" :to-have-face 'font-lock-warning-face)
        (expect "Warning 26 [unused-var]:" :to-have-face 'font-lock-warning-face)
        (expect "Alert deprecated:" :to-have-face 'font-lock-warning-face))))

  (it "fontifies the input with tree-sitter"
    (let ((neocaml-repl-history-file nil)
          (neocaml-repl-fontify-input t))
      (with-temp-buffer
        (neocaml-repl-mode)
        (insert (propertize "# " 'field 'output) "let x = 1")
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "let" :to-have-face 'font-lock-keyword-face)))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defmacro neocaml-repl-test-with-output (output &rest body)
  "Run BODY at the start of a REPL buffer showing the toplevel's OUTPUT."
  (declare (indent 1))
  `(let ((neocaml-repl-history-file nil)
         (neocaml-repl-fontify-input nil))
     (with-temp-buffer
       (neocaml-repl-mode)
       (insert (propertize ,output 'field 'output))
       (font-lock-ensure)
       (goto-char (point-min))
       ,@body)))

(describe "neocaml-repl output fontification"
  (it "fontifies the signature and the value of a response"
    (neocaml-repl-test-with-output "# val r : float option = Some 3.14\n# "
      (expect "val" :to-have-face 'font-lock-keyword-face)
      (expect "r" :to-have-face 'font-lock-variable-name-face)
      (expect "float" :to-have-face 'font-lock-builtin-face)
      (expect "option" :to-have-face 'font-lock-builtin-face)
      (expect "Some" :to-have-face 'font-lock-constant-face)
      (expect "3.14" :to-have-face 'font-lock-number-face)))

  (it "fontifies the type of an anonymous value"
    (neocaml-repl-test-with-output "- : string list = [\"a\"]"
      (expect "string" :to-have-face 'font-lock-builtin-face)
      (expect "list" :to-have-face 'font-lock-builtin-face)
      (expect "\"a\"" :to-have-face 'font-lock-string-face)))

  (it "fontifies definitions as an interface would"
    (neocaml-repl-test-with-output "# type 'a t = Leaf | Node of 'a t * 'a t\n# "
      (expect "type" :to-have-face 'font-lock-keyword-face)
      (expect "Leaf" :to-have-face 'font-lock-constant-face)
      (expect "Node" :to-have-face 'font-lock-constant-face)
      (expect "of" :to-have-face 'font-lock-keyword-face)))

  (it "leaves the values the toplevel doesn't show unfontified"
    (neocaml-repl-test-with-output "val f : int -> int = <fun>\nval l : int list = [1; 2; ...]\n- : exn = <unknown constructor>\n"
      (expect "<fun>" :to-have-face nil)
      (expect "2" :to-have-face 'font-lock-number-face)
      (expect "..." :to-have-face nil)
      (expect "<unknown constructor>" :to-have-face nil)))

  (it "skips the = of a package type looking for the value"
    (neocaml-repl-test-with-output "val m : (module S with type t = int) = <module>"
      (expect "int" :to-have-face 'font-lock-builtin-face)
      (expect "<module>" :to-have-face nil)))

  (it "fontifies the indented lines a response goes on over"
    (neocaml-repl-test-with-output "val long_name :\n  int list -> unit = <fun>\nval l : int list =\n  [1;\n   2]\n# "
      (expect "int" :to-have-face 'font-lock-builtin-face)
      (expect "unit" :to-have-face 'font-lock-builtin-face)
      (expect "2" :to-have-face 'font-lock-number-face)))

  (it "fontifies a response whose lines arrive separately"
    (neocaml-repl-test-with-output "val l : int list =\n"
      (goto-char (point-max))
      (insert (propertize "  [1;\n   2]\n# " 'field 'output))
      ;; Fontify only the new text, as jit-lock would.
      (font-lock-fontify-region (line-beginning-position -1) (point-max))
      (goto-char (point-min))
      (expect "2" :to-have-face 'font-lock-number-face)))

  (it "doesn't extend fontification over a response too long to fontify"
    (neocaml-repl-test-with-output
        (apply #'concat "val l : int list =\n"
               (make-list 400 "  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];\n"))
      (goto-char (point-max))
      (let ((font-lock-beg (line-beginning-position 0))
            (font-lock-end (point-max)))
        (expect (neocaml-repl--extend-region) :not :to-be-truthy))))

  (it "fontifies uncaught exceptions"
    (neocaml-repl-test-with-output "# Exception: Failure \"boom\".\n# "
      (expect "Exception:" :to-have-face 'font-lock-warning-face)
      (expect "\"boom\"" :to-have-face 'font-lock-string-face)))

  (it "leaves input alone"
    (let ((neocaml-repl-history-file nil)
          (neocaml-repl-fontify-input nil))
      (with-temp-buffer
        (neocaml-repl-mode)
        (insert (propertize "# " 'field 'output) "exception E of int")
        (font-lock-ensure)
        (goto-char (point-min))
        (expect "E" :to-have-face nil))))

  (it "works along with the tree-sitter fontification of input"
    (let ((neocaml-repl-history-file nil)
          (neocaml-repl-fontify-input t))
      (with-temp-buffer
        (neocaml-repl-mode)
        (insert (propertize "# " 'field 'output))
        ;; The first fontification creates comint's indirect buffer for the
        ;; input, which fontifies the whole buffer as OCaml.
        (font-lock-ensure)
        (insert (propertize "val x : int option = None\n# " 'field 'output) "let y = 1")
        (font-lock-fontify-region (point-min) (point-max))
        (goto-char (point-min))
        (expect "option" :to-have-face 'font-lock-builtin-face)
        (expect "None" :to-have-face 'font-lock-constant-face)
        (expect "let" :to-have-face 'font-lock-keyword-face))))

  (it "leaves output to the basic keywords when disabled"
    (let ((neocaml-repl-fontify-output nil))
      (neocaml-repl-test-with-output "val x : int = 1"
        (expect "val" :to-have-face 'font-lock-keyword-face)
        (expect "int" :to-have-face nil)))))

(describe "neocaml-repl error navigation"
  :var (dir)

  (before-each
    (setq dir (file-name-as-directory (make-temp-file "neocaml-repl-errors" t)))
    (with-temp-file (expand-file-name "bad.ml" dir)
      (insert "let unused_warning () =\n  let unused = 1 in\n  2\n")))

  (after-each
    (neocaml-test-kill-tracked-buffers)
    (delete-directory dir t))

  (cl-flet ((transcript (output)
              (concat "OCaml version 5.4.0\nEnter #help;; for help.\n\n" output))
            (messages ()
              ;; (TYPE FILE LINE COLUMN) of each message.  Font-lock has
              ;; parsed them, as it does in a displayed buffer, a chunk at a
              ;; time; `compilation-next-error' parses only a line past point,
              ;; which would cut a warning off from its "Warning".
              (goto-char (point-min))
              (let ((messages nil)
                    (match nil))
                (while (setq match (text-property-search-forward 'compilation-message))
                  (let* ((msg (prop-match-value match))
                         (loc (compilation--message->loc msg)))
                    (push (list (compilation--message->type msg)
                                (caar (compilation--loc->file-struct loc))
                                (compilation--loc->line loc)
                                (compilation--loc->col loc))
                          messages)))
                (nreverse messages))))

    (it "finds the errors, warnings and backtraces the toplevel reports in files"
      (let ((default-directory dir))
        (neocaml-repl-test-with-output
            (transcript (concat "# File \"bad.ml\", line 2, characters 6-12:\n"
                                "2 |   let unused = 1 in\n"
                                "          ^^^^^^\n"
                                "Warning 26 [unused-var]: unused variable unused.\n\n"
                                "val unused_warning : unit -> int = <fun>\n\n"
                                "File \"bad.ml\", line 9, characters 3-8:\n"
                                "Error: This constant has type string but an expression was expected of type\n"
                                "         int\n"
                                "# File \"sigerr.ml\", lines 3-5, characters 6-3:\n"
                                "Error: Signature mismatch:\n"
                                "       File \"sigerr.ml\", line 2, characters 2-13: Expected declaration\n"
                                "# Exception: Failure \"nth\".\n"
                                "Raised at Stdlib.failwith in file \"stdlib.ml\", line 29, characters 17-33\n"
                                "Called from <unknown> in file \"//toplevel//\", line 1, characters 0-13\n"
                                "# "))
          (expect (messages)
                  :to-equal '((1 "bad.ml" 2 7)
                              (2 "bad.ml" 9 4)
                              (2 "sigerr.ml" 3 7)
                              (0 "sigerr.ml" 2 3)
                              (2 "stdlib.ml" 29 18))))))

    (it "leaves alone the locations in phrases typed or sent to the toplevel"
      (neocaml-repl-test-with-output
          (transcript (concat "# Line 1, characters 12-15:\n"
                              "1 | let x = 1 + \"a\";;\n"
                              "                ^^^\n"
                              "Error: This constant has type string but an expression was expected of type\n"
                              "         int\n"
                              "# "))
        (expect (messages) :to-equal nil)))

    (it "visits the location of a message"
      (let ((default-directory dir))
        (neocaml-repl-test-with-output
            (transcript (concat "# File \"bad.ml\", line 2, characters 6-12:\n"
                                "Warning 26 [unused-var]: unused variable unused.\n# "))
          (compilation-next-error 1)
          (compile-goto-error)
          (neocaml-test-track-buffer (current-buffer))
          (expect (buffer-file-name) :to-equal (expand-file-name "bad.ml" dir))
          (expect (line-number-at-pos) :to-equal 2)
          (expect (current-column) :to-equal 6))))))

(describe "neocaml-repl input sender"
  ;; Spy on `comint-send-string' to capture what would be sent to the
  ;; toplevel without a real process.
  (before-each (spy-on 'comint-send-string))

  (cl-flet ((send (input)
              (neocaml-repl--input-sender 'fake-proc input)
              (nth 1 (spy-context-args
                      (spy-calls-most-recent 'comint-send-string)))))

    (it "appends `;;' and a newline when input is unterminated"
      (expect (send "let x = 1") :to-equal "let x = 1\n;;\n"))

    (it "leaves input alone when it already ends with `;;'"
      (expect (send "let x = 1;;") :to-equal "let x = 1;;\n"))

    (it "treats trailing whitespace as still terminated"
      (expect (send "let x = 1;;   ") :to-equal "let x = 1;;   \n")
      (expect (send "let x = 1;;\n") :to-equal "let x = 1;;\n\n"))

    (it "does not get fooled by `;;' inside a string"
      ;; The trailing-`;;' check is purely textual; if the literal
      ;; last two chars aren't `;;' we must terminate.
      (expect (send "let s = \";;\"") :to-equal "let s = \";;\"\n;;\n"))))

(describe "neocaml-repl phrase delimiter search"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "finds a top-level `;;' delimiter"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        (expect pos :not :to-be nil)
        ;; Forward search returns the position past the delimiter.
        (expect (buffer-substring-no-properties (- pos 2) pos) :to-equal ";;"))))

  (it "skips `;;' inside a string literal"
    (with-neocaml-buffer "let s = \"a;;b\";;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        ;; The first real delimiter is the one after the string.
        (expect pos :not :to-be nil)
        (expect (char-before pos) :to-equal ?\;)
        (expect (char-before (1- pos)) :to-equal ?\;)
        ;; And it should sit past the closing quote, not inside the string.
        (expect (> pos (1+ (string-search "\"" (buffer-string)))) :to-be-truthy))))

  (it "skips `;;' inside a block comment"
    (with-neocaml-buffer "let x = (* ;; *) 1;;\nlet y = 2"
      (goto-char (point-min))
      (let ((pos (neocaml-repl--search-phrase-delimiter 1)))
        (expect pos :not :to-be nil)
        ;; The found `;;' must come after the closing `*)' of the comment.
        (expect (> pos (string-search "*)" (buffer-string))) :to-be-truthy))))

  (it "returns nil when no delimiter exists"
    (with-neocaml-buffer "let x = 1"
      (goto-char (point-min))
      (expect (neocaml-repl--search-phrase-delimiter 1) :to-be nil)))

  (it "searches backward when direction is negative"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2"
      (goto-char (point-max))
      (let ((pos (neocaml-repl--search-phrase-delimiter -1)))
        (expect pos :not :to-be nil)
        ;; Backward search returns the position before the delimiter.
        (expect (buffer-substring-no-properties pos (+ pos 2)) :to-equal ";;")))))

(describe "neocaml-repl source-to-repl switch"
  (after-each (neocaml-test-kill-tracked-buffers))

  (it "pops to an existing REPL buffer and records the source"
    (let ((source (neocaml-test-track-buffer
                   (generate-new-buffer "*neocaml-repl-test-source*")))
          (repl (neocaml-test-track-buffer
                 (get-buffer-create neocaml-repl-buffer-name))))
      (with-current-buffer source
        ;; Existing REPL has the same flavor as requested, so no restart prompt.
        (with-current-buffer repl (setq neocaml-repl--flavor neocaml-repl-flavor))
        (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
        (spy-on 'comint-check-proc :and-return-value t)
        (spy-on 'pop-to-buffer)
        (neocaml-repl-switch-to-repl)
        (expect 'pop-to-buffer :to-have-been-called-with neocaml-repl-buffer-name)
        (expect (buffer-local-value 'neocaml-repl--source-buffer repl)
                :to-equal source))))

  (it "starts a new REPL when none is running and records the source"
    (let ((source (neocaml-test-track-buffer
                   (generate-new-buffer "*neocaml-repl-test-source*")))
          (repl (neocaml-test-track-buffer
                 (get-buffer-create neocaml-repl-buffer-name))))
      (with-current-buffer source
        (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
        (spy-on 'comint-check-proc :and-return-value nil)
        (spy-on 'neocaml-repl-start)
        (neocaml-repl-switch-to-repl)
        (expect 'neocaml-repl-start :to-have-been-called)
        (expect (buffer-local-value 'neocaml-repl--source-buffer repl)
                :to-equal source))))

  (it "offers to restart the REPL when the requested flavor differs"
    (let ((source (neocaml-test-track-buffer
                   (generate-new-buffer "*neocaml-repl-test-source*")))
          (repl (neocaml-test-track-buffer
                 (get-buffer-create neocaml-repl-buffer-name))))
      (with-current-buffer source
        (with-current-buffer repl (setq neocaml-repl--flavor 'ocaml))
        (let ((neocaml-repl-flavor 'utop))
          (spy-on 'neocaml-repl--buffer :and-return-value neocaml-repl-buffer-name)
          (spy-on 'comint-check-proc :and-return-value t)
          (spy-on 'y-or-n-p :and-return-value t)
          (spy-on 'neocaml-repl--kill)
          (spy-on 'neocaml-repl-start)
          (neocaml-repl-switch-to-repl)
          (expect 'neocaml-repl--kill :to-have-been-called)
          (expect 'neocaml-repl-start :to-have-been-called))))))

(describe "neocaml-repl buffer switching"
  (after-each (neocaml-test-kill-tracked-buffers))

  (it "pops to the saved source buffer"
    (let ((source (neocaml-test-track-buffer
                   (generate-new-buffer "*neocaml-repl-test-source*")))
          (repl (neocaml-test-track-buffer
                 (generate-new-buffer "*neocaml-repl-test-repl*"))))
      (with-current-buffer repl
        (setq-local neocaml-repl--source-buffer source)
        (spy-on 'pop-to-buffer)
        (neocaml-repl-switch-to-source)
        (expect 'pop-to-buffer :to-have-been-called-with source))))

  (it "messages when no source buffer is set"
    (let ((repl (neocaml-test-track-buffer
                 (generate-new-buffer "*neocaml-repl-test-repl*"))))
      (with-current-buffer repl
        (setq-local neocaml-repl--source-buffer nil)
        (spy-on 'message)
        (neocaml-repl-switch-to-source)
        (expect 'message :to-have-been-called-with "No source buffer to return to"))))

  (it "messages when the saved source buffer is dead"
    (let ((source (neocaml-test-track-buffer
                   (generate-new-buffer "*neocaml-repl-test-source*")))
          (repl (neocaml-test-track-buffer
                 (generate-new-buffer "*neocaml-repl-test-repl*"))))
      (with-current-buffer repl
        (setq-local neocaml-repl--source-buffer source)
        (kill-buffer source)
        (spy-on 'message)
        (neocaml-repl-switch-to-source)
        (expect 'message :to-have-been-called-with "No source buffer to return to")))))

(describe "neocaml-repl--start-command"
  (after-each (neocaml-test-kill-tracked-buffers))

  (it "records the flavor and command and sets the mode line"
    (spy-on 'make-comint-in-buffer :and-call-fake
            (lambda (_name buffer &rest _) (get-buffer-create buffer)))
    (spy-on 'neocaml-repl-mode)
    (let* ((dir default-directory)
           (buf (neocaml-test-track-buffer
                 (neocaml-repl--start-command "*neocaml-repl-startcmd-test*"
                                              (list dir "utop") 'utop))))
      (with-current-buffer buf
        (expect neocaml-repl--flavor :to-equal 'utop)
        ;; The directory it ran in is recorded too, for restarts.
        (expect neocaml-repl--command-line :to-equal (list dir "utop"))
        (expect mode-name :to-equal "OCaml-REPL[utop]"))))

  (it "runs the toplevel in the directory the command names"
    (let (started-in)
      (spy-on 'make-comint-in-buffer :and-call-fake
              (lambda (&rest _) (setq started-in default-directory)))
      (spy-on 'neocaml-repl-mode)
      (neocaml-test-track-buffer
       (neocaml-repl--start-command "*neocaml-repl-startcmd-test*"
                                    '("/tmp/proj/" "dune" "utop" ".") 'dune-utop))
      (expect started-in :to-equal "/tmp/proj/")
      (expect (nthcdr 2 (spy-calls-args-for 'make-comint-in-buffer 0))
              :to-equal '("dune" nil "utop" ".")))))

(describe "neocaml-repl restart"
  (after-each (neocaml-test-kill-tracked-buffers))

  (it "relaunches with the recorded flavor and command"
    (let ((repl (neocaml-test-track-buffer
                 (get-buffer-create "*neocaml-repl-restart-test*"))))
      (with-current-buffer repl
        (setq-local neocaml-repl--flavor 'utop)
        (setq-local neocaml-repl--command-line '("/tmp/" "utop")))
      (spy-on 'neocaml-repl--buffer :and-return-value "*neocaml-repl-restart-test*")
      (spy-on 'neocaml-repl--kill)
      (spy-on 'neocaml-repl--start-command)
      (neocaml-repl-restart)
      (expect 'neocaml-repl--kill
              :to-have-been-called-with "*neocaml-repl-restart-test*")
      (expect 'neocaml-repl--start-command
              :to-have-been-called-with "*neocaml-repl-restart-test*"
              '("/tmp/" "utop") 'utop)))

  (it "falls back to a fresh start when nothing was recorded"
    (spy-on 'neocaml-repl--buffer :and-return-value "*neocaml-repl-absent*")
    (spy-on 'neocaml-repl--kill)
    (spy-on 'neocaml-repl-start)
    (neocaml-repl-restart)
    (expect 'neocaml-repl-start :to-have-been-called)))

(describe "neocaml-repl send-phrase-and-step"
  (before-all
    (unless (treesit-language-available-p 'ocaml)
      (signal 'buttercup-pending "tree-sitter OCaml grammar not available")))

  (it "sends the current phrase and moves to the next one"
    (with-neocaml-buffer "let x = 1;;\nlet y = 2;;\n"
      (goto-char (point-min))
      (let (sent)
        (spy-on 'neocaml-repl-send-region :and-call-fake
                (lambda (s e) (setq sent (string-trim
                                          (buffer-substring-no-properties s e)))))
        (neocaml-repl-send-phrase-and-step)
        (expect sent :to-match "let x = 1")
        ;; point should now sit at the start of the second phrase
        (expect (looking-at "let y") :to-be-truthy)))))

(describe "neocaml-repl--project-id"
  :var (mono)
  (before-each
    (setq mono (file-name-as-directory (make-temp-file "mono" t)))
    (make-directory (expand-file-name "pkgs/a/lib/" mono) t)
    (write-region "" nil (expand-file-name "pkgs/a/dune-project" mono) nil 'silent))
  (after-each (delete-directory mono t))

  (cl-flet ((id-in (dir &optional project-root)
              (let ((default-directory (expand-file-name dir mono)))
                (spy-on 'project-current
                        :and-return-value (and project-root
                                               (cons 'transient project-root)))
                (neocaml-repl--project-id))))

    (it "adds the path of a dune project nested below the project.el root"
      (expect (id-in "pkgs/a/lib/" mono)
              :to-equal (concat (file-name-nondirectory (directory-file-name mono))
                                "/pkgs/a")))

    (it "uses the project name when the dune project is the project root"
      (write-region "" nil (expand-file-name "dune-project" mono) nil 'silent)
      (expect (id-in "./" mono)
              :to-equal (file-name-nondirectory (directory-file-name mono))))

    (it "uses the project name outside any dune project"
      (expect (id-in "pkgs/" mono)
              :to-equal (file-name-nondirectory (directory-file-name mono))))

    (it "falls back to the dune project's name without a project.el project"
      (expect (id-in "pkgs/a/lib/") :to-equal "a"))

    (it "is nil outside both"
      (expect (id-in "pkgs/") :to-be nil))

    (it "adds the path when the roots are spelled differently"
      ;; Reach the dune project through a symlink to the project root.
      (let ((link (concat (directory-file-name mono) "-link")))
        (make-symbolic-link (directory-file-name mono) link)
        (unwind-protect
            (let ((default-directory (expand-file-name "pkgs/a/lib/" link)))
              (spy-on 'project-current :and-return-value (cons 'transient mono))
              (expect (neocaml-repl--project-id)
                      :to-equal (concat (file-name-nondirectory
                                         (directory-file-name mono))
                                        "/pkgs/a")))
          (delete-file link))))

    (it "names the REPL buffer after the nested dune project"
      (spy-on 'project-current :and-return-value (cons 'transient mono))
      (let ((neocaml-repl-buffer-name "*OCaml*")
            (default-directory (expand-file-name "pkgs/a/lib/" mono)))
        (expect (neocaml-repl--buffer)
                :to-equal (format "*OCaml: %s/pkgs/a*"
                                  (file-name-nondirectory
                                   (directory-file-name mono))))))))

(describe "neocaml-repl per-project buffer name"
  (it "derives a per-project name from the base name"
    (spy-on 'neocaml-repl--project-id :and-return-value "myproj")
    (let ((neocaml-repl-buffer-name "*OCaml*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*OCaml: myproj*"))))

  (it "falls back to the base name outside a project"
    (spy-on 'neocaml-repl--project-id :and-return-value nil)
    (let ((neocaml-repl-buffer-name "*OCaml*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*OCaml*"))))

  (it "honors a customized base name"
    (spy-on 'neocaml-repl--project-id :and-return-value "p")
    (let ((neocaml-repl-buffer-name "*ocaml-repl*"))
      (with-temp-buffer
        (expect (neocaml-repl--buffer) :to-equal "*ocaml-repl: p*"))))

  (it "targets the current buffer when inside a REPL buffer"
    (with-temp-buffer
      (rename-buffer "*OCaml: other*" t)
      (setq major-mode 'neocaml-repl-mode)
      (expect (neocaml-repl--buffer) :to-equal (buffer-name)))))

(describe "neocaml-repl require"
  (it "sends a #require directive for the given package"
    (let (sent)
      (spy-on 'neocaml-repl--ensure-repl-running)
      (spy-on 'neocaml-repl--process :and-return-value 'proc)
      (spy-on 'neocaml-repl--input-sender :and-call-fake
              (lambda (_proc input) (setq sent input)))
      (neocaml-repl-require "lwt")
      (expect sent :to-equal "#require \"lwt\""))))

(describe "neocaml-repl--read-package"
  (it "completes over the findlib packages of the current project"
    (spy-on 'neocaml-common-dune-project-root :and-return-value "/proj/")
    (spy-on 'neocaml-dune--ocamlfind-libraries
            :and-return-value '("lwt" "lwt.unix"))
    (spy-on 'completing-read :and-return-value "lwt.unix")
    (expect (neocaml-repl--read-package) :to-equal "lwt.unix")
    (expect 'neocaml-dune--ocamlfind-libraries :to-have-been-called-with "/proj/")
    ;; No REQUIRE-MATCH, so a package ocamlfind doesn't list can be entered.
    (expect (spy-calls-args-for 'completing-read 0)
            :to-equal '("Require package: " ("lwt" "lwt.unix")))))

(describe "neocaml-repl--command"
  :var (root lib)
  (before-each
    (setq root (file-name-as-directory (make-temp-file "neocaml-repl-proj" t))
          lib (expand-file-name "lib/" root))
    (write-region "" nil (expand-file-name "dune-project" root) nil 'silent)
    (make-directory lib))
  (after-each
    (neocaml-test-kill-tracked-buffers)
    (delete-directory root t))

  (it "uses the program vars for the ocaml flavor"
    (let ((neocaml-repl-flavor 'ocaml)
          (neocaml-repl-program-name "ocaml")
          (neocaml-repl-program-args '("-nopromptcont")))
      (expect (neocaml-repl--command)
              :to-equal (list default-directory "ocaml" "-nopromptcont"))))

  (it "uses utop for the utop flavor"
    (let ((neocaml-repl-flavor 'utop))
      (expect (neocaml-repl--command) :to-equal (list default-directory "utop"))))

  (it "runs dune utop from the project root for the dune-utop flavor"
    (let ((neocaml-repl-flavor 'dune-utop)
          (neocaml-dune-program "dune")
          (neocaml-dune-use-opam-exec nil)
          (default-directory lib))
      (expect (neocaml-repl--command) :to-equal (list root "dune" "utop" "."))))

  (it "honors the dune settings for the dune-utop flavor"
    (let ((neocaml-repl-flavor 'dune-utop)
          (neocaml-dune-program "/opt/dune")
          (neocaml-dune-use-opam-exec t)
          (default-directory lib))
      (expect (neocaml-repl--command)
              :to-equal (list root "opam" "exec" "--" "/opt/dune" "utop" "."))))

  (it "starts dune utop for a dune-utop flavor set in .dir-locals.el"
    (let (started-in)
      (spy-on 'make-comint-in-buffer
              :and-call-fake (lambda (&rest _) (setq started-in default-directory)))
      (spy-on 'pop-to-buffer)
      (let ((neocaml-repl-history-file nil)
            (neocaml-repl-fontify-input nil)
            (neocaml-dune-program "dune")
            (neocaml-dune-use-opam-exec nil))
        (with-temp-buffer
          (setq default-directory lib)
          ;; What a directory-local `neocaml-repl-flavor' amounts to.
          (setq-local neocaml-repl-flavor 'dune-utop)
          (neocaml-test-track-buffer (get-buffer-create (neocaml-repl--buffer)))
          (neocaml-repl-start)))
      (expect started-in :to-equal root)
      (expect (nthcdr 2 (spy-calls-args-for 'make-comint-in-buffer 0))
              :to-equal '("dune" nil "utop" "."))))

  (it "fails outside a dune project before creating a REPL buffer"
    (let* ((outside (file-name-as-directory (make-temp-file "neocaml-repl-none" t)))
           (default-directory outside)
           (neocaml-repl-flavor 'dune-utop)
           (neocaml-repl-buffer-name "*neocaml-repl-outside*"))
      (unwind-protect
          (progn
            (spy-on 'neocaml-repl--project-id :and-return-value nil)
            (expect (neocaml-repl-start) :to-throw 'error)
            (expect (get-buffer "*neocaml-repl-outside*") :to-be nil))
        (delete-directory outside t)))))

;;; neocaml-repl-test.el ends here
