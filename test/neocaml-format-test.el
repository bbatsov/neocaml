;;; neocaml-format-test.el --- Formatting tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests for ocamlformat integration in neocaml.

;;; Code:

(require 'neocaml-test-helpers)

(describe "neocaml--ocamlformat-args"
  (it "uses --name when the buffer visits a file"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example.ml")
      (expect (neocaml--ocamlformat-args)
              :to-equal '("--name" "/tmp/example.ml" "-"))))

  (it "falls back to --impl for a fileless implementation buffer"
    (with-temp-buffer
      (when (treesit-language-available-p 'ocaml)
        (neocaml-mode))
      (setq buffer-file-name nil)
      (expect (neocaml--ocamlformat-args)
              :to-equal '("--impl" "-"))))

  (it "falls back to --intf for a fileless interface buffer"
    (with-temp-buffer
      (if (treesit-language-available-p 'ocaml-interface)
          (neocaml-interface-mode)
        (signal 'buttercup-pending "interface grammar not available"))
      (setq buffer-file-name nil)
      (expect (neocaml--ocamlformat-args)
              :to-equal '("--intf" "-")))))

(describe "neocaml-format-buffer"
  (it "keeps point and markers on the same text"
    (with-temp-buffer
      (insert "let x  =  1\nlet y = 2\nlet z = 3\n")
      (goto-char (point-min))
      (forward-line 1)
      (let ((marker (copy-marker (line-beginning-position 2))))
        (neocaml-test-stub-formatter "let x = 1\nlet y = 2\nlet z = 3\n")
        (neocaml-format-buffer)
        (expect (buffer-string) :to-equal "let x = 1\nlet y = 2\nlet z = 3\n")
        (expect (looking-at "let y") :to-be-truthy)
        (expect (buffer-substring marker (+ marker 5)) :to-equal "let z"))))

  (it "leaves a buffer that is already formatted alone"
    (with-temp-buffer
      (insert "let x = 1\n")
      (set-buffer-modified-p nil)
      (let (changed)
        (add-hook 'after-change-functions (lambda (&rest _) (setq changed t))
                  nil t)
        (neocaml-test-stub-formatter "let x = 1\n")
        (neocaml-format-buffer)
        (expect changed :not :to-be-truthy)
        (expect (buffer-modified-p) :not :to-be-truthy))))

  (it "keeps point's position when diffing takes too long"
    (with-temp-buffer
      (insert "let x  =  1\nlet y = 2\n")
      (goto-char 15)
      ;; Past the time limit the text is replaced wholesale, which leaves
      ;; point at the end, and the replacement returns nil.
      (spy-on (if (>= emacs-major-version 31)
                  'replace-region-contents
                'replace-buffer-contents)
              :and-call-fake (lambda (&rest _)
                               (erase-buffer)
                               (insert "let x = 1\nlet y = 2\n")
                               nil))
      (neocaml-test-stub-formatter "let x = 1\nlet y = 2\n")
      (neocaml-format-buffer)
      (expect (buffer-string) :to-equal "let x = 1\nlet y = 2\n")
      (expect (point) :to-equal 15)))

  (it "reports ocamlformat's error and leaves the buffer alone on failure"
    (with-temp-buffer
      (insert "let x =\n")
      (neocaml-test-stub-formatter "" 1 "Error: Syntax error\n")
      (expect (neocaml-format-buffer)
              :to-throw 'user-error
              '("Running ocamlformat failed: Error: Syntax error"))
      (expect (buffer-string) :to-equal "let x =\n"))))

(provide 'neocaml-format-test)

;;; neocaml-format-test.el ends here
