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

(provide 'neocaml-format-test)

;;; neocaml-format-test.el ends here
