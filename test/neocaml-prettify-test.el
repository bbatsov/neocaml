;;; neocaml-prettify-test.el --- Tests for prettify-symbols setup -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Tests for `neocaml-prettify-symbols-alist' and the full/extra toggle.

;;; Code:

(require 'buttercup)
(require 'neocaml)

(describe "prettify-symbols"
  (it "includes base symbols by default"
    (with-temp-buffer
      (neocaml-mode)
      (expect (assoc "=>" prettify-symbols-alist) :not :to-be nil)
      (expect (assoc "&&" prettify-symbols-alist) :not :to-be nil)))

  (it "excludes extra symbols by default"
    (with-temp-buffer
      (let ((neocaml-prettify-symbols-full nil))
        (neocaml-mode)
        (expect (assoc "fun" prettify-symbols-alist) :to-be nil)
        (expect (assoc "->" prettify-symbols-alist) :to-be nil))))

  (it "includes extra symbols when full is enabled"
    (with-temp-buffer
      (let ((neocaml-prettify-symbols-full t))
        (neocaml-mode)
        (expect (assoc "fun" prettify-symbols-alist) :not :to-be nil)
        (expect (assoc "->" prettify-symbols-alist) :not :to-be nil)
        (expect (assoc "not" prettify-symbols-alist) :not :to-be nil)
        ;; base symbols still present
        (expect (assoc "=>" prettify-symbols-alist) :not :to-be nil)))))

;;; neocaml-prettify-test.el ends here
