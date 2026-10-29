;;; neocaml-keybindings-test.el --- Keybinding clash tests for neocaml -*- lexical-binding: t; -*-

;; Copyright © 2025-2026 Bozhidar Batsov

;;; Commentary:

;; Buttercup tests that neocaml's keymaps don't step on each other, on
;; the minor modes people run alongside neocaml (ocaml-eglot and merlin,
;; both test dependencies), or on the keys Emacs reserves for users.

;;; Code:

(require 'neocaml-test-helpers)
(require 'neocaml-repl)
(require 'neocaml-utop)
(require 'neocaml-dune)
(require 'neocaml-dune-interaction)
(require 'neocaml-opam)
(require 'neocaml-cram)
(require 'neocaml-ocamllex)
(require 'neocaml-menhir)
(require 'neocaml-odoc)
(require 'neocaml-mlx)
(require 'neocaml-objinfo)
(require 'ocaml-eglot)
(require 'merlin)

(defun neocaml-keys-test--bindings (map &optional prefix)
  "Return the key sequences MAP itself binds to commands, below PREFIX.
Parent keymaps are left out, and so are menu, tool bar and remapping
entries."
  (let (seqs)
    (map-keymap-internal
     (lambda (event binding)
       (let ((seq (vconcat prefix (vector event))))
         (cond
          ((memq event '(menu-bar tool-bar remap)))
          ((keymapp binding)
           (setq seqs (append (neocaml-keys-test--bindings binding seq) seqs)))
          (binding (push seq seqs)))))
     map)
    seqs))

(defun neocaml-keys-test--descriptions (map)
  "Return the keys MAP binds, as key descriptions."
  (mapcar #'key-description (neocaml-keys-test--bindings map)))

(defun neocaml-keys-test--clashes (map other)
  "Return the keys MAP binds that OTHER binds too, as key descriptions.
That includes a key one map binds and the other uses as a prefix, since
either way one of the two commands becomes unreachable."
  (sort (mapcar #'key-description
                (seq-filter
                 (lambda (seq)
                   (let ((found (lookup-key other seq)))
                     (if (numberp found)
                         ;; The first FOUND events don't form a prefix in
                         ;; OTHER: either they're unbound, or a command.
                         (lookup-key other (seq-take seq found))
                       found)))
                 (neocaml-keys-test--bindings map)))
        #'string<))

(defun neocaml-keys-test--keymaps ()
  "Return the names of all of neocaml's keymaps."
  (seq-filter (lambda (sym)
                (and (boundp sym) (keymapp (symbol-value sym))))
              (apropos-internal "\\`neocaml-.*-map\\'")))

(describe "neocaml keymaps"
  (it "include every mode's keymap"
    (expect (neocaml-keys-test--keymaps) :to-contain 'neocaml-repl-minor-mode-map)
    (expect (neocaml-keys-test--keymaps) :to-contain 'neocaml-cram-mode-map))

  ;; C-c followed by a letter, and F5 to F9, are reserved for users.
  (dolist (name (neocaml-keys-test--keymaps))
    (it (format "leave the keys reserved for users alone in %s" name)
      (expect (seq-filter (lambda (key)
                            (string-match-p "\\`\\(?:C-c [A-Za-z]\\|<f[5-9]>\\)\\(?: \\|\\'\\)"
                                            key))
                          (neocaml-keys-test--descriptions (symbol-value name)))
              :to-equal nil)))

  ;; The REPL backends share their keys on purpose, since a buffer uses one
  ;; or the other, but either can be enabled together with the dune mode.
  (dolist (name '(neocaml-repl-minor-mode-map neocaml-utop-minor-mode-map))
    (it (format "let the dune commands live next to %s" name)
      (expect (neocaml-keys-test--clashes neocaml-dune-interaction-mode-map
                                          (symbol-value name))
              :to-equal nil)))

  (it "give both REPL backends the same keys"
    (expect (neocaml-keys-test--descriptions neocaml-utop-minor-mode-map)
            :to-have-same-items-as
            (neocaml-keys-test--descriptions neocaml-repl-minor-mode-map))))

(defconst neocaml-keys-test--companions
  '((ocaml-eglot-mode-map ("C-c C-a"))
    (merlin-mode-map ()))
  "Keymaps of minor modes people run alongside neocaml, as (KEYMAP SHARED).
SHARED lists the keys our major modes are allowed to share with KEYMAP:
ocaml-eglot binds C-c C-a to its own command for switching between the
.ml and .mli files, which is what we bind it to as well.")

(dolist (companion neocaml-keys-test--companions)
  (pcase-let ((`(,keymap ,shared) companion))
    (describe (format "neocaml keymaps next to %s" keymap)
      (dolist (name '(neocaml-repl-minor-mode-map
                      neocaml-utop-minor-mode-map
                      neocaml-dune-interaction-mode-map))
        (it (format "don't clash in %s" name)
          (expect (neocaml-keys-test--clashes (symbol-value name)
                                              (symbol-value keymap))
                  :to-equal nil)))

      ;; The source modes inherit their keys from the base mode's map.
      (it "only share the intended keys in the major modes"
        (expect (neocaml-keys-test--clashes neocaml-base-mode-map
                                            (symbol-value keymap))
                :to-equal shared)))))

(provide 'neocaml-keybindings-test)

;;; neocaml-keybindings-test.el ends here
