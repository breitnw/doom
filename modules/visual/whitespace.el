;;; modules/visual/whitespace.el -*- lexical-binding: t; -*-

;; whitespace rendering
(defvar fill-column-settings
  '((rustic-mode . 80)
    (java-mode . 100)
    (c-mode . 80)
    (c++-mode . 80)
    (python-mode . 100)
    (emacs-lisp-mode . 80)
    (haskell-mode . 80)
    (racket-mode . 80)
    (nix-mode . 80)
    (zig-mode . 80)))

(defun configure-fill-column ()
  (setq fill-column
        (alist-get major-mode fill-column-settings 80))
  (display-fill-column-indicator-mode)
  (whitespace-mode))

;; set the proper fill column for each language
(dolist (elt fill-column-settings)
  (add-hook (intern (format "%s-hook" (symbol-name (car elt))))
            #'configure-fill-column))

;; whitespace indicators for if we're past the fill column
;; this is helpful in case we have inlay hints!
(use-package! whitespace
  ;; TODO load lazily?
  :defer t
  :custom
  (whitespace-style '(face
                      tabs
                      tab-mark
                      trailing
                      lines-tail
                      space-before-tab
                      indentation
                      empty
                      space-after-tab
                      missing-newline-at_eof))
  :config
  ;; fill column indicator config
  (setq-default display-fill-column-indicator-character ?▏)
  (custom-set-faces!
    `(fill-column-indicator :foreground ,(doom-color 'bg-alt))))
