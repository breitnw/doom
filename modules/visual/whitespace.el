;; whitespace rendering
(defvar fill-column-settings
  '((rustic-mode . 100)
    (java-mode . 100)
    (c-mode . 100)
    (c++-mode . 100)
    (python-mode . 100)
    (emacs-lisp-mode . 80)
    (haskell-mode . 80)
    (racket-mode . 80)
    (nix-mode . 80)))

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

;; word wrapping
(use-package adaptive-wrap
  :after whitespace
  :config
  (add-to-list '+word-wrap-text-modes 'org-msg-edit-mode)
  (add-hook! 'prog-mode-hook #'+word-wrap-mode))
