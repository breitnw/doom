;;; modules/visual/whitespace.el -*- lexical-binding: t; -*-

;; whitespace rendering
(defvar fill-column-settings
  '((rust-mode . 80)
    (java-mode . 100)
    (c-mode . 80)
    (c++-mode . 80)
    (python-mode . 100)
    (emacs-lisp-mode . 80)
    (haskell-mode . 80)
    (racket-mode . 80)
    (nix-mode . 80)
    (zig-mode . 80)))

;; whitespace indicators for if we're past the fill column
;; this is helpful in case we have inlay hints!
(use-package! whitespace
  :hook
  (whitespace-mode . display-fill-column-indicator-mode)
  :custom
  (whitespace-style '(face
                      tabs
                      tab-mark
                      ;; ws-butler takes care of trailing whitespace, so we can
                      ;; save the mental strain :)
                      ;; trailing
                      lines-tail
                      space-before-tab
                      indentation
                      empty
                      space-after-tab
                      missing-newline-at_eof))
  :config
  (setq whitespace-global-modes (mapcar #'car fill-column-settings))

  ;; fill column indicator
  (add-hook! 'display-fill-column-indicator-mode-hook
    (setq display-fill-column-indicator-character ?▏)
    (let ((setting (-find (lambda (entry) (derived-mode-p (car entry)))
                          fill-column-settings)))
      ;; this will (hopefully) never be nil, since we only enable
      ;; display-fill-column-indicator-mode when a mode listed in
      ;; fill-column-settings is enabled
      (setq fill-column (cdr setting))))

  ;; customize fill column indicator face to be a little less harsh
  (custom-set-faces!
    `(fill-column-indicator :foreground ,(doom-color 'bg-alt)))

  (global-whitespace-mode))

;; appearance of indent guides
(use-package! indent-bars
  :config
  ;; disable indent guides for lispy languages
  (setq indent-guides-inhibit-modes
        '(racket-mode emacs-lisp-mode))
  (dolist (mode indent-guides-inhibit-modes)
    (add-to-list '+indent-guides-inhibit-functions
                 (lambda () (derived-mode-p mode)))))
