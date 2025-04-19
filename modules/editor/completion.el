;; -*- lexical-binding: t -*-

(use-package! prescient
  :after corfu
  :config
  (custom-set-faces!
    `(prescient-primary-highlight :foreground ,(doom-color 'fg) :weight bold)
    `(prescient-secondary-highlight :foreground ,(doom-color 'green)))
  (corfu-prescient-mode)
  (prescient-persist-mode))

(use-package! corfu
  :defer t
  :custom
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  :config
  ;; automatically select the first candidate
  ;; need to do this later because Doom sets a default
  (setq corfu-preselect 'first)
  ;; custom keymap for corfu
  (map! :map corfu-map
        ;; separator for orderless
        "C-<SPC>" #'corfu-insert-separator
        ;; prevent stealing the backspace key
        [backspace] nil
        "DEL" nil)
  ;; dim characters that haven't been typed
  (custom-set-faces!
    `(corfu-default :foreground ,(doom-color 'comments))
    `(corfu-popupinfo :foreground ,(doom-color 'fg))
    `(corfu-border :background ,(doom-color 'comments))))
