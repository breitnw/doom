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
  (corfu-preselect 'first) ;; automatically select the first candidate
  :config
  (map! :map corfu-map
        ;; separator for orderless
        "s-s" #'corfu-insert-separator)
  (custom-set-faces!
    `(corfu-default :foreground ,(doom-color 'comments))
    `(corfu-popupinfo :foreground ,(doom-color 'fg))
    `(corfu-border :background ,(doom-color 'comments))))
