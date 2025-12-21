;;; modules/editor/completion.el -*- lexical-binding: t; -*-

(use-package! prescient
  :after corfu
  :config
  (custom-set-faces!
    `(prescient-primary-highlight :foreground ,(doom-color 'fg) :weight bold)
    `(prescient-secondary-highlight :foreground ,(doom-color 'green)))
  (corfu-prescient-mode)
  (prescient-persist-mode))

(use-package! yasnippet
  :custom
  (map! :map yas-keymap
        "C-e" nil
        "C-u" #'+snippets/goto-end-of-field))

;; TODO for some reason, <tab> is prioritized for corfu and <backtab> is
;; prioritized for yasnippet, which is extremely confusing
(use-package! corfu
  :defer t
  :custom
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  :config
  (setq corfu-max-width 80)
  ;; automatically select the first candidate
  ;; need to do this later because Doom sets a default
  (setq corfu-preselect 'first)
  (setq corfu-preview-current nil)
  ;; prefer navigating snippets with tab
  (setq +corfu-want-tab-prefer-navigating-snippets t)
  ;; custom keymap for corfu
  (map! :map corfu-map
        ;; separator for orderless
        "C-<SPC>" #'corfu-insert-separator
        ;; move through options with n and e
        :i "C-n" #'corfu-next
        :i "C-e" #'corfu-previous
        ;; don't fall back immediately to normal mode on escape
        ;; :i "<escape>" #'corfu-quit
        )
  ;; dim characters that haven't been typed
  (custom-set-faces!
    `(corfu-default :foreground ,(doom-color 'comments))
    `(corfu-popupinfo :foreground ,(doom-color 'fg))
    `(corfu-border :background ,(doom-color 'comments))))

(use-package! yasnippet
  :config)
