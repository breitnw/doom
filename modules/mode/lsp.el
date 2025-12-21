;;; modules/mode/lsp.el -*- lexical-binding: t; -*-

;; global eglot configuration

;; NOTE It might also be necessary to set LSP_USE_PLISTS in the terminal
;; before installing Doom, I had some issues with this
(setenv "LSP_USE_PLISTS" "true")

(use-package! eglot
  :custom
  (eglot-code-action-indications '()))

(use-package! breadcrumb
  :config
  (custom-set-faces!
    `(header-line
      :inherit mode-line
      :background ,(doom-color 'bg)
      :box (:line-width 6 :color ,(doom-color 'bg)))
    `(breadcrumb-project-leaf-face
      :background ,(doom-color 'bg)))
  (breadcrumb-mode))

(use-package! eldoc
  :defer t
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-use-multiline-p nil))

(use-package! eldoc-box
  :after eldoc
  :custom
  (eldoc-box-max-pixel-width 500)
  (eldoc-box-max-pixel-height 300)
  :config
  (map! :map 'prog-mode-map
        :n "?" #'eldoc-box-help-at-point)
  (custom-set-faces!
    `(eldoc-box-border :background ,(doom-color 'comments))
    `(eldoc-box-markdown-separator :foreground ,(doom-color 'comments) :strike-through t)
    `(markdown-code-face :background ,(doom-color 'bg))))
