;;; modules/mode/lsp.el -*- lexical-binding: t; -*-

;; global lsp-mode configuration

(use-package! lsp-mode
  :defer t
  :custom
  (lsp-inlay-hint-enable t)
  (lsp-inlay-hint-param-format "%s")
  (lsp-inlay-hint-type-format "%s")
  (lsp-eldoc-render-all t)
  (lsp-enable-suggest-server-download nil)
  :config
  (custom-set-faces!
    `(lsp-inlay-hint-face
      :foreground ,(doom-color 'base6)
      ;; :background ,(doom-color 'bg-alt)
      :weight normal)))

(use-package! eldoc
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package! eldoc-box
  :after eldoc
  :custom
  (eldoc-box-max-pixel-width 400)
  (eldoc-box-max-pixel-height 300)
  :config
  (map! :map 'prog-mode-map
        :n "?" #'eldoc-box-help-at-point)
  (custom-set-faces!
    `(eldoc-box-border :background ,(doom-color 'comments))
    `(eldoc-box-markdown-separator :foreground ,(doom-color 'comments) :strike-through t)
    `(markdown-code-face :background ,(doom-color 'bg))))
