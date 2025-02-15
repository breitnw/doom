;; global lsp-mode configuration

;; inlay hints
(add-hook! 'lsp-mode-hook
  (setq-local lsp-inlay-hint-enable t
              lsp-inlay-hint-param-format "%s"
              lsp-inlay-hint-type-format "%s"))
;;lsp-ui-sideline-diagnostic-max-lines 3))
