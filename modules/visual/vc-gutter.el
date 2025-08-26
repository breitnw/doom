;;; modules/visual/vc-gutter.el -*- lexical-binding: t; -*-

;; reload git gutter faces after theme reload, because things can get weird
(use-package! diff-hl
  :config
  (add-hook! 'doom-load-theme-hook
    (when (functionp #'+vc-gutter-make-diff-hl-faces-transparent-h)
      (message "reloaded vc-gutter faces!")
      (+vc-gutter-make-diff-hl-faces-transparent-h))))
