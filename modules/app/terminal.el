;;; modules/app/terminal.el -*- lexical-binding: t; -*-

;; our interactive vterm shell should be zsh
;; it works a whole lot better than fish...
(use-package! vterm
  :defer t
  :custom
  (vterm-shell (executable-find "zsh")))
