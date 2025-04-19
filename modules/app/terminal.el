;; -*- lexical-binding: t -*-

;; our interactive vterm shell should be fish
(use-package! vterm
  :defer t
  :custom
  (vterm-shell (executable-find "fish")))
