;; our interactive vterm shell should be fish
(after! vterm
  (setq vterm-shell (executable-find "fish")))
