;; KEYMAPS ---------------------------------------------------------------------

;; load evil-colemak-basics and modify keybinds
(use-package! evil-colemak-basics
  :after evil evil-snipe
  :init
  ;; needs to be set before the package is loaded
  (setq evil-colemak-basics-rotate-t-f-j t
        evil-respect-visual-line-mode t
        evil-colemak-basics-char-jump-commands 'evil-snipe)

  :config
  ;; modify keymap
  (evil-define-key '(motion normal visual) evil-colemak-basics-keymap
    "H" #'evil-first-non-blank
    "N" #'evil-scroll-down
    "E" #'evil-scroll-up
    "I" #'evil-end-of-line
    ;; since i'm not using evil-snipe
    "s" #'avy-goto-char-2
    "L" #'evil-redo)

  ;; enable global mode
  (global-evil-colemak-basics-mode))

;; override keymaps - always active
(map! :map 'override
      :leader
      ;; window: w
      :n "w h" #'evil-window-left
      :n "w n" #'evil-window-down
      :n "w e" #'evil-window-up
      :n "w i" #'evil-window-right
      ;; workspace: TAB
      :n "TAB h" #'+workspace:switch-previous
      :n "TAB i" #'+workspace:switch-next)
