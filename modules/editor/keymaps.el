;; KEYMAPS ---------------------------------------------------------------------

;; load evil-colemak-basics and modify keybinds
(use-package! evil-colemak-basics
  :after evil evil-snipe
  :init
  ;; needs to be set before the package is loaded
  (setq evil-colemak-basics-rotate-t-f-j nil
        evil-respect-visual-line-mode t
        evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  ;; add "big" scroll functions and redo functionality to evil-colemak-basics
  (evil-define-key '(motion normal visual) evil-colemak-basics-keymap
    "H" #'evil-first-non-blank
    "N" #'big-scroll-down
    "E" #'big-scroll-up
    "I" #'evil-end-of-line
    "L" #'evil-redo)

  ;; enable global mode
  (global-evil-colemak-basics-mode))

;; override keymaps - always active
(map! :map 'override
      ;; pressing escape should also cancel commands; luckily
      ;; evil-escape does this
      "<escape>" #'evil-escape
      ;; for convenience, also activate M-x with s-x
      "s-x" #'execute-extended-command
      ;; redraw frame with '
      :n "'" #'redraw-display

      ;; all other keymaps start with <SPC>
      :leader
      ;; window: w
      :n "w h" #'evil-window-left
      :n "w n" #'evil-window-down
      :n "w e" #'evil-window-up
      :n "w i" #'evil-window-right
      :n "w H" #'+evil/window-move-left
      :n "w N" #'+evil/window-move-down
      :n "w E" #'+evil/window-move-up
      :n "w I" #'+evil/window-move-right
      ;; workspace: TAB
      :n "TAB h" #'+workspace:switch-previous
      :n "TAB i" #'+workspace:switch-next
      ;; open: o
      :n "o c" #'my-open-calendar)

;; calendar keymaps
(defvar cfw-custom-map (make-sparse-keymap))
(map! :map 'cfw-custom-map
      :nm "<right>" 'cfw:navi-next-day-command
      :nm "<left>" 'cfw:navi-previous-day-command
      :nm "<down>" 'cfw:navi-next-week-command
      :nm "<up>" 'cfw:navi-previous-week-command
      ;; Vi style
      :nm "h" 'cfw:navi-previous-day-command
      :nm "n" 'cfw:navi-next-week-command
      :nm "e" 'cfw:navi-previous-week-command
      :nm "i" 'cfw:navi-next-day-command
      :nm "H" 'cfw:navi-goto-week-begin-command
      :nm "N" 'cfw:navi-previous-month-command
      :nm "E" 'cfw:navi-next-month-command
      :nm "I" 'cfw:navi-goto-week-end-command
      :nm "g" 'cfw:navi-goto-date-command
      :nm "." 'cfw:navi-goto-today-command
      :nm "TAB" 'cfw:navi-prev-item-command
      :nm "S-TAB" 'cfw:navi-next-item-command

      :nm "RET" 'cfw:show-details-command
      :nm "q" 'cfw:org-clean-exit)
