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
  (map! :map evil-colemak-basics-keymap
        :mnv "H" #'evil-first-non-blank
        :mnv "N" #'big-scroll-down
        :mnv "E" #'big-scroll-up
        :mnv "I" #'evil-end-of-line
        :n "l" #'evil-undo
        :n "L" #'evil-redo)
  ;; use evil-snipe-mode-map for "s"; otherwise this overrides magit staging :(
  (map! :map evil-snipe-mode-map
        :n "s" #'evil-avy-goto-char-2)

  ;; enable global mode
  (global-evil-colemak-basics-mode))

;; override keymaps - always active
(map! :map 'override
      ;; pressing escape should also cancel commands; luckily
      ;; evil-escape does this
      :g "<escape>" #'evil-escape
      ;; for convenience, also activate M-x with s-x
      :g "s-x" #'execute-extended-command
      ;; redraw frame with '
      :mnv "'" #'redraw-display

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
      :n "o c" #'my-open-calendar
      :n "o g" #'magit)

;; calendar keymaps
;; (defvar cfw-custom-map (make-sparse-keymap))
(map! :map 'cfw:calendar-mode-map
      :nmv "<right>" 'cfw:navi-next-day-command
      :nmv "<left>" 'cfw:navi-previous-day-command
      :nmv "<down>" 'cfw:navi-next-week-command
      :nmv "<up>" 'cfw:navi-previous-week-command
      ;; Vi style
      :nmv "h" 'cfw:navi-previous-day-command
      :nmv "n" 'cfw:navi-next-week-command
      :nmv "e" 'cfw:navi-previous-week-command
      :nmv "i" 'cfw:navi-next-day-command
      :nmv "H" 'cfw:navi-goto-week-begin-command
      :nmv "N" 'cfw:navi-previous-month-command
      :nmv "E" 'cfw:navi-next-month-command
      :nmv "I" 'cfw:navi-goto-week-end-command
      :nmv "g" 'cfw:navi-goto-date-command
      :nmv "." 'cfw:navi-goto-today-command
      :nmv "TAB" 'cfw:navi-prev-item-command
      :nmv "S-TAB" 'cfw:navi-next-item-command

      :nmv "RET" 'cfw:show-details-command
      :nmv "q" 'cfw:org-clean-exit)

(map! :map 'cfw:details-mode-map
      :mnv "q" #'+workspace/close-window-or-workspace)
