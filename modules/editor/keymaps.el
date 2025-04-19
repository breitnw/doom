;; -*- lexical-binding: t -*-

;; KEYMAPS ---------------------------------------------------------------------

;; load evil-colemak-basics and modify keybinds
(use-package! evil-colemak-basics
  :after evil evil-snipe
  :custom
  (evil-colemak-basics-rotate-t-f-j nil)
  (evil-respect-visual-line-mode t)
  (evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  ;; add "big" scroll functions and redo functionality to evil-colemak-basics
  (evil-define-command my-scroll-to-start ()
    "scroll to the beginning of the visual line and place the cursor on the
     first character"
    (evil-beginning-of-visual-line) ;; to scroll to the beginning of the line
    (evil-first-non-blank-of-visual-line))

  (defvar scroll-pages 0.25)
  (defun pages->lines (num-pages)
    (truncate (* (window-height) num-pages)))

  (evil-define-command my-scroll-up ()
    "scroll up 0.25 pages"
    (evil-scroll-up (pages->lines scroll-pages)))

  (evil-define-command my-scroll-down ()
    "scroll down 0.25 pages"
    (evil-scroll-down (pages->lines scroll-pages)))

  (map! :map evil-colemak-basics-keymap
        :mnv "H" #'my-scroll-to-start
        :mnv "N" #'my-scroll-down
        :mnv "E" #'my-scroll-up
        :mnv "I" #'evil-end-of-line
        :n "l" #'evil-undo
        :n "L" #'evil-redo)
  ;; use evil-snipe-mode-map for "s"; otherwise this overrides magit staging :(
  (map! :map evil-snipe-mode-map
        :n "s" #'evil-avy-goto-char-2)
  ;; enable global mode
  (global-evil-colemak-basics-mode))

;; colemak keybinds for evil-org
(use-package! evil-org
  :after org
  :custom
  (evil-org-movement-bindings
   `((up . "e") (down . "n") (left . "h") (right . "i"))))

;; HACK: command to reload the current frame in case scrolling goes awry
;; I think the scrolling is a PGTK issue?
(evil-define-command my-reload-frame ()
  (let ((old-frame (selected-frame)))
    (clone-frame)
    (delete-frame old-frame)))

;; override keymaps - always active
(map! :map 'override
      ;; pressing escape should also cancel commands; luckily
      ;; evil-escape does this
      :g "<escape>" #'evil-escape
      ;; but we do want to ignore escape in motion state, for
      ;; calendar and such
      :m "<escape>" #'ignore
      ;; for convenience, also activate M-x with s-x
      :g "s-x" #'execute-extended-command

      ;; all other keymaps start with <SPC>
      :leader
      ;; reload: r
      :n "r f" #'my-reload-frame
      :n "r d" #'redraw-display
      ;; window: w
      :n "w s" #'evil-window-split
      :n "w v" #'evil-window-vsplit
      :n "w h" #'evil-window-left
      :n "w n" #'evil-window-down
      :n "w e" #'evil-window-up
      :n "w i" #'evil-window-right
      :n "w H" #'+evil/window-move-left
      :n "w N" #'+evil/window-move-down
      :n "w E" #'+evil/window-move-up
      :n "w I" #'+evil/window-move-right
      :n "w d" #'kill-buffer-and-window
      :n "w c" #'+workspace/close-window-or-workspace
      ;; workspace: TAB
      :n "TAB h" #'+workspace:switch-previous
      :n "TAB i" #'+workspace:switch-next
      ;; open: o
      ;; :n "o c" #'my-open-calendar
      :n "o g" #'magit
      :n "o a" #'org-agenda-list)
