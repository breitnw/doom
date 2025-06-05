;;; modules/editor/keymaps.el -*- lexical-binding: t; -*-

;; KEYMAPS ---------------------------------------------------------------------

;; load evil-colemak-basics and modify keybinds
(use-package! evil-colemak-basics
  :after evil evil-snipe
  :custom
  (evil-colemak-basics-rotate-t-f-j nil)
  (evil-respect-visual-line-mode t)
  (evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  ;; moving to the start of line and resetting horizontal scroll
  (evil-define-command my-scroll-to-start ()
    "scroll to the beginning of the visual line and place the cursor on the
     first character"
    (evil-beginning-of-visual-line) ;; to scroll to the beginning of the line
    (evil-first-non-blank-of-visual-line))

  ;; "big" scroll up and down
  (defvar scroll-pages 0.25)
  (defun pages->lines (num-pages)
    (truncate (* (window-height) num-pages)))
  (evil-define-command my-scroll-up ()
    "scroll up 0.25 pages"
    (evil-scroll-up (pages->lines scroll-pages)))
  (evil-define-command my-scroll-down ()
    "scroll down 0.25 pages"
    (evil-scroll-down (pages->lines scroll-pages)))

  ;; custom keymaps for big scrolls and undo/redo
  (map! :map evil-colemak-basics-keymap
        :mnv "H" #'my-scroll-to-start
        :mnv "N" #'my-scroll-down
        :mnv "E" #'my-scroll-up
        :mnv "I" #'evil-end-of-line
        :n "l" #'evil-undo
        :n "L" #'evil-redo)

  ;; use evil-snipe-mode-map for "s"
  (map! :map evil-snipe-mode-map
        :n "s" #'evil-avy-goto-char-2)

  ;; decode C-i as [control-i], rather than TAB.
  ;; I'm pretty sure this gets messed up by evil-colemak-basics somehow, but
  ;; putting it here seems to fix the issue.
  (add-hook! 'evil-colemak-basics-mode-hook
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])))

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
      :g "s-x" #'execute-extended-command)

(map! :leader
      (:prefix-map ("r" . "reload")
       :n "r f" #'my-reload-frame
       :n "r d" #'redraw-display)
      (:prefix-map ("TAB" . "workspace")
       :desc "Switch to previous workspace"  "h" #'+workspace:switch-previous
       :desc "Switch to next workspace"      "i" #'+workspace:switch-next)
      (:prefix-map ("o" . "open")
       :desc "Open Magit"                    "g" #'magit
       :desc "Open Org-Agenda for week"      "a" #'org-agenda-list))

;; window keymaps are contained in evil-window-map
(map! :map 'evil-window-map
      "s" #'evil-window-split
      "v" #'evil-window-vsplit
      "h" #'evil-window-left
      "n" #'evil-window-down
      "e" #'evil-window-up
      "i" #'evil-window-right
      "H" #'+evil/window-move-left
      "N" #'+evil/window-move-down
      "E" #'+evil/window-move-up
      "I" #'+evil/window-move-right
      "d" #'kill-buffer-and-window
      "c" #'+workspace/close-window-or-workspace)

(map! :map 'evil-motion-state-map
      "C-u" nil
      "C-d" nil)
