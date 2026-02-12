;;; modules/editor/layout.el -*- lexical-binding: t; -*-

;; utilities to customize the keyboard layout when using evil-mode

;; enable global mode

(defconst layout-priority-keys
  '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h)
  "The keys that are most accessible to the user, for tools such as Avy.")

(defconst layout-motion-keys
  '(?h ?n ?e ?i)
  "The keys used for motion left, down, up, and right, respectively.")

(defun layout--motion-key (direction)
  "Gets the key corresponding with DIRECTION in the layout."
  (cond ((eq direction 'left)  (car layout-motion-keys))
        ((eq direction 'down)  (cadr layout-motion-keys))
        ((eq direction 'up)    (caddr layout-motion-keys))
        ((eq direction 'right) (cadddr layout-motion-keys))))

(defun layout-remap-keys ()
  "Remap keys for the current layout"

  ;; for convenience, map from chars to strings
  (setq lpk-str (mapcar #'string layout-priority-keys)
        lmk-str (mapcar #'string layout-motion-keys))

  ;; avy: search for characters on the screen
  (after! avy
    (setq! avy-keys layout-priority-keys))

  ;; ace-window: priority keys for switching windows
  (after! ace-window
    (setq! aw-keys layout-priority-keys)
    ;; TODO put this somewhere better
    (ace-window-posframe-mode)
    (map! :map 'evil-window-map
          "w" #'ace-window))

  ;; "leader" keymaps ----------------------------------------------------------

  ;; window navigation
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
        "w" #'ace-window
        "c" #'+workspace/close-window-or-workspace)

  ;; workspace navigation
  (map! :prefix-map ("TAB" . "workspace")
        :desc "Switch to previous workspace"  "h" #'+workspace:switch-previous
        :desc "Switch to next workspace"      "i" #'+workspace:switch-next)

  ;; keymaps for other modes ---------------------------------------------------

  ;; TeX
  (after! evil-tex
    (evil-define-key '(visual operator) 'evil-tex-mode
      "u" evil-tex-inner-text-objects-map
      "a" evil-tex-outer-text-objects-map
      "i" nil))

  ;; org
  (after! evil-org
    (setq evil-org-movement-bindings
          `((left  . ,(string (layout--motion-key 'left)))
            (down  . ,(string (layout--motion-key 'down)))
            (up    . ,(string (layout--motion-key 'up)))
            (right . ,(string (layout--motion-key 'right)))))
    (evil-define-key '(visual operator) evil-org-mode-map
      "i" nil
      (kbd "u E") #'evil-org-inner-element
      (kbd "u R") #'evil-org-inner-subtree
      (kbd "u e") #'evil-org-inner-object))

  ;; all other modes in evil-collection
  (after! evil-collection
    (evil-collection-translate-key
      nil
      '(evil-normal-state-map
        evil-motion-state-map
        evil-visual-state-map
        evil-operator-state-map)
      "n" "j"
      "e" "k"
      "i" "l"
      "k" "n"
      "K" "N"
      "u" "i"
      "U" "I"
      "l" "u"
      "L" "U"
      "f" "e"
      "j" "e")
    (evil-collection-init)))

;; remap keys on startup -------------------------------------------------------



;; evil-colemak-basics configuration -------------------------------------------

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
        :mnv "N" #'big-scroll-down
        :mnv "E" #'big-scroll-up
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
    (define-key input-decode-map [(control ?I)] [(shift control-i)]))

  ;; enable evil-colemak-basics
  (global-evil-colemak-basics-mode)
  ;; enable other colemak keybinds
  (layout-remap-keys))
