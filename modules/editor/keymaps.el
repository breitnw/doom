;;; modules/editor/keymaps.el -*- lexical-binding: t; -*-

;; keymap definitions (not colemak-specific)

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

;; load evil-colemak-basics and modify keybinds


