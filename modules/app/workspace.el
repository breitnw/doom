;; -*- lexical-binding: t -*-

;; workspaces and tabs
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(use-package! centaur-tabs
  :defer t
  :config
  (setq centaur-tabs-buffer-groups-function
        #'centaur-tabs-projectile-buffer-groups
        centaur-tabs-show-count t
        centaur-tabs-height 30)
  (map! :map centaur-tabs-mode-map
        :mniv "C-e" #'centaur-tabs-backward
        :mniv "C-n" #'centaur-tabs-forward
        :mniv "C-h" #'centaur-tabs-toggle-groups
        :mniv "C-w" #'kill-current-buffer
        :leader
        "b b" #'centaur-tabs-toggle-groups))
