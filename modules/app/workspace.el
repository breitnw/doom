;; -*- lexical-binding: t -*-

;; workspaces and tabs
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

;; TODO: show org roam note title if possible

;; HACK: centaur tabs seems to be broken, and does not show tab groups when
;; the tab width is fixed. So, i'm just between fixed/unfixed whenever groups
;; are toggled
(defvar my-centaur-tabs-label-fixed-length 15)
(evil-define-command my-centaur-tabs-toggle-groups ()
  ;; if we're currently showing the groups, reset the tab size
  (let ((show-groups (not centaur-tabs--buffer-show-groups)))
    (setq centaur-tabs-label-fixed-length
          (if show-groups 0 my-centaur-tabs-label-fixed-length))
    (centaur-tabs-buffer-show-groups (not centaur-tabs--buffer-show-groups)))
  (centaur-tabs-display-update))

(evil-define-command my-centaur-tabs-close-current ()
  "Close the active centaur-tabs tab"
  (kill-buffer (current-buffer))
  (centaur-tabs-buffer-update-groups)
  (centaur-tabs-display-update))

(use-package! centaur-tabs
  :defer t
  :init
  ;; keymaps
  ;; need to separate C-i from TAB (works in GUI emacs)
  (add-hook! 'centaur-tabs-mode-hook
    (evil-define-key '(motion normal insert) centaur-tabs-mode-map
      (kbd "<control-i>") #'centaur-tabs-forward
      (kbd "C-h") #'centaur-tabs-backward
      (kbd "C-t") #'my-centaur-tabs-toggle-groups
      (kbd "C-w") #'my-centaur-tabs-close-current))

  :config
  ;; style and grouping for tabs
  (setq centaur-tabs-buffer-groups-function
        #'centaur-tabs-projectile-buffer-groups
        centaur-tabs-show-count t
        centaur-tabs-height 30
        centaur-tabs-label-fixed-length 15)

  ;; we should start out with buffer names visible
  (setq centaur-tabs-label-fixed-length my-centaur-tabs-label-fixed-length)
  (centaur-tabs-buffer-show-groups nil)
  (centaur-tabs-display-update)

  ;; match the solaire-mode background
  (setq centaur-tabs-style "zigzag"
        centaur-tabs-set-bar nil)
  (let ((selected-bg (doom-color 'bg))
        (unselected-bg (face-background 'solaire-default-face)))
    (custom-set-faces!
      `(centaur-tabs-selected :background ,selected-bg)
      `(centaur-tabs-default :background ,unselected-bg)
      `(centaur-tabs-unselected :background ,unselected-bg)))

  ;; match headline color whenever a theme is loaded and at startup
  (centaur-tabs-headline-match)
  (add-hook! 'enable-theme-functions
    (centaur-tabs-headline-match)))
