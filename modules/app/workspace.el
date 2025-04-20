;; -*- lexical-binding: t -*-

;; workspaces and tabs
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

;; Unbind <C-i> from the TAB key and bind it to centaur-tabs-forward.
(define-key input-decode-map [(control ?i)] [control-i])
(define-key input-decode-map [(control ?I)] [(shift control-i)])

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

(use-package! centaur-tabs
  :defer t
  :config
  (setq centaur-tabs-buffer-groups-function
        #'centaur-tabs-projectile-buffer-groups
        centaur-tabs-show-count t
        centaur-tabs-height 30
        ;; FIXME: why does this hide tab groups?
        centaur-tabs-label-fixed-length 15)

  ;; we should start out with buffer names visible
  (setq centaur-tabs-label-fixed-length my-centaur-tabs-label-fixed-length)
  (centaur-tabs-buffer-show-groups nil)
  (centaur-tabs-display-update)

  ;; key bindings
  (define-key centaur-tabs-mode-map [control-i] #'centaur-tabs-forward)
  (map! :map centaur-tabs-mode-map
        ;; doesn't override unless we use nvm
        :nmvi "C-h" #'centaur-tabs-backward
        :nmvi "C-t" #'my-centaur-tabs-toggle-groups
        :nmvi "C-w" #'centaur-tabs--kill-this-buffer-dont-ask))
