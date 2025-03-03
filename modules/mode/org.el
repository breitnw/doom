;; centered text with darkroom ====================================

;; hide line numbers and center text
(use-package! olivetti
  :hook (org-mode markdown-mode)

  :custom
  (olivetti-margin-width 5)
  (olivetti-body-width 100)
  (olivetti-minimum-body-width 50)
  (olivetti-style nil)

  :config
  (add-hook! 'olivetti-mode-hook
    (vi-tilde-fringe-mode -1)
    (display-line-numbers-mode -1)
    (diff-hl-mode -1)))


;; better LaTeX previews ==========================================

;; org-latex-preview: realtime rendering of latex fragments in orgmode
;; see https://abode.karthinks.com/org-latex-preview/#org194474f
(use-package! org-latex-preview
  :custom
  (org-latex-preview-numbered t)
  (org-latex-preview-live t)
  (org-latex-preview-live-debounce 0.25)
  (org-startup-with-latex-preview t)

  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line))


;; org-roam =======================================================

;; node-based note taking
(use-package! org-roam
  :custom
  (org-directory "~/Documents/org/")
  (org-agenda-files '("~/Documents/org/agenda" "~/Documents/org/calendar/inbox"))
  (org-roam-directory (file-truename "~/Documents/org/notes/"))
  :config
  (org-roam-db-autosync-mode))

;; websocket is necessary for org-roam-ui
(use-package! websocket
  :after org-roam)

;; shows the roam graph via a webserver
(use-package! org-roam-ui
  :after org-roam websocket
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  (org-id-link-to-org-use-id t)
  (org-hide-emphasis-markers t))
