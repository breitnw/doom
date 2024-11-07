;; centered text with sublimity ===================================

;; sublimity: smooth scrolling and distraction-free mode
(use-package! sublimity
  :config
  ;; (require 'sublimity-scroll)
  ;; (setq sublimity-scroll-weight 15
  ;;       sublimity-scroll-drift-length 10)
  (require 'sublimity-attractive)
  (setq sublimity-attractive-centering-width nil)
  (sublimity-mode))

(define-minor-mode zen-mode
  "Hides line numbers and centers text"
  :init-value nil
  :lighter " Zen"
  (if zen-mode
      (progn (setq-local sublimity-attractive-centering-width 100)
             (display-line-numbers-mode -1))
    (progn (setq-local sublimity-attractive-centering-width nil)
           (display-line-numbers-mode t))))

;; enable automatic LaTeX previews (this is done earlier dw)
;; (add-hook! 'org-mode-hook #'org-latex-preview-auto-mode)
;; hide line numbers and center text
(add-hook! 'org-mode-hook #'zen-mode)


;; better LaTeX previews ==========================================

;; org-latex-preview: realtime rendering of latex fragments in orgmode
;; see https://abode.karthinks.com/org-latex-preview/#org194474f
(use-package! org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)

  ;; Finally, make it so previews are shown on startup
  (setq org-startup-with-latex-preview t))


;; org-roam =======================================================

;; org-roam-ui, which shows the roam graph via a webserver
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq org-id-link-to-org-use-id t)
(setq org-hide-emphasis-markers t)