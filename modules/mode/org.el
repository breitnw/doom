;; centered text with darkroom ====================================

;; hide line numbers and center text
(use-package! darkroom
  :config
  (setq darkroom-margins
        (lambda (w)
          (let* ((max-content-width 100)
                 (min-margin 5)
                 (window-width (car (darkroom--window-width w)))
                 (content-width (min window-width max-content-width))
                 (margin-width (max (/ (- window-width content-width) 2)
                                    min-margin)))
            `(,margin-width . ,margin-width))))
  (add-hook! 'org-mode-hook #'darkroom-mode)
  (add-hook! 'darkroom-mode-hook
             ;; re-enable the modeline
             (progn
               (doom-modeline-refresh-bars)        ; Create bars
               (doom-modeline-set-main-modeline t) ; Set default mode-line
               (dolist (buf (buffer-list))
                 (with-current-buffer buf
                   (unless (doom-modeline-auto-set-modeline)
                     (doom-modeline-set-main-modeline)))))
             ;; display line numbers
             (setq-local display-line-numbers nil)))

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
