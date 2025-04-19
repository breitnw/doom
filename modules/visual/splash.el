;; -*- lexical-binding: t -*-

(defun wizard ()
  (let* ((banner '("            ,    _      *"
                   "           /|   | |  *   "
                   "         _/_\\_  >_< *  * "
                   "        .-\\-/.   |       "
                   "       /  | | \\_ |       "
                   "       \\ \\| |\\__(/  doom emacs"
                   "       /(`---')  |       "
                   "      / /     \\  |       "
                   "   _.'  \\'-'  /  |       "
                   "   `----'`=-='   '       "
                   ""
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line
                                                    (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'wizard)

(setq +doom-dashboard-menu-sections
      '(("Recently opened files" :icon
         (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Reload last session" :icon
         (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
         :when
         (cond
          ((modulep! :ui workspaces)
           (file-exists-p
            (expand-file-name persp-auto-save-fname persp-save-dir)))
          ((require 'desktop nil t)
           (file-exists-p
            (desktop-full-file-name))))
         :action doom/quickload-session)
        ("Open org-agenda" :icon
         (nerd-icons-faicon "nf-fa-calendar" :face 'doom-dashboard-menu-title)
         :when
         (fboundp 'org-agenda)
         :action org-agenda)
        ("Open project" :icon
         (nerd-icons-faicon "nf-fa-archive" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Jump to bookmark" :icon
         (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)
        ("Open private configuration" :icon
         (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :when
         (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ("Open documentation" :icon
         (nerd-icons-faicon "nf-fa-book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

(advice-add 'doom-dashboard-widget-footer :override
            (lambda ()
              (insert
               "\n"
               (+doom-dashboard--center
                (- +doom-dashboard--width 2)
                (with-temp-buffer
                  (insert-text-button (or (nerd-icons-faicon "nf-fa-github_alt" :face 'doom-dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                                          (propertize "github" 'face 'doom-dashboard-footer))
                                      'action (lambda (_) (browse-url "https://github.com/hlissner/doom-emacs"))
                                      'follow-link t
                                      'help-echo "Open Doom Emacs github page")
                  (buffer-string)))
               "\n")))
