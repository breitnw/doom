;; linux-specific configuration

;; (setq doom-font (font-spec :family "Cascadia Code" :size 15)
;;       doom-variable-pitch-font (font-spec :family "Cascadia Code" :size 15)
;;       doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 15))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; fallback directory for org stuff
(setq org-directory "~/Documents/org")

;; directory for org-agenda
(setq org-agenda-files "~/Documents/org/agenda")

;; directory for org-roam
(setq org-roam-directory (file-truename "~/Documents/org/notes"))
(org-roam-db-autosync-mode)
