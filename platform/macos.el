;; macOS-specific configuration

(setq doom-font (font-spec :family "Cascadia Code" :size 15)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; FIXME: correct directory
(setq org-directory "/mnt/webdav/org/")
