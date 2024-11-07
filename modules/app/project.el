;; project management apps and plugins

(after! projectile
  (setq projectile-project-search-path '(("~/Documents/code/" . 3) "~/WebDAV/org/")))

(after! treemacs
  (load! "../../packages/treemacs-icons.el")
  (treemacs-load-theme "nerd-icons-custom")
  (setq treemacs-indentation-string " "
        treemacs-collapse-dirs 10
        treemacs-show-hidden-files nil
        treemacs-no-png-images t))
