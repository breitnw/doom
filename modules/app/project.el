;; project management apps and plugins

(use-package! projectile
  :defer t
  :custom
  (projectile-project-search-path '(("~/Documents/code/" . 3) "~/WebDAV/org/")))

(use-package! treemacs
  :defer t
  :custom
  (treemacs-indentation-string " ")
  (treemacs-collapse-dirs 10)
  (treemacs-show-hidden-files nil)
  (treemacs-no-png-images t)
  :config
  (load! "../../packages/treemacs-icons.el")
  (treemacs-load-theme "nerd-icons-custom"))
