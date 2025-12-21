;;; modules/app/project.el -*- lexical-binding: t; -*-

;; project management apps and plugins

(use-package! projectile
  :defer t
  :custom
  (projectile-project-search-path '(("~/Documents/code/" . 3)
                                    ("~/Desktop/school/" . 2)
                                    ("~/Config/" . 2))))

(use-package! treemacs
  :defer t
  :after nitree
  :config
  (setq treemacs-collapse-dirs 3
        treemacs-follow-mode t
        treemacs-show-hidden-files nil
        treemacs-indentation-string " ")
  (treemacs-load-theme "nitree"))
