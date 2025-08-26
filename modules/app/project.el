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
  :config
  (setq treemacs-collapse-dirs 3
        treemacs-follow-mode t
        treemacs-show-hidden-files nil
        treemacs-indentation-string " ")
  (load! "../../packages/nitree.el")
  (treemacs-load-theme "nitree"))

;; (use-package! lsp-treemacs
;;   :custom
;;   (lsp-treemacs-theme "nitree"))

;; also set it anytime lsp-mode is loaded
;; (use-package! lsp-mode
;;   :custom
;;   (lsp-treemacs-theme "nitree"))
