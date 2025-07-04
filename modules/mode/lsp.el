;;; modules/mode/lsp.el -*- lexical-binding: t; -*-

;; global lsp-mode configuration

(setenv "LSP_USE_PLISTS" "true")

(use-package! lsp-mode
  :defer t
  :custom
  (lsp-inlay-hint-enable t)
  (lsp-inlay-hint-param-format "%s")
  (lsp-inlay-hint-type-format "%s")
  (lsp-eldoc-render-all t)
  (lsp-enable-suggest-server-download nil)
  (lsp-headerline-breadcrumb-enable t)
  :config
  ;; inlay hint faces
  (custom-set-faces!
    `(lsp-inlay-hint-face
      :foreground ,(doom-color 'base6)
      :weight normal))

  ;; breadcrumb faces
  (custom-set-faces!
    `(header-line
      :inherit mode-line
      :background ,(doom-color 'bg)
      :box (:line-width 6 :color ,(doom-color 'bg)))

    `(lsp-headerline-breadcrumb-path-hint-face
      :underline ,(face-attribute 'flycheck-info :underline))
    `(lsp-headerline-breadcrumb-path-info-face
      :underline ,(face-attribute 'flycheck-info :underline))
    `(lsp-headerline-breadcrumb-symbols-hint-face
      :underline ,(face-attribute 'flycheck-info :underline))
    `(lsp-headerline-breadcrumb-symbols-info-face
      :underline ,(face-attribute 'flycheck-info :underline))
    `(lsp-headerline-breadcrumb-path-warning-face
      :underline ,(face-attribute 'flycheck-warning :underline))
    `(lsp-headerline-breadcrumb-symbols-warning-face
      :underline ,(face-attribute 'flycheck-warning :underline))
    `(lsp-headerline-breadcrumb-path-error-face
      :underline ,(face-attribute 'flycheck-error :underline))
    `(lsp-headerline-breadcrumb-symbols-error-face
      :underline ,(face-attribute 'flycheck-error :underline)))

  ;; boost performance of lsp-mode with emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection)))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        (progn
          (message "Starting emacs-lsp-booster failed for %s!" orig-result)
          (when test?
            (message "(not test?) failed"))
          (when (file-remote-p default-directory)
            (message "(not (file-remote-p default-directory)) condition failed"))
          (when (not lsp-use-plists)
            (message "lsp-use-plists condition failed"))
          (when (functionp 'json-rpc-connection)
            (message "(not (functionp 'json-rpc-connection)) condition failed"))
          (when (not (executable-find "emacs-lsp-booster"))
            (message "(executable-find \"emacs-lsp-booster\") condition failed"))
          orig-result))))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package! eldoc
  :defer t
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-use-multiline-p nil))

(use-package! eldoc-box
  :after eldoc
  :custom
  (eldoc-box-max-pixel-width 400)
  (eldoc-box-max-pixel-height 300)
  :config
  (map! :map 'prog-mode-map
        :n "?" #'eldoc-box-help-at-point)
  (custom-set-faces!
    `(eldoc-box-border :background ,(doom-color 'comments))
    `(eldoc-box-markdown-separator :foreground ,(doom-color 'comments) :strike-through t)
    `(markdown-code-face :background ,(doom-color 'bg))))
