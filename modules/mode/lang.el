;;; modules/mode/lang.el -*- lexical-binding: t; -*-

;; language modes

(use-package! tuareg
  :config
  (remove-hook! tuareg-mode-local-vars-hook #'opam-switch-mode)
  (custom-set-faces!
    `(tuareg-font-double-semicolon-face
      :inherit font-lock-builtin-face)
    `(tuareg-font-lock-operator-face
      :inherit font-lock-operator-face)
    `(tuareg-font-lock-governing-face
      :inherit font-lock-keyword-face)
    `(tuareg-font-lock-constructor-face
      :inherit font-lock-function-name-face)))

;; agda ---------------------------------------
(defun load-agda2-mode ()
  (interactive)
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (agda2-mode))

;; load agda2-mode lazily, since we can only access it in direnv
(use-package! agda2-mode
  :defer t
  :custom
  (agda2-version "2.7.0.1"))

;; rust ---------------------------------------
;; for editing .ron files
(use-package! ron-mode
  :config
  (setq ron-indent-offset 2))

;; nix ----------------------------------------
(defvar flake-path nil
  "[NIX CONFIG] The path to the system configuration flake")

;; (use-package! nix-mode
;;   :defer t
;;   :custom
;;   (lsp-nix-nixd-server-path "nixd")
;;   (lsp-nix-nixd-formatting-command [ "alejandra" ])
;;   (lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }")
;;   :config
;;   ;; error if flake-path is not set
;;   (when (not flake-path)
;;     (error "lsp-nix-nixd: path to configuration is `nil'"))
;;   ;; error [flake-path]/flake.nix does not exist
;;   (setq flake-dot-nix-path
;;         (concat (file-name-as-directory flake-path) "flake.nix"))
;;   (when (not (file-exists-p flake-dot-nix-path))
;;     (error (format "lsp-nix-nixd: configuration flake %s does not exist"
;;                    flake-dot-nix-path)))
;;   ;; otherwise, set options and hm-options expressions
;;   (setq flake (format "(builtins.getFlake \"%s\")" flake-path)
;;         lsp-nix-nixd-nixos-options-expr
;;         (format "%s.nixosConfigurations.mnd.options" flake)
;;         lsp-nix-nixd-home-manager-options-expr
;;         (format "%s.homeConfigurations.\"breitnw@mnd\".options" flake))

;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              `(nix-mode . ("nixd" :initializationOptions
;;   ;;                            (:formatting (:command ["alejandra"])
;;   ;;                             :options
;;   ;;                             (:nixos (:expr ,lsp-nix-nixd-nixos-options-expr)
;;   ;;                              :home_manager (:expr ,lsp-nix-nixd-home-manager-options-expr))))))
;;   )

;; java ---------------------------------------
(use-package! lsp-java
  :defer t
  :custom
  (lsp-java-java-path "/usr/bin/java")
  (lsp-java-import-maven-enabled t)
  (lsp-java-jdt-download-url (concat "https://www.eclipse.org/downloads/"
                                     "download.php?file=/jdtls/snapshots/"
                                     "jdt-language-server-latest.tar.gz")))

;; web ----------------------------------------
(add-hook! 'js-mode-hook
  (lsp-deferred))

;; (add-hook! 'mhtml-mode-hook
;;   (lsp-deferred))

;; C ------------------------------------------

(defconst c-indent-offset 2)

;; tree-sitter
(use-package! c-ts-mode
  :defer t
  :config
  (setq c-ts-mode-indent-offset c-indent-offset)
  (add-hook! 'c-ts-mode-hook
    (setq-local treesit-simple-indent-rules
                `((c . ,(append
                         `(((node-is ")") standalone-parent 0)
                           ((match nil "parameter_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
                           ((parent-is "parameter_list") c-ts-mode--anchor-prev-sibling 0)
                           ((match nil "argument_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
                           ((parent-is "argument_list") c-ts-mode--anchor-prev-sibling 0))
                         (cdr (assq 'c treesit-simple-indent-rules))))))))

;; non tree-sitter
(use-package! cc-mode
  :defer t
  :config
  ;; base indentation off of the defined style
  (setq-default c-basic-offset 'set-from-style)
  ;; configure custom indentation style
  (c-add-style
   "custom"
   '("doom" (c-special-indent-hook)
     (c-basic-offset . 2)
     (c-offsets-alist (innamespace . [0]))))
  (setq c-default-style "custom"))



(use-package! platformio-mode
  :defer t
  :autoload #'platformio-conditionally-enable)

;; Zig ----------------------------------------
(defun zig-build-run ()
  (interactive)
  (zig--run-cmd "build" "run"))

(defun zig-build-test ()
  (interactive)
  (zig--run-cmd "build" "test"))

(defun zig-build ()
  (interactive)
  (zig--run-cmd "build"))

(use-package! zig-mode
  :defer t
  :custom
  (lsp-zig-warn-style t)
  :config
  (map! :map zig-mode-map
        :leader
        "m b r" #'zig-build-run
        "m b t" #'zig-build-test
        "m b b" #'zig-build))

;; Racket -------------------------------------
(use-package! racket-mode
  :defer t
  :custom
  (racket-xp-eldoc-level 'minimal)
  :config
  (map! :map 'racket-mode-map
        :i "C-\\" #'racket-insert-lambda
        :nmv "<TAB>" #'evil-jump-item
        :nmv "<RET>" #'racket-cycle-paren-shapes))

;; haskell ------------------------------------

(use-package! haskell-indentation
  :hook 'haskell-mode-hook)

;; tidal --------------------------------------
(use-package! tidal
  :defer t
  :config
  (setq tidal-boot-script-path "~/Tidal/boot.hs")
  (map! :map 'tidal-mode-map
        :nmv "<RET>" #'tidal-run-multiple-lines))

;; markdown -----------------------------------
(add-hook! 'markdown-mode-hook
  (visual-wrap-prefix-mode))

;; config files -------------------------------

;; major mode for editing .ron files
(use-package ron-mode)

;; latex --------------------------------------

(use-package! auctex
  :defer t
  :config
  ;; automatically show TeX errors when compiling
  (setq TeX-error-overview-open-after-TeX-run t
        font-latex-fontify-script nil))

