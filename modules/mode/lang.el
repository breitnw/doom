;;; modules/mode/lang.el -*- lexical-binding: t; -*-

;; language modes

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

;; nix ----------------------------------------
(use-package! nix-mode
  :defer t
  :custom
  (lsp-nix-nixd-server-path "nixd")
  (lsp-nix-nixd-formatting-command [ "nixfmt" ])
  (lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }")
  (lsp-nix-nixd-nixos-options-expr
   (concat "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\")"
           ".nixosConfigurations.mnd.options"))
  (lsp-nix-nixd-home-manager-options-expr
   (concat "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\")"
           ".homeConfigurations.\"breitnw@mnd\".options")))

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
(setq c-basic-offset 2)

;; Enable ccls for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook! 'c++-mode-hook
  (lsp-deferred)
  (platformio-conditionally-enable))

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
  :config
  (map! :map 'racket-mode-map
        :i "C-\\" #'racket-insert-lambda
        :nmv "<TAB>" #'evil-jump-item
        :nmv "<RET>" #'racket-cycle-paren-shapes))

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
