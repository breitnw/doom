;; language modes

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

;; C ------------------------------------------
(setq c-basic-offset 4)

;; Enable ccls for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook! 'c++-mode-hook
  (lsp-deferred)
  (platformio-conditionally-enable))

(use-package! platformio-mode
  :defer t
  :autoload #'platformio-conditionally-enable)


;; Racket -------------------------------------
(use-package! racket-mode
  :defer t
  :config
  (map! :map 'racket-mode-map
        :i "C-\\" #'racket-insert-lambda
        :nmv "<TAB>" #'evil-jump-item
        :nmv "<RET>" #'racket-cycle-paren-shapes))

;; TeX ----------------------------------------
;; prefer spaces instead of tabs
(after! apheleia
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent"
          "-y=defaultIndent:'  '"
          "--logfile=/dev/null")))

;; tidal --------------------------------------
(use-package! tidal
  :config
  (map! :map 'tidal-mode-map
        :nmv "<RET>" #'tidal-run-multiple-lines))
