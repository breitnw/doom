;; language modes

;; nix ----------------------------------------
(after! nix-mode
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-nixos-options-expr "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\").nixosConfigurations.mnd.options"
        lsp-nix-nixd-home-manager-options-expr "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\").homeConfigurations.\"breitnw@mnd\".options"))

;; java ---------------------------------------
(after! lsp-java
  (setq lsp-java-java-path "/usr/bin/java"
        lsp-java-import-maven-enabled t
        lsp-java-jdt-download-url (concat "https://www.eclipse.org/downloads/"
                                          "download.php?file=/jdtls/snapshots/"
                                          "jdt-language-server-latest.tar.gz")))

;; C ------------------------------------------
(setq-default c-basic-offset 4)

(after! platformio-mode
  ;; Enable ccls for all c++ files, and platformio-mode only
  ;; when needed (platformio.ini present in project root).
  (add-hook! 'c++-mode-hook
    (lsp-deferred)
    (platformio-conditionally-enable)))


;; Racket -------------------------------------
(after! racket-mode
  (add-hook! 'racket-mode-hook
    (lsp))
  (map! :map 'racket-mode-map
        :i "C-\\" #'racket-insert-lambda
        :nmv "<TAB>" #'evil-jump-item
        :nmv "<RET>" #'racket-cycle-paren-shapes))

;; TeX ----------------------------------------
(after! apheleia
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent"
          "-y=defaultIndent:'  '"
          "--logfile=/dev/null")))
