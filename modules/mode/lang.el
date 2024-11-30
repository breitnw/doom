;; language modes

;; nix ----------------------------------------
(after! nix-mode
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-nixos-options-expr "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\").nixosConfigurations.mnd.options"
        lsp-nix-nixd-home-manager-options-expr "(builtins.getFlake \"/home/breitnw/Documents/code/nixos\").homeConfigurations.\"breitnw@mnd\".options"))

;; for some reason, nix-mode disables company autocompletion, so we
;; need to manually re-enable it
(add-hook! 'nix-mode-hook
           (setq company-idle-delay 0.1))

;; java ---------------------------------------
(after! lsp-java
  (setq lsp-java-java-path "/usr/bin/java"
        lsp-java-import-maven-enabled t
        lsp-java-jdt-download-url (concat "https://www.eclipse.org/downloads/"
                                          "download.php?file=/jdtls/snapshots/"
                                          "jdt-language-server-latest.tar.gz")))

;; C ------------------------------------------
(setq-default c-basic-offset 4)

;; DSSL2 --------------------------------------
(define-derived-mode dssl2-mode
  racket-mode
  (setq font-lock-defaults
        '((python-font-lock-keywords-level-1 python-font-lock-keywords-level-1 python-font-lock-keywords-level-2 python-font-lock-keywords-maximum-decoration)
         nil nil nil nil
         (font-lock-syntactic-face-function . python-font-lock-syntactic-face-function))
        comment-start "# "
        comment-padding " "
        sp-comment-char "#"
        comment-start-skip "#+\\s-*"
        font-lock-comment-start-skip nil))
