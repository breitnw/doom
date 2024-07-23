;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; SEE PLATFORM-SPECIFIC CONFIG

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs stiel can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
;; (setq doom-theme 'doom-zenburn)

(setq doom-everforest-background "medium")  ; or hard (defaults to soft)
(setq doom-everforest-light-background "medium") ; or hard (defaults to soft)
;; (setq doom-everforest-palette "original")
(setq doom-theme 'doom-everforest) ; dark variant

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; System-dependent configuration
(cond ((eq system-type 'darwin) (load! "platform/macos.el"))
      ((eq system-type 'gnu/linux) (load! "platform/linux.el")))

;; BUILTIN PACKAGES

(after! projectile
  (setq projectile-project-search-path '(("~/code/" . 3))))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

;; Configure nerd icons to look good
;; TODO: customize nerd-icons-faces.el?
;; TODO: replace +/~ with unicode chevrons?
(after! nerd-icons
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"
        nerd-icons-fonts-subdirectory "/usr/local/share/fonts/s"
        nerd-icons-font-names '("SymbolsNerdFontMono_Regular.ttf")))

(after! treemacs
  (load! "packages/treemacs-icons.el")
  (treemacs-load-theme "nerd-icons-custom")
  (setq treemacs-indentation-string " "
        treemacs-collapse-dirs 10
        treemacs-no-png-images t))

(after! lsp-java
  (setq lsp-java-java-path "/usr/bin/java"))

(after! vterm
  (setq vterm-shell "fish"))

;; EMAIL

;; (after! mu4e
;;   (setq mail-user-agent 'mu4e-user-agent
;;         mu4e-maildir "~/Maildir"
;;         mu4e-sent-messages-behavior 'delete))


;; (after! lsp
;;   (setq lsp-inlay-hint-enable t))

;; LOCAL PACKAGES

;; load a custom file for evil-colemak-basics to modify keybinds
(load! "packages/evil-colemak-basics.el")
(after! evil-colemak-basics
  (global-evil-colemak-basics-mode))

;; MELPA PACKAGES

;; automatically detect and use treesit when applicable
;; treesitter (the non-builtin one) uses tree-sitter-langs, which includes .so
;; files built for x86. Also, it's probably better to use the builtin feature,
;; even though features are a bit more sparse
(use-package! treesit-auto
  :config
  (global-treesit-auto-mode))

;; HOOKS

;; (after! (:and lsp rustic)
;;   (setq lsp-inlay-hint-enable t))

;; (add-hook! 'lsp-mode (setq lsp-inlay-hint-enable t))

;; enables inlay hints
;; (defun enable-hints ()
;;     (lsp-inlay-hints-mode)
;;     (setq lsp-inlay-hint-enable t))

;; (add-hook 'rust-mode-hook 'enable-hints)
;; (add-hook! 'java-mode-hook 'enable-hints)
