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

(setq doom-font (font-spec :family "Cozette"))
(setq doom-symbol-font (font-spec :family "Cozette"))
;; i like everything monospace
(setq doom-variable-pitch-font (font-spec :family "Cozette"))
;; variable pitch text is slightly bigger by default
(set-face-attribute 'variable-pitch-text nil :height 1.0)

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "cozette"))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs stiel can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; CONFIGURED IN NIX CONFIG
;; (setq doom-theme 'doom-sourcerer) ; dark variant

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

;; emacs should run in bash
(setq shell-file-name (executable-find "bash"))

;; platform: system-dependent configuration
;; TODO load system-dependent paths with yaml instead
(cond ((eq system-type 'darwin) (load! "modules/platform/macos.el"))
      ((eq system-type 'gnu/linux) (load! "modules/platform/linux.el")))

;; FIXME
;; (use-package! exec-path-from-shell
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize))
;;   (when (daemonp)
;;     (exec-path-from-shell-initialize)))

;; editor: configuration to aid in text editing
(load! "modules/editor/completion.el")
(load! "modules/editor/keymaps.el")
(load! "modules/editor/motion.el")

;; mode: editor mode configuration
(load! "modules/mode/lang.el")
(load! "modules/mode/lsp.el")
(load! "modules/mode/org.el")

;; visual: configuration to make emacs pretty
(load! "modules/visual/icons.el")
(load! "modules/visual/splash.el")
(load! "modules/visual/whitespace.el")

;; app: app-like plugins, providing functionality other than editing
(load! "modules/app/project.el")
(load! "modules/app/terminal.el")

;; TODO move this somewhere else
;; (require 'mu4e-contrib)
(setq shr-color-visible-luminance-min 60
      shr-color-visible-distance-min 5
      shr-use-fonts nil
      shr-bullet "  "
      shr-internal-bullet "  ")

;; (setq shr-use-colors nil)
;; (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

;; Load the default init file. Doom disables this by default, but we need it
;; since nix uses it for extra config.
(load "default" 'noerror 'nomessage)
