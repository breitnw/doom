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

;; (setq doom-everforest-background "medium")  ; or hard (defaults to soft)
;; (setq doom-everforest-light-background "medium") ; or hard (defaults to soft)
;; (setq doom-everforest-palette "original")
(setq doom-theme 'doom-sourcerer) ; dark variant

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


;; BUILTIN PACKAGES ------------------------------------------------------------

(after! projectile
  (setq projectile-project-search-path '(("~/Code/" . 3) "~/WebDAV/org/")))

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
        treemacs-show-hidden-files nil
        treemacs-no-png-images t))

(after! lsp-java
  (setq lsp-java-java-path "/usr/bin/java"
        lsp-java-import-maven-enabled t
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz"))

(after! vterm
  (setq vterm-shell "fish"))

;; make sure to turn off whitespace rendering in org, since we want to be able
;; to have lines of any length
;; TODO: configure line lengths per language
(after! whitespace
  (global-whitespace-mode)
  (setq-default whitespace-style
                '(face
                  tabs
                  tab-mark
                  ;; spaces
                  ;; space-mark
                  trailing
                  lines-tail
                  space-before-tab
                  indentation
                  empty
                  space-after-tab
                  missing-newline-at_eof))
  (setq-default whitespace-global-modes
                '(not magit-mode
                  magit-diff-mode
                  vterm-mode
                  dired-mode
                  org-mode)))


;; SPLASH SCREEN ---------------------------------------------------------------

(defun wizard ()
  (let* ((banner '("            ,    _      *"
                   "           /|   | |  *   "
                   "         _/_\\_  >_< *  * "
                   "        .-\\-/.   |       "
                   "       /  | | \\_ |       "
                   "       \\ \\| |\\__(/  doom emacs"
                   "       /(`---')  |       "
                   "      / /     \\  |       "
                   "   _.'  \\'-'  /  |       "
                   "   `----'`=-='   '       "
                   ""
                   ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line
                                                    (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'wizard)


;; EMAIL -----------------------------------------------------------------------

;; (after! mu4e
;;   (setq mail-user-agent 'mu4e-user-agent
;;         mu4e-maildir "~/Maildir"
;;         mu4e-sent-messages-behavior 'delete))


;; (after! lsp
;;   (setq lsp-inlay-hint-enable t))


;; KEYMAPS ---------------------------------------------------------------------

;; load a custom file for evil-colemak-basics to modify keybinds
;; TODO: load from MELPA instead of custom file and move custom keybinds to a
;;  map! macro
(load! "packages/evil-colemak-basics.el")
(after! evil-colemak-basics
  (global-evil-colemak-basics-mode))

(map! :after evil-colemak-basics
      :leader
      ;; window: w
      :n (kbd "w h") #'evil-window-left
      :n (kbd "w n") #'evil-window-down
      :n (kbd "w e") #'evil-window-up
      :n (kbd "w i") #'evil-window-right
      ;; perspective: n
      :n (kbd "j s") #'persp-switch
      :n (kbd "j q") #'persp-kill
      :n (kbd "j n") #'persp-add-new)


;; MELPA PACKAGES --------------------------------------------------------------

;; automatically detect and use treesit when applicable
;; treesitter (the non-builtin one) uses tree-sitter-langs, which includes .so
;; files built for x86. Also, it's probably better to use the builtin feature,
;; even though features are a bit more sparse
;; TODO: do we need/use this?
;; (use-package! treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; sublimity: smooth scrolling and distraction-free mode
(use-package! sublimity
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 10
        sublimity-scroll-drift-length 5
        sublimity-scroll-vertical-frame-delay 0.002)
  (require 'sublimity-attractive)
  (setq sublimity-attractive-centering-width nil)
  (sublimity-mode))

;; org-latex-preview: realtime rendering of latex fragments in orgmode
;; see https://abode.karthinks.com/org-latex-preview/#org194474f
(use-package! org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))


;; MODES AND HOOKS -------------------------------------------------------------

;; CUSTOM MINOR MODES

(define-minor-mode zen-mode
  "Hides line numbers and centers text"
  :init-value nil
  :lighter " Zen"
  (if zen-mode
      (progn (setq-local sublimity-attractive-centering-width 100)
             (display-line-numbers-mode -1))
    (progn (setq-local sublimity-attractive-centering-width nil)
           (display-line-numbers-mode t))))

;; ORG MODE

;; enable automatic LaTeX previews
(add-hook! 'org-mode-hook #'org-latex-preview-auto-mode)
;; hide line numbers and center text
(add-hook! 'org-mode-hook #'zen-mode)

;; OTHER MODES

;; inlay hints
(add-hook! 'lsp-mode-hook
  (setq lsp-inlay-hint-enable t
        lsp-inlay-hint-param-format "%s"
        lsp-inlay-hint-type-format "%s"))

;; whitespace per-language
;; TODO: is there any way to consolidate this?
;; TODO: and why does it work with c++ even though we don't have a hook????
;; TODO: AND whitespace isn't rendering properly for java, it was only up to line 80. Maybe this is
;;  all a cache issue?
(add-hook! 'rust-mode-hook (setq-local whitespace-line-column 100)
           'rust-mode-hook #'lsp-inlay-hints-mode
           'java-mode-hook (setq-local whitespace-line-column 100)
           'rust-mode-hook #'lsp-inlay-hints-mode)
