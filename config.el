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

(setq doom-font (font-spec :family "Monospace"))
;; i like everything monospace
(setq doom-variable-pitch-font (font-spec :family "Monospace"))
;; variable pitch text is slightly bigger by default
(set-face-attribute 'variable-pitch-text nil :height 1.0)

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "cozette"))

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

;; emacs should run in bash
(setq shell-file-name (executable-find "bash"))

;; platform: system-dependent configuration
;; TODO load system-dependent paths with yaml instead
(cond ((eq system-type 'darwin) (load! "modules/platform/macos.el"))
      ((eq system-type 'gnu/linux) (load! "modules/platform/linux.el")))

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
;; (load! "modules/app/mail.el")
(load! "modules/app/project.el")
(load! "modules/app/terminal.el")

;; TODO temporary solution since mail doesn't work in a module for some reason
;; define a list of accounts that should be accessible
(setq mail-accounts
      '(("School" . "breitnw@u.northwestern.edu")
        ("Personal" . "breitling.nw@gmail.com")))

(after! mu4e
  ;; appearance tweak: instead of the ugly highlight do blue arrows
  (set-face-attribute 'mu4e-highlight-face nil :inherit 'nerd-icons-blue)

  (setq mu4e-headers-thread-blank-prefix '("  " . "  ")
        mu4e-headers-thread-child-prefix '("├─" . "├─")
        mu4e-headers-thread-connection-prefix '("│ " . "│ ")
        mu4e-headers-thread-duplicate-prefix '("= " . "= ")
        mu4e-headers-thread-first-child-prefix '("├─" . "├─")
        mu4e-headers-thread-last-child-prefix '("└─" . "└─")
        mu4e-headers-thread-orphan-prefix '("┬─" . "┬─")
        mu4e-headers-thread-root-prefix '("□ " . "□ ")
        mu4e-headers-thread-single-orphan-prefix '("── " . "── "))

  ;; avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; send messages with msmtp
  (setq send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        ;; mail-user-agent 'sendmail-user-agent
        sendmail-program (executable-find "msmtp")
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; select the right sender email from the context.
  (setq message-sendmail-envelope-from 'header)

  ;; refresh mail every 5 minutes
  (setq mu4e-update-interval (* 5 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-root-maildir "~/Mail")

  ;; use said accounts to create mu4e contexts
  (setq mu4e-contexts
        (mapcar
         (lambda (account)
           (let ((acc-name (car account))
                 (acc-address (cdr account)))
             (make-mu4e-context
              :name acc-name
              :enter-func
              (lambda () (mu4e-message (concat "Entered " acc-name " context")))
              :match-func
              (lambda (msg)
                (when msg
                  (string-prefix-p (format "/%s" acc-name) (mu4e-message-field msg :maildir))))
              :vars
              `((user-mail-address  . ,acc-address)
                (smtpmail-smtp-user . ,acc-address)
                (mu4e-compose-signature . "Nick Breitling")
                (user-full-name     . "Nick Breitling")
                (mu4e-drafts-folder . ,(format "/%s/Drafts" acc-name))
                (mu4e-trash-folder  . ,(format "/%s/Trash" acc-name))
                (mu4e-sent-folder   . ,(format "/%s/Sent" acc-name))
                (mu4e-maildir-shortcuts . ((,(format "/%s/Inbox" acc-name)   . ?i)
                                           (,(format "/%s/Sent" acc-name)    . ?s)
                                           (,(format "/%s/Drafts" acc-name)  . ?d)
                                           (,(format "/%s/Spam" acc-name)    . ?p)
                                           (,(format "/%s/Trash" acc-name)   . ?t)))
                (smtpmail-smtp-server . "smtp.gmail.com")
                (smtpmail-default-smtp-server . "smtp.gmail.com")
                (smtpmail-local-domain . "gmail.com")))))
         mail-accounts)))
