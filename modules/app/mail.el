;;; modules/app/mail.el -*- lexical-binding: t; -*-

;; Mail functionality is configured by Nix. This file only holds config related
;; to the appearance of mu4e.

(after! mu4e
  ;; don't colorize messages, it reduces readability
  (setq shr-color-visible-luminance-min 60
        shr-color-visible-distance-min 5
        shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest _)))

  ;; hide addresses on the main page to conserve space
  (setq mu4e-main-hide-personal-addresses t)

  ;; don't hard wrap lines when composing messages
  (setq mu4e-compose-format-flowed t))
