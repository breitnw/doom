;;; modules/visual/modeline.el -*- lexical-binding: t; -*-

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail nil
        nyan-bar-length 14
        ;; hiding nyan cat is handled by doom-modeline
        nyan-minimum-window-width 0))

(use-package! doom-modeline
  :config
  (setq doom-base16-padded-modeline t
        doom-themes-padded-modeline t
        doom-modeline-height 20
        doom-modeline-window-width-limit 65
        doom-modeline-buffer-file-name-style 'auto)

  (defface my-nyan-face '((t))
    "Face for nyan cat background."
    :group 'doom-modeline-mode)
  (custom-set-faces!
    `(my-nyan-face
      :background ,(doom-darken (face-background 'mode-line) 0.2)))

  (doom-modeline-def-segment my-buffer-position
    "The buffer position information."
    (let ((visible (doom-modeline--segment-visible 'buffer-position))
          (sep (doom-modeline-spc))
          (face (doom-modeline-face))
          (nyan-face (doom-modeline-face 'my-nyan-face))
          (help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu")
          (mouse-face 'doom-modeline-highlight)
          (local-map mode-line-column-line-number-mode-map))
      `(,sep
        ;; Position
        (,visible
         ,(cond
           ((bound-and-true-p nyan-mode)
            (concat sep (propertize (nyan-create) 'face nyan-face) sep))
           (t "")))

        ;; Line and column
        (:propertize
         ((line-number-mode
           (column-number-mode
            (doom-modeline-column-zero-based
             doom-modeline-position-column-line-format
             ,(string-replace
               "%c" "%C" (car doom-modeline-position-column-line-format)))
            doom-modeline-position-line-format)
           (column-number-mode
            (doom-modeline-column-zero-based
             doom-modeline-position-column-format
             ,(string-replace
               "%c" "%C" (car doom-modeline-position-column-format)))))
          (doom-modeline-total-line-number
           ,(and doom-modeline-total-line-number
                 (format "/%d" (line-number-at-pos (point-max))))))
         face ,face
         help-echo ,help-echo
         mouse-face ,mouse-face
         local-map ,local-map)

        ((or line-number-mode column-number-mode)
         ,sep))))

  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host my-buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process check))

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-simple-line 'default))))

;; HACK to fix solaire-mode issue
;; switching from a solaire window to a non-solaire window doesn't seem to reset
;; the mode line face properly. to fix this, I'm just setting the solaire mode
;; line face to inherit from the normal mode line face.
(custom-set-faces!
  `(solaire-mode-line-face
    :box unspecified
    :background unspecified
    :inherit mode-line))
