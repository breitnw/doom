;; -*- lexical-binding: t -*-

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat nil
        nyan-wavy-trail t
        nyan-bar-length 16
        nyan-minimum-window-width 50
        ;; nyancat is too big
        nyan-cat-image
        '(image :type xpm :file
          "/home/breitnw/.config/emacs/.local/straight/build-30.1/nyan-mode/img/nyan.xpm"
          :scale 0.9 :ascent center :background "#303030")))

(setq doom-base16-padded-modeline t
      doom-themes-padded-modeline t
      doom-modeline-height 20
      doom-modeline-position-column-line-format '("%3l:%c"))

;; HACK to fix solaire-mode issue
;; switching from a solaire window to a non-solaire window doesn't seem to reset
;; the mode line face properly. to fix this, I'm just setting the solaire mode
;; line face to inherit from the normal mode line face.
(custom-set-faces!
  `(solaire-mode-line-face
    :box unspecified
    :background unspecified
    :inherit mode-line))
