
;; cursor motion ------------------------------------------------

;; avy: search for characters on the screen
(after! avy
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h
                   ?w ?f ?p ?l ?u ?y)))

;; evil-snipe is used solely for one-character motions
(after! evil-snipe
  (setq evil-snipe-scope 'visible))

;; frame motion (scrolling) -------------------------------------

;; better pixel scrolling with ultra-scroll
(use-package! ultra-scroll
  :init
  (setq auto-hscroll-mode t
        scroll-margin 0
        scroll-conservatively 101
        scroll-down-aggressively nil
        scroll-up-aggressively nil
        scroll-step 0)
  :config
  (ultra-scroll-mode 1))

;; big-scroll-up and big-scroll-down are used because pixel scrolling magically fixes
;; the artifacting bugs when using gpu acceleration

(defun pages->pixels (num-pages)
  ;; don't convert directly, since we want the value to be a whole number of lines
  (* (line-pixel-height) (truncate (* (window-height) num-pages))))

(evil-define-command big-scroll-up ()
  "scrolls the frame up 0.25 pages"
  (pixel-scroll-precision-scroll-up (pages->pixels 0.25)))

(evil-define-command big-scroll-down ()
  "scrolls the frame down 0.25 pages"
  (pixel-scroll-precision-scroll-down-page (pages->pixels 0.25)))

