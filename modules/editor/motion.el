
;; cursor motion ------------------------------------------------

;; avy: search for characters on the screen
(after! avy
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h
                   ?w ?f ?p ?l ?u ?y)))

;; evil-snipe is used solely for one-character motions
(after! evil-snipe
  (setq evil-snipe-scope 'visible))

;; frame motion (scrolling) -------------------------------------

;; configure scrolling behavior
(setq auto-hscroll-mode t
      scroll-conservatively 5
      scroll-margin 2
      scroll-down-aggressively nil
      scroll-up-aggressively nil
      scroll-step 0)

;; big-scroll-up and big-scroll-down are used because pixel scrolling magically fixes
;; the artifacting bugs when using gpu acceleration
(pixel-scroll-precision-mode)

(defun pages->pixels (num-pages)
  ;; don't convert directly, since we want the value to be a whole number of lines
  (* (line-pixel-height) (truncate (* (window-height) num-pages))))

(evil-define-command big-scroll-up ()
  "scrolls the frame up 0.25 pages"
  (pixel-scroll-precision-scroll-up (pages->pixels 0.25)))

(evil-define-command big-scroll-down ()
  "scrolls the frame down 0.25 pages"
  (pixel-scroll-precision-scroll-down-page (pages->pixels 0.25)))

;; (defvar big-scroll-fringe-size 5
;;   "The allowed size of the fringe, in lines, when using big-scroll functions")

;; FIXME attempt to clamp scrolling so that it doesn't jump at bottom; doesn't work
;;       for some reason, (window-end) and (cdr buffer-text-pixel-size) don't seem to be
;;       in the same units (or something like that)
;;       this is also necessary to prevent corruption as we approach the bottom!
;; (evil-define-command big-scroll-down ()
;;   "scrolls the frame down 0.25 pages"
;;   (let* ((scroll-amt (pages->pixels 0.25))
;;          (window-end-after-move (+ (window-end) scroll-amt))
;;          (buffer-bottom (cdr (buffer-text-pixel-size)))
;;          (true-scroll-amt (if (> window-bottom-after-move buffer-bottom)
;;                               (- buffer-bottom window-bottom)
;;                             scroll-amt)))
;;     (pixel-scroll-precision-scroll-down-page true-scroll-amt)))
