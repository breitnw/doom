;; -*- lexical-binding: t -*-

;; cursor motion ------------------------------------------------

;; avy: search for characters on the screen
(use-package! avy
  :defer t
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?d ?h
              ?w ?f ?p ?l ?u ?y)))

;; frame motion (scrolling) -------------------------------------

;; don't accelerate when scrolling quickly
(setq mouse-wheel-progressive-speed nil)

;; big-scroll-up and big-scroll-down are used because pixel scrolling magically fixes
;; the artifacting bugs when using gpu acceleration

(defun pages->lines (num-pages)
  (truncate (* (window-height) num-pages)))

(defun pages->pixels (num-pages)
  ;; don't convert directly, since we want the value to be a whole number of lines
  (* (line-pixel-height) (pages->lines num-pages)))

(defvar big-scroll-pages 0.25)

(evil-define-command big-scroll-up ()
  "scrolls the frame up 0.25 pages"
  (pixel-scroll-precision-scroll-up (pages->pixels big-scroll-pages)))

(evil-define-command big-scroll-down ()
  "scrolls the frame down 0.25 pages"
  (pixel-scroll-precision-scroll-down-page (pages->pixels big-scroll-pages)))

