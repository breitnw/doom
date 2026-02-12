;;; modules/editor/motion.el -*- lexical-binding: t; -*-

;; (use-package! evil-textobj-tree-sitter
;;   :config
;;   ;; TODO
;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;;   ;; You can also bind multiple items and we will match the first one we can find
;;   (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;; frame motion (scrolling) -------------------------------------

;; don't accelerate when scrolling quickly
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 101)

;; (setq mwheel-scroll-up-function #'evil-scroll-down)
;; (setq mwheel-scroll-down-function #'evil-scroll-up)


;; big-scroll-up and big-scroll-down are used because pixel scrolling magically fixes
;; the artifacting bugs when using gpu acceleration

(defun pages->lines (num-pages)
  (truncate (* (window-height) num-pages)))

(defun pages->pixels (num-pages)
  ;; don't convert directly, since we want the value to be a whole number of lines
  (* (line-pixel-height) (pages->lines num-pages)))

(defvar big-scroll-pages 0.25)

(after! evil
  (evil-define-motion big-scroll-up ()
    "scrolls the frame up 0.25 pages"
    (evil-previous-visual-line (pages->lines big-scroll-pages))
    ;; (pixel-scroll-precision-scroll-up-page (pages->pixels big-scroll-pages))
    )
  (evil-define-motion big-scroll-down ()
    "scrolls the frame down 0.25 pages"
    (evil-next-visual-line (pages->lines big-scroll-pages))
    ;; (pixel-scroll-precision-scroll-down-page (pages->pixels big-scroll-pages))
    ))
