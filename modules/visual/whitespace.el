;; whitespace rendering
(defvar whitespace-columns
  '((rust-mode . 100)
    (java-mode . 100)
    (c-mode . 100)
    (c++-mode . 100)
    (python-mode . 100)
    (emacs-lisp-mode . 80)
    (haskell-mode . 80)))

(defun add-whitespace-hooks (col-alist)
  (when (consp col-alist)
    (let* ((mode (caar col-table))
           (cols (cadr col-table))
           (rest (cdr col-table)))
      (add-hook! mode
        (setq-local
         ;; the line after which characters will appear red
         whitespace-line-column nil
         ;; the line that the fill indicator should appear
         ;; at, if applicable
         fill-column cols))
      (add-whitespace-hooks rest))))

;; (use-package! whitespace
;;   :defer t
;;   :custom
;;   (whitespace-style '(face
;;                       tabs
;;                       tab-mark
;;                       trailing
;;                       lines-tail
;;                       space-before-tab
;;                       indentation
;;                       empty
;;                       space-after-tab
;;                       missing-newline-at_eof))
;;   (whitespace-global-modes (mapcar car whitespace-columns))
;;   :config
;;   (configure-whitespace whitespace-columns)
;;   (global-whitespace-mode))
