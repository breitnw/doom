;; whitespace rendering
;; make sure to turn off whitespace rendering in org, since we want to be able
;; to have lines of any length
(setq whitespace-columns
      '(rust-mode 100
        java-mode 100
        c-mode 100
        c++-mode 100
        python-mode 100
        elisp-mode 80
        haskell-mode 80))

(defun whitespace-modes (col-table)
  (when (consp col-table)
    (let* ((mode (car col-table))
           (rest (cddr col-table)))
      (cons mode (whitespace-modes rest)))))

(defun add-whitespace-hooks (col-table)
  (when (consp col-table)
    (let* ((mode (car col-table))
           (cols (cdar col-table))
           (rest (cddr col-table)))
      (progn (add-hook mode
                       (lambda ()
                         (setq-local
                          ;; the line after which characters will appear red
                          whitespace-line-column cols
                          ;; the line that the fill indicator should appear
                          ;; at, if applicable
                          fill-column cols)))
             (add-whitespace-hooks rest)))))

(after! whitespace
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
  (setq-default whitespace-global-modes (whitespace-modes whitespace-columns))
  (global-whitespace-mode))

;; TODO properly configure whitespace
;; (configure-whitespace whitespace-columns))
