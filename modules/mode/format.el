;; -*- lexical-binding: t -*-

;; code formatting

(after! apheleia
  ;; TeX ----------------------------------------
  ;; prefer spaces instead of tabs
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent"
          "-y=defaultIndent:'  '"
          "--logfile=/dev/null"))
  ;; Rust ---------------------------------------
  ;; set max line width
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt"
          "--quiet"
          "--emit"
          "stdout"
          "--edition"
          "2021"
          "--config"
          (format "max_width=%d"
                  (alist-get 'rustic-mode fill-column-settings)))))
