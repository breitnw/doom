;;; modules/mode/format.el -*- lexical-binding: t; -*-

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
                  (alist-get 'rustic-mode fill-column-settings))))
  ;; Nix ----------------------------------------
  ;; use alejandra instead of nixfmt
  (setf (alist-get 'nixfmt apheleia-formatters)
        '("alejandra"))

  ;; Haskell ------------------------------------
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu)
  (setf (alist-get 'haskell-ts-mode apheleia-mode-alist) 'ormolu))
