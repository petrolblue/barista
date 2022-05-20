;;; dark-theme -*- lexical-binding: t; -*-

(require 'barista-base-colors)

(defun barista-theme-set-dark ()
  (setq frame-background-mode    'dark)
  (setq barista-color-foreground "#fdf4c1")
  (setq barista-color-strong     "#fdf4c1")
  (setq barista-color-background "#1d2021")
  (setq barista-color-highlight  "#1d2021")
  (setq barista-color-critical   "#fb4933")
  (setq barista-color-salient    "#fb4933")
  (setq barista-color-popout     "#b8bb26")
  (setq barista-color-subtle     "#333637")
  (setq barista-color-faded      "#777979")
  (setq barista-color-region     "#333637")
  
  ;; programming
  (setq barista-color-numbers    "#f0f1d3")
  (setq barista-color-function   "#211e1d")
  (setq barista-color-comment    "#17191a"))



(barista-theme-set-dark)

(provide 'barista-theme-dark)
