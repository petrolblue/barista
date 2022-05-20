;;; -*- lexical-binding: t; -*-

(require 'barista-base-colors)

(defun barista-theme-set-light ()
  (setq frame-background-mode    'light)
  
  ;; basic colors
  (setq barista-color-background "#f4f4f4")
  (setq barista-color-foreground "#161616")
  (setq barista-color-highlight  "#f4f4f4")
  (setq barista-color-strong     "#525252")
  (setq barista-color-critical   "#F2C6EC")
  (setq barista-color-salient    "#EEC8EC")
  (setq barista-color-popout     "#EEC8EC")
  (setq barista-color-subtle     "#e0e0e0") 
  (setq barista-color-faded      "#8d8d8d")
  (setq barista-color-region     "#ffd6e8")
  
  ;; programming
  (setq barista-color-numbers    "#491d8b")
  (setq barista-color-function   "#C7E4DF")
  (setq barista-color-comment    "#F7EB9F"))

(barista-theme-set-light)

(provide 'barista-theme-light)

;;; barista-theme-light.el ends here
