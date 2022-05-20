;; barista -*- lexical-binding: t; -*-
(require 'barista
         (concat user-emacs-directory "core/barista"))

;; packages
(barista/init
  +barista-theme
  +numbers
  +parentheses
  +company
  +buffer-move
  +ctrls
  +clojure
  +flycheck
  +c
  +meow
  +haskell
  ;;+cl
  )
