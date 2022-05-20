;; load-theme -*- lexical-binding: t; -*-

(require 'barista-layout)

(setq barista-font-family-monospaced "PragmataPro Mono Liga 1.4"
          barista-font-size 13)

(add-to-list 'command-switch-alist '("--dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("--light"  . (lambda (args))))

(cond
 ((member "--default" command-line-args) t)
 ((member "--dark" command-line-args) (require 'barista-theme-dark))
 (t (require 'barista-theme-light)))

(require 'barista-faces)
(barista-faces)

(require 'barista-theme)
(barista-theme)

(require 'barista-modeline)
(require 'barista-bindings)
(require 'barista-defaults)
(require 'barista-command)
(require 'barista-writer)

(provide '+barista-theme)
