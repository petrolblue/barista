;;; haskell -*- lexical-binding: t; -*-


; Haskell
(require 'hindent)
(require 'hs-lint)

(use-package haskell-mode
  :hook ((haskell-mode . haskell-indent-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . hindent-mode))
  :bind (("C-c C-l" . haskell-process-load-or-reload))
  :config
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-log t))

(provide '+haskell)
