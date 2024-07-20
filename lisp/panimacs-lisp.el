;;; panimacs-modal-editing.el --- panimacs :: editing lisp-like languages -*- lexical-binding: t -*-

(require 'panimacs-packages)

(use-package lispyville
  :init
  (add-hook 'lisp-mode-hook #'lispyville-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators additional additional-wrap text-objects slurp/barf-lispy))
  )

(provide 'panimacs-lisp)
