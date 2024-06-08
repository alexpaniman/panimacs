;;; panimacs-perfomance.el --- panimacs :: perfomance hacks to make emacs more responsive -*- lexical-binding: t -*-

(require 'panimacs-packages)


(use-package gcmh
  :init (gcmh-mode 1))

(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.2)
(setq jit-lock-chunk-size 4096)

(provide 'panimacs-performance)
