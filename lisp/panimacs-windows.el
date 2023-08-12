;;; panimacs-windows.el --- panimacs :: managing windows -*- lexical-binding: t -*-

(require 'panimacs-packages)

(use-package ace-window
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(global-set-key (kbd "M-o") 'ace-window)

(winner-mode 1)
(global-set-key (kbd "C-c C-k") 'winner-redo)
(global-set-key (kbd "C-c C-j") 'winner-undo)


(provide 'panimacs-windows)


