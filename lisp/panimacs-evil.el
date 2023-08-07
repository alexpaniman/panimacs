;;; panimacs-evil.el --- panimacs :: evil integration -*- lexical-binding: t -*-


(require 'panimacs-packages)


(use-package evil
  :init
  (setq evil-want-integration        t
	evil-undo-system      'undo-fu

	evil-want-C-w-delete       nil

	evil-want-C-u-scroll         t
	evil-want-C-i-jump         nil
	)

  :hook (prog-mode . evil-mode)
  )


(provide 'panimacs-evil)
