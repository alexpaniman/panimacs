;;; panimacs-evil.el --- panimacs :: evil integration -*- lexical-binding: t -*-


(require 'panimacs-packages)


(use-package evil
  :init
  (setq evil-want-integration        t
	evil-undo-system      'undo-fu
	evil-want-keybinding       nil

	evil-want-C-w-delete       nil

	evil-want-C-u-scroll         t
	evil-want-C-i-jump         nil
	)

  :config (evil-mode 1)
  )

(use-package evil-collection
  :after evil-mode
  :ensure t
  :config
  (evil-collection-init))


(provide 'panimacs-evil)
