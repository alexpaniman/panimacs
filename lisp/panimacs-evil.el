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
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
              ("TAB" . nil)

         )
  )

(use-package evil-collection
  :after evil-mode
  :ensure t
  :config
  (evil-collection-init))

(evil-set-initial-state 'pdf-view-mode 'normal)



(provide 'panimacs-evil)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; (use-package evil-tex
;;   :ensure t)

;; (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
;; (add-hook 'org-mode-hook #'evil-tex-mode)


(global-set-key (kbd "C-x :") 'eval-expression)

(setq whitespace-style '(face tabs spaces space-mark tab-mark trailing indentation))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))

;; (setq whitespace-display-mappings
;;   '((tab-mark 9 [124 9] [92 9])))


;;(use-package org-evil
;;  :config
;;  (add-hook 'org-mode-hook (lambda ()
;;                             (org-evil-mode +1)))
;;  )

(global-set-key (kbd "C-x W") 'whitespace-mode)
