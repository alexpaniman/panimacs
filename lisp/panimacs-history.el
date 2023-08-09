;;; panimacs-history.el --- panimacs :: editing history -*- lexical-binding: t -*-


(require 'panimacs-packages)


(use-package undo-fu
  :config 
  (setq undo-limit            67108864 ;  64mb
        undo-strong-limit    100663296 ;  96mb
	undo-outer-limit    1006632960 ; 960mb
	)
  )


(use-package undo-fu-session
  :after undo-fu
  :init (undo-fu-session-global-mode 1)
  )


(use-package vundo
  :commands (vundo)

  :straight (vundo :type git :host github :repo "casouri/vundo")

  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "JetBrains Mono")

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l"	   ) #'vundo-forward  )
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward  )
  (define-key vundo-mode-map (kbd "h"	   ) #'vundo-backward )
  (define-key vundo-mode-map (kbd "<left>" ) #'vundo-backward )
  (define-key vundo-mode-map (kbd "j"	   ) #'vundo-next     )
  (define-key vundo-mode-map (kbd "<down>" ) #'vundo-next     )
  (define-key vundo-mode-map (kbd "k"	   ) #'vundo-previous )
  (define-key vundo-mode-map (kbd "<up>"   ) #'vundo-previous )
  (define-key vundo-mode-map (kbd "<home>" ) #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>"  ) #'vundo-stem-end )
  (define-key vundo-mode-map (kbd "q"	   ) #'vundo-quit     )
  (define-key vundo-mode-map (kbd "C-g"	   ) #'vundo-quit     )
  (define-key vundo-mode-map (kbd "RET"	   ) #'vundo-confirm  )
  )


;; Save list of recent files
(recentf-mode 1)
(global-set-key (kbd "C-c r") #'consult-recent-file)


(provide 'panimacs-history)
