;;; init.el --- panimacs :: alexpaniman's emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex Paniman

;; Author: Alex Paniman <alexpaniman@gmail.com>
;; Created: 28 May 2023

;; Keywords: panimacs emacs-configuration

;; ================ SETUP SEARCH OF LISP FILES ================

;; Use "<user-emacs-directory>/lisp/" directory to store parts
;; of the configuration. Add all subdirectories with .el files
;; there and add them to load-path:
(let* ((path (expand-file-name "lisp" user-emacs-directory))
       (local-pkgs
	(mapcar 'file-name-directory
		(directory-files-recursively path "\\.el$"))))
  (if (file-accessible-directory-p path)
      (mapc
       (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

(require 'panimacs-packages)

(use-package no-littering)
(require 'no-littering)

;; ======================= GUI DEFAULTS =======================
(setq panimacs/font                "JetBrains Mono"
      panimacs/variable-pitch-font "Fira Sans"
      panimacs/serif-font          "Iosevka Comfy Motion"
      panimacs/unicode-font        "Noto Color Emoji"

      panimacs/default-font-size   20

      panimacs/default-color-theme 'doom-old-hope
      )


;; ==> Apply settings:

(require 'panimacs-ui-defaults)
(require 'panimacs-setup-fonts)
(require 'panimacs-themes)

;; ========================== SPLASH ==========================

;; Load and enable splash screen with panimacs name and logo
;; that will automatically show in every launched frame:
(require 'panimacs-splash-screen)


;; ======================= TUNE EDITING ======================= 

;; TODO: Work in progress, should replace evil when completed!
;; (require 'panimacs-modal-editing)

;; For now, use evil for modal editing instead:
(require 'panimacs-evil)

 ;; Enable editing history recording, saving and easy querying:
(require 'panimacs-history)

(require 'panimacs-completion)

(require 'panimacs-lsp)

(require 'panimacs-config)

(require 'panimacs-windows)

(require 'panimacs-vcs)

(require 'panimacs-bindings)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit default :background "gray" :weight bold)))))
