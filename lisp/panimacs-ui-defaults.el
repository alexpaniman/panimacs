;;; -*- lexical-binding: t -*-

(setq initial-scratch-message nil
      inhibit-startup-screen    t
      inhibit-startup-message   t

      default-frame-alist
      '((vertical-scroll-bars  . nil)
	(internal-border-width .   2)
	(left-fringe           .  10)
	(right-fringe          .  10)
	(tool-bar-lines        .   0)
	(menu-bar-lines        .   0))
      )


(defun panimacs/enable-line-numbers ()
  (interactive)
  (setq-local display-line-numbers 'relative))

(add-hook 'prog-mode-hook 'panimacs/enable-line-numbers)

(setq native-comp-async-report-warnings-errors nil)

(require 'panimacs-packages)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.7))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'panimacs-ui-defaults)
