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


(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
	 (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) "")) right)))

(setq-default mode-line-format
            '((:eval (simple-mode-line-render
         		;; left aligned
         		(quote
         		 ("%e"
         		  mode-line-front-space
         		  mode-line-mule-info
         		  " "
         		  mode-line-client
         		  mode-line-modified
         		  mode-line-remote
         		  mode-line-frame-identification
         		  mode-line-buffer-identification
         		  " [%*]"
			  mode-line-misc-info
         		  )
         		 )


         		;; right aligned
         		(quote ("%l:%c" " " mode-name " ")))
         	       ))
	    )

(setq backup-directory-alist '((".*" . "~/.trash/")))
(setq create-lockfiles nil) 


(provide 'panimacs-ui-defaults)
