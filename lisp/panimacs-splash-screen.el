;;; -*- lexical-binding: t -*-


(defvar panimacs/splash-screen-image
  (create-image (expand-file-name "panimacs-logo.svg" user-emacs-directory)))

(defvar panimacs/splash-screen-buffer-name "*panimacs*")


(defun panimacs/splash-screen-get-buffer ()
  (get-buffer-create panimacs/splash-screen-buffer-name))

(defun panimacs/enable-splash-screen () 
  (interactive)

  (with-current-buffer (panimacs/splash-screen-get-buffer)
    (panimacs/splash-screen-mode)

    (panimacs/splash-screen-resize-image)
    (add-hook
     'window-configuration-change-hook
     #'panimacs/splash-screen-resize-image)))
      
(define-derived-mode panimacs/splash-screen-mode special-mode
  "Panimacs"
  "Major mode for Panimacs splash screen."

  :syntax-table nil :abbrev-table nil (font-lock-mode 1))

(defun panimacs/splash-screen-resize-image ()
  (with-current-buffer (panimacs/splash-screen-get-buffer)
    (let* ((img-size (image-size panimacs/splash-screen-image))
	   (img-height (ceiling (cdr img-size)))
	   (img-width  (ceiling (car img-size)))

	   (win (get-buffer-window))
	   (win-height (window-height win))
	   (win-width  (window-width  win))

	   (title
	    (concat
	     (propertize "p"    'font-lock-face '(:foreground "#f66774" :weight bold))
	     (propertize "a"    'font-lock-face '(:foreground "#bf6281" :weight bold))
	     (propertize "n"    'font-lock-face '(:foreground "#f5d662" :weight bold))
	     (propertize "i"    'font-lock-face '(:foreground "#c8a6dd" :weight bold))
	     (propertize "macs" 'font-lock-face '(:foreground "#88acce" :weight bold))))

	   (title-length (length title))

           (setup-padding
            (lambda (total-size centered-size win)
              (when (> total-size centered-size)
                (when (window-live-p win)
                  (let ((padding-side-width (/ (max 0 (- (window-total-width win) (floor centered-size))) 2)))
                    (set-window-start win 0)
                    (set-window-fringes win 0 0)
                    (set-window-margins win padding-side-width))))))

	   (insert-padding
            (lambda (total-size centered-size padding-char)
	      (when (> total-size centered-size)
		(insert (make-string
			 (floor (/ (- total-size centered-size) 2)) padding-char)))))

	   (inhibit-read-only t))

      (erase-buffer)
      (insert (make-string 3 ?\n))

      (insert-image panimacs/splash-screen-image)
      (when-let (windows (get-buffer-window-list (panimacs/splash-screen-get-buffer) nil t))
        (dolist (win windows)
          (funcall setup-padding win-width img-width win))
        )

      (insert (make-string 3 ?\n))

      (funcall insert-padding img-width title-length ?\ )
      (insert title))))


;; Install splash screen, automatically show it in new frames:

(setq initial-buffer-choice #'panimacs/splash-screen-get-buffer)
(add-hook 'emacs-startup-hook           #'panimacs/enable-splash-screen)
(add-hook 'server-after-make-frame-hook #'panimacs/enable-splash-screen)

(provide 'panimacs-splash-screen)
