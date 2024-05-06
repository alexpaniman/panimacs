;;; panimacs-windows.el --- panimacs :: managing windows -*- lexical-binding: t -*-

(require 'panimacs-packages)

(use-package popper
  :ensure t ; or :straight t
  :bind (("M-j"   . popper-cycle)
         ("M-k"   . popper-toggle)
         ("M-S-j" . popper-toggle-type)
         ("M-S-k" . popper-kill-latest-popup))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-group-function #'popper-group-by-project)
  (setq popper-display-function #'display-buffer-in-side-window)
  (setq popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  (setq popper-display-control nil)
  )

;; Setup display-buffer-alist for different windows
(add-to-list 'display-buffer-alist
            '("\\*e?shell\\*"
                (display-buffer-in-side-window)
                (side . bottom)
                (slot . -1) ;; -1 == L  0 == Mid 1 == R
                (window-height . 0.33) ;; take 2/3 on bottom left
                (window-parameters
                (no-delete-other-windows . nil))))

(add-to-list 'display-buffer-alist
            '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\)\\*"
                (display-buffer-in-side-window)
                (side . bottom)
                (slot . 0)
                (window-height . 0.33)
                (window-parameters
                (no-delete-other-windows . nil))))

(add-to-list 'display-buffer-alist
            '("\\*\\([Hh]elp\\|Command History\\|command-log\\)\\*"
                (display-buffer-in-side-window)
                (side . right)
                (slot . 0)
                (window-width . 80)
                (window-parameters
                (no-delete-other-windows . nil))))

(add-to-list 'display-buffer-alist
            '("\\*TeX errors\\*"
                (display-buffer-in-side-window)
                (side . bottom)
                (slot . 3)
                (window-height . shrink-window-if-larger-than-buffer)
                (dedicated . t)))

(add-to-list 'display-buffer-alist
            '("\\*TeX Help\\*"
                (display-buffer-in-side-window)
                (side . bottom)
                (slot . 4)
                (window-height . shrink-window-if-larger-than-buffer)
                (dedicated . t)))


(use-package ace-window
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(global-set-key (kbd "C-o") 'ace-window)
(evil-global-set-key 'normal (kbd "C-o") 'ace-window)

(winner-mode 1)

(global-set-key (kbd "C-x C-j") #'winner-undo)
(global-set-key (kbd "C-x C-k") #'winner-redo)



(with-eval-after-load 'embark
  (defun panimacs/image-display-external (file)
    "Display file at point using an external viewer.
The viewer is specified by the value of `image-dired-external-viewer'."

    ;; TODO: For now it doesn't delete output file when image viewer is closed,
    ;;       probably should make it do so.

    (interactive "fSelect image: ")
    (let* ((local-file (if (not (file-remote-p file)) file
			 (let ((temp-image-file
				(make-temp-file "/tmp/panimacs-remote-image-copy" nil (format ".%s" (file-name-extension file)))))
                           (copy-file file temp-image-file :override-if-exists)
                           temp-image-file)
			 )
                       )
           )

      (start-process "image-dired-external" nil
                     image-dired-external-viewer local-file)))

  (define-key embark-file-map (kbd "V") 'panimacs/image-display-external))

(provide 'panimacs-windows)


