;;; panimacs-windows.el --- panimacs :: managing windows -*- lexical-binding: t -*-

(require 'panimacs-packages)

(use-package ace-window
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(global-set-key (kbd "C-o") 'ace-window)
(evil-global-set-key 'normal (kbd "C-o") 'ace-window)

(winner-mode 1)

(global-set-key (kbd "C-x C-j") #'winner-undo)
(global-set-key (kbd "C-x C-k") #'winner-redo)


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

(define-key embark-file-map (kbd "V") 'panimacs/image-display-external)

(provide 'panimacs-windows)


