;;; -*- lexical-binding: t -*-

(defun panimacs/open-config ()
  "Open panimacs config directory, let user select file."

  (interactive)
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

(global-set-key (kbd "C-x RET c") #'panimacs/open-config)

(provide 'panimacs-config)
