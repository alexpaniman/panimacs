;;; -*- lexical-binding: t -*-


(require 'cl-lib)


(defvar panimacs/font)
(defvar panimacs/variable-pitch-font)
(defvar panimacs/serif-font)
(defvar panimacs/unicode-font)

(defvar panimacs/default-font-size nil)
(defvar panimacs/font-increment 2)

(defvar panimacs/font-size nil)


(defun panimacs/init-fonts--apply-defaults (font-name)
  (font-spec :family font-name :size panimacs/font-size))

(defun panimacs/init-fonts ()
  (when (null panimacs/font-size)
    (setq panimacs/font-size panimacs/default-font-size))

  (dolist (map `((default           . ,panimacs/font               )
		 (fixed-pitch       . ,panimacs/font               )
		 (fixed-pitch-serif . ,panimacs/serif-font         )
		 (variable-pitch    . ,panimacs/variable-pitch-font)))

    (when-let* ((face (car map)) (font-name (cdr map)))
      (dolist (frame (frame-list))
	(when (display-multi-font-p frame)
	  (set-face-attribute face frame
	   :width 'normal :weight 'normal :slant 'normal
	   :font (panimacs/init-fonts--apply-defaults font-name))))))

  (when (and (fboundp 'set-fontset-font)  panimacs/unicode-font)
    (set-fontset-font t 'unicode
     (panimacs/init-fonts--apply-defaults panimacs/unicode-font))))


(defun panimacs/adjust-font-size (delta)
  (setq panimacs/font-size (+ panimacs/font-size delta))
  (panimacs/init-fonts))

(defun panimacs/increase-font-size (count &optional increment)
  "Enlargens the font size across the current and child frames."
  (interactive "p")
  (panimacs/adjust-font-size (* count (or increment panimacs/font-increment))))

(defun panimacs/decrease-font-size (count &optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (panimacs/adjust-font-size (* (- count) (or increment panimacs/font-increment))))

(defun panimacs/restore-font-size ()
  "Shrinks the font size across the current and child frames."
  (interactive)
  (panimacs/adjust-font-size (- panimacs/default-font-size panimacs/font-size)))


(global-set-key (kbd "C-M-=") #'panimacs/increase-font-size)
(global-set-key (kbd "C-M--") #'panimacs/decrease-font-size)
(global-set-key (kbd "C-M-0") #'panimacs/restore-font-size)


(let ((hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)))
  (add-hook hook #'panimacs/init-fonts))


(provide 'panimacs-setup-fonts)
