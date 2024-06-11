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
  :init (ace-window-display-mode 1)
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setq aw-background t)
  (setq aw--lead-overlay-fn 'ignore)

  (defun panimacs/select-window (index)
    ;; TODO: make sure it always matches with number:
    (aw-switch-to-window (nth index (aw-window-list))))

  (defvar-keymap panimacs/ace-winum-keymap
    :doc "Keymap for winum-mode actions."
    "M-0" (lambda () (interactive) (panimacs/select-window 0))
    "M-1" (lambda () (interactive) (panimacs/select-window 1))
    "M-2" (lambda () (interactive) (panimacs/select-window 2))
    "M-3" (lambda () (interactive) (panimacs/select-window 3))
    "M-4" (lambda () (interactive) (panimacs/select-window 4))
    "M-5" (lambda () (interactive) (panimacs/select-window 5))
    "M-6" (lambda () (interactive) (panimacs/select-window 6))
    "M-7" (lambda () (interactive) (panimacs/select-window 7))
    "M-8" (lambda () (interactive) (panimacs/select-window 8))
    "M-9" (lambda () (interactive) (panimacs/select-window 9))
    )

  (define-minor-mode panimacs/ace-winum-mode
    "Switch to windows by pressing M-<number>"
    :global t :keymap panimacs/ace-winum-keymap)

  (panimacs/ace-winum-mode 1)

  (global-set-key (kbd "C-o") #'ace-window)
  (evil-global-set-key 'normal (kbd "C-o") #'ace-window)
  (evil-global-set-key 'motion (kbd "C-o") #'ace-window)

  (defun other-window-mru ()
    "Select the most recently used window on this frame."
    (interactive)
    (when-let ((mru-window
                (get-mru-window
                 nil nil 'not-this-one-dummy)))
      (select-window mru-window)))

  (advice-add 'other-window-mru :before
              (defun other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly))))

  (keymap-global-set "M-o" 'other-window-mru)
  (evil-global-set-key 'normal (kbd "M-o") 'other-window-mru)
  (evil-global-set-key 'motion (kbd "M-o") 'other-window-mru)

  (defun panimacs/ace-window--wrapped-aw-select (action)
    (if action
        (funcall action (selected-window))))

  (defvar panimacs/ace-window--aw-dispatch-alist-current-window
    (cl-loop for (symbol action description)
             in aw-dispatch-alist
             collect
             (remove
              nil
              (list symbol
                    (if description
                        `(lambda ()
                           (panimacs/ace-window--wrapped-aw-select ',action))
                      action)
                    (if description (concat description " (current window)") nil)))
             )
    )

  (defun panimacs/ace-window-current ()
    (interactive)

    (let ((aw-dispatch-alist panimacs/ace-window--aw-dispatch-alist-current-window)
          (aw-dispatch-function
           (lambda (char)
             (funcall (cadr
                       (assoc char panimacs/ace-window--aw-dispatch-alist-current-window))))))
      (ignore-errors (ace-window 1))
      )
    )

  (keymap-global-set "C-S-o" #'panimacs/ace-window-current)
  (evil-global-set-key 'normal (kbd "C-S-o") #'panimacs/ace-window-current)
  (evil-global-set-key 'motion (kbd "C-S-o") #'panimacs/ace-window-current)
  
)


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


