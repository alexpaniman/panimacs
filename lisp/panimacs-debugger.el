;;; panimacs-evil.el --- panimacs :: debugger integration -*- lexical-binding: t -*-


(defun panimacs/custom-gdb-setup-windows ()
  "Lay out the window pattern for option `gdb-many-windows'."
  (if gdb-default-window-configuration-file
      (gdb-load-window-configuration
       (if (file-name-absolute-p gdb-default-window-configuration-file)
           gdb-default-window-configuration-file
         (expand-file-name gdb-default-window-configuration-file
                           gdb-window-configuration-directory)))
    ;; Create default layout as before.
    ;; Make sure that local values are updated before locals.
    (gdb-get-buffer-create 'gdb-locals-values-buffer)
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (gdb-get-buffer-create 'gdb-stack-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)


    (let* ((win-source (split-window nil (/ (* (window-width ) 2) 5) 'left))

           (height (window-height))
           (win-io     (split-window win-source (/ (* height 4) 5) 'below))

           (win-locals (split-window nil (/ (* height 1) 2) 'above))
           (win-frames (selected-window))
           )

      (gdb-set-window-buffer (gdb-locals-buffer-name) nil win-locals)

      (set-window-buffer win-source (or (gdb-get-source-buffer) (list-buffers-noselect)))
      (setq gdb-source-window-list (list win-source))

      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win-io)
      (gdb-set-window-buffer (gdb-stack-buffer-name) nil win-frames)

      (select-window win-source)
      )
    )
  )

(advice-add 'gdb-setup-windows :override #'panimacs/custom-gdb-setup-windows)
(setq gdb-many-windows t)




(provide 'panimacs-debugger)
