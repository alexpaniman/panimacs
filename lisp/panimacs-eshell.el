
(setq eshell-buffer-shorthand t)

(defun eshell/-buffer-as-args (buffer separator command)
  "Takes the contents of BUFFER, and splits it on SEPARATOR, and
runs the COMMAND with the contents as arguments. Use an argument
`%' to substitute the contents at a particular point, otherwise,
they are appended."
  (let* ((lines (with-current-buffer buffer
                  (split-string
                   (buffer-substring-no-properties (point-min) (point-max))
                   separator)))
         (subcmd (if (-contains? command "%")
                     (-flatten (-replace "%" lines command))
                   (-concat command lines)))
         (cmd-str  (string-join subcmd " ")))
    (message cmd-str)
    (eshell-command-result cmd-str)))

(defun eshell/bargs (buffer &rest command)
  "Passes the lines from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer "\n" command))

(defun eshell/sargs (buffer &rest command)
  "Passes the words from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer nil command))

(setq eshell-destroy-buffer-when-process-dies t)

(setq eshell-up-ignore-case nil)
(setq eshell-up-print-parent-dir t)

(use-package aweshell
  :straight (abc-mode :fetcher github :repo "manateelazycat/aweshell"))

(unbind-key "C-x e")
(global-set-key (kbd "C-x e e") #'aweshell-new)
(global-set-key (kbd "C-x e k") #'aweshell-prev)
(global-set-key (kbd "C-x e j") #'aweshell-next)
(define-key eshell-mode-map (kbd "C-l") #'aweshell-clear-buffer)
(define-key eshell-mode-map (kbd "C-s") #'aweshell-sudo-toggle)
(define-key eshell-mode-map (kbd "C-b") #'aweshell-switch-buffer)


(evil-define-key 'insert eshell-mode-map (kbd "C-d") #'kill-buffer-and-window)

(defun eshell/e (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (let ((win (get-lru-window))
          (files (mapcar
                  #'expand-file-name
                  (eshell-flatten-list (reverse args)))))

      (if win (mapc
               (lambda (buffer) (set-window-buffer win buffer))
               (mapcar #'find-file-noselect files))
        (mapc #'find-file files)))
    ))

(defun eshell/clear (&rest args)
  (aweshell-clear-buffer))

