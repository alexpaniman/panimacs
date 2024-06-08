;;; -*- lexical-binding: t -*-

(setq initial-scratch-message nil
      inhibit-startup-screen    t
      inhibit-startup-message   t

      use-short-answers         t

      default-frame-alist
      '((vertical-scroll-bars  . nil)
	(internal-border-width .   2)
	(left-fringe           .   8)
	(right-fringe          .   8)
	(tool-bar-lines        .   0)
	(menu-bar-lines        .   0))
      )

;; Very important, disable insanely annoying bell sound!
(setq ring-bell-function 'ignore)


(defun panimacs/enable-line-numbers ()
  (interactive)
  (setq-local display-line-numbers 'visual))

(add-hook 'prog-mode-hook 'panimacs/enable-line-numbers)
(add-hook  'org-mode-hook 'panimacs/enable-line-numbers)

(setq native-comp-async-report-warnings-errors nil)

(require 'panimacs-packages)

;; By default emacs 
(use-package good-scroll
  :ensure t
  :init
  (good-scroll-mode 1))


(use-package helpful
  :bind
  (([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable))

  :config
  ;; I really want helpful to stop "helping" immediately when I type "q", PLEASE
  (evil-define-key 'normal helpful-mode-map (kbd "q") #'quit-window)
  (evil-define-key 'insert helpful-mode-map (kbd "q") #'quit-window)
  )

(use-package popwin
  :config (popwin-mode 1))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.7))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom    
  (doom-modeline-height 20)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil)
  (doom-modeline-hud nil))


(setq backup-directory-alist '((".*" . "~/.trash/")))
(setq create-lockfiles nil)


;; TODO: update docs
(defun panimacs/save-buffer+delete-frame (fun)
  "Save buffer and delete current frame.

This is useful when calling from console just to edit line of text.
And immediately close current frame, when done.

This only work when minor mode panimacs/close-on-save is enabled,
otherwise it functions just as regular save-buffer does."

  (interactive)
  (funcall fun)

  (when panimacs/close-on-save-mode
    (delete-frame)))

(define-minor-mode panimacs/close-on-save-mode
  "Replace save-buffer with panimacs/save-buffer+delete-frame")

(advice-add 'save-buffer :around #'panimacs/save-buffer+delete-frame)

(defun panimacs/quick-edit (file mode)
  (find-file file)
  (funcall mode)
  (panimacs/close-on-save-mode)
  (evil-append 1))


;; TODO: move this
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(setq recentf-max-saved-items 100000)

(global-set-key (kbd "C-x o") #'other-window-prefix)


(use-package keycast)





(provide 'panimacs-ui-defaults)
