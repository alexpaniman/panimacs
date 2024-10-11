;;; -*- lexical-binding: t -*-

(setq initial-scratch-message nil
      inhibit-startup-screen    t
      inhibit-startup-message   t

      resize-mini-windows       nil

      use-short-answers         t

      default-frame-alist
      '((vertical-scroll-bars  . nil)
	(internal-border-width .   2)
	(left-fringe           .   8)
	(right-fringe          .   8)
	(tool-bar-lines        .   0)
	(menu-bar-lines        .   0))
      )




(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Very important, disable insanely annoying bell sound!
(setq ring-bell-function 'ignore)


(defun panimacs/enable-line-numbers ()
  (interactive)
  (setq-local display-line-numbers 'visual))

(add-hook 'prog-mode-hook 'panimacs/enable-line-numbers)
(add-hook  'org-mode-hook 'panimacs/enable-line-numbers)

(setq native-comp-async-report-warnings-errors nil)

(require 'panimacs-packages)

;; == Scrolling

(defun panimacs/scroll-up (&optional arg)
  (interactive)
  (pixel-scroll-interpolate-up))

(defun panimacs/scroll-down (&optional arg)
  (interactive)
  (pixel-scroll-interpolate-down))

(use-package emacs
  :bind
  ([remap scroll-up-command]   . panimacs/scroll-up)
  ([remap scroll-down-command] . panimacs/scroll-down)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode 1)
  :config
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))) ;; one line at a time
  (setq mouse-wheel-scroll-amount-horizontal 2)

  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time

  ;; Don't jump & recenter after cursor gets out of the window
  (setq scroll-conservatively 30)
  (setq auto-window-vscroll nil)
  )




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

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.7))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom    
  (doom-modeline-height 0) ;; TODO: What to do here?
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

(defun panimacs/xdg-open (file)
  "Open FILE to edit in a running emacs daemon in a separate workspace.
Meant to be called to xdg-open program."
  (interactive)
  (when (bound-and-true-p persp-mode)
    (let ((xdg-open-frame-exists nil))
      (dolist (frame (frame-list))
        (when (and (frame-live-p frame) (frame-visible-p frame))
          (let ((frame-persp (get-frame-persp frame)))
            (when (and frame-persp (string= (persp-name frame-persp) "xdg-open"))
              (select-frame-set-input-focus frame)
              (setq xdg-open-frame-exists t))))
        )
      (unless xdg-open-frame-exists
        (select-frame-set-input-focus (make-frame '((display . ":0"))))
        (persp-switch "xdg-open")
        )
      )
    )
  (let ((opened-buffer (find-file file)))
    (persp-add-buffer opened-buffer)
    )
  )


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


(use-package form-feed
  :init (global-form-feed-mode 1))


(provide 'panimacs-ui-defaults)
