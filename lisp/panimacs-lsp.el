;;; panimacs-lsp.el --- panimacs :: completions and fuzzy matching -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook (lambda () (toggle-truncate-lines +1)))

(require 'panimacs-packages)

(use-package projectile
  :bind
  ("C-x p f" . projectile-find-file)
  ("C-x p r" . projectile-ripgrep)
  ("C-x p d" . projectile-find-dir)
  ("C-x p v" . projectile-run-vterm)
  ("C-x p c" . projectile-compile-project)
  ("C-x p o" . projectile-find-other-file)
)

(use-package ripgrep)


(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . typescript-ts-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package cc-mode
  :config

  (defun panimacs/c-mode-common-hook ()
    ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
    (c-set-offset 'substatement-open 0)
    ;; other customizations can go here

    (setq c++-tab-always-indent t)
    (setq c-basic-offset 4)
    (setq c-ts-common-indent-offset 4)
    (setq c-indent-level 4)

    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq tab-width 4)
    (setq indent-tabs-mode nil))

  (add-hook 'c-mode-common-hook 'panimacs/c-mode-common-hook))


(use-package cmake-mode)


(use-package nix-mode
  :mode "\\.nix\\'")

(defun my-indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(;; Here are your custom rules
    ((node-is ")") parent-bol 0)
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") prev-sibling 0)
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") prev-sibling 0)

    ;; Append here the indent style you want as base
   ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
 :if (treesit-language-available-p 'c)
 :custom
 (c-ts-mode-indent-offset 4)
 (c-ts-mode-indent-style 'gnu)
 :init
 ;; Remap the standard C/C++ modes
 (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
 )

(use-package aggressive-indent)
(global-aggressive-indent-mode)


(use-package haskell-mode)


;; Configure compilation buffer colors
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))


(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package hide-mode-line)

(use-package sudo-edit
  :ensure t
  :after embark
  :bind
  (:map embark-file-map
        ("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map
        ("s" . sudo-edit-find-file)))

(use-package vterm
  :ensure t
  ;; :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :hook (vterm-mode . turn-off-evil-mode)
  :init
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-timer-delay 0.01)

  (evil-set-initial-state 'vterm-mode 'emacs)
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-tramp-shells '(("ssh" "/usr/bin/zsh") ("scp" "/usr/bin/zsh") ("su" "/usr/bin/zsh") ("docker" "/bin/sh")))

  ;; 50000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 50000))

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(global-set-key (kbd "C-x M-RET") #'run-in-vterm)


(use-package multi-vterm
	:config
	(setq vterm-keymap-exceptions nil)
	(global-set-key            (kbd "C-x v v")    #'multi-vterm)
        (global-set-key            (kbd "C-x p RET")  #'multi-vterm-project)
        (global-set-key            (kbd "C-'")        #'multi-vterm-dedicated-toggle)
	(define-key vterm-mode-map (kbd "C-x v v")    #'multi-vterm)
	(define-key vterm-mode-map (kbd "M-TAB")      #'multi-vterm-next)
	(define-key vterm-mode-map (kbd "M-S-TAB")    #'multi-vterm-prev))

(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")


;; TODO: make it so it works in tramp
(cl-loop for file in '("/bin/zsh" "/usr/bin/zsh" "/usr/local/bin/bash" "/bin/bash")
        when (file-exists-p file)
        do (progn
            (setq shell-file-name file)
            (cl-return)))

(setenv "SHELL" shell-file-name)
(setq vterm-shell shell-file-name)
(setq-default vterm-shell shell-file-name)


(use-package magit)
(use-package evil-magit
  :config
  (evil-magit-init)
  (evil-define-key* evil-magit-state magit-mode-map [escape] nil))

(use-package diff-hl
  :config (global-diff-hl-mode 1)

  ;; FIXME: shamelessly stolen from doom, don't use internal defadvice!
  (defmacro defadvice! (symbol arglist &optional docstring &rest body)
    "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
    (declare (doc-string 3) (indent defun))
    (unless (stringp docstring)
      (push docstring body)
      (setq docstring nil))
    (let (where-alist)
      (while (keywordp (car body))
        (push `(cons ,(pop body) (ensure-list ,(pop body)))
              where-alist))
      `(progn
         (defun ,symbol ,arglist ,docstring ,@body)
         (dolist (targets (list ,@(nreverse where-alist)))
           (dolist (target (cdr targets))
             (advice-add target (car targets) #',symbol))))))
  (defun doom-rpartial (fn &rest args)
    "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
    (declare (side-effect-free t))
    (lambda (&rest pre-args)
      (apply fn (append pre-args args))))

  (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
    :override #'diff-hl-define-bitmaps
    (define-fringe-bitmap 'diff-hl-bmp-middle [128] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))

  (defun +vc-gutter-type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))

  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter-type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'+vc-gutter-type-at-pos-fn)

  (setq diff-hl-draw-borders nil)


  (add-hook 'diff-hl-mode-hook
            (defun +vc-gutter-fix-diff-hl-faces-h ()
              (mapc (doom-rpartial #'set-face-background nil)
                    '(diff-hl-insert
                      diff-hl-delete
                      diff-hl-change))))

  :hook (prog-mode . diff-hl-flydiff-mode))



(use-package python-mode
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; Make eglot correctly detect project root in some cases:

(use-package project
  :config

  ;; All taken from https://andreyor.st/posts/2022-07-16-project-el-enhancements/

  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "deps.edn" "shadow-cljs.edn" ".projectile" ".envrc" "build")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
	(when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (let ((path (expand-file-name path)))
      (catch 'found
	(while (not (equal "/" path))
	  (if (not (project-root-p path))
	      (setq path (file-name-directory (directory-file-name path)))
	    (throw 'found (cons 'transient path)))))))

  (add-to-list 'project-find-functions #'project-find-root)

  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))

  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))

  (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
    "Only ask to save project-related buffers if inside a project."
    (if (project-current)
	(let* ((project-buffers (project-buffers (project-current)))
               (compilation-save-buffers-predicate
		(lambda () (memq (current-buffer) project-buffers))))
          (funcall fn edit-command))
      (funcall fn edit-command)))
  )


;; (use-package lsp-mode
;;   :custom
;;   ((lsp-keymap-prefix "C-c l")
;;    (lsp-headerline-breadcrumb-enable nil)
;;    ;; OPTIMIZATION
;;    (read-process-output-max (* 1024 1024)))
;;   :hook ((c-ts-mode . lsp-deferred) (c++-ts-mode . lsp-deferred))
;;   :config
;;   (lsp-enable-which-key-integration t)
;;   :commands (lsp lsp-deferred))

;; (use-package lsp-ui
;;   :custom
;;   (flycheck-indication-mode 'right-fringe)
;;   :custom
;;   ((lsp-ui-sideline-show-diagnostics t)
;;    (lsp-ui-sideline-show-code-actions t)
;;    (lsp-ui-doc-show-with-cursor nil)
;;    (lsp-ui-doc-show-with-cursor nil)
;;    (lsp-ui-doc-show-with-mouse  nil)
;;    ))

(use-package envrc
  :after (flymake eglot)
  :init (envrc-global-mode))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)


(setq python-indent-offset 4)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))



(defun panimacs/compile-project-advice-no-project (old-function)
  "Advice for project-compile that invokes compile
when there's no active project."

  (condition-case nil (funcall old-function)
    (error (call-interactively #'compile)))
  )

(advice-add 'project-compile :around #'panimacs/compile-project-advice-no-project)



;; Eshell

(with-eval-after-load 'corfu
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  )



(provide 'panimacs-lsp)
