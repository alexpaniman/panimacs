;;; panimacs-lsp.el --- panimacs :: completions and fuzzy matching -*- lexical-binding: t -*-


(require 'panimacs-packages)


(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :demand t
  :custom ((corfu-auto        t)
	   (corfu-auto-delay  0)
	   (corfu-auto-prefix 2)
	   (corfu-quit-no-match 'separator)
	   (completion-styles '(orderless basic)))
  :init (global-corfu-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . typescript-ts-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom (
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1)))

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
 (c-ts-mode-indent-style #'my-indent-style)
 :init
 ;; Remap the standard C/C++ modes
 (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
 (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))


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
  (setq vterm-tramp-shells '(("ssh" "/bin/zsh") ("scp" "/bin/zsh") ("su" "/bin/zsh") ("docker" "/bin/sh")))

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
	(global-set-key            (kbd "M-RET")      #'multi-vterm)
        (global-set-key            (kbd "C-x p RET")  #'multi-vterm-project)
        (global-set-key            (kbd "C-'")        #'multi-vterm-dedicated-toggle)
	(define-key vterm-mode-map (kbd "M-RET")      #'multi-vterm)
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
  :hook (prog-mode . diff-hl-flydiff-mode))


;; (use-package eglot
;;   :defer t
;;   :ensure t

;;   :config
;;   (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
;;   (add-to-list 'eglot-server-programs '(  c-mode . ("clangd")))

;;   :hook
;;   ((    c++-mode       . eglot-ensure)
;;    (      c-mode       . eglot-ensure)

;;    ( c++-ts-mode       . eglot-ensure)
;;    (   c-ts-mode       . eglot-ensure)

;;    (typescript-ts-mode . eglot-ensure)
;;    (        js-ts-mode . eglot-ensure)
;;    )

;;   :bind
;;   (:map prog-mode-map
;;    ("C-c C-r" . eglot-rename)
;;    )

;;   :config
;;   (setq read-process-output-max (* 1024 1024)))



(use-package lsp-mode
  :custom
  ((lsp-keymap-prefix "C-c l")
   (lsp-headerline-breadcrumb-mode nil)
   ;; OPTIMIZATION
   (read-process-output-max (* 1024 1024)))
  :hook ((c-mode . lsp) (c++-mode . lsp))
  :config
  (setq read-process-output-max (* 1024 1024)))

;; TODO: read about :commands



(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)


(setq python-indent-offset 4)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(provide 'panimacs-lsp)
