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
	   (corfu-auto-prefix 0)
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

(use-package eglot
  :defer t
  :ensure t

  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(  c-mode . ("clangd")))

  :hook
  ((    c++-mode . eglot-ensure)
   (      c-mode . eglot-ensure)
   (   java-mode . eglot-ensure)

   ( c++-ts-mode . eglot-ensure)
   (   c-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure))

  :config
  (setq read-process-output-max (* 1024 1024)))

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
    (setq c-basic-offset 4)            ;; Default is 2
    (setq c-indent-level 4)            ;; Default is 2

    (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq tab-width 4)
    (setq indent-tabs-mode nil))

  (add-hook 'c-mode-common-hook 'panimacs/c-mode-common-hook))


;; Configure compilation buffer colors
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

(provide 'panimacs-lsp)
