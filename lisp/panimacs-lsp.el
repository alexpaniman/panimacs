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
  ((c++-mode . eglot-ensure)
   (  c-mode . eglot-ensure))

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


(provide 'panimacs-lsp)
