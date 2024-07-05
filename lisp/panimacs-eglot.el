;; -*- lexical-binding: t -*-


(use-package corfu
  :demand t
  :custom ((corfu-auto        t)
	   (corfu-auto-delay  0)
	   (corfu-auto-prefix 2)
	   (corfu-quit-no-match 'separator)
	   (completion-styles '(orderless basic)))
  :init (global-corfu-mode))

(use-package kind-icon
  :ensure t
  ;; :after corfu
  :custom (
           ;; (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
           (kind-icon-default-style
            '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1)))

  :config
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(use-package eglot
  :custom
  ((read-process-output-max (* 1024 1024)))

  :hook ((       c-ts-mode . eglot-ensure)
         (     c++-ts-mode . eglot-ensure)
         (c-or-c++-ts-mode . eglot-ensure)

         (  python-ts-mode . eglot-ensure))
  :commands (eglot eglot-ensure))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(provide 'panimacs-eglot)
