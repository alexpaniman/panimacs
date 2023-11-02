;;; -*- lexical-binding: t -*-


(require 'panimacs-packages)
(panimacs/ensure-straight)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(provide 'panimacs-use-package)

