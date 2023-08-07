;;; -*- lexical-binding: t -*-


(require 'panimacs-packages)
(panimacs/ensure-straight)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

(provide 'panimacs-use-package)

