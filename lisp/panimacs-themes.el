;;; -*- lexical-binding: t -*-


(require 'panimacs-packages)


(defvar panimacs/default-color-theme)


(use-package doom-themes
  :init (load-theme panimacs/default-color-theme t))


(provide 'panimacs-themes)
