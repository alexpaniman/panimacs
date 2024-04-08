;;; -*- lexical-binding: t -*-

(setq native-comp-speed 3) ;; maximum native Elisp speed!

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Disable some GUI elements
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'default nil
    :width 'normal :weight 'normal :slant 'normal
    :font (font-spec :family "JetBrains Mono" :size 20))
