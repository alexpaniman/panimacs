;;; -*- lexical-binding: t -*-

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Disable some GUI elements
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
