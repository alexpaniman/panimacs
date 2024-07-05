;; -*- lexical-binding: t -*-

;; dependencies: pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz
(use-package markdown-mode)
(use-package yasnippet)

(add-to-list 'load-path "panimacs/lisp/lsp-bridge")

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)


(provide 'panimacs-lsp-bridge)
