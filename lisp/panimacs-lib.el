;;; panimacs-lib.el --- panimacs :: general library -*- lexical-binding: t -*-


(defun panimacs/unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'panimacs-lib)
