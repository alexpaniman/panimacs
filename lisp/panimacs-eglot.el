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
  :after corfu
  :custom (
           (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
           (kind-icon-default-style
            '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1)))

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

(setq flymake-indicator-type 'fringes)

;; Pasted from eglot-booster.el:


(load "project.elc")
(load "xref.elc")
(require 'eglot)

(use-package eglot
  :custom
  ((read-process-output-max (* 1024 1024)))

  :hook ((       c-ts-mode . eglot-ensure)
         (     c++-ts-mode . eglot-ensure)
         (c-or-c++-ts-mode . eglot-ensure)

         (  python-ts-mode . eglot-ensure))
  :commands (eglot eglot-ensure)
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'lazy-highlight))


(require 'jsonrpc)

(defcustom eglot-booster-no-remote-boost nil
  "If non-nil, do not boost remote hosts."
  :group 'eglot
  :type 'boolean)

(defun eglot-booster-plain-command (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (memq :autoport com))))

(defvar-local eglot-booster-boosted nil)
(defun eglot-booster--jsonrpc--json-read (orig-func)
  "Read JSON or bytecode, wrapping the ORIG-FUNC JSON reader."
  (if eglot-booster-boosted ; local to process-buffer
      (or (and (= (following-char) ?#)
	       (let ((bytecode (read (current-buffer))))
		 (when (byte-code-function-p bytecode)
		   (funcall bytecode))))
	  (funcall orig-func))
    ;; Not in a boosted process, fallback
    (funcall orig-func)))

(defun eglot-booster--init (server)
  "Register eglot SERVER as boosted if it is."
  (when-let ((server)
	     (proc (jsonrpc--process server))
	     (com (process-command proc))
	     (buf (process-buffer proc)))
    (unless (and (file-remote-p default-directory) eglot-booster-no-remote-boost)
      (if (file-remote-p default-directory) ; com will likely be /bin/sh -i or so
	  (when (seq-find (apply-partially #'string-search "emacs-lsp-booster")
			  (process-get proc 'remote-command)) ; tramp applies this
	    (with-current-buffer buf (setq eglot-booster-boosted t)))
	(when (string-search "emacs-lsp-booster" (car-safe com))
	  (with-current-buffer buf (setq eglot-booster-boosted t)))))))

(defvar eglot-booster--boost
  '("emacs-lsp-booster" "--json-false-value" ":json-false" "--"))

(defun eglot-booster--wrap-contact (args)
  "Wrap contact within ARGS if possible."
  (let ((contact (nth 3 args)))
    (cond
     ((and eglot-booster-no-remote-boost (file-remote-p default-directory)))
     ((functionp contact)
      (setf (nth 3 args)
	    (lambda (&optional interactive)
	      (let ((res (funcall contact interactive)))
		(if (eglot-booster-plain-command res)
		    (append eglot-booster--boost res)
		  res)))))
     ((eglot-booster-plain-command contact)
      (setf (nth 3 args) (append eglot-booster--boost contact))))
    args))

;;;###autoload
(define-minor-mode eglot-booster-mode
  "Minor mode which boosts plain eglot server programs with emacs-lsp-booster.
The emacs-lsp-booster program must be compiled and available on
variable `exec-path'.  Only local stdin/out-based lsp servers can
be boosted."
  :global t
  :group 'eglot
  (cond
   (eglot-booster-mode
    (unless (executable-find "emacs-lsp-booster")
      (setq eglot-booster-mode nil)
      (user-error "The emacs-lsp-booster program is not installed"))
    (advice-add 'jsonrpc--json-read :around #'eglot-booster--jsonrpc--json-read)
    (advice-add 'eglot--connect :filter-args #'eglot-booster--wrap-contact)
    (add-hook 'eglot-server-initialized-hook #'eglot-booster--init))
   (t
    (advice-remove 'jsonrpc--json-read #'eglot-booster--jsonrpc--json-read)
    (advice-remove 'eglot--connect #'eglot-booster--wrap-contact)
    (remove-hook 'eglot-server-initialized-hook #'eglot-booster--init))))



(eglot-booster-mode)

(provide 'panimacs-eglot)
