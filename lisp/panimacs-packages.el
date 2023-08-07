;;; -*- lexical-binding: t -*-


;; (setq straight-base-dir (file-truename (file-name-concat user-emacs-directory ".local"))
;;       straight-repository-branch "develop"
;;       ;; Since byte-code is rarely compatible across different versions of
;;       ;; Emacs, it's best we build them in separate directories, per emacs version:
;;       straight-build-dir (format "build-%s" emacs-version))


;; (defun panimacs/call-process (command &rest args)
;;   "Execute COMMAND with ARGS synchronously.

;; Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
;; code of the process and OUTPUT is its stdout output."
;;   (with-temp-buffer
;;     (cons (or (apply #'call-process command nil t nil (remq nil args)) -1)
;; 	  (string-trim (buffer-string)))))


;; (defun panimacs/ensure-straight ()
;;   (with-environment-variables (("GIT_CONFIG" nil) ("GIT_CONFIG_NOSYSTEM" "1")
;; 			       ("GIT_CONFIG_GLOBAL" "/dev/null"))

;;     (let ((repo-dir (file-name-concat straight-base-dir "straight/repos/straight.el"))
;; 	  (repo-url "https://github.com/radian-software/straight.el")
;; 	  (call (lambda (&rest args) (apply #'panimacs/call-process args))))
;;       (unless (file-directory-p repo-dir)
;; 	(message "Cloning straight.el...")
;; 	(funcall call "git" "clone" "--origin" "origin" "--depth" "1" repo-url repo-dir))
;;       (require 'straight (concat repo-dir "/straight.el"))
;;       (mapc
;;        #'straight-use-recipes
;;        '((org-elpa :local-repo nil)
;;          (nongnu-elpa :type git
;; 	  :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git" :local-repo "nongnu-elpa")
;; 	 (melpa              :type git :host github :repo          "melpa/melpa"             )
;; 	 (gnu-elpa-mirror    :type git :host github :repo "emacs-straight/gnu-elpa-mirror"   )
;;          (el-get             :type git :host github :repo        "dimitri/el-get"            )
;;          (emacsmirror-mirror :type git :host github :repo "emacs-straight/emacsmirror-mirror"))))))

(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(provide 'panimacs-packages)
