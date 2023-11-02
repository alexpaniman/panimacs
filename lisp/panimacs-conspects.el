;;; panimacs-lsp.el --- panimacs :: -*- lexical-binding: t -*-

(use-package cdlatex
  :ensure t
  :config
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

  (setq cdlatex-env-alist '(("pmatrix" "\\begin{pmatrix}\n?\n\\end{pmatrix}" nil)))
  ;; TODO: needs a latex command!
  ;; (setq cdlatex-command-alist '(("mat" "Insert matrix" "\\mat{?}{}{}{}{}{}{}{}{}" cdlatex-position-cursor nil nil t)))
  )

(setq org-confirm-babel-evaluate nil)

;; syntax highlighting for latex fragments in org mode
(setq org-highlight-latex-and-related '(native latex script entities))

;; evil mode support for org
(use-package evil-org)

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))


(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("extarticle" "\\documentclass{extarticle}"

                 ("\\section{%s}"       . "\\section*{%s}"      )
                 ("\\subsection{%s}"    . "\\subsection*{%s}"   )
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (require 'seq)

  ;; If I want to use russian, than some of default packages are 
  ;; configured wrongly, let's delete them and do it correctly:
  (setq-local org-latex-default-packages-alist
              (seq-filter (lambda (package)
                            (seq-every-p
                             (lambda (wrong-package)
                               (not (equal (cadr package) wrong-package)))
                             '("inputenc" "fontenc" "babel"))
                            )
                          org-latex-default-packages-alist
                          )
              )
  )


(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.1))
(setq org-startup-with-latex-preview t)

;; better than the default, works for tikzpicture
;; (setq org-preview-latex-default-process 'imagemagick)

(setq org-preview-latex-default-process 'dvisvgm)
;;       org-image-actual-width nil)

;; (defun panimacs/text-scale-adjust-latex-previews ()
;;   "Adjust the size of latex preview fragments when changing the
;; buffer's text scale."

;;   (pcase major-mode
;;     ('latex-mode
;;      (dolist (ov (overlays-in (point-min) (point-max)))
;;        (if (eq (overlay-get ov 'category)
;;                'preview-overlay)
;;            (panimacs/text-scale--resize-fragment ov))))
;;     ('org-mode
;;      (dolist (ov (overlays-in (point-min) (point-max)))
;;        (if (eq (overlay-get ov 'org-overlay-type)
;;                'org-latex-overlay)
;;            (panimacs/text-scale--resize-fragment ov)))))
;;   )

;; (defun panimacs/text-scale--resize-fragment (ov)
;;   (overlay-put
;;    ov 'display
;;    (cons 'image
;;          (plist-put
;;           (cdr (overlay-get ov 'display))
;;           :scale  (* +org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))))
;;   ))

;; (add-hook 'text-scale-mode-hook #'panimacs/text-scale-adjust-latex-previews)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)


(define-key org-mode-map (kbd "C-c C-a") #'org-latex-export-to-pdf)

;; Hide execution results by default
;; (add-hook 'org-babel-after-execute-hook (lambda () (org-babel-hide-result-toggle t)))

;; Render latex images possibly produced with block (TODO: speed up?)
(add-hook 'org-babel-after-execute-hook (lambda () (org-latex-preview +16)))

;; (defvar org-babel-default-header-args:cpp
;;   '((:flags . "-std=c++20")
;;     (:includes . "<cppgfplots.h>")
;;     (:results . "outputs drawer")
;;     (:exports . "results")))


(require 'org-tempo)

;; TODO: is it useful?
(straight-use-package
 '(virtual-auto-fill
   :type git
   :host github
   :repo "luisgerhorst/virtual-auto-fill"))

(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))

(provide 'panimacs-conspects)
