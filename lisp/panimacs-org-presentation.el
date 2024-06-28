(require 'panimacs-packages)

(use-package org-modern
  :config
  (setq org-modern-star 'replace)

  ;; Can't decide between these two: ...
  ;; (setq org-modern-replace-stars "⁖◉○◉○◉○◉○")
  (setq org-modern-replace-stars "◉○◉○◉○◉○")

  (setq org-modern-block-fringe 0)
  (setq org-modern-block-name '("‣" . "‣"))
  )

(defvar panimacs/org-presentation-mode-headline-cookies nil)
(make-variable-buffer-local 'panimacs/org-presentation-mode-headline-cookies)


(defvar panimacs/org-presentation-mode--display-line-numbers-mode-status nil)
(make-variable-buffer-local 'panimacs/org-presentation-mode--display-line-numbers-mode-status)

(defvar panimacs/org-presentation-mode--org-modern-status nil)
(make-variable-buffer-local 'panimacs/org-presentation-mode--org-modern-status)

(defvar panimacs/org-presentation-mode--org-indent-mode-status nil)
(make-variable-buffer-local 'panimacs/org-presentation-mode--org-indent-mode-status)


(defvar panimacs/org-presentation-mode-hide-line-numbers t)


(defvar panimacs/org-presentation-title-scale-factor 4.235)
(defvar panimacs/org-presentation-headings-scale-factor 1.618)

(defvar panimacs/org-presentation-headings-progression 1.618)


(define-minor-mode panimacs/org-presentation-mode
  "Make org presentation-ready.

It does a few things:
1. Makes org headings bigger (thus making them more legible)
3. Enables parts of minad's org-modern
4. Disables line numbers"
  :lighter nil
  (if panimacs/org-presentation-mode
      (let ((remap-face (lambda (face &rest specs)
                          (push (apply #'face-remap-add-relative face specs)
                                panimacs/org-presentation-mode-headline-cookies))))

        (setq panimacs/org-presentation-mode--display-line-numbers-mode-status
              (or display-line-numbers
                  (bound-and-true-p display-line-numbers-mode)))

        ;; Save currently set org's appearance to be restored if this minor mode is disabled:
        (setq panimacs/org-presentation-mode--org-modern-status (bound-and-true-p org-modern-mode))
        (setq panimacs/org-presentation-mode--org-indent-mode-status
              (bound-and-true-p org-indent-mode))

        ;; It's cleaner to hide line numbers, but it's configurable:
        (when panimacs/org-presentation-mode-hide-line-numbers
          (display-line-numbers-mode 0))

        ;; Makes use of the fringe to visually indent nested headings in Org
        (org-indent-mode 1)

        ;; This two modes provide some visual prettifications like bullets and tags
        (org-modern-mode 1)

        (mapc (lambda (face)
                (funcall remap-face face :inherit 'variable-pitch :weight 'regular
                         :height panimacs/org-presentation-headings-scale-factor))
              org-level-faces)

        (funcall remap-face 'org-document-title :inherit 'variable-pitch :weight 'regular
                 :height panimacs/org-presentation-title-scale-factor)

        (mapc (lambda (face)
                (funcall remap-face face :weight 'bold
                         :height panimacs/org-presentation-headings-scale-factor))
              (list 'org-modern-tag 'org-modern-todo 'org-modern-done))
        )

    (when panimacs/org-presentation-mode-hide-line-numbers
      (display-line-numbers-mode (if panimacs/org-presentation-mode--display-line-numbers-mode-status 1 0)))

    (org-modern-mode (if panimacs/org-presentation-mode--org-modern-status 1 0))
    (org-indent-mode (if panimacs/org-presentation-mode--org-indent-mode-status 1 0))

    (mapc #'face-remap-remove-relative panimacs/org-presentation-mode-headline-cookies)
    (setq panimacs/org-presentation-mode-headline-cookies nil))
  (force-window-update (current-buffer)))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c p") #'panimacs/org-presentation-mode))


(define-globalized-minor-mode panimacs/global-org-presentation-mode
  panimacs/org-presentation-mode panimacs/org-presentation-mode--on)

(defun panimacs/org-presentation-mode--on ()
  "Enable `panimacs/org-presentation-mode' in every Org buffer."
  (when (derived-mode-p #'org-mode)
    (panimacs/org-presentation-mode)))


(provide 'panimacs-org-presentation)
