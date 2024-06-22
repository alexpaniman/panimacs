(require 'panimacs-packages)

(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸")
        org-superstar-prettify-item-bullets nil))

(use-package org-modern
  :config
  (setq org-modern-star nil)

  (setq org-modern-block-fringe 0)
  (setq org-modern-block-name '("‣" . "‣"))
  )

(defvar panimacs/org-presentation-mode-headline-cookies nil) 
(make-variable-buffer-local 'panimacs/org-presentation-mode-headline-cookies)


(defvar panimacs/org-presentation-mode-display-line-number-type-status nil) 
(make-variable-buffer-local 'panimacs/org-presentation-mode-display-line-number-type-status)

(defvar panimacs/org-presentation-mode-org-modern-status nil) 
(make-variable-buffer-local 'panimacs/org-presentation-mode-org-modern-status)

(defvar panimacs/org-presentation-mode-org-superstar-status nil) 
(make-variable-buffer-local 'panimacs/org-presentation-mode-org-superstar-status)

(defvar panimacs/org-presentation-mode-org-indent-mode-status nil) 
(make-variable-buffer-local 'panimacs/org-presentation-mode-org-indent-mode-status)


(defvar panimacs/org-presentation-title-scale-factor 4.235)
(defvar panimacs/org-presentation-headings-scale-factor 1.618)

(defvar panimacs/org-presentation-headings-progression 1.618)


(define-minor-mode panimacs/org-presentation-mode
  "Make org presentation-ready.

It does a few things:
1. Makes org headings bigger (thus making them more legible)
2. Enables org-superstar mode on headings
3. Enables parts of minad's org-modern
4. Disables line numbers"
  :lighter nil
  (if panimacs/org-presentation-mode
      (let ((remap-face (lambda (face &rest specs)
                          (push (apply #'face-remap-add-relative face specs)
                                panimacs/org-presentation-mode-headline-cookies))))

        (setq panimacs/org-presentation-mode-display-line-number-type-status display-line-numbers-type
              panimacs/org-presentation-mode-org-modern-status org-modern-mode
              panimacs/org-presentation-mode-org-superstar-status org-superstar-mode
              panimacs/org-presentation-mode-org-indent-mode-status org-indent-mode)

        (org-indent-mode 1)
        (display-line-numbers-mode 0)

        (org-modern-mode 1) (org-superstar-mode 1)

        (mapc (lambda (face)
                (funcall remap-face face :inherit 'variable-pitch :weight 'bold
                         :height panimacs/org-presentation-headings-scale-factor))
              org-level-faces)

        (funcall remap-face 'org-document-title :inherit 'variable-pitch :weight 'bold 
                 :height panimacs/org-presentation-title-scale-factor)

        (mapc (lambda (face)
                (funcall remap-face face :weight 'bold
                         :height panimacs/org-presentation-headings-scale-factor))
              (list 'org-modern-tag 'org-modern-todo 'org-modern-done))
        )

    (setq display-line-numbers-type panimacs/org-presentation-mode-display-line-number-type-status)
    (display-line-numbers-mode (if panimacs/org-presentation-mode-display-line-number-type-status 1 0))

    (org-modern-mode (if panimacs/org-presentation-mode-org-modern-status 1 0))
    (org-superstar-mode (if panimacs/org-presentation-mode-org-superstar-status 1 0))

    (org-indent-mode (if panimacs/org-presentation-mode-org-indent-mode-status 1 0))

    (mapc #'face-remap-remove-relative panimacs/org-presentation-mode-headline-cookies)
    (setq panimacs/org-presentation-mode-headline-cookies nil))
  (force-window-update (current-buffer)))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c p") #'panimacs/org-presentation-mode))

(provide 'panimacs-org-presentation)
