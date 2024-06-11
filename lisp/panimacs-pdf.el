;;; -*- lexical-binding: t -*-


(use-package pdf-tools
  :ensure t
  :init
  (add-hook 'doc-view-mode-hook #'pdf-tools-install) ;; TODO: Is this better?

  (pdf-tools-install t)
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("\\" . hydra-pdftools/body)
              ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
              ("g"  . pdf-view-first-page)
              ("G"  . pdf-view-last-page)
              ("l"  . image-forward-hscroll)
              ("h"  . image-backward-hscroll)
              ("j"  . pdf-view-next-page)
              ("k"  . pdf-view-previous-page)
              ("e"  . pdf-view-goto-page)
              ("u"  . pdf-view-revert-buffer)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("am" . pdf-annot-add-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("y"  . pdf-view-kill-ring-save)
              ("i"  . pdf-misc-display-metadata)
              ("s"  . pdf-occur)
              ("b"  . pdf-view-set-slice-from-bounding-box)
              ("r"  . pdf-view-reset-slice)
              ([remap image-next-line] . hydra-pdftools/body))
  :config
  (keymap-set pdf-view-mode-map "<remap> <scroll-up-command>"
              #'pdf-view-scroll-up-or-next-page)
  (keymap-set pdf-view-mode-map "<remap> <scroll-down-command>"
              #'pdf-view-scroll-down-or-previous-page)
  )

(evil-set-initial-state 'pdf-view-mode 'insert)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))

(add-hook 'pdf-view-mode-hook #'turn-off-evil-mode nil)
(add-hook 'pdf-view-enabled #'turn-off-evil-mode nil)
(add-hook 'doc-view-mode-hook #'turn-off-evil-mode nil)


(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(provide 'panimacs-pdf)
