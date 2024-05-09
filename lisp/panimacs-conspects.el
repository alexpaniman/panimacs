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


(setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
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

(defvar panimacs/org-default-svg-figure
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->

<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"240mm\"
   height=\"120mm\"
   viewBox=\"0 0 240 120\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"0.92.4 (unknown)\"
   sodipodi:docname=\"figure.svg\">
  <defs
     id=\"defs2\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"0.99437388\"
     inkscape:cx=\"284.27627\"
     inkscape:cy=\"182.72055\"
     inkscape:document-units=\"mm\"
     inkscape:current-layer=\"layer1\"
     showgrid=\"false\"
     showborder=\"true\"
     width=\"200mm\"
     showguides=\"true\"
     inkscape:guide-bbox=\"true\"
     inkscape:window-width=\"2520\"
     inkscape:window-height=\"995\"
     inkscape:window-x=\"20\"
     inkscape:window-y=\"65\"
     inkscape:window-maximized=\"1\">
    <inkscape:grid
       type=\"xygrid\"
       id=\"grid815\"
       units=\"mm\"
       spacingx=\"10\"
       spacingy=\"10\"
       empspacing=\"4\"
       dotted=\"false\" />
  </sodipodi:namedview>
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title />
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\"
     transform=\"translate(0,-177)\" />
</svg>"
  "Default file template.")

(defvar panimacs/org-folder-for-figures "figures")

(defun panimacs/org-open-or-create-figure (image)
  (interactive
   (list
    (completing-read
     "Select figure: "
     (cond ((file-directory-p panimacs/org-folder-for-figures)
            (seq-map
             (lambda (file) (file-name-sans-extension file))
             (seq-filter
              (lambda (file) (equal (file-name-extension file) "svg"))
              (directory-files panimacs/org-folder-for-figures))))
           ((file-exists-p panimacs/org-folder-for-figures)
            (error "Folder for figures '%s/' already exists as a file!"
                   panimacs/org-folder-for-figures))
           (t '()))
     )))

  (unless (file-exists-p panimacs/org-folder-for-figures)
    (make-directory panimacs/org-folder-for-figures))

  (let* ((relative-filename (format "%s/%s.svg" panimacs/org-folder-for-figures image))
          (file (expand-file-name relative-filename)))

    (unless (file-exists-p file)
      (write-region panimacs/org-default-svg-figure nil file)
      (insert (format "[[file:./%s]]" relative-filename)))
    
    (make-process
     :name "inkscape"
     :buffer nil
     :stderr nil
     :command
     (list "inkscape" file)
     )
    )
  )

(define-key org-mode-map (kbd "C-c f") #'panimacs/org-open-or-create-figure)

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
)

(use-package org-tidy
  :ensure t
  :hook (org-mode . org-tidy-mode)
  )

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode))


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
