;;; panimacs-modal-editing.el --- panimacs :: modal editing -*- lexical-binding: t -*-

;;; Commentary:
;; Modal editing meant to be simple yet efficient.
;; This is part of panimacs emacs distribution and designed to 
;; integrate well with all it's other parts.


;; TODO: WIP, should replace evil when completed!

;;; Code:

(require 'cl-lib)


(cl-defstruct painmacs-reported-symbol
  (modifiers :type vector)
  (languages :type vector)
  reported-symbol)

(cl-defstruct painmacs-keyboard-key
  key-name
  (reported-symbols :type vector))


(defconst painmacs-qwerty-keyboard
  '(
    (?\1 ((?\1 nil en) (?\! S en) (?\1 nil ru) (?\! S ru)))
    (?\2 ((?\2 nil en) (?\@ S en) (?\2 nil ru) (?\@ S ru)))
    (?\3 ((?\3 nil en) (?\# S en) (?\3 nil ru) (?\# S ru)))
    (?\4 ((?\4 nil en) (?\$ S en) (?\4 nil ru) (?\$ S ru)))
    (?\5 ((?\5 nil en) (?\% S en) (?\5 nil ru) (?\% S ru)))
    (?\6 ((?\6 nil en) (?^  S en) (?\6 nil ru) (?^  S ru)))
    (?\7 ((?\7 nil en) (?\& S en) (?\7 nil ru) (?\& S ru)))
    (?\8 ((?\8 nil en) (?\* S en) (?\8 nil ru) (?\* S ru)))
    (?\9 ((?\9 nil en) (?\( S en) (?\9 nil ru) (?\( S ru)))
    (?\0 ((?\0 nil en) (?\) S en) (?\0 nil ru) (?\) S ru)))
    (?\q ((?\q nil en) (?\Q S en) (?\й nil ru) (?\Й S ru)))
    (?\w ((?\w nil en) (?\W S en) (?\ц nil ru) (?\Ц S ru)))
    (?\e ((?\e nil en) (?\E S en) (?\у nil ru) (?\У S ru)))
    (?\r ((?\r nil en) (?\R S en) (?\к nil ru) (?\К S ru)))
    (?\t ((?\t nil en) (?\T S en) (?\е nil ru) (?\Е S ru)))
    (?\y ((?\y nil en) (?\Y S en) (?\н nil ru) (?\Н S ru)))

    (?\u ((?\u nil en) (?\U S en) (?\н nil ru) (?\Г S ru)))
    (?\i ((?\i nil en) (?\I S en) (?\н nil ru) (?\Ш S ru)))
    (?\o ((?\o nil en) (?\O S en) (?\н nil ru) (?\Щ S ru)))
    (?\p ((?\p nil en) (?\P S en) (?\н nil ru) (?\Н S ru)))
    (?\[ ((?\[ nil en) (?\[ S en) (?\н nil ru) (?\Н S ru)))
    (?\] ((?\] nil en) (?\] S en) (?\н nil ru) (?\Н S ru)))
    (?\a ((?\a nil en) (?\A S en) (?\н nil ru) (?\Н S ru)))
    (?\s ((?\s nil en) (?\S S en) (?\н nil ru) (?\Н S ru)))
    (?\d ((?\d nil en) (?\D S en) (?\н nil ru) (?\Н S ru)))
    (?\f ((?\f nil en) (?\F S en) (?\н nil ru) (?\Н S ru)))
    (?\g ((?\g nil en) (?\G S en) (?\н nil ru) (?\Н S ru)))
    (?\h ((?\h nil en) (?\H S en) (?\н nil ru) (?\Н S ru)))
    (?\j ((?\j nil en) (?\J S en) (?\н nil ru) (?\Н S ru)))
    (?\k ((?\k nil en) (?\K S en) (?\н nil ru) (?\Н S ru)))
    (?\l ((?\l nil en) (?\L S en) (?\н nil ru) (?\Н S ru)))
    (?\; ((?\; nil en) (?\: S en) (?\н nil ru) (?\Н S ru)))
    (?\' ((?\' nil en) (?\" S en) (?\н nil ru) (?\Н S ru)))

    (?\z ((?\z nil en) (?\Z S en) (?\н nil ru) (?\Н S ru)))
    (?\x ((?\x nil en) (?\X S en) (?\н nil ru) (?\Н S ru)))
    (?\c ((?\c nil en) (?\C S en) (?\н nil ru) (?\Н S ru)))
    (?\v ((?\v nil en) (?\V S en) (?\н nil ru) (?\Н S ru)))
    (?\b ((?\b nil en) (?\B S en) (?\н nil ru) (?\Н S ru)))
    (?\n ((?\n nil en) (?\N S en) (?\н nil ru) (?\Н S ru)))
    (?\m ((?\m nil en) (?\M S en) (?\н nil ru) (?\Н S ru)))
    (?\, ((?\, nil en) (?\< S en) (?\н nil ru) (?\Н S ru)))
    (?\. ((?\. nil en) (?\> S en) (?\н nil ru) (?\Н S ru)))
    (?\/ ((?\/ nil en) (?\? S en) (?\н nil ru) (?\Н S ru)))
    )
  ) 




(defconst painmacs-qwerty-layout
  '((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9  ?0   ?-  ?=
     ?q ?w ?e ?r ?t ?y ?u ?i ?o  ?p   ?\[ ?\] ?\\
     ?a ?s ?d ?f ?g ?h ?j ?k ?l  ?\;  ?\'
     ?z ?x ?c ?v ?b ?n ?m ?, ?.  ?/)

    (?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\)  ?-  ?=
     ?Q ?W ?E ?R ?T ?Y ?U ?I ?O  ?P   ?\{ ?\} ?\|
     ?A ?S ?D ?F ?G ?H ?J ?K ?L  ?\:  ?\"
     ?Z ?X ?C ?V ?B ?N ?M ?< ?>  ??)))


(defvar painmacs-mode-map (make-sparse-keymap))

(defun painmacs-define-all-keys (action)
  "Bind all keys from painmacs-qwerty-layout to ACTION."
  (cl-loop
   for layout in painmacs-qwerty-layout ; for upper/lower case 
   do (cl-loop
       for symbol in layout
       do (define-key
	   painmacs-mode-map (kbd (string symbol)) action)
       )
   )
  )


(defun painmacs-unbound-key ()
  "Command for binding keys that have no purprose (yet)."
  (interactive)

  (message "Not available!"))


(defun painmacs-load-keys (layout)
  "Define keys listed in LAYOUT in MAP.

Layout should be in format: '((key action) ...)
And every key should be a string in kbd format."

  (cl-loop
   for (key function) in layout
   do (define-key painmacs-mode-map (kbd key) function))
  )


(defconst painmacs-insert-mode-layout
  `(("" ,#'painmacs-switch-mode-to-insert)))

(defun painmacs-switch-mode-to-insert ()
  "Switch to normal mode."
  (interactive)

  ;; Make every key in the map just insert
  ;; corresponding letter, I mean, it's insert mode :)
  (painmacs-define-all-keys #'self-insert-command)
  (painmacs-load-keys painmacs-insert-mode-layout))


(defconst painmacs-normal-mode-layout
  `((?i ,#'painmacs-switch-mode-to-insert)))

(defun painmacs-switch-mode-to-normal ()
  "Switch to normal mode."
  (interactive)

  (painmacs-define-all-keys #'painmacs-unbound-key)
  (painmacs-load-keys painmacs-normal-mode-layout))


;; (defvar painmacs-mode-active-mode 'normal
;;   "Current editing mode, one of: 'normal, 'insert.")


(define-minor-mode painmacs-mode
  "Don't painmacs yourself, use painmacs-mode modal editing."
  :lighter " 🥥"
  :keymap painmacs-mode-map

  ;; Normal mode is the default:
  (painmacs-switch-mode-to-normal))


(add-hook 'prog-mode-hook #'painmacs-mode)


(provide 'panimacs-modal-editing)

;;; panimacs-modal-editing.el ends here
