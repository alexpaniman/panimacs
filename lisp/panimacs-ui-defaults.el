;;; -*- lexical-binding: t -*-


(setq initial-scratch-message nil
      inhibit-startup-screen    t
      inhibit-startup-message   t

      default-frame-alist
      '((vertical-scroll-bars  . nil)
	(internal-border-width .   2)
	(left-fringe           .  10)
	(right-fringe          .  10)
	(tool-bar-lines        .   0)
	(menu-bar-lines        .   0))
      )

(setq native-comp-async-report-warnings-errors nil)


(provide 'panimacs-ui-defaults)
