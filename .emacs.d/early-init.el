;;;; I set defaults in early-init rather than in .emacs to not waste CPU cycles
;;;; in excessive overwrites to them

;; only run garbage collection on idle
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq gc-cons-threshold (* 1024 1024 1024)
      frame-resize-pixelwise t ;; needs to be set before a frame is created
      kill-ring-max 1 ;; I never use more than one entry anyway
      reb-re-syntax 'string ;; standard syntax for re-builder

      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      native-compile-prune-cache t ;; with native compilation, remove unused .eln's
      dabbrev-upcase-means-case-search t) ;; case sensitive dabbrev
(setq-default case-fold-search nil)

(setq-default display-line-numbers 'visual) ;; show the line numbers
(setq-default tab-width 4) ;; set tab width
(setq-default fill-column 85) ;; set apropriate lenght of a line
(setq-default cursor-type 'bar) ;; set flat cursor type
(setq-default major-mode 'text-mode) ;; why Fundamental-mode is default? Text-mode is more aproppriate
