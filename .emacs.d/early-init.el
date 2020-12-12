;; only run garbage collection on idle
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(setq frame-resize-pixelwise t) ;; needs to be set before a frame is created
