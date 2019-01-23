;;; https://lists.ourproject.org/pipermail/implementations-list/2012-February/001513.html
(require 'evil)
;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "[ m") 'beginning-of-defun)
(define-key evil-normal-state-map (kbd "] m") 'end-of-defun)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)

(evil-mode t)
(define-key evil-normal-state-map (kbd "C-z") 'undo-tree-redo)

(defun evil-goto-definition-next-split ()
	"If there's a free split, goto definition in this split,
	otherwise use current one (except when a definition in the
	current split)"
	(interactive)
	(let ((origin-spl (selected-window))
		  (origin-buf (current-buffer)))
      (evil--jumps-push)
      (if (bound-and-true-p racer-mode)
          (racer-find-definition)
        (evil-goto-definition))
	  (when (and (eq origin-spl (selected-window)) ;; otherwise it's done
				 (not (eq origin-buf (current-buffer)))) ;; otherwise either definition not found, or
														 ;; it's in the same buffer
		(let ((defin-buf (current-buffer))
			  (defin-point (point)))
		  (switch-to-buffer origin-buf)
		  (other-window 1)
		  (switch-to-buffer defin-buf)
		  (goto-char defin-point)
		  ))
	  ))
(define-key evil-normal-state-map (kbd "g d") 'evil-goto-definition-next-split)

;; add a "function" text object
(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  (evil-select-an-object 'evil-defun beg end type count))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun."
  (evil-select-inner-object 'evil-defun beg end type count))

(define-key evil-outer-text-objects-map "m" 'evil-a-defun)
(define-key evil-inner-text-objects-map "m" 'evil-inner-defun)

(provide 'emvil)
