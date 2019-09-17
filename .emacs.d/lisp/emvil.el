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

(defun find-window (f)
  "loops over subwindows in current window until they're finished
to `f' returns t. Returns nil on fail or a window on success"
  (let* ((curr-window (selected-window))
        (next-window (next-window))
        (fail        (lambda () (eq curr-window next-window))))
    (while (and (not (funcall f next-window))
                (not (funcall fail)))
      (setq next-window (next-window next-window)))
    (if (funcall fail)
        nil
      next-window)))

(defun evil-goto-definition-next-split ()
  "If the buffer where definition found has a frame, jump
there. Otherwise jump to definition in the next split"
  (interactive)
  (let ((origin-spl (selected-window))
        (origin-buf (current-buffer)))
    (evil--jumps-push)
    (evil-goto-definition)
    (when (and (eq origin-spl (selected-window)) ;; otherwise it's done
               (not (eq origin-buf (current-buffer)))) ;; otherwise either definition not found, or
                                                       ;; it's in the same buffer
      (let ((defin-buf (current-buffer))
            (defin-point (point)))
        (switch-to-buffer origin-buf)
        (let ((maybe-win-with-defin (find-window (lambda (win) (eq defin-buf (window-buffer win))))))
          (if maybe-win-with-defin
              (select-window maybe-win-with-defin)
            (progn (other-window 1)
                   (switch-to-buffer defin-buf)))
          (goto-char defin-point)
          )))))
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

;;; START c++-like variable detection
(defvar c++-like-variable-regex (rx (or whitespace "(" ")"  "{" "}" "," line-end line-start)))

(defun scan-fwd-c++-like-variable ()
  "Walks forward until the first symbol that doesn't look like variable"
  (if (re-search-forward c++-like-variable-regex nil t)
      ;; Note: now we at past_end point
      (progn
        (if (or (eq (char-after (point)) ?\)) ;; let's match foo() and foo{} too.
                (eq (char-after (point)) ?\}))
            (+ (point) 1)
          (- (point) 1)))
    nil))

(defun skip-paren-back ()
  "Skips a single () or {} sentence. Return t if these were skipped."
  (if (and (or (eq (char-after (point)) ?\}) (eq (char-after (point)) ?\)))
           (or (eq (char-after (- (point) 1)) ?\() (eq (char-after (- (point) 1)) ?\{)))
      (progn
        (backward-char 2)
        t)
    nil))

(defun range-c++-like-variable ()
  (skip-paren-back)
  (let ((beg (re-search-backward c++-like-variable-regex nil t)))
    (if (eq beg nil)
        nil
      (if (skip-paren-back)
          (range-c++-like-variable)
        (unless (eq (char-after (+ 1 (point))) ?\n)
          (forward-char) ;; whitespace of whatever matched gotta be skipped
          (setq beg (point)))
        (let ((end (scan-fwd-c++-like-variable)))
          (if (eq end nil)
              nil
            `(,beg ,end)))))))

(evil-define-text-object evil-inner-variable (count &optional beg end type)
  "Tries to select a variable or an expression that would result in a variable"
  (range-c++-like-variable))

(evil-define-text-object evil-a-variable (count &optional beg end type)
  "Tries to select a variable or an expression that would result in a variable"
  (let ((beg-end (range-c++-like-variable)))
    (goto-char (nth 1 beg-end))
    (re-search-forward "\\s-*" nil t)
    `(,(nth 0 beg-end) ,(point))))

(define-key evil-inner-text-objects-map "v" 'evil-inner-variable)
(define-key evil-outer-text-objects-map "v" 'evil-a-variable)
;;; END c++-like variable detection

;;; START html navigation
;; make sp-select-next-thing works even the cusor is in the open/close tag
;; like matchit in vim
;; @return t => start from open tag; nil start from close tag
(defun my-sp-select-next-thing (&optional NUM)
  (interactive "p")
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (char (following-char))
         (p (point))
         rbeg
         rend
         (rlt t))
    ;; "<" char code is 60
    ;; search backward
    (if (not (= char 60))
        (save-excursion
          (while (and (<= b (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (backward-char))))
    ;; search forward
    (if (not (= char 60))
        (save-excursion
          (while (and (>= e (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (forward-char))))
    ;; do the real thing
    (when (and (= char 60) (< p e))
      (goto-char p)
      (forward-char)
      (if (= (following-char) 47)
          (progn
            ;; </
            (backward-char)
            (setq rlt nil))
        (progn
          ;; < , looks fine
          (backward-char)
          (setq rlt t)))
      (sp-select-next-thing)
      (setq rbeg (region-beginning))
      (setq rend (region-end))

      (while (> NUM 1)
        ;; well, sp-select-next-thing is kind of wierd
        (re-search-forward "<[^!]")
        (backward-char 2)
        (sp-select-next-thing)
        (setq rend (region-end))
        (setq NUM (1- NUM)))
      (push-mark rbeg t t)
      (goto-char (1- rend)))
    rlt))

;; {{ evil-matchit
(defun my-evil-jump-item-enhanced-for-html ()
  (interactive)
  (if (or (eq major-mode 'html-mode)
          (eq major-mode 'xml-mode)
          (eq major-mode 'nxml-mode))
      (progn
        (if (not (my-sp-select-next-thing 1)) (exchange-point-and-mark))
        (deactivate-mark))
    (progn
      (evil-jump-item))))
(define-key evil-normal-state-map "%" 'my-evil-jump-item-enhanced-for-html)
;; }}
;;; END html navigation

(provide 'emvil)
