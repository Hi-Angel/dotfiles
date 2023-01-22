(use-package evil
  :init
  (setq evil-jumps-cross-buffers nil
        evil-undo-system 'undo-fu)
  (setq-default evil-shift-round nil ;; make '>' not to round the indentation
                evil-goto-definition-functions '(evil-lsp-find-definition
                                                 evil-goto-definition-imenu
                                                 evil-goto-definition-xref
                                                 evil-goto-definition-search))

  :config
  ;; remove all keybindings from insert-state keymap https://lists.ourproject.org/pipermail/implementations-list/2012-February/001513.html
  (setcdr evil-insert-state-map nil)
  ;; disable undo-tree-mode mandated by Evil as it's broken (see "unrecognized
  ;; entry in undo list" on the internet), and use undo-fu instead.
  ;; UPD: apparently in newer release it's no longer mandatory, so check if it's even defined.
  (when (boundp 'global-undo-tree-mode)
    (global-undo-tree-mode -1))
  (bind-key "C-z"   'undo-fu-only-undo)
  (bind-key "C-S-z" 'undo-fu-only-redo)

  ;; newer Evil versions seem to handle this by default, however the older one was
  ;; removing trailing space when you press Escape. This however can be worked around
  ;; by overriding the function below to a noop.
  (defun evil-maybe-remove-spaces (&optional _))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  (use-package evil-magit ;; without this package Evil keys are broken in magit
    :after magit)

  ;; highlight regions I work with. Just fancies.
  (use-package evil-goggles
    :init
    (setq evil-goggles-blocking-duration 0.05)
    :config
    (evil-goggles-mode 1)
    )

  (defun fill-paragraph-or-region ()
    (interactive)
    (if (region-active-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))
    )
  (defun end-of-buffer-keep-bottom (&optional count)
    "Without a digit-argument it goes to the end of a buffer, but
keeps the bottom of the buffer at the bottom, as opposed to
bringing it to the middle of the screen."
    (interactive "P")
    (if count
        ;; `let`ting scroll-conservatively actually won't work w evil-goto-line
        (evil-goto-line count)
      (let ((scroll-conservatively 101))
        (end-of-buffer))))
  (evil-mode)
  :bind (:map evil-insert-state-map
         ;; after having insert-state keymap wiped out make [escape] switch back to
         ;; normal state
         ([escape] . 'evil-normal-state)

         :map evil-normal-state-map
         ("C-u"    . 'evil-scroll-up)
         ("k"      . 'evil-previous-visual-line)
         ("j"      . 'evil-next-visual-line)
         ("G"      . 'end-of-buffer-keep-bottom)
         ("g a"    . 'evil-avy-goto-char) ;; let's have some avy integration!
         ("u"      . 'undo-fu-only-undo)
         ("\C-r"   . 'undo-fu-only-redo)
         ("\C-]"   . 'find-tag) ;; same as in insert mode
         ("S"      . 'evil-surround-region)
         ("M-q"    . 'fill-paragraph-or-region)

         :map evil-visual-state-map
         ("k"      . 'evil-previous-visual-line)
         ("j"      . 'evil-next-visual-line)
         ("G"      . 'end-of-buffer-keep-bottom)

         :map isearch-mode-map
         ;; allow for "up/down" history scrolling in / search
         ("<down>" . 'isearch-ring-advance)
         ("<up>"   . 'isearch-ring-retreat)
         )
  )

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
  ;; (evil-select-an-object …) for me often results in errors, so reimplement it with
  ;; inner-object instead
  (pcase-let ((`(,beg ,past_end) (evil-select-inner-object 'evil-defun beg end type count)))
    (goto-char past_end)
    (let ((lst-space-addr (re-search-forward "[^[:space:]\n]" nil t)))
      (if lst-space-addr
          `(,beg ,(- lst-space-addr 1))
        `(,beg ,(point-max))))))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun."
  (evil-select-inner-object 'evil-defun beg end type count))

(define-key evil-outer-text-objects-map "m" 'evil-a-defun)
(define-key evil-inner-text-objects-map "m" 'evil-inner-defun)

;;; START c++-like variable detection
(defvar c++like-non-variable-regex (rx (or whitespace "(" ")"  "{" "}" "," line-end line-start ";" "\"" "'" "&")))

(defun is-in-between-parens ()
  "t if point is as (|) or {|}"
  (and (or (eq (char-after) ?\}) (eq (char-after) ?\)))
       (or (eq (char-after (- (point) 1)) ?\() (eq (char-after (- (point) 1)) ?\{))))

(defun scan-fwd-c++-like-variable ()
  "Walks forward until the first symbol that doesn't look like variable"
  (if (re-search-forward c++like-non-variable-regex nil t)
      (let ((match (match-string 0)))
        ;; Note: now we're at past_end point
        (if (is-in-between-parens) ;; let's match foo() and foo{} too.
            (progn
              (forward-char)
              (if (or (eq (char-after) ?.)  ;; like "foo()."
                      (eq (char-after) ?-)) ;; like "foo()->"
                  (scan-fwd-c++-like-variable)
                (point)))
          (if (string-empty-p match) ;; means we matched EOL
              (point)
            (- (point) 1))))
    nil))

(defun skip-paren-back ()
  "Skips a single () or {} sentence. Return t if these were skipped."
  (if (is-in-between-parens)
      (progn
        (backward-char 2)
        t)
    nil))

(defun range-c++-like-variable ()
  (skip-paren-back)
  (let ((beg (re-search-backward c++like-non-variable-regex nil t)))
    (if (eq beg nil)
        nil
      (let ((match (match-string 0)))
        (if (skip-paren-back)
            (range-c++-like-variable)
          (setq beg (if (string-empty-p match) ;; means we matched line-beginning
                        (point)
                      (+ 1 (point))))
          (forward-char) ;; the match gotta be skipped
          (let ((past_end (scan-fwd-c++-like-variable)))
            (if (eq past_end nil)
                nil
              `(,beg ,past_end))))))))

(evil-define-text-object evil-inner-variable (count &optional beg past_end type)
  "Tries to select a variable or an expression that would result in a variable"
  (range-c++-like-variable))

(evil-define-text-object evil-a-variable (count &optional beg past_end type)
  "Tries to select a variable or an expression that would result in a variable"
  (pcase-let ((`(,beg ,past_end) (range-c++-like-variable)))
    (goto-char past_end)
    (re-search-forward "\\S-" nil t)
    (if (not (eq (point) (+ 1 past_end)))
        `(,beg ,(- (point) 1))
      ;; otherwise try to match whitespace backwards
      (goto-char beg)
      (re-search-backward "\\S-" nil t)
      ;; here we can return (+ 1 (point)) disregarding whether search failed
      `(,(+ 1 (point)) ,past_end))))

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

(defun beginning-of-defun-mark (&optional ARG)
  "Wrapper around beginning-of-defun that saves jump position"
  (interactive)
  (evil--jumps-push)
  (beginning-of-defun ARG))

(defun end-of-defun-mark (&optional ARG)
  "Wrapper around end-of-defun that saves jump position"
  (interactive)
  (evil--jumps-push)
  (end-of-defun ARG))

(define-key evil-normal-state-map (kbd "[ m") 'beginning-of-defun-mark)
(define-key evil-normal-state-map (kbd "] m") 'end-of-defun-mark)

(provide 'emvil)
