;;; -*- lexical-binding: t -*-
(use-package evil
  :init
  (setq evil-jumps-cross-buffers nil
        evil-undo-system 'undo-redo
        evil-want-keybinding nil ;; evil-collection replaces/conflicts evil-keybindings
        ;; use vanilla Emacs keybindings in insert-mode excluding Escape
        evil-disable-insert-state-bindings t)
  (setq-default evil-shift-round nil ;; make '>' not to round the indentation
                evil-goto-definition-functions '(evil-lsp-find-definition ;; declared in main .emacs file
                                                 evil-goto-definition-imenu
                                                 evil-goto-definition-xref
                                                 evil-goto-definition-search))

  :config
  (bind-key "C-/"   'evil-undo)

  ;; newer Evil versions seem to handle this by default, however the older one was
  ;; removing trailing space when you press Escape. This however can be worked around
  ;; by overriding the function below to a noop.
  (defun evil-maybe-remove-spaces (&optional _))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)
    (push '(?» . ("«" . "»")) evil-surround-pairs-alist) ;; add a « » pair
    )

  ;; highlight regions I work with. Just fancies.
  (use-package evil-goggles
    :config
    (setq evil-goggles-blocking-duration 0.05)
    (evil-goggles-mode 1)
    )

  (use-package evil-collection
    :init
    :config
    ;; I don't want to modify hotkeys for modes below
    (delete 'go-mode evil-collection-mode-list)
    (delete 'term evil-collection-mode-list)
    (delete 'diff-mode evil-collection-mode-list)
    (evil-collection-init))

  (use-package goto-chg
    :defer t)

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

  (defun advice-push-jumps(&rest _)
    (evil-set-jump))
  (advice-add 'evil-up-paren :before #'advice-push-jumps)

  (defun evil-select-pasted ()
    "Visually select last pasted text."
    (interactive)
    (if (= (evil-get-marker ?\[)
           (evil-get-marker ?\]))
        (exchange-point-and-mark)
      (evil-goto-mark ?\[)
      (evil-visual-char)
      (evil-goto-mark ?\])))

  (defun evil-execute-macro-no-ding (orig-func count macro)
    "Make mistyped search while recording a macro never break the replay"
    (let ((isearch-wrap-pause 'no-ding))
      (funcall orig-func count macro)))
  (advice-add 'evil-execute-macro :around #'evil-execute-macro-no-ding)

  (evil-mode)
  :bind (:map evil-normal-state-map
         ("C-u"    . 'evil-scroll-up)
         ("k"      . 'evil-previous-visual-line)
         ("j"      . 'evil-next-visual-line)
         ("G"      . 'end-of-buffer-keep-bottom)
         ("g a"    . 'evil-avy-goto-char) ;; let's have some avy integration!
         ("C-j "   . 'evil-avy-goto-char)
         ("C-]"    . 'find-tag) ;; same as in insert mode
         ("S"      . 'evil-surround-region)
         ("M-q"    . 'fill-paragraph-or-region)
         ("g C-y"  . 'exchange-point-and-mark)
         :map evil-insert-state-map
         ("C-j "   . 'evil-avy-goto-char)
         :map evil-visual-state-map
         ("k"      . 'evil-previous-visual-line)
         ("j"      . 'evil-next-visual-line)

         ;; The (end-of-buffer) is buggy, it doesn't work when visual selection is
         ;; active. Gotta debug it when have time.
         ;; ("G"      . 'end-of-buffer-keep-bottom)

         :map isearch-mode-map
         ;; allow for "up/down" history scrolling in / search
         ("<down>" . 'isearch-ring-advance)
         ("<up>"   . 'isearch-ring-retreat)
         )
  )

(defun myhook-evil-mode ()
  ;; I want underscore be part of word syntax table, but not in regexp-replace buffer
  ;; where I'm more comfortable having more verbose navigation with underscore not
  ;; being a part of a word. To achieve this I check if current mode has a syntax
  ;; table different from the global one. The `(eq)' is a lightweight test of whether
  ;; the args point to the same object.
  (unless (eq (standard-syntax-table) (syntax-table))
    ;; make underscore part of a word
    (modify-syntax-entry ?_ "w")))
;; doesn't work with :hook for some reason, so have to call add-hook manually
(add-hook 'evil-local-mode-hook 'myhook-evil-mode)

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
  "If the buffer with the definition is visible in a window, jump
in there, i.e. avoid making buffer visible in multiple places. Otherwise
jump to definition in the next split as usual"
  (interactive)
  (let ((origin-spl (selected-window))
        (origin-buf (current-buffer)))
    (evil-set-jump)
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
  (evil-set-jump)
  (beginning-of-defun ARG))

(defun end-of-defun-mark (&optional ARG)
  "Wrapper around end-of-defun that saves jump position"
  (interactive)
  (evil-set-jump)
  (end-of-defun ARG))

(define-key evil-normal-state-map (kbd "[ m") 'beginning-of-defun-mark)
(define-key evil-normal-state-map (kbd "] m") 'end-of-defun-mark)

(provide 'emvil)
