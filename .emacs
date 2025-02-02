;;; -*- lexical-binding: t -*-

;; NOTE: this file is accompanied by .emacs.d/early-init.el, many vars are set there

;;;; Installation of packages required by this config:
;; It's unfortunately a bit involved, but not too much.
;;
;; 1. In `.emacs' remove `:defer t', so all packages required by use-package would be
;; searched immediately. You'll restore them later
;; 2. Insert at the top of .emacs:
;;       (require 'use-package-ensure)
;;       (setq use-package-always-ensure t)
;;   then start emacs.

;; Allows in a case of an ∞ loop send with «killall -SIGUSR1 emacs» to break it
(setq debug-on-event 'sigusr1)
(define-key special-event-map [sigusr2]
            (lambda () (interactive)
              (save-some-buffers t)
              (kill-emacs)))

(setq
 compile-command "ninja -C build"
 ;; * `interactive-only' warns about replace-regexp, and I tried rewriting this
 ;; function in terms of others — the simple loop they documented is not what I get.
 ;; * `docstring' works around poorly designed behavior of single quotes
 ;; https://emacs.stackexchange.com/questions/73047/emacs-29-docstring-single-quote-escaping-rules-compiler-level-event#comment135034_73048
 byte-compile-warnings '(not interactive-only docstrings))

;; Turn off the bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-indicate-empty-lines) ;; visually indicate where a file ends

(bind-key "C-x C-c" nil) ;; I never use it, but do accidentally press
(bind-key "C-x s" 'save-buffer) ;; I accidentally press it instead of C-x C-s
(bind-key "<f1>" nil) ; am not using it in Emacs, but do globally for recording

(add-to-list 'load-path "~/.emacs.d/lisp")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 95 :width normal))))
 ;; make some evil-goggle colors to look fancier than the default boring gray
 '(evil-goggles-change-face ((t (:background "#ffcccc"))))
 '(evil-goggles-delete-face ((t (:background "#ffcccc"))))
 '(evil-goggles-paste-face ((t (:background "#bbffbb"))))
 '(evil-goggles-undo-redo-add-face ((t (:background "#bbffbb"))))
 '(evil-goggles-undo-redo-change-face ((t (:background "#bbffbb"))))
 '(evil-goggles-undo-redo-remove-face ((t (:background "#bbffbb"))))
 '(evil-goggles-yank-face ((t (:background "#ffff55"))))
 '(evil-goggles-indent-face ((t (:background "light blue"))))
 '(fixed-pitch ((t (:inherit default)))) ; isn't inherited by default
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight bold))))
 '(font-lock-doc-face ((t (:foreground "blue"))))
 '(font-lock-doc-string-face ((t (:foreground "medium blue" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :underline t))))
 '(font-lock-type-face ((t (:foreground "ForestGreen"))))
 '(fringe ((t (:background "grey95" :foreground "blue"))))
 '(line-number ((t (:inherit (shadow default)))))
 '(mode-line ((t (:background "light yellow" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:background "dim gray" :foreground "white"))))
 '(region ((t (:background "gray")))))

;; >> Dynamic font calculation based on current screen size
(defun my-best-font-size (screen-height-mm)
  "Calculates comfortable font size for the screen that Emacs is on"
  (cond
   ((<= screen-height-mm 250) 105)
   (t 95)))

(defvar my-last-screen-height (make-hash-table :test 'equal))
(defun my-set-best-font-size ()
  "Sets font size based on the screen height. Differently sized screens may
have same resolution, so we're interested in height millimeters."
  (let ((curr-screen-height (nth 2 (assoc 'mm-size (frame-monitor-attributes))))
        (last-screen-height (gethash (selected-frame) my-last-screen-height)))
    (when (and curr-screen-height ; terminal would have it equal nil
               (/= (or last-screen-height 0) curr-screen-height))
      (set-face-attribute 'default (selected-frame) :height (my-best-font-size curr-screen-height))
      (puthash (selected-frame) curr-screen-height my-last-screen-height))))
;; << Dynamic font calculation based on current screen size

(defalias 'ar 'align-regexp)
(defalias 'ss 'server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(custom-safe-themes
   '("e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "54a63c60d03a025672ad021381a8bf96788c045908593d535fadb3695fd852c6" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(yasnippet-snippets evil-surround text-mode sp-sublimetext-like smartparens-config emvil autorevert avy evil-goggles undo-fu xr aggressive-fill-paragraph lsp-mode symbol-overlay evil evil-magit magit racer ## smex async go-mode winum company-ngram flycheck-rust php-mode htmlize csharp-mode meson-mode surround ess minizinc-mode rainbow-delimiters atom-dark-theme highlight-numbers color-identifiers-mode company-anaconda anaconda-mode markdown-mode yasnippet smartparens slime pretty-symbols paredit lua-mode idomenu highlight-parentheses helm-company emms ctypes company-c-headers cmake-mode autopair))
 '(semantic-imenu-bucketize-file nil)
 '(semantic-imenu-bucketize-type-members nil)
 '(semantic-imenu-buckets-to-submenu nil)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25))

(defun add-list-to-list (dst src)
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

(defun my-tramp-root-mode-line-indicator ()
  "Add a red 'root' indicator to the mode line when editing as tramp-root."
  (when (and buffer-file-name (string-match "^/sudo:" buffer-file-name))
    (let ((root-indicator '((:eval (propertize " root " 'face '(:foreground "red"))))))
      (setq mode-line-format
            (append (list (nth 0 mode-line-format) (nth 1 mode-line-format))
                    root-indicator
                    (nthcdr 2 mode-line-format))))))
(add-hook 'find-file-hook 'my-tramp-root-mode-line-indicator)

(xterm-mouse-mode -1) ; don't want clicking in terminal to move the pointer
(save-place-mode 1) ; save last caret position in a file

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package package
  :defer t
  :config
  (push '("melpa" . "https://melpa.org/packages/")
        package-archives)
  )

(use-package flycheck
  :defer t
  :init
  (setq flycheck-check-syntax-automatically '(save)) ;; I only want it on save
  :custom-face
  (flycheck-error-list-warning ((t (:inherit warning :foreground "blue"))))
  (flycheck-fringe-warning ((t (:inherit warning :foreground "blue"))))
  (flycheck-warning ((t (:underline (:color "blue" :style wave)))))
  :config
  (defun myactions-flycheck-mode-hook ()
    (when (and (bound-and-true-p flycheck-mode) ;; flycheck hooks is called upon disabling it
               (bound-and-true-p haskell-mode))
      (flycheck-haskell-setup)
      ))
  (add-hook 'flycheck-mode-hook 'myactions-flycheck-mode-hook)
  (add-to-list 'flycheck-clang-warnings "-Wno-missing-braces")
  (add-to-list 'flycheck-clang-args "-frelaxed-template-template-args")
  )

(use-package minibuffer
  :defer t
  :init
  ;; we want partial-completion to be the default one
  (setq completion-styles '(partial-completion basic emacs22)))

(use-package company
  :defer nil ;; :bind implies `defer t', override it
  :bind ("s-/" . company-complete)
  :init
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2 ;; delay before completition
        )
  :bind (:map company-active-map
         ("C-n" . nil)
         ("C-p" . nil)
         ("M-n" . 'company-select-next)
         ("M-p" . 'company-select-previous)
         (:map company-search-map
         ("C-n" . nil)
         ("C-p" . nil)
         ("M-n" . 'company-select-next)
         ("M-p" . 'company-select-previous)))
  :config
  (global-company-mode 1)
  (add-list-to-list 'company-dabbrev-code-modes
                    '(c++-mode c-mode php-mode))
  )
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :defer 2 ;; lazy-load after 2 seconds of being idle
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode)
  )

(use-package autorevert
  :init
  (setq
   ;; Do not break markers in a buffer upon reverting a buffer. Details:
   ;; https://github.com/magit/magit/issues/4442
   revert-buffer-insert-file-contents-function 'revert-buffer-insert-file-contents-delicately
   ;; For some undocumented reason Emacs uses both inotify and polls on
   ;; files. That's stupid, just a waste of resources, sure let's avoid that.
   auto-revert-avoid-polling t)
  :config
  ;; Auto-revert buffers, whose files changed on disk. NOTE: even if I don't use the
  ;; mode per se (because the "on-focus" hook seems to be enough), the
  ;; `auto-revert-buffers' won't work without it being enabled.
  (global-auto-revert-mode 1)
  (defun my-on-focus-hook ()
    (when (frame-focus-state)
      (auto-revert-buffers)
      (my-set-best-font-size)))
  (add-function :after after-focus-change-function #'my-on-focus-hook)
  )

(add-list-to-list 'auto-mode-alist
                  '(("\\.m$"               . octave-mode)
                    ("\\.service\\'"       . conf-mode)
                    ("\\.rules\\'"         . conf-mode)
                    ("\\.glade$\\'"        . xml-mode)
                    ("\\.mzn\\'"           . minizinc-mode)
                    ("[Dd]ockerfile[^.]*$" . dockerfile-mode)
                    ("\\tsx\\'"       . tsx-ts-mode)
                    ))

(defun sort-lines-nocase (beg end)
  (defvar sort-fold-case nil)
  (let ((sort-fold-case t))
    (sort-lines nil beg end)))

(defun replace-char-after (character-number replacement)
  "Replaces char in the buffer after the `character-number' with `replacement'"
  (save-excursion
	(goto-char character-number)
	(delete-char 1)
	(insert-char replacement)))

(defun replace-delimiters (old-closing-char new-opening-char new-closing-char opening-point end-point)
  "Replaces delimiters between `opening-point' and the
`end-point'. Note, that the `opening-point' should point to the
opening symbol, thus the function seeks only the closing"
  (cl-block replace-delimiters
	(let ((closing-point opening-point))
	  (setq closing-point (+ 1 opening-point))
	  (while (< closing-point end-point)
		(if (eq (char-after closing-point) ?\n) ;;no closing delimiter
			(progn
			  (print "Err: no closing delimiter")
			  (cl-return-from replace-delimiters nil))
		  (when (eq (char-after closing-point) old-closing-char)
			(progn
			  (replace-char-after opening-point new-opening-char);;opening delimiter
			  (replace-char-after closing-point new-closing-char);;closing delimiter
			  (cl-return-from replace-delimiters (+ 1 closing-point)))))
		(setq closing-point (+ closing-point 1))))))

(defun swap-<-and-quote-includes (beg end)
  "Swaps in the text between `beg' and `end' the matching «<» and
  «>» character to the \" quote, and vice versa. Mainly used
  before sorting to swap the order of these characters, next
  after the sort to restore the text."
  (cl-block swap-<-and-quote-includes
	(let ((curr-point beg))
	  (while (< curr-point end)
		(setq curr-point (+ curr-point 1))
		;;first check «"»
		(if (eq (char-after curr-point) ?\")
			(progn
			  (setq curr-point (replace-delimiters ?\" ?< ?> curr-point end))
			  (if (eq curr-point nil)
				  (cl-return-from swap-<-and-quote-includes t)))
		  ;;else if «<»
		  (if (eq (char-after curr-point) ?<)
			  (progn
				(setq curr-point (replace-delimiters ?\> ?\" ?\" curr-point end))
				(if (eq curr-point nil)
					(cl-return-from swap-<-and-quote-includes t)))))))))

(defun sort-paragraphs-with-keyword (string)
  "Sorts statements with the `string', case insensitive"
  (save-excursion
	(let (beg end)
	  (goto-char (point-min))
	  (while (and (not (looking-at string));;look for includes, if no then
				 (eq (forward-line 1) 0) ;;go one line down (if not EOF).
				 ))
	  (setq beg (point))
	  (while (and (looking-at string)
				  (eq (forward-line 1) 0)));;to not hang cuz of EOF
	  (setq end (point))
	  (sort-lines-nocase beg end)
	  )))

(defun c-sort-includes ()
  "Sorts #include statements"
  (interactive)
  (save-excursion
	(let (beg end orig-content sorted-content)
	  (goto-char (point-min))
	  (while (and (not (looking-at "#include "));;look for includes, if no then
				  (eq (forward-line 1) 0) ;;go one line down (if not EOF).
				  ))
	  (setq beg (point))
	  (while (and (looking-at "#include ")
				  (eq (forward-line 1) 0)));;to not hang cuz of EOF
	  (setq end (point))
      (setq orig-content (buffer-substring-no-properties beg end))
      (setq sorted-content (with-temp-buffer
                             (insert orig-content)
                             (swap-<-and-quote-includes (point-min) (point-max)) ;;swap characters < and > in includes
                             (sort-lines-nocase (point-min) (point-max)) ;;sort
                             (swap-<-and-quote-includes (point-min) (point-max)) ;;swap the characters  back
                             (buffer-string)))
      (when (not (string= orig-content sorted-content))
        (kill-region beg end)
        (insert sorted-content))
	  )))

(defun csharp-sort-usings ()
  "Sorts using statements by length(for csharp))"
  (interactive)
  (save-excursion
	(let (beg end)
	  (goto-char (point-min))
	  (while (and (not (looking-at "using "));;look for usings, if no then
				 (eq (forward-line 1) 0) ;;go one line down (if not EOF).
				 ))
	  (setq beg (point))
	  (while (and (looking-at "using ")
				  (eq (forward-line 1) 0)));;to not hang cuz of EOF
	  (setq end (point))
	  (sort-lines-nocase beg end))))

;;Ponification syntax table for c=like lamguages
(defun init-prettify-table-c-like ()
  (setq prettify-symbols-alist (list
								'("!=" . ?≠)
								'(">=" . ?≥)
								'("<=" . ?≤)
								;;'("null" . ?∅)
								;;'("NULL" . ?∅)
								;;'("nullptr" . ?∅)
								;; '("int" . ?ℤ)
								'("..." . ?…)
								;; '("float" . ?ℚ);;rational numbers
								)))

(defun myactionsfor-c-mode-common-hook ()
  (fix-c-style-indentation)
  (turn-on-auto-fill) ;;auto fill mode for c modes.
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'csharp-mode 'lua-mode)
    (init-prettify-table-c-like)
    (prettify-symbols-mode 1)
    (if (derived-mode-p 'c-mode)
        (setq flycheck-clang-language-standard "c11")
      (setq flycheck-clang-language-standard "c++17"))
    ))

(use-package cc-mode
  :defer t
  :hook (c-mode-common . myactionsfor-c-mode-common-hook)
  )

(defun exec-cmd-foreach-backward (regex cmd begin end)
  "Executes a command for every match of group #1. Searches
backward, so you can mutate text forward"
  (save-excursion
    (goto-char end)
    (while (re-search-backward regex begin t)
      (goto-char (match-beginning 1))
      (funcall cmd)
      )))

(defun expand-c-args-in-region ()
  (interactive)
  (let ((re (if (member major-mode '(purescript-mode haskell-mode))
                "\\(\\), " ; the style where comma goes first
              ",\\( \\)")))
    (exec-cmd-foreach-backward re 'newline-and-indent
                               (region-beginning) (region-end))))

(use-package csharp-mode
  :defer t
  :config
  (defun myhook-csharp-mode ()
    (flycheck-mode -1) ;; for some reason it lags with C#
    (c-set-offset 'innamespace '+)
    (make-local-variable 'before-save-hook)
    (add-hook 'before-save-hook 'csharp-sort-usings))
  :hook (csharp-mode . myhook-csharp-mode)
  )

(use-package term
  :defer t
  :config
  (yas-minor-mode -1) ;; useless in term-mode, and causes troubles with <tab>
  ;; (global-set-key (kbd "<RET>")
  ;; 				(lambda ()
  ;; 				   (comint-truncate-buffer)
  ;; 				   (term-send-input)
  ;; 				   ))
  )

(use-package avy
  :config
  (setq avy-case-fold-search nil) ;; make searches case sensitive. Well, at least
                                  ;; upcase ones, that as good as avy allows.
  )

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-enable-indentation nil ;; I prefer default indentation functional
        lsp-enable-on-type-formatting nil ;; don't reformat my code

        ;; disable "path in project + in class hierarchy" header. Not useful to me.
        lsp-headerline-breadcrumb-enable nil

        ;; don't show signature/docs in the minibuffer. For me it's almost never useful; at
        ;; the same time, I find annoying that it overrides flycheck messages.
        lsp-eldoc-enable-hover nil

        ;; For these purposes I use symbol-overlay mode instead. Not that am against having
        ;; the two at the same time, but due to some bug in either clangd or lsp-mode, on
        ;; rare occasions I get the wrong symbol highlighted. So let's just disable that.
        lsp-enable-symbol-highlighting nil
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-lens-enable nil ;; more doc annotations, disable them
        lsp-completion-default-behaviour :insert ;; in foo|buzz complete to foobar|buzz \wout eating buzz

        ;; even if given python project has typing utterly broken, mypy still gives
        ;; immensely useful syntax checking that is lacking otherwise with pyls.
        lsp-pylsp-plugins-mypy-live-mode nil
        lsp-pylsp-plugins-mypy-enabled t
        )

  (defun evil-lsp-find-definition (_string _position)
    (condition-case nil
        (lsp-find-definition)
      ('error nil)
      (:success t)))

  :config
  (defun myactionsfor-lsp-mode-hook ()
    (set (make-local-variable 'company-backends)
         ;; lsp-mode provides company-capf. company-lsp they say not supported, Idk
         ;; why. Perhaps because last commit was at 2019, so it may be unmaintainted
         '(company-capf company-etags company-dabbrev))
    )
  (add-hook 'lsp-mode-hook 'myactionsfor-lsp-mode-hook)

  ;; even if given python project has typing utterly broken, mypy still gives
  ;; immensely useful syntax checking that is lacking otherwise with pyls.
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)
                                  ("pyls.plugins.pyls_mypy.live_mode" nil t)))

  ;; I am forgetting the actual name to apply a "fix available" of clangd, because
  ;; it's a bit non-intuitive. So let's alias it to somethig clearer
  (defalias 'lsp-apply-fix 'lsp-execute-code-action)
  )

(use-package emvil ;; my Evil config, in a separate file
  :ensure nil)

(use-package ido
  :init
  (setq-default ido-case-fold t) ;; case insensistivity
  (setq ido-enable-flex-matching t) ;; fuzzy match
  :config
  (ido-mode)
  )

(use-package smex
  :bind ("M-x" . smex)
  :config
  (smex-initialize)
  )

;; visible whitespace config
;; (setq whitespace-style (list 'face 'tabs 'spaces 'space-before-tab 'empty 'space-mark 'tab-mark))
;; (global-whitespace-mode 1) commented out due to problems with markdown

;; scrolling related changes
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; three line at a time
      mouse-wheel-progressive-speed nil            ;; don't accelerate scroll
      mouse-wheel-follow-mouse t                   ;; scroll window under mouse
      scroll-step 2) ;; keyboard scroll two lines at a time

;;A few c-like indentation fixes follows
(setq-default indent-tabs-mode nil)
(setq c-default-stile "stroustrup"
	  c-basic-offset 4)
(defun fix-c-style-indentation ()
  "Fixes indentation for c-like langs"
  (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0);;don't indent namespaces
  (c-set-offset 'func-decl-cont 0)
  (c-set-offset 'cpp-macro 0 nil)
  (c-set-offset 'substatement-open 0) ;; brackets the same level as the statement
  (c-set-offset 'statement-case-open 0) ;; last `{' after `switch() { case foo: {` on the same level
  (c-set-offset 'brace-list-intro c-basic-offset) ;; enums
  (c-set-offset 'inextern-lang 0) ;; extern "C" { … }
  (c-set-offset 'inlambda '+) ;; extern "C" { … }
  )

(use-package lisp-mode
  :defer t
  :init
  ;; the inferior-lisp-program is actually defined by `inf-lisp' but should work
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (add-hook 'lisp-mode-hook (lambda ()
                              (turn-on-auto-fill)
                              ))
  )

;;Make C-a key to work as the home key in a most code editors
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun clipboard-yank-fixed ()
  "A version of clipboard-yank that pastes \"instead\" of selection if one
exists"
  (interactive)
  (when (use-region-p)
	  (kill-region (region-beginning) (region-end)))
  (let ((select-enable-clipboard t))
	(clipboard-yank)))
(bind-key "C-y" 'clipboard-yank-fixed)

(defun clipboard-copy-fixed ()
  (interactive)
  (let ((select-enable-clipboard t))
	(clipboard-kill-ring-save (region-beginning) (region-end))))
(bind-key "M-w" 'clipboard-copy-fixed)

(defun copy-text-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (clipboard-kill-region (point-min) (point-max))))

(defun copy-obj-to-clipboard (obj)
  (copy-text-to-clipboard (prin1-to-string obj)))

(defalias 'obj-to-string #'prin1-to-string)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun align-regexp-with-spaces (orig-func BEG END REGEXP &optional GROUP SPACING REPEAT)
  "Make align-regexp always align with spaces rather than tabs"
  (let ((indent-tabs-mode nil))
    (funcall orig-func BEG END REGEXP GROUP SPACING REPEAT)))
(advice-add 'align-regexp :around #'align-regexp-with-spaces)

(defun kmacro-call-macro-no-ding (orig-func arg &optional no-repeat end-macro macro)
  "Make mistyped search while recording a macro never break the replay"
  (let ((isearch-wrap-pause 'no-ding))
    (funcall orig-func arg no-repeat end-macro macro)))
(advice-add 'kmacro-call-macro :around #'kmacro-call-macro-no-ding)

(defun evil-exchange-point-and-mark (orig-func &optional arg)
  "Move caret one char to the right before passing control
over. Needed in my case because I use `exchange-point-and-mark'
to select last pasted text, and I usually go to normal mode
before doing that, whic by itself makes caret move one char left"
  (when (and (memq evil-state '(visual normal))
             (< (point) (point-max)))
   (forward-char))
  (funcall orig-func arg))
(advice-add 'exchange-point-and-mark :around #'evil-exchange-point-and-mark)

;;I am using only eng and ru layout, and sometimes TeX. So it would be better to use layout toggling
;;only with this two layouts, and don't mess it with TeX or whatever some day come in my mind to try.
(defun input-switch-eng-ru ()
  (interactive)
  "Swithes an input methods between russian and english.

Works just as the standard «toggle-input-method»(and in fact
calls it), but before checks if an input method one of russian or
english. If it isn't, set the lang to english."
  (if (not current-input-method)
      (set-input-method "russian-computer")
    (deactivate-input-method)))
(bind-key "C-\\" 'input-switch-eng-ru)
(global-set-key (kbd "s-\\") (lambda () (interactive);;sets input method to «TeX»
							   (set-input-method "TeX")))
(define-key isearch-mode-map "\C-g" nil) ;; I like to interrupt search immediately

(defun improved-newline-and-indent()
	"Calls `newline-and-indent' always with an exception of being
called insede of '{}' braces. In this case it throws the braces
in a few lines, and puts the cursor at the middle line"
  (interactive "*")
  (if (and (eq (char-before) ?{);;if inside "{|}"
		   (eq (char-after) ?}))
      (progn
        (when (bound-and-true-p smartparens-mode)
          ;; smartparens tries to do the same thing as me, but fails at that,
          ;; because it only works when I just typed the pair. Otherwise it does
          ;; nothing.
          ;;
          ;; So I tried detecting the situation, but it gets worse: sometimes the
          ;; sp-last-inserted-pair is set, other times it isn't. All in all, the
          ;; only way to work around this problem seems to be to restart the
          ;; mode.
          (smartparens-mode 0)
          (smartparens-mode 1))
        (indent-according-to-mode) ;; indent the line
        (newline 2) ;; 2 newlines
        (indent-according-to-mode) ;; indent the line
        (forward-line -1)
        (indent-according-to-mode) ;; indent the line
        )
    (newline-and-indent)))

(bind-key "C-w" 'clipboard-kill-region)
(bind-key "s-y" 'yank)
(use-package idomenu
  :bind ("s-i" . idomenu))
(bind-key "<RET>" 'improved-newline-and-indent)
(global-set-key (kbd "<f11>") (lambda () (interactive) (ff-find-other-file nil t))) ;switch between a corresponding c/c++ header and a file
(bind-key "<C-mouse-4>" 'text-scale-decrease);set in wheel font decrease
(bind-key "<C-mouse-5>" 'text-scale-increase);set in wheel font increase

(use-package highlight-numbers
  :init
  (defun enable-highlight-numbers-mode ()
    ;; the mode breaks rendering in the following modes. The upstream fix would
    ;; simple (or not): the
    ;;      `((,regexp . 'highlight-numbers-number))
    ;; line has to be replaced with
    ;;      `((,regexp . (0 highlight-numbers-number prepend)))
    ;; or something similar. Unfortunately it makes font-lock fail with "unknown
    ;; variable highlight-numbers-number" even though it is not not a variable but a
    ;; facename and the syntax is correct per `font-lock-keywords' Help. That's as
    ;; much of a motivation I had to debug that.
    (unless (derived-mode-p 'eww-mode 'diff-mode)
      (highlight-numbers-mode t)))
  (define-globalized-minor-mode global-highlight-numbers-mode
    highlight-numbers-mode enable-highlight-numbers-mode)

  :config
  (set-face-attribute 'highlight-numbers-number nil :weight 'bold :foreground "blue" :background "light gray")
  (global-highlight-numbers-mode)
  )

(column-number-mode)
(delete-selection-mode)

;; setup and enable flyspell mode
(setq-default flyspell-issue-message-flag nil)
;; (dolist (hook '(c-mode-hook))
;;   (add-hook hook (lambda ()
;; 				   (ispell-change-dictionary "english")
;; 				   (flyspell-mode 1)
;; 				   )));;enable for c
;; (dolist (hook '(c++-mode-hook))
;;   (add-hook hook (lambda ()
;; 				   (ispell-change-dictionary "english")
;; 				   (flyspell-mode 1)
;; 				   )));;enable for c++

(defun is-in-comment ()
  "tests if point is in comment"
  (nth 4 (syntax-ppss)))

(defun current-line-string ()
  "returns current line as a string"
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(use-package smartparens-config
  :ensure nil
  :init
  (setq-default sp-autoskip-closing-pair t) ;; skip only when pair is active
  (setq sp-show-pair-from-inside t
        sp-escape-quotes-after-insert nil ;; https://github.com/Fuco1/smartparens/issues/783#issuecomment-324598759
        sp-python-insert-colon-in-function-definitions t)
  :config
  (use-package sp-sublimetext-like :ensure nil) ;; sublime-like behavior of smartparens
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  ;; I use smartparens which can highlight matching pairs, so I don't need
  ;; emacs's default blink parenthesis functionality
  (setq blink-paren-function nil)

  (defun maybe-add-semicolon-paren (_id action _context)
    "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
    ;; here, caret supposed to be in between parens, i.e. (|)
    (when (and (eq action 'insert)
               (looking-at ")\\s-*$")
               (not (is-in-comment))
               (not (string-match-p
                     (regexp-opt '("if" "else" "switch" "for" "while" "do" "define") 'words)
                     (current-line-string))))
      (save-excursion
        (forward-char) ;; skip closing brace
        (insert ";"))))

  (defun maybe-add-semicolon-paren-rust (_id action _context)
    "A helper function that inserts semicolon after closing
parentheses when appropriate, for Rust lang"
    ;; here, caret supposed to be in between parens, i.e. (|)
    (when (and (eq action 'insert)
               (looking-at ")\\s-*$")
               (not (is-in-comment))
               (not (string-match-p
                     (regexp-opt '("fn" "if" "for" "while") 'words)
                     (current-line-string))))
      (save-excursion
        (forward-char) ;; skip closing brace
        (insert ";"))))

  (defun maybe-add-semicolon-bracket (_id action _context)
    "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
    ;; here, caret supposed to be in between braces, i.e. {|}
    (when (and (eq action 'insert)
               (looking-at "}\\s-*$")
               (not (is-in-comment))
               (string-match-p "\\breturn\\b" (current-line-string)))
      (save-excursion
        (forward-char) ;; skip closing brace
        (insert ";"))))

  (defun maybe-complete-lambda (_id action _context)
    "Completes C++ lambda, given a pair of square brackets"
    (when (eq action 'insert)
      (let ((curr-line (current-line-string))
            ;; try detecting "auto foo = []"
            (lambda-assign-regex "=\\s-*\\[\\]$")
            ;; try detecting "func([])" and "func(arg1, [])"
            (lambda-inline-regex "[(,]\\s-*\\[\\]"))
        (when (or (string-match-p lambda-assign-regex curr-line)
                  (string-match-p lambda-inline-regex curr-line))
          (save-excursion
            ;; here, caret supposed to be in between brackets, i.e. [|]
            (forward-char) ;; skip closing brace
            (insert "() {}")
            (when (eolp)
              (insert ";"))
            )))))

  (sp-with-modes '(c-mode c++-mode java-mode csharp-mode vala-mode js-mode)
    (sp-local-pair "(" nil :post-handlers '(:add maybe-add-semicolon-paren))
    (sp-local-pair "{" nil :post-handlers '(:add maybe-add-semicolon-bracket)))
  (sp-local-pair 'c++-mode "[" nil :post-handlers '(:add maybe-complete-lambda))
  (sp-local-pair 'rust-mode "(" nil :post-handlers '(:add maybe-add-semicolon-paren-rust))

  (defun sp-emacs-style-backtick (_ _ _)
    "Text-mode is used for editing the commit messages. Emacs has style where
a backtick ends with a singular quote, so let's check if current dir is
part of Emacs repo, in which case replace the pair that SP inserted."
    (when (string-match-p "/emacs/" default-directory)
      (save-excursion
        (delete-char 1)
        (insert "'"))))

  (sp-local-pair 'text-mode "`" nil :post-handlers '(:add sp-emacs-style-backtick))
  )

;; mode to highlight outside parentheses
(use-package highlight-parentheses
  :init
  (setq highlight-parentheses-colors nil
        highlight-parentheses-background-colors '("light green" "yellow" "orange" "pink"))
  :config
  (global-highlight-parentheses-mode)
  )

(defun large-files-throttling ()
  "Disables certain features if file is too large"
  (when (> (buffer-size) (* 1024 1024 10))
    (highlight-parentheses-mode -1)))
(add-hook 'find-file-hook #'large-files-throttling)

(defun myfunc-before-save-hook ()
  (unless (derived-mode-p 'diff-mode)
    (delete-trailing-whitespace))
  ;; (when (derived-mode-p 'c-mode 'c++-mode)
  ;; 	 (c-sort-includes))
  )
(add-hook 'before-save-hook 'myfunc-before-save-hook)

(use-package rust-mode
  :defer t
  :config
  (cl-assert (boundp 'company-backends)) ;; I always use company-mode
  (defun myfunc-rust-mode-hook ()
    ;; note: if racer-mode breaks in some way (e.g. no more completions), do:
    ;; 1. `rustup component add rust-src` 2. `cargo +nightly install racer --force'
    (racer-mode)
    (set (make-local-variable 'company-backends)
         '(company-capf company-etags company-dabbrev))
    )
  (add-hook 'rust-mode-hook 'myfunc-rust-mode-hook)
  )

(use-package symbol-overlay
  :bind ("s-`" . symbol-overlay-put)
  :defer nil ;; :bind implies `defer t', override it
  :init
  (setq symbol-overlay-ignore-functions nil      ;; don't ignore keywords in various languages
        symbol-overlay-map (make-sparse-keymap)) ;; disable special cmds on overlays
  (defun enable-symbol-overlay-mode ()
    (unless (or (minibufferp)
                (derived-mode-p 'magit-mode)
                (derived-mode-p 'xref--xref-buffer-mode))
      (symbol-overlay-mode t)))
  (define-global-minor-mode global-symbol-overlay-mode ;; name of the new global mode
    symbol-overlay-mode ;; name of the minor mode
    enable-symbol-overlay-mode)
  :config
  (global-symbol-overlay-mode)
  )

(defun make-«»-pairs ()
  ;; TODO: I can make it globally by using (standard-syntax-table) but for some
  ;; reason pairing it makes it think that ( is a pair to ». Looks like a bug to
  ;; me. Gotta ask on the mailing list.
  (modify-syntax-entry ?« "(»")
  (modify-syntax-entry ?» ")«"))

(defun common-hook-for-text-modes ()
  (make-«»-pairs)
  (setq case-fold-search t) ;; ignore case in search
  (set (make-local-variable 'dabbrev-upcase-means-case-search) nil) ;; ignore case
  (set (make-local-variable 'company-minimum-prefix-length) 2)
  (flyspell-mode))

(use-package text-mode
  :defer t
  :ensure nil
  :hook (text-mode . common-hook-for-text-modes)
  )

(defun myfunc-gud-gdb-mode ()
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (company-mode 0)
  (local-set-key (kbd "C-d") 'delete-char) ;; gdb rebinds the key
  )
(add-hook 'gud-mode-hook 'myfunc-gud-gdb-mode)
(add-hook 'gdb-mode-hook (lambda () (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))
(put 'erase-buffer 'disabled nil)

(defun latin-to-gothic (φp1 φp2 φreverse-direction-p)
  "Replace English alphabets to Unicode gothic characters.
For example, A ⇒ 𝔄, a ⇒ 𝔞.

When called interactively, work on current line or text selection.

If any `universal-argument' is called first, reverse direction.

When called in elisp, the φp1 and φp2 are region begin/end positions to work on.

URL `http://ergoemacs.org/misc/thou_shalt_use_emacs_lisp.html'
Version 2015-04-12"
  (interactive
   (if (use-region-p)
       (progn
         (list (region-beginning) (region-end) current-prefix-arg ))
     (list (line-beginning-position) (line-end-position) current-prefix-arg )))
  (let (
        (ξlatin-to-gothic [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])
        ξuseMap
        )
    (if φreverse-direction-p
        (progn (setq ξuseMap
                     (mapcar
                      (lambda (ξx)
                        (vector (aref ξx 1) (aref ξx 0)))
                      ξlatin-to-gothic)))
      (progn (setq ξuseMap ξlatin-to-gothic)))
    (save-excursion
      (save-restriction
        (narrow-to-region φp1 φp2)
        (let ( (case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while (search-forward (elt ξx 0) nil t)
               (replace-match (elt ξx 1) 'FIXEDCASE 'LITERAL)))
           ξuseMap))))))

(defun text-to-html ()
  "In active region quotes a spec. chars like «<» and «&», and
  inserts «<br>» tag for line breaks"
  (interactive)
  (let ((start (region-beginning))
		(end (region-end)))
    (use-package sgml-mode)
    (sgml-quote start end)
    (replace-regexp "^\\(.*\\)$" "\\1<br>" nil start end)))

(use-package haskell-mode
  :defer t
  :config
  (defun haskell-sort-n-align-imports ()
    "Sorts and aligns Haskell imports"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (looking-at "import "));;look for usings, if no then
                  (eq (forward-line 1) 0) ;;go one line down (if not EOF).
                  ))
      (haskell-sort-imports)))

  (defun myhook-haskell-mode ()
    (haskell-indent-mode)
    (make-local-variable 'before-save-hook)
    (add-hook 'before-save-hook 'haskell-sort-n-align-imports))
  (add-hook 'haskell-mode-hook 'myhook-haskell-mode)
  :hook (haskell-mode . myhook-haskell-mode)
  )

(use-package shell
  :defer t
  :config
  (defun myfunc-shell-mode ()
    (flycheck-mode 1)
    )
  :hook (shell-mode . myfunc-shell-mode)
  )

(use-package ispell
  :defer t
  :init
  ;; It's unclear if the default aspell supports multiple langs at once, but Emacs
  ;; with aspel backend doesn't. Let's use hunspell instead.
  (setq ispell-program-name "hunspell")
  (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
  :config
  (ispell-set-spellchecker-params) ;; ispell initialization, a mandatory call
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (ispell-change-dictionary "en_US,ru_RU" t) ;; with t set dict globally
  )

(use-package markdown-mode
  :defer t
  :init
    (setq-default markdown-enable-math t) ;; enable latex delimiters
    (setq markdown-command "pandoc")
    (defun my-markdown-hook ()
      (common-hook-for-text-modes)
      (setq-local evil-shift-width 2))
  :custom-face
  (markdown-inline-code-face ((t (:inherit markdown-code-face :background "light blue"))))
  :config
  (add-hook 'markdown-mode-hook 'my-markdown-hook))

(defun just-one-space-region ()
  "Replaces every space in active region between words to one
  space and removes newlines (useful e.g. to join arguments in a
  function declaration), for inactive region calls
  `just-one-space'"
  (interactive)
  (if mark-active
	  (progn (replace-regexp "\\s-+" " " nil (region-beginning) (region-end))
			 (replace-regexp "
" "" nil (region-beginning) (region-end)))
	(just-one-space)))
(bind-key "M-<SPC>" 'just-one-space-region)

;; Force gdb-mi to not dedicate any windows
(advice-add 'gdb-display-buffer
	    :around (lambda (orig-fun &rest r)
		      (let ((window (apply orig-fun r)))
			(set-window-dedicated-p window nil)
			window)))

(advice-add 'gdb-set-window-buffer
	    :around (lambda (orig-fun name &optional ignore-dedicated window)
		      (funcall orig-fun name ignore-dedicated window)
		      (set-window-dedicated-p window nil)))

(use-package color-identifiers-mode
  :init
  (setq color-identifiers:recoloring-delay 1
        color-identifiers:extra-face-attributes '(:weight bold))
  :config

  (defun myfunc-color-identifiers-mode-hook ()
    (let ((faces '(font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-builtin-face font-lock-preprocessor-face font-lock-constant-face)))
      (dolist (face faces)
        (face-remap-add-relative face '(:inherit default))))
    (face-remap-add-relative 'font-lock-keyword-face '(bold))
    (face-remap-add-relative 'font-lock-builtin-face '(bold))
    (face-remap-add-relative 'font-lock-preprocessor-face '(bold))
    (face-remap-add-relative 'font-lock-function-name-face '(bold))
    (face-remap-add-relative 'font-lock-string-face '((:foreground "#b33200000000")))
    (face-remap-add-relative 'font-lock-constant-face '(bold))
    (face-remap-add-relative 'haskell-operator-face '((:foreground "#b33200000000")))
    )
  (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)

  ;; (defun color-identifiers:colorize (limit) — the function changed to make vars "bold". FTR.
  ;;   (color-identifiers:scan-identifiers
  ;;    (lambda (start end)
  ;;      (let* ((identifier (buffer-substring-no-properties start end))
  ;;             (hex (color-identifiers:color-identifier identifier)))
  ;;        (when hex
  ;;          (put-text-property start end 'face `(:foreground ,hex :weight ,'bold))
  ;;          ;; (put-text-property start end 'face `(:weight ,'bold))
  ;;          (put-text-property start end 'color-identifiers:fontified t))))
  ;;    limit))
  (global-color-identifiers-mode 1) ;; semantic highlight of variables
)

(split-window-right) ;; something I always do, let's automatize that

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;; heaader guards, source https://www.emacswiki.org/emacs/AutoInsertHeaderGuards
(defun maybe-add-newline-at-buf-start ()
  (if (and (not (= (point-min) (point-max)))
           (char-equal (char-after (point-min)) ?\n)
           (char-equal (char-after (1+ (point-min))) ?\n))
      ""
    "\n"))
(defun maybe-add-newline-at-buf-end ()
  (if (and (not (= (point-min) (point-max)))
           (char-equal (char-before (point-max)) ?\n)
           (char-equal (char-before (1- (point-max))) ?\n))
      ""
    "\n"))

(defun my-insert-C-header ()
  "Inserts C header guard"
  (if (buffer-file-name)
      (let*
          ((fName (upcase (replace-in-string "-" "_" (file-name-nondirectory (file-name-sans-extension buffer-file-name)))))
           (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H"
                          (maybe-add-newline-at-buf-start)))
           (begin (point-marker))
           )
        (progn
          ;; Insert the Header Guard
          (goto-char (point-min))
          (insert ifDef)
          (goto-char (point-max))
          (insert (maybe-add-newline-at-buf-end) "#endif" " // " fName "_H")
          (goto-char begin))
        )
    (message (concat "Buffer " (buffer-name) " must have a filename"))))

(defun my-insert-purescript-header ()
  "Inserts typical purescript module header"
  (if (buffer-file-name)
      (let*
          ((fName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
           (str-intro (concat "module " fName " where" "\n\nimport Prelude\n")))
        ;; Insert the Header Guard
        (goto-char (point-min))
        (insert str-intro)
        ;; don't bother restoring the point 'cause unlike c-like langs, here the file
        ;; without the header is nonfunctional, so most likely the file was empty
        ;; when this got called
        (goto-char (point-max)))
    (message (concat "Buffer " (buffer-name) " must have a filename"))))

(defun my-insert-lang-header ()
  (interactive)
  (funcall
   (pcase major-mode
     ('c-mode          #'my-insert-C-header)
     ('purescript-mode #'my-insert-purescript-header)
     (_                 #'ignore))))

(global-set-key [f12] #'my-insert-lang-header)

(use-package winum
  :defer nil ;; :bind implies `defer t', override it
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)
         ("M-0" . winum-select-window-0) ;; minibuf
         )
  :init
  (defun restore-winum-mode-map (mode-map)
    (define-key mode-map (kbd "<M-DEL>") nil)
    (define-key mode-map (kbd "M-1") nil)
    (define-key mode-map (kbd "M-2") nil)
    (define-key mode-map (kbd "M-3") nil)
    (define-key mode-map (kbd "M-4") nil)
    (define-key mode-map (kbd "M-5") nil)
    (define-key mode-map (kbd "M-6") nil)
    (define-key mode-map (kbd "M-7") nil)
    (define-key mode-map (kbd "M-8") nil)
    (define-key mode-map (kbd "M-9") nil)
    (define-key mode-map (kbd "M-0") nil))
  (defun winum-restore-diff-mode () ;; diff mode overrides the keys, undo that
    (restore-winum-mode-map diff-mode-map))
  (add-hook 'diff-mode-hook 'winum-restore-diff-mode)

  (defun winum-restore-magit-mode () ;; magit overrides the keys, undo that
    (restore-winum-mode-map magit-mode-map))
  (add-hook 'magit-mode-hook 'winum-restore-magit-mode)
  :config
  (winum-mode)
  )

;; C++ regex to conver constructor args to initialization (assumes no types with spaces & commas)
;; \b[^,]+? \b\(.+?\)\b → \1(\1)

;;;; VALA stuff
;; I hack on vala stuff, and existing vala modes are so bad that it's better to
;; derive some other mode
(define-derived-mode vala-mode csharp-mode "Quick'n'dirty vala mode")
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))

;; the vanilla etags-goto-tag-location, but trims the pattern before match
(defun etags-goto-tag-location (tag-info)
  "Go to location of tag specified by TAG-INFO.
TAG-INFO is a cons (TEXT LINE . POSITION).
TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present.

If the tag isn't exactly at the given position, then look near that
position using a search window that expands progressively until it
hits the start of file."
  (let ((startpos (cdr (cdr tag-info)))
	(line (car (cdr tag-info)))
	offset pat)
    (if (eq (car tag-info) t)
	;; Direct file tag.
	(cond (line (progn (goto-char (point-min))
			   (forward-line (1- line))))
	      (startpos (goto-char startpos))
	      (t (error "etags.el BUG: bogus direct file tag")))
      ;; This constant is 1/2 the initial search window.
      ;; There is no sense in making it too small,
      ;; since just going around the loop once probably
      ;; costs about as much as searching 2000 chars.
      (setq offset 1000
            ;; Improve the match by trimming the pattern. It's
            ;; impossible anyway that 2 tags would only differ by
            ;; trailing whitespace.
	    pat (regexp-quote (string-trim (car tag-info))))
      ;; The character position in the tags table is 0-origin and counts CRs.
      ;; Convert it to a 1-origin Emacs character position.
      (when startpos
        (setq startpos (1+ startpos))
        (when (and line
                   (eq 1 (coding-system-eol-type buffer-file-coding-system)))
          ;; Act as if CRs were elided from all preceding lines.
          ;; Although this doesn't always give exactly the correct position,
          ;; it does typically improve the guess.
          (setq startpos (- startpos (1- line)))))
      ;; If no char pos was given, try the given line number.
      (or startpos
	  (if line
	      (setq startpos (progn (goto-char (point-min))
				    (forward-line (1- line))
				    (point)))))
      (or startpos
	  (setq startpos (point-min)))
      ;; First see if the tag is right at the specified location.
      (goto-char startpos)
      (setq found (looking-at pat))
      (while (and (not found)
		  (progn
		    (goto-char (- startpos offset))
		    (not (bobp))))
	(setq found
	      (re-search-forward pat (+ startpos offset) t)
	      offset (* 3 offset)))	; expand search window
      (or found
	  (re-search-forward pat nil t)
	  (user-error "Rerun etags: `%s' not found in %s"
                      pat buffer-file-name)))
    ;; Position point at the right place
    ;; if the search string matched an extra Ctrl-m at the beginning.
    (and (eq selective-display t)
	 (looking-at "\^m")
	 (forward-char 1))
    (beginning-of-line)))
;;;; END of VALA stuff

;;;; BUGS workarounds START
;; removes warning: backend company-capf error "Nothing to complete" with args (prefix)
(defun et/semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))
;;;; BUGS workarounds END

;;; START Transpose arguments for c-like mode. Credits: https://emacs.stackexchange.com/a/47934/2671
(defun c-forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while
      (progn
        (comment-forward most-positive-fixnum)
        (looking-at "[^,)]"))
    (condition-case ex (forward-sexp)
      ('scan-error (if (looking-at "[<>]") ;; likely c++ template
                       (forward-char)
                     (throw ex nil))))
    )
  (point)
  )

(defun c-backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point)) cur)
    (up-list -1) ;; try to quit first balanced expression
    (while (looking-at "<") ;; c++ template opening bracket
      (up-list -1))
    (forward-char)
    (while
      (progn
        (setq cur (point))
        (> pt (c-forward-to-argsep))
        )
      (forward-char)
      )
    (goto-char cur))
  )
(defun c-transpose-args-direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let*
    (
      ;; only different to pt when not 'is_forward'
      (pt-original (point))
      (pt
        (progn
          (when (not is_forward)
            (goto-char (- (c-backward-to-argsep) 1))
            (unless (looking-at ",")
              (goto-char pt-original)
              (user-error "Argument separator not found"))
            )
          (point))
        )
      (b (c-backward-to-argsep))
      (sep
        (progn (goto-char pt)
          (c-forward-to-argsep)))
      (e
        (progn
          (unless (looking-at ",")
            (goto-char pt-original)
            (user-error "Argument separator not found"))
          (forward-char)
          (c-forward-to-argsep))
        )
      (ws-first
        (buffer-substring-no-properties
          (goto-char b)
          (progn (skip-chars-forward "[[:space:]\n]")
            (point))
          )
        )
      (first (buffer-substring-no-properties (point) sep))
      (ws-second
        (buffer-substring-no-properties
          (goto-char (1+ sep))
          (progn (skip-chars-forward "[[:space:]\n]")
            (point))
          )
        )
      (second (buffer-substring-no-properties (point) e))
      )
    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
      (goto-char
        (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))
          )
        )
      (goto-char
        (+
          b (length ws-first)
          ;; Apply initial offset within the word.
          (- pt-original (+ pt 1 (length ws-second)))
          )
        )
      )
    )
  )

(defun c-transpose-args-forward () (interactive) (c-transpose-args-direction t))
(defun c-transpose-args-backward () (interactive) (c-transpose-args-direction nil))
(define-key evil-normal-state-map (kbd "t f") 'c-transpose-args-forward)
(define-key evil-normal-state-map (kbd "t b") 'c-transpose-args-backward)
;;; END Transpose arguments in c-like mode.

(defun get-file:line (&optional is-interactive)
  "returns a string \"buffer_name:line_at_point\""
  (interactive "p")
  (let ((ret (concat (file-name-nondirectory buffer-file-truename) ":" (number-to-string (line-number-at-pos)))))
    (if is-interactive
        (copy-text-to-clipboard ret)
      ret)))

(defun get-line-commit ()
  "Get to clipboard last Git commit that modified current line."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-number (line-number-at-pos))
         (command (append
                   (split-string
                    (format "git blame -L %d,%d --"
                            line-number line-number))
                   (list file-name)))
         (line (car (apply 'process-lines command)))
         (commit-hash (car (split-string line))))
    (copy-text-to-clipboard commit-hash)))

;; note: some modes override this. In particular, C and C++ standard requires a newline
(setq-default require-final-newline nil)

;; By default keyboard-quit is giltchy, it randomly fails to work until you press it
;; second time. Let's just bind C-g to press it twice always
(global-set-key (kbd "C-g") (lambda () (interactive) (keyboard-quit) (keyboard-quit)))

(defun insert-print-for-the-word()
  "Inserts a print above current line for the word a cursor is
upon or for the selected text if it's active"
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'word 'no-properties)))
        (print-str (pcase major-mode
                     ('c++mode '("printf(\"%s\", " ");"))
                     ('c-mode '("printf(\"%s\", " ");"))
                     ('rust-mode '("println!(\"{}\", " ");"))
                     )))
    (if (not word)
        (print "no word at point")
      (if (not print-str)
        (print "unknown mode")
      (evil-open-above 0)
      (insert (concat (nth 0 print-str)
                      word
                      (nth 1 print-str)))
          ))))

(defun csv-to-tabs()
  (let ((text (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))
              ))
    (replace-regexp-in-string "[[:blank:]]*|[[:blank:]]*" "\t" text)))

(defun csv-to-tabs-copy ()
  (interactive)
  (copy-text-to-clipboard (csv-to-tabs)))

(defun insert-and-indent-align (str)
  "Inserts a text and indents acc. to mode"
    (push-mark)
    (insert str)
    (indent-region (region-beginning) (region-end))
    (align (region-beginning) (region-end)))

(use-package python
  :defer t
  :ensure nil
  :init
  ;; Note: we don't set python-indent-offset to anything other than 4. But it may get
  ;; changed automatically by a python-mode, which scans the file it opens to see
  ;; what indentation size should be used. I presume one can disable that, but am not
  ;; sure, that sounds useful actually.

  (defun python-insert__init__ (params-w-comma)
    "Creates a python __init__() based on arguments"
    (let ((params-list (split-string params-w-comma "," t "\s-*")))
      (let ((init-header "def __init__(self")
            (init-body "")
            (init ""))
        (dolist (param params-list)
          (setq init-header (concat init-header ", " param))
          (let ((param-no-type (car (split-string param ":" t "\s-*"))))
            (setq init-body (concat init-body "\nself." param-no-type " = " param-no-type)))
          )
        (setq init (concat init-header "):" init-body))
        (insert-and-indent-align init)
        )))

  (defun python-insert__init__and__repr__ (params-w-comma)
    "Creates python __init__() and __repr__() based on arguments
Bug: inserted __repr__ needs to have added indentation level
manually. Arguably, it is a bug in python-mode, because
indentation is implemented there"
    (let ((params-list (split-string params-w-comma "," t "\s-*")))
      (let ((init-header "def __init__(self")
            (init-body "    ")
            (init "    ")
            (repr-body "    ")
            (fst-param-is-processed t))
        (dolist (param params-list)
          (setq init-header (concat init-header ", " param))
          (let ((param-no-type (car (split-string param ":" t "\s-*"))))
            (setq init-body (concat init-body "\nself." param-no-type "= " param-no-type))
            (setq repr-body (concat repr-body
                                    (if fst-param-is-processed
                                        ;; Note: the 4 spaces is a workaround because otherwise
                                        ;; python-mode refuses to properly indent the body
                                        "\n        return f'{{"
                                      "\\\n    + f'")
                                    param-no-type " = {self." param-no-type "}, '"))
            (setq fst-param-is-processed nil))
          )
        ;; now replace last "}" with "}}}" since it's hard to set it in place beforehand otherwise.
        (setq repr-body (replace-regexp-in-string "}, '$" "}}}'" repr-body))
        (setq init (concat init-header "):" init-body
                           "\n\n    def __repr__(self):" repr-body
                           ))
        (insert-and-indent-align init)
        )))
  )

;; credits to https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; make `rgrep' function skip binary files
(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nHI --null -e <R> \{\} +")

(defun window-swap-states-rev ()
  "window-swap-states that works in the reverse direction"
  (interactive)
  (window-swap-states (window-normalize-window nil t) (previous-window))
  )

;;;; a few functions to remember/recall layout of opened buffers
(defvar winstack-stack '()
  "A Stack holding window configurations.
Use `winstack-push' and
`winstack-pop' to modify it.")

(defun winstack-push()
  "Push the current window configuration onto `winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (car winstack-stack))
         (compare-window-configurations (car winstack-stack) (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) winstack-stack)
           (message (concat "pushed " (number-to-string
                                       (length (window-list (selected-frame)))) " frame config")))))

(defun winstack-pop()
  "Pop the last window configuration off `winstack-stack' and apply it."
  (interactive)
  (if (car winstack-stack)
      (progn (set-window-configuration (pop winstack-stack))
             (message "popped"))
    (message "End of window stack")))
;;;; END OF a few functions to remember/recall layout of opened buffers

;;;; some perofrmance related changes, credits to https://github.com/geza-herman/emacs/tree/fast-emacs
;; Don't care about bidirectional text. These settings make processing long lines faster.
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

(defun redmine-insert-issue-template ()
  (interactive)
  (insert "
h2. Шаги к воспроизведению



h3. Ожидается



h3. На самом деле



h2. Доп. информация
"))

(defun markdown-insert-spoiler ()
  (interactive)
  (insert "
<details>
    <summary>Details</summary>

    Text here. A mandatory empty line above, otherwise markdown misrenders it.
</details>"))

(defun markdown-insert-issue-template ()
  (interactive)
  (insert "
# Steps to reproduce



## Expected



## Actual



# Additional information
"))

;;;; notify-if-file-modified BEGIN
(defun notify-if-file-modified (_ _)
  "Check if a file on disk changed before changing the buffer"
  (unless (verify-visited-file-modtime)
    (revert-buffer t t)))
(defun enable-notify-if-file-modified ()
  ;; before-change-functions is a buffer local var, so hook it up when a file is opened
  (when (buffer-file-name) ;; ignore buffers with no file attached
    (add-to-list 'before-change-functions 'notify-if-file-modified))
  )
(add-to-list 'find-file-hook 'enable-notify-if-file-modified)
;;;; notify-if-file-modified END

(use-package meson-mode
  :defer t
  :config
  (cl-assert (boundp 'company-backends)) ;; I always use company-mode
  (defun myfunc-meson-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-dabbrev))
    (setq-local company-dabbrev-downcase t)
    )
  (add-hook 'meson-mode-hook 'myfunc-meson-mode-hook)
  )

(use-package smerge-mode ;; make smerge-vc-next-conflict always available
  :config
  (defun smerge-resolve-all-in-file-to (to-keep)
    "Resolves all conflicts inside a file in preference of TO-KEEP.

TO-KEEP decides which part to keep and is one of `upper', `base',
`lower'."
    (interactive
     (list (completing-read "Keeping: " '(upper base lower))))
    (let ((resolve-func
           (pcase to-keep
             ("upper" 'smerge-keep-upper)
             ("base"  'smerge-keep-base)
             ("lower" 'smerge-keep-lower)
             (_ (error "Unknown resolution argument!"))))
          (num-chars-before (point-max)))
      (save-excursion
        (goto-char (point-min))
        (while (ignore-errors (not (smerge-next)))
          (funcall resolve-func)))
      (when (= num-chars-before (point-max))
        (message "No conflicts were found"))))
  )

(use-package diff-mode
  :defer t
  :init
  ;; update-on-the-fly is broken https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50762
  ;; use old good recalculation upon saving the buffer
  (setq diff-update-on-the-fly nil)
  )

(use-package paren
  :defer t
  :init
  ;; I use smartparens, so have show-parens mode disabled
  (setq show-paren-mode nil)
  )

(use-package textile-mode
  :defer t
  :init
  (defun rm-insert-code()
    (interactive)
    (insert "<pre><code class=\"haskell\">\n</code></pre>"))
  (defalias 'rm-mode 'textile-mode) ;; rm = redmine
  :custom-face
  (textile-inline-code-face ((t (:background "light blue" :foreground "black"))))
  (textile-pre-face ((t (:foreground "dark cyan"))))
  (textile-class-face ((t (:foreground "dim gray" :slant italic)))) ;; actually, that's not what I think it is. Textile-mode is buggy, it applies the wrong highlight.
  :config
  (sp-local-pair 'textile-mode "@" "@")
  (add-hook 'textile-mode-hook 'common-hook-for-text-modes)
  )

(use-package nxml-mode
  :defer t
  :ensure nil
  :config
  (defun my-nxml-hook ()
    ;; I prefer commenting out XMLs with a single comment
    (setq-local comment-style 'multi-line))
  (add-hook 'nxml-mode-hook 'my-nxml-hook)
  )

(defun line-to-string (buffer line-num)
  "Retrieve the nth line from a buffer into a string variable."
  (with-current-buffer buffer
    (goto-line line-num)
    (thing-at-point 'line t)))

(use-package projectile
  :init
  (setq projectile-git-command "git ls-files -z"
        ;; don't want submodules in my list of files
        projectile-git-submodule-command nil
        projectile-auto-update-cache nil ;; don't want random files in cache
        projectile-enable-caching t
        projectile-use-git-grep t) ;; FTR: git-grep cmd is determined by vc-git-grep-template var
  (projectile-mode 1)


  :bind (("C-c p" . projectile-command-map)
         ("C-x C-h" . projectile-find-file))
  :config
  ;; I mount sshfs and stuff at /tmp, and I definitely wouldn't want a `git whatever'
  ;; to be run over a slow network link. So exclude it.
  (add-to-list 'projectile-globally-ignored-directories "/tmp")
  (defun projectile-clean-changes()
    "Cleans up changes in a project.
TODO: perhaps contribute it upstream?"
    (interactive)
    (let ((project-root (projectile-acquire-root)))
      (if (null project-root)
          (error "No project root here")
        (let ((default-directory project-root))
          (shell-command "git checkout -- .")
          (revert-buffer nil t t)))))

  (defun projectile-find-file-from-primary-clipboard ()
    "Opens a file represented by a string foo/bar/buzz.c:777 in the
current project and goes to line 777. It is allowed some starting
path components to be invalid (it's useful e.g. when copying from
gdb or whatever, where the path is relative to a build dir
used). It also supports syntax `foo.c:123:345' where the 345 is
being ignored, currently at the cost of assuming a file won't
contain a colon. May be fixed, but I don't bother for now."
    (interactive)
    (let* ((path (string-trim (gui-get-selection 'PRIMARY)))
           (file-path (replace-regexp-in-string "\\(:[^/]*$\\)" "" path))
           (line-number (if (string-match-p ":" path)
                            (string-to-number (car (cdr (split-string path ":"))))
                          1))
           (stdout-buf (get-buffer-create "*git-ls-files-output*"))
           (found-file nil))
      (while (and (not found-file) file-path)
        (let ((cmd (concat "git ls-files --error-unmatch \":/*" file-path "\"")))
          (if (= 0 (shell-command cmd stdout-buf))
              (progn
                (let ((older-buf (buffer-name)))
                  (find-file (string-trim-right (line-to-string stdout-buf 1)))
                  (when (string= older-buf (buffer-name))
                    (evil-set-jump))
                  (setq found-file t)
                  (goto-line line-number)))
            (let ((new-path (replace-regexp-in-string "^[^/]+/" "" file-path)))
              (setq file-path
                    (if (string= file-path new-path)
                        nil ;; all components were removed, file not found
                      new-path))))))
      (kill-buffer stdout-buf)
      (when (not found-file)
          (message "File not found"))))
  )

(use-package eldoc
  :config
  (defun myhook-eldoc-mode ()
    (when (and eldoc-mode ;; eldoc calls hooks upon being disabled, bail out in that case
               buffer-file-name
               (string-prefix-p "/tmp" buffer-file-name))
      ;; I mount sshfs and stuff at /tmp, so minimize IO there
      (print ".emacs: disabling eldoc in /tmp")
      (eldoc-mode -1))
    )
  :hook (eldoc-mode . myhook-eldoc-mode)
  )

(use-package magit
  ;; can't ":defer t" it, otherwise `git-commit` doesn't work
  :init
  (setq magit-save-repository-buffers nil)
  :config
  ;; Don't use magit for interactive rebase, git-rebase-mode-map is a bunch of
  ;; annoying keybinds
  (setq auto-mode-alist (rassq-delete-all 'git-rebase-mode auto-mode-alist))
  (defun myhook-git-commit-mode ()
    (setq fill-column 70
          pop-up-windows t ;; for good usability magit needs control over splits
          )
    (flyspell-mode)
    )
  (add-hook 'git-commit-setup-hook 'myhook-git-commit-mode)

  (defun unset-magit-pop-up-windows (_)
    (setq pop-up-windows nil)) ;; we change it in magit hook, so undo after its buffer quits
  (add-hook 'delete-frame-functions 'unset-magit-pop-up-windows)

  ;; I use plain global-auto-revert mode instead, which works much better with
  ;; network-mounted dirs
  (magit-auto-revert-mode -1)
  )

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package ninja-mode
  :defer t
  :config
  ;; make sure `$' and `/' symbols not word constituents
  (modify-syntax-entry ?$ "_")
  (modify-syntax-entry ?/ "_")
  )

(use-package yaml-mode
  :defer t
  :config
  ;; make sure `$' symbol is not a word constituent
  (modify-syntax-entry ?$ "_")
  )

(defun yank-prev-killring ()
  (interactive)
  (insert (nth 1 kill-ring)))
(global-set-key (kbd "M-y") #'yank-prev-killring)

(use-package desktop
  :defer t
  :init
  ;; I find this query useless and annoying, even if an Emacs is using the session
  (setq desktop-load-locked-desktop t)
  )

(use-package emacspeak-setup
  :defer t
  :init
  (setq espeak-default-speech-rate 250
        ;; Make hotkeys start with C-t. Inconvenient, but I don't use the keymap too often
        emacspeak-prefix (vector ?\C-t))
  )

(use-package treesitter
  ;; References: there's an amazing tutorial mentioning various features
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  :defer t
  :init
  ;; TODO: use `treesit-language-available-p' when it's a new Emacs installation to
  ;; install libs in `treesit-language-source-alist'

  ;; variables to use with M-x treesit-install-language-grammar
  (setq
   treesit-language-source-alist
   '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  )

(use-package purescript-mode
  :defer t
  :config
  (defun my-purescript-insert-func-impl ()
    "Given `foo :: …', makes newline and inserts `foo ' i.e. implementation"
    (let ((line (thing-at-point 'line t))
          (indentation (current-indentation))
          (re-typehint (rx line-start (0+ whitespace) (group (1+ wordchar))
                           ;; match :: unless followed by <-
                           (0+ whitespace) "::" (1+ (not ?<)) string-end)))
      (if (not (string-match re-typehint line))
          nil
        (end-of-line)
        (newline)
        (indent-to indentation)
        (insert (concat (match-string-no-properties 1 line) " "))
        t)))
  (defun my-purescript-newline-and-indent (orig-fun)
    (unless (my-purescript-insert-func-impl)
      (funcall orig-fun)))
  (advice-add 'purescript-newline-and-indent
              :around #'my-purescript-newline-and-indent)

  (defun my-purescript-evil-open-below ()
    (interactive)
    (if (my-purescript-insert-func-impl)
        (evil-insert-state)
      (evil-open-below 1)))

  (defun myhook-purescript-mode ()
    ;; Fix completion for operators and functions with symbols in the name
    (dolist (ch '(?' ?/))
      (modify-syntax-entry ch "w"))
    (turn-on-purescript-indentation)
    ;; When completing dot-separated `foo.bar', don't consider it a single word
    (setq dabbrev-abbrev-char-regexp "\\sw")
    (evil-local-set-key 'normal (kbd "o") #'my-purescript-evil-open-below)
    (setq-local evil-shift-width purescript-indentation-left-offset))
  (add-hook 'purescript-mode-hook 'myhook-purescript-mode)
  )

(use-package register
  :defer t
  :ensure nil ; built-in package
  :bind (("C-x <f1>" . window-configuration-to-register)
         ("C-x <f2>" . jump-to-register))
  )

;;;;;;;;;;;;;; START utils: miscellaneous utility functions

(defun create-or-clear-buffer (buffer-name)
  "Create a new buffer with BUFFER-NAME or clear it if it already exists."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer))
    buffer))

(defun markdown-to-textile (&optional curr-buffer)
  "Convert the active region or the entire buffer from Textile to Markdown.

By default it pops result to a new buffer, but if CURR-BUFFER is t it
will replace the current one."
  (interactive)
  (let ((textile-content
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (buffer-string))))
    (if curr-buffer
        (erase-buffer)
      (switch-to-buffer (create-or-clear-buffer "*markdown-to-textile*")))
    (insert textile-content)
    (shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t textile" t t)))

(defun markdown-to-textile-and-close ()
  "Backup the current file, convert its Markdown content to Textile, save,
and close the frame."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (unless current-file
      (error "Buffer is not visiting a file"))
    (let ((backup-file (concat current-file "-BCKP")))
      (copy-file current-file backup-file t)
      (message "Backup created: %s" backup-file))
    (markdown-to-textile t)
    (save-buffer)
    (delete-frame)))

(evil-ex-define-cmd "xr" #'markdown-to-textile-and-close)

(defvar my-re-markdown-ordered-list
  (rx line-start (* space)
      (group (+ digit)) ". "
      (group (+ (not (any "\n"))))))

(defun markdown-ordered-lists-to-bbcode ()
  "Convert Markdown ordered lists to BBCode ordered lists in the current buffer."
  (while (re-search-forward my-re-markdown-ordered-list nil t)
    (let ((start (match-beginning 0))
          (end (progn (while (re-search-forward my-re-markdown-ordered-list nil t))
               (point))))
      (insert "\n[/list]")
      ;; Convert items to BBCode
      (goto-char start)
      (insert "[list=1]\n")
      (while (re-search-forward my-re-markdown-ordered-list end t)
        (replace-match "[*]\\2" t)))))

(defun markdown-to-bbcode (&optional curr-buffer)
  "Convert the current buffer's Markdown content to BBCode format.

A hacky O(n²) written by AI and edited by me, but good enough."
  (interactive)
  (let ((markdown-text (buffer-substring-no-properties (point-min) (point-max))))
    (if curr-buffer
        (erase-buffer)
      (switch-to-buffer (create-or-clear-buffer "*markdown-to-bbcode*")))
    (insert markdown-text)

    ;; Convert headers: # Header -> [SIZE=Xem][B]Header[/B][/SIZE]
    (goto-char (point-min))
    (while (re-search-forward (rx line-start
                                  (group (+ "#")) " "
                                  (group (+ (not (any "\n")))))
                              nil t)
      (let ((text (match-string 2))
            (em-len (pcase (length (match-string 1))
                      (1 "2em")
                      (2 "1.5em")
                      (3 "1.25em")
                      (4 "1em")
                      (5 "0.875em")
                      (6 "0.85em")
                      (_ "UNSUPPORTED"))))
        (replace-match (format "[SIZE=%s][B]%s[/B][/SIZE]" em-len text))))

    ;; Convert links: [text](url) -> [url=url]text[/url]
    (goto-char (point-min))
    (while (re-search-forward (rx "[" (group (+? any)) "]"
                                  "(" (group (+? any)) ")")
                              nil t)
      (replace-match "[url=\\2]\\1[/url]" t))

    ;; Convert unordered lists: - Item -> [*]Item
    (goto-char (point-min))
    (while (re-search-forward (rx line-start (* space) (or ?- ?*) " ") nil t)
      (replace-match "[*]" t))

    ;; Convert bold: **text** -> [b]text[/b]
    (goto-char (point-min))
    (while (re-search-forward (rx "**" (group (+? any)) "**") nil t)
      (replace-match "[b]\\1[/b]" t))

    ;; Convert italics: *text* -> [i]text[/i]
    (goto-char (point-min))
    (while (re-search-forward (rx "*" (group
                                       (not ?\]) ; avoid matching BBCode's [*]
                                       (+? any)) "*") nil t)
      (replace-match "[i]\\1[/i]" t))

    ;; Convert ordered lists: - Item -> [*]Item
    (goto-char (point-min))
    (markdown-ordered-lists-to-bbcode)

    ;; Wrap code blocks: ```code``` -> [code]code[/code]
    (goto-char (point-min))
    (while (re-search-forward (rx "```" (group (+ (not (any "`")))) "```") nil t)
      (replace-match "[code]\\1[/code]" t))

    ;; Replace current buffer with converted BBCode
    (let ((bbcode-text (buffer-string)))
      (erase-buffer)
      (insert bbcode-text))))

(defun markdown-to-bbcode-and-close ()
  "Backup the current file, convert its Markdown content to bbcode, save,
and close the frame."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (unless current-file
      (error "Buffer is not visiting a file"))
    (let ((backup-file (concat current-file "-BCKP")))
      (copy-file current-file backup-file t)
      (message "Backup created: %s" backup-file))
    (markdown-to-bbcode t)
    (save-buffer)
    (delete-frame)))

(evil-ex-define-cmd "xb" #'markdown-to-bbcode-and-close)
;;;;;;;;;;;;;; END of utils
