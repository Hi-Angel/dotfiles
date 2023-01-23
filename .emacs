;;; -*- lexical-binding: t -*-

;;;; Installation of packages required by this config:
;; It's unfortunately a bit involved, but not too much.
;;
;; 1. In `.emacs' remove `:defer t', so all packages required by use-package would be
;; searched immediately. You'll return them back later
;; 2. `emacs -Q', then evaluate (for older emacs (package-initialize)),
;;   (push '("melpa" . "https://melpa.org/packages/") package-archives)
;;   then `M-x package-list-packages', then install `use-package'
;; 3. Insert at the top of .emacs:
;;   (require 'use-package-ensure)
;;   (setq use-package-always-ensure t)
;;   then start emacs. It will start installing packages, but will fail at some of them
;; 4. Optional: restart Emacs. At point 2 it will print lots of stuff (during
;;   compilation etc), so restart might be useful to regenerate solely ones about
;;   packages that use-package failed installing
;; 5. Repeat 1, and install those packages that use-package failed to install at 3.
;;   use-package is buggy, and can't install some modules.
;; 6. Return back all `defer t' removed in step 1.

;; This warns about replace-regexp, and I tried rewriting this function in terms of
;; others — the simple loop they documented is not what I get. Screw this warning, it
;; is not worth the hassle, really.
(setq byte-compile-warnings '(not interactive-only))

;; turn off the bars (I prefer to disable it in the very beginning ⇒ before Emacs
;; starts loading unneeded bars increasing a load time without a sense)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-indicate-empty-lines) ;; visually indicate where a file ends

(bind-key "C-x C-c" nil) ;; I never use it, but do accidantally press

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
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight bold :height 105 :family "Ubuntu Mono"))))
 '(font-lock-doc-face ((t (:foreground "blue"))))
 '(font-lock-doc-string-face ((t (:foreground "medium blue" :slant italic :height 95 :family "Ubuntu Mono"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :underline t))))
 '(font-lock-type-face ((t (:foreground "ForestGreen" :height 105 :family "Ubuntu Mono"))))
 '(line-number ((t (:inherit (shadow default) :family "Ubuntu Mono"))))
 '(mode-line ((t (:background "light yellow" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:background "dim gray" :foreground "white"))))
 '(region ((t (:background "gray")))))

;;theme loading should be done first, as below I changing some it's variables
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-oswald)))

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
               (bound-and-true-p haskell-mode)
        (flycheck-haskell-setup))
      ))
  (add-hook 'flycheck-mode-hook 'myactions-flycheck-mode-hook)
  (add-to-list 'flycheck-clang-warnings "-Wno-missing-braces")
  (add-to-list 'flycheck-clang-args "-frelaxed-template-template-args")
  )

(use-package company
  :defer nil ;; :bind implies `defer t', override it
  :bind ("s-/" . company-complete)
  :init
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2 ;; delay before completition
        ;; below is default value, but with company-capf removed 'cause it's laggy
        company-backends '(company-bbdb company-semantic company-cmake company-clang company-files (company-dabbrev-code company-gtags company-etags company-keywords) company-oddmuse company-dabbrev))
  :config
  (global-company-mode 1)
  (add-list-to-list 'company-dabbrev-code-modes
                    '(c++-mode c-mode php-mode))
  )

(use-package yasnippet
  :defer 2 ;; lazy-load after 2 seconds of being idle
  :config
  (yas-global-mode)
  )

(use-package autorevert
  :init
  (setq
   ;; Do not break markers in a buffer upon reverting a buffer. Details:
   ;; https://github.com/magit/magit/issues/4442
   revert-buffer-insert-file-contents-function 'revert-buffer-insert-file-contents-delicately
   ;; For some undocumented reason Emacs uses both inotify and polls on
   ;; files. That's stupid, just a waste of resoureces, sure let's avoid that.
   auto-revert-avoid-polling t)
  :config
  (global-auto-revert-mode 1) ;; automatically revert any buffeers, whose files changed on disk
  )

(add-list-to-list 'auto-mode-alist
                  '(("\\.m$"               . octave-mode)
                    ("\\.service\\'"       . conf-mode)
                    ("\\.rules\\'"         . conf-mode)
                    ("\\PKGBUILD\\'"       . sh-mode)
                    ("\\.glade$\\'"        . xml-mode)
                    ("\\.mzn\\'"           . minizinc-mode)
                    ("[Dd]ockerfile[^.]*$" . dockerfile-mode)
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

;; START: make underscore part of a word
(with-eval-after-load 'text-mode
  (modify-syntax-entry ?_ "w" text-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'rust-mode
  (modify-syntax-entry ?_ "w" rust-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'cc-mode
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'cc-mode
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'python
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'elisp-mode
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'gud
  (modify-syntax-entry ?_ "w" gud-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'php-mode
  (modify-syntax-entry ?_ "w" php-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'haskell-mode
  (modify-syntax-entry ?_ "w" haskell-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'markdown-mode
  (modify-syntax-entry ?_ "w" markdown-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'diff-mode
  (modify-syntax-entry ?_ "w" diff-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'meson-mode
  (modify-syntax-entry ?_ "w" meson-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'vala-mode
  (modify-syntax-entry ?_ "w" vala-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'sh-script
  (modify-syntax-entry ?_ "w" sh-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'perl-mode
  (modify-syntax-entry ?_ "w" perl-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'cmake-mode
  (modify-syntax-entry ?_ "w" cmake-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'js
  (modify-syntax-entry ?_ "w" js-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'conf-mode
  (modify-syntax-entry ?_ "w" conf-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'make-mode
  (modify-syntax-entry ?_ "w" makefile-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'ruby-mode
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'yaml-mode
  (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)) ;; make underscore part of a word
(with-eval-after-load 'dockerfile-mode
  (modify-syntax-entry ?_ "w" dockerfile-mode-syntax-table)) ;; make underscore part of a word
;; END: make underscore part of a word

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
  (exec-cmd-foreach-backward ",\\( \\)" 'newline-and-indent
                             (region-beginning) (region-end)))

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
  ;; 				'(lambda ()
  ;; 				   (comint-truncate-buffer)
  ;; 				   (term-send-input)
  ;; 				   ))
  )

(use-package avy
  :config
  (setq avy-case-fold-search nil) ;; make searches case sensitive. Well, at least
  ;; upcase ones, that as good as avy allows.
  )

(defun evil-lsp-find-definition (_string _position)
  (condition-case nil
      (lsp-find-definition)
    ('error nil)
    (:success t)))

(use-package emvil ;; my Evil config, in a separate file
  :ensure nil)

(use-package undo-fu)

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

;;scrolling related fixes
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; three line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scroll
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 2) ;; keyboard scroll two line at a time$

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

(setq-default inferior-lisp-program "/usr/bin/sbcl")
(add-hook 'lisp-mode-hook (lambda ()
							(turn-on-auto-fill) ;;auto fill mode for c modes.
							))

;;force inserting tabs instead of spaces
;; (setq indent-tabs-mode t)

;;Make c-a key to work as the home key in a most code editors
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

;;fixed version of clipboard-yank
(defun clipboard-yank-fixed ()
  "Fixed version of clipboard-yank — it is pastes «instead» of
  selection if such exist(like any normal text editor)"
  (interactive)
  (when mark-active
	  (kill-region (region-beginning) (region-end)))
  (let ((select-enable-clipboard t))
	(clipboard-yank)))
(bind-key "C-y" 'clipboard-yank-fixed)

(defun clipboard-copy-fixed ()
  (interactive)
  (let ((select-enable-clipboard t))
	(clipboard-kill-ring-save (region-beginning)(region-end))))
(bind-key "M-w" 'clipboard-copy-fixed)

(defun copy-text-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (clipboard-kill-region (point-min) (point-max))))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Make align-regexp always align with spaces rather than tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

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
        (indent-according-to-mode);;indent the line
        (newline 2);;2 newlines
        (indent-according-to-mode);;indent the line
        (while (not (eq (char-after) ?\n))
          (backward-char));;go back till newline «after»
        (indent-according-to-mode);;indent the line
        )
    (newline-and-indent)
    ))

;;set copy/paste work as usual, i.e. not overwrite when delete a characters
;;(and a few another useful keybindings)
(setq select-enable-clipboard nil)
(bind-key "C-w" 'clipboard-kill-region)
(bind-key "s-y" 'yank)
(use-package idomenu
  :bind ("s-i" . idomenu)
  )
(bind-key "<RET>" 'improved-newline-and-indent)
(global-set-key (kbd "<f11>") (lambda () (interactive) (ff-find-other-file nil t))) ;switch between a corresponding c/c++ header and a file
(bind-key "<C-mouse-4>" 'text-scale-decrease);set in wheel font decrease
(bind-key "<C-mouse-5>" 'text-scale-increase);set in wheel font increase

(use-package highlight-numbers
  :config
  (set-face-attribute 'highlight-numbers-number nil :weight 'bold :foreground "blue" :background "light gray")
  )

;;some useful minor modes
(defun myfunc-prog-mode ()
  (highlight-numbers-mode)
  (column-number-mode)
  (delete-selection-mode)
  )
(add-hook 'prog-mode-hook 'myfunc-prog-mode)

;;allows in a case of an ∞ loop send with «killall -SIGUSR1 emacs» to break it
(setq debug-on-event 'sigusr1)
(define-key special-event-map [sigusr2]
  (lambda
      ()
    (interactive)
    (save-some-buffers t)
    (kill-emacs)))

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
        sp-escape-quotes-after-insert nil) ;; https://github.com/Fuco1/smartparens/issues/783#issuecomment-324598759
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

  (sp-with-modes '(c-mode c++-mode java-mode csharp-mode lua-mode vala-mode js-mode)
    (sp-local-pair "(" nil :post-handlers '(:add maybe-add-semicolon-paren))
    (sp-local-pair "{" nil :post-handlers '(:add maybe-add-semicolon-bracket)))
  (sp-local-pair 'c++-mode "[" nil :post-handlers '(:add maybe-complete-lambda))
  (sp-local-pair 'rust-mode "(" nil :post-handlers '(:add maybe-add-semicolon-paren-rust))
  )

;; mode to highlight a matching parentheses from the inside
(use-package highlight-parentheses
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  )

;; package managment
(use-package package
  :defer t
  :config
  (push '("melpa" . "https://melpa.org/packages/")
        package-archives)
  )

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

(use-package text-mode
  :defer t
  :ensure nil
  :init
  (defun myfunc-text-mode ()
    (set (make-local-variable 'company-minimum-prefix-length) 2))
  :hook (text-mode . myfunc-text-mode)
  )

(defun myfunc-gud-gdb-mode ()
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (company-mode 0)
  (local-set-key (kbd "C-d") 'delete-char) ;; gdb rebinds the key
  )
(add-hook 'gud-mode-hook 'myfunc-gud-gdb-mode)
(add-hook 'gdb-mode-hook '(lambda () (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))
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
  (add-hook 'haskell-mode-hook 'myactions-haskell-mode-hook)
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

(defun common-hook-for-text-modes ()
  (setq case-fold-search t) ;; ignore case in search
  (set (make-local-variable 'dabbrev-upcase-means-case-search) nil) ;; ignore case
  (flyspell-mode)
  )

(use-package markdown-mode
  :defer t
  :init
    (setq-default markdown-enable-math t) ;; enable latex delimiters
  :custom-face
  (markdown-inline-code-face ((t (:inherit markdown-code-face :background "light blue"))))
  :config
  (add-hook 'markdown-mode-hook 'common-hook-for-text-modes)
  )

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

(setq-default enable-local-variables :all)

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
  (setq color-identifiers:recoloring-delay 1)
  :config

  (defun myfunc-color-identifiers-mode-hook ()
    (let ((faces '(font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-builtin-face font-lock-preprocessor-face font-lock-constant-face)))
      (dolist (face faces)
        (face-remap-add-relative face '(:inherit default))))
    (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))
    (face-remap-add-relative 'font-lock-builtin-face '((:weight bold)))
    (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
    (face-remap-add-relative 'font-lock-function-name-face '((:weight bold)))
    (face-remap-add-relative 'font-lock-string-face '((:foreground "#b33200000000")))
    (face-remap-add-relative 'font-lock-constant-face '((:weight bold)))
    (face-remap-add-relative 'haskell-operator-face '((:foreground "#b33200000000")))
    )
  (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)

  ;; (defun test ()
  ;;   (let ((faces '(font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-builtin-face font-lock-preprocessor-face font-lock-constant-face)))
  ;;     (dolist (face faces)
  ;;       (add-to-list 'face-remapping-alist (cons face '((:foreground nil :weight normal :slant normal))))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-keyword-face '((:weight bold))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-builtin-face '((:weight bold))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-preprocessor-face '((:weight bold))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-function-name-face '((:weight bold))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-constant-face '((:weight bold))))
  ;;   (add-to-list 'face-remapping-alist (cons 'font-lock-string-face '((:foreground "#b33200000000"))))
  ;;   (add-to-list 'face-remapping-alist (cons 'haskell-operator-face '((:foreground "#b33200000000"))))
  ;;   )
  ;; (add-hook 'color-identifiers-mode-hook 'test)


  ;; (let ((faces '(font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-builtin-face font-lock-preprocessor-face)))
  ;;   (dolist (face faces)
  ;;     (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

  ;; (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
  ;; (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
  ;; (set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)
  ;; (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  ;; (set-face-attribute 'font-lock-string-face nil :foreground "#b33200000000")
  ;; (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  ;; (set-face-attribute 'haskell-operator-face nil :foreground "#b33200000000")

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
  (if (and (char-equal (char-after (point-min)) ?\n)
           (char-equal (char-after (1+ (point-min))) ?\n))
      ""
    "\n"))
(defun maybe-add-newline-at-buf-end ()
  (if (and (char-equal (char-before (point-max)) ?\n)
           (char-equal (char-before (1- (point-max))) ?\n))
      ""
    "\n"))
(global-set-key [f12]
                '(lambda ()
                   (interactive)
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
                                        ;else
  		             (message (concat "Buffer " (buffer-name) " must have a filename"))
  		             )
  		           )
  		        )

(setq compile-command "ninja -C build")

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

;; note: some modes override this. In particular, C and C++ standard requires a newline
(setq-default require-final-newline nil)

;; By default keyboard-quit is giltchy, it randomly fails to work until you press it
;; second time. Let's just bind C-g to press it twice always
(global-set-key (kbd "C-g") '(lambda () (interactive) (keyboard-quit) (keyboard-quit)))

(defun insert-print-for-the-word()
  "Inserts a print above current line for the word a cursor is
upon or for the selected text if it's active"
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'word 'no-properties)))
        (print-str (pcase major-mode
                     ('c++mode '("printf(\"%s\", " ");"))
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
            (init-body "")
            (init "")
            (repr-body "")
            (fst-param-is-processed t))
        (dolist (param params-list)
          (setq init-header (concat init-header ", " param))
          (let ((param-no-type (car (split-string param ":" t "\s-*"))))
            (setq init-body (concat init-body "\nself." param-no-type "= " param-no-type))
            (setq repr-body (concat repr-body
                                    (if fst-param-is-processed
                                        ;; Note: the 4 spaces is a workaround because otherwise
                                        ;; python-mode refuses to properly indent the body
                                        "\n    return f'{{"
                                      "\\\n+ f'")
                                    param-no-type " = {self." param-no-type "}, '"))
            (setq fst-param-is-processed nil))
          )
        ;; now replace last "}" with "}}}" since it's hard to set it in place beforehand otherwise.
        (setq repr-body (replace-regexp-in-string "}, '$" "}}}'" repr-body))
        (setq init (concat init-header "):" init-body
                           "\n\ndef __repr__(self):" repr-body
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
        lsp-enable-symbol-highlighting nil)
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
  )

;; make `rgrep' function skip binary files
(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nHI --null -e <R> \{\} +")

(defun window-swap-states-rev ()
  "window-swap-states that works in the reverse direction"
  (interactive)
  (window-swap-states (window-normalize-window nil t) (previous-window))
  )

;;;; some perofrmance related changes, credits to https://github.com/geza-herman/emacs/tree/fast-emacs
;; Don't care about bidirectional text. These settings make processing long lines faster.
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

(defun insert-redmine-issue-template ()
  (interactive)
  (insert "
h1. Шаги к воспроизведению



h2. Ожидается



h2. На самом деле



h1. Доп. информация
"))

(defun insert-markdown-spoiler ()
  (interactive)
  (insert "
<details>
    <summary>Details</summary>

    Text here. A mandatory empty line above, otherwise markdown misrenders it.
</details>"))

(defun insert-markdown-issue-template ()
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

(use-package smerge-mode) ;; make smerge-vc-next-conflict always available

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
  (defalias 'redmine-mode 'textile-mode)
  :custom-face
  (textile-inline-code-face ((t (:background "light blue"))))
  (textile-pre-face ((t (:foreground "dark cyan"))))
  (textile-class-face ((t (:foreground "dim gray" :slant italic)))) ;; actually, that's not what I think it is. Textile-mode is buggy, it applies the wrong highlight.
  :config
  (add-hook 'textile-mode-hook 'common-hook-for-text-modes)
  )

(use-package projectile
  :init
  (setq projectile-git-command "git ls-files -z"
        ;; don't want submodules in my list of files
        projectile-git-submodule-command nil
        projectile-enable-caching t)
  :bind (("C-c p" . projectile-command-map)
         ("C-x C-h" . projectile-find-file))
  :config
  ;; I mount sshfs and stuff at /tmp, and I definitely wouldn't want a `git whatever'
  ;; to be run over a slow network link. So exclude it.
  (add-to-list 'projectile-globally-ignored-directories "/tmp")
  (projectile-mode 1)
  )
