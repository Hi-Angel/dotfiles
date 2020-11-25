;;; -*- lexical-binding: t -*-
;; run garbage collection only when idle
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq frame-resize-pixelwise t) ;; needs to be set before a frame is created

;; this warns about replace-regexp, and I tried rewriting this function in terms of
;; others — the simple loop they documented is not what I get. Screw this warning, it
;; is not worth the hassle, really.
(setq byte-compile-warnings '(not interactive-only))

;;(package-initialize)

;; turn off the bars (I prefer to disable it in the very beginning ⇒ before Emacs
;; starts loading unneeded bars increasing a load time without a sense)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "C-x C-c") nil) ;; I never use it, but do accidantally press
(setq kill-ring-max 1) ;; I never use more than one entry anyway

(setq reb-re-syntax 'string) ;;standard syntax for re-builder

(add-to-list 'load-path "/home/constantine/.emacs.d/lisp")
(load-file "/home/constantine/.emacs.d/lisp/loc-additions.elc") ;;a local features

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 95 :width normal))))
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(flycheck-error-list-warning ((t (:inherit warning :foreground "blue"))))
 '(flycheck-fringe-warning ((t (:inherit warning :foreground "blue"))))
 '(flycheck-warning ((t (:underline (:color "blue" :style wave)))))
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight bold :height 95 :family "Ubuntu Mono"))))
 '(font-lock-doc-face ((t (:foreground "blue"))))
 '(font-lock-doc-string-face ((t (:foreground "medium blue" :slant italic :height 95 :family "Ubuntu Mono"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :underline t))))
 '(font-lock-type-face ((t (:foreground "ForestGreen" :height 105 :family "Ubuntu Mono"))))
 '(line-number ((t (:inherit (shadow default) :family "Ubuntu Mono"))))
 '(markdown-inline-code-face ((t (:inherit markdown-code-face :background "light blue"))))
 '(mode-line ((t (:background "light yellow" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:background "dim gray" :foreground "white"))))
 '(region ((t (:background "gray")))))

;;theme loading should be done first, as below I changing some it's variables
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-oswald)))

(setq make-backup-files nil)
(setq auto-save-default nil) ;;disable auto save

;;case sensitive dabbrev
(setq dabbrev-upcase-means-case-search t)
(setq-default case-fold-search nil)

(defalias 'ar 'align-regexp)
(defalias 'etags-goto-def-alias 'find-tag)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;; 	("54a63c60d03a025672ad021381a8bf96788c045908593d535fadb3695fd852c6" default))))

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
   '(lsp-mode undo-fu yaml-mode undo-tree xr symbol-overlay evil-magit smex irony winum company company-ngram flycheck-rust php-mode htmlize csharp-mode meson-mode rust-mode flycheck ess minizinc-mode atom-dark-theme highlight-numbers color-identifiers-mode company-anaconda anaconda-mode markdown-mode smartparens slime pretty-symbols paredit lua-mode indent-guide idomenu highlight-parentheses helm-company flycheck-irony flycheck-haskell evil ctypes company-irony cmake-mode evil-surround))
 '(semantic-imenu-bucketize-file nil)
 '(semantic-imenu-bucketize-type-members nil)
 '(semantic-imenu-buckets-to-submenu nil)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25))

(setq-default display-line-numbers 'visual) ;; show the line numbers
(show-paren-mode 1) ;; show matching braces

(setq-default tab-width 4) ;;set tab width

(require 'flycheck) ;; I always use it anyway
(setq flycheck-check-syntax-automatically '(save)) ;; I only want it on save

(require 'evil-surround)
(global-evil-surround-mode 1)
(define-key evil-normal-state-map "S" 'evil-surround-region)
(require 'evil-magit) ;; without this Evil keys are broken in magit
(global-company-mode 1)
(add-to-list 'company-dabbrev-code-modes 'c++-mode)
(add-to-list 'company-dabbrev-code-modes 'c-mode)
(add-to-list 'company-dabbrev-code-modes 'php-mode)
(setq-default company-idle-delay 0.7);;delay before completition

;(add-to-list 'company-backends 'company-yasnippet) commented out due to killing company's completition

(require 'yasnippet)
(yas-global-mode)

(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))
(add-to-list 'auto-mode-alist '("\\.glade$\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))

(defun sort-lines-nocase (beg end)
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

(defun haskell-sort-n-align-imports ()
  "Sorts and aligns Haskell imports"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (looking-at "import "));;look for usings, if no then
                (eq (forward-line 1) 0) ;;go one line down (if not EOF).
                ))
    (haskell-sort-imports)))

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
;; END: make underscore part of a word

(defun myactionsfor-c-mode-common-hook ()
  (c-set-offset 'case-label '+)
  (fix-c-style-indentation)
  (turn-on-auto-fill) ;;auto fill mode for c modes.
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'csharp-mode 'lua-mode)
    (init-prettify-table-c-like)
    (prettify-symbols-mode 1)
    (if (derived-mode-p 'c-mode)
        (setq flycheck-clang-language-standard "c11")
      (setq flycheck-clang-language-standard "c++17"))
    (add-to-list 'flycheck-clang-warnings '"-Wno-missing-braces")
    (add-to-list 'flycheck-clang-args "-frelaxed-template-template-args")
              ))
(add-hook 'c-mode-common-hook 'myactionsfor-c-mode-common-hook)

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

(defun myactionsfor-csharp-mode-common-hook ()
  (when (string= major-mode "csharp-mode")
    (flycheck-mode -1) ;; disable, it for some reason lags with C#
    (c-set-offset 'innamespace '+)
    ))
(add-hook  'csharp-mode-hook 'myactionsfor-csharp-mode-common-hook)

;;in term-mode the «yas» have no a sense, plus causes a problem with <tab>. So disable it.
(add-hook 'term-mode-hook (lambda()
							(yas-minor-mode -1)
							;; (global-set-key (kbd "<RET>")
							;; 				'(lambda ()
							;; 				   (comint-truncate-buffer)
							;; 				   (term-send-input)
							;; 				   ))
							))


;; Enable Evil, and disable it's keybinds except for «ESC» in an insert mode
(require 'emvil)
(setq evil-jumps-cross-buffers nil)
(setq-default evil-shift-round nil) ;; make '>' not to round the indentation

;; disable undo-tree-mode mandated by Evil as it's broken (see "unrecognized
;; entry in undo list" on the internet), and use undo-fu instead.
(global-undo-tree-mode -1)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; newer Evil versions seem to handle this by default, however the older one was
;; removing trailing space when you press Escape. This however can be worked around
;; by overriding the function below to a noop.
(defun evil-maybe-remove-spaces (&optional _))

(require 'ido)
(setq-default ido-case-fold t) ;; case insensistivity
(setq ido-enable-flex-matching t) ;; fuzzy match
(ido-mode)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

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
  (c-set-offset 'innamespace 0);;don't indent namespaces
  (c-set-offset 'func-decl-cont 0)
  (c-set-offset 'cpp-macro 0 nil)
  (c-set-offset 'substatement-open 0) ;; brackets the same level as the statement
  (c-set-offset 'brace-list-intro c-basic-offset) ;; enums
  (c-set-offset 'inextern-lang 0) ;; extern "C" { … }
  (c-set-offset 'inlambda '+) ;; extern "C" { … }
  )

(setq-default inferior-lisp-program "/usr/bin/sbcl")
(add-hook 'lisp-mode-hook (lambda ()
							(turn-on-auto-fill) ;;auto fill mode for c modes.
							))

;;force inserting tabs instead of spaces
;; (setq indent-line-function 'insert-tab)

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
	(x-clipboard-yank)))
(global-set-key (kbd "C-y") 'clipboard-yank-fixed)

(defun clipboard-copy-fixed ()
  (interactive)
  (let ((select-enable-clipboard t))
	(clipboard-kill-ring-save (region-beginning)(region-end))))
(global-set-key (kbd "M-w") 'clipboard-copy-fixed)

(defun copy-text-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (clipboard-kill-region (point-min) (point-max))))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; newline-withoug-break-of-line (analog of the vim's "o" command)
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
  2. insert newline with index"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "s-o") 'newline-without-break-of-line)

;;A fix for align-regexp to use spaces instead of tabs to align
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
(global-set-key (kbd "C-\\") 'input-switch-eng-ru)
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
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "s-y") 'yank)
(require 'idomenu)
(global-set-key (kbd "s-i") 'idomenu)
(global-set-key (kbd "<RET>") 'improved-newline-and-indent)
(global-set-key (kbd "<f11>") (lambda () (interactive) (ff-find-other-file nil t))) ;switch between a corresponding c/c++ header and a file
(global-set-key (kbd "s-o") 'newline-without-break-of-line)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease);set in wheel font decrease
(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase);set in wheel font increase
(global-set-key (kbd "s-/") 'company-complete)


(setq-default fill-column 85) ;; set apropriate lenght of a line
(setq-default cursor-type 'bar) ;set flat cursor type
(setq-default major-mode 'text-mode) ;;why Fundamental-mode? Text-mode more aproppriate

;;some useful minor modes
(defun myfunc-prog-mode ()
  (highlight-numbers-mode)
  (column-number-mode)
  (delete-selection-mode)
  (set-face-attribute 'highlight-numbers-number nil :weight 'bold :foreground "blue" :background "light gray")
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

(add-hook 'twittering-edit-mode-hook '(lambda ()
										(flyspell-mode 1)))

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
;; (dolist (hook '(twittering-edit-mode-hook)) commented out — the flycheck is a scum! It mess up with input.
;;   (add-hook hook (lambda ()
;; 				   (ispell-change-dictionary "ru")
;; 				   (flyspell-mode 1))));;enable for twittering-mode

(defun is-in-comment ()
  "tests if point is in comment"
  (nth 4 (syntax-ppss)))

(defun current-line-string ()
  "returns current line as a string"
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;;; BEGIN: smartparens configuration
(require 'smartparens-config)
(require 'sp-sublimelike) ;;sublime like behavior of smartparens
(setq-default sp-autoskip-closing-pair t) ;; skip only when it's active
(smartparens-global-mode 1)
(setq sp-escape-quotes-after-insert nil) ;; https://github.com/Fuco1/smartparens/issues/783#issuecomment-324598759

(defun maybe-add-semicolon-paren (_id action _context)
  "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
  (when (eq action 'insert)
    (save-excursion
      ;; here, caret supposed to be in between parens, i.e. (|)
      (forward-char) ;; skip closing brace
      (when (and (looking-at "\\s-*$")
                 (not (string-match-p
                       (regexp-opt '("if" "else" "switch" "for" "while" "do" "define") 'words)
                       (current-line-string)))
                 (not (is-in-comment)))
        (insert ";")))))

(defun maybe-add-semicolon-bracket (_id action _context)
  "A helper function that inserts semicolon after closing
parentheses when appropriate. Mainly useful in C, C++, and other
languages with similar syntax"
  (when (eq action 'insert)
    (save-excursion
      ;; here, caret supposed to be in between parens, i.e. {|}
      (forward-char) ;; skip closing brace
      (when (and (looking-at "\\s-*$")
                 (string-match-p "\\breturn\\b" (current-line-string))
                 (not (is-in-comment)))
        (insert ";")))))

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

(defun maybe-add-colon-python (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char) ;; skip closing brace
      (when (and (looking-at "\\s-*$")
                 (string-match-p "\\s-*def.*" (current-line-string)))
        (insert ":")))))

(let ((c-like-modes-list '(c-mode c++-mode java-mode csharp-mode lua-mode vala-mode js-mode)))
  (sp-local-pair c-like-modes-list "(" nil :post-handlers '(:add maybe-add-semicolon-paren))
  (sp-local-pair c-like-modes-list "{" nil :post-handlers '(:add maybe-add-semicolon-bracket)))
(sp-local-pair 'c++-mode "[" nil :post-handlers '(:add maybe-complete-lambda))
(sp-local-pair 'python-mode "(" nil :post-handlers '(:add maybe-add-colon-python))
;;; END: smartparens configuration

;;mode to highlight a matching parenthese for inside of a code between these
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;######################NOTE: below is loading of the addons

;package managment
(require 'package)
;; (push '("marmalade" . "http://marmalade-repo.org/packages/")
;; 	  package-archives )
(push '("melpa" . "https://melpa.org/packages/")
	  package-archives)

;deletes trailing whitespaces before saving the file and sort includes
(defun myfunc-before-save-hook () (interactive)
       (unless (derived-mode-p 'diff-mode)
         (delete-trailing-whitespace))
       ;; (when (derived-mode-p 'c-mode 'c++-mode)
       ;; 	 (c-sort-includes))
       (when (derived-mode-p 'csharp-mode)
         (csharp-sort-usings))
       (when (derived-mode-p 'haskell-mode)
         (haskell-sort-n-align-imports))
       )
(add-hook 'before-save-hook 'myfunc-before-save-hook)

(defun myfunc-rust-mode-hook ()
  ;; note: if racer-mode breaks in some way (e.g. no more completions), do:
  ;; 1. `rustup component add rust-src` 2. `cargo +nightly install racer --force'
  (racer-mode)
  (cl-assert (boundp 'company-backends)) ;; I always use company-mode
  (set (make-local-variable 'company-backends)
       '(company-capf company-etags company-dabbrev))
       )
(add-hook 'rust-mode-hook 'myfunc-rust-mode-hook)

(require 'symbol-overlay)
(defun enable-symbol-overlay-mode ()
  (unless (or (minibufferp)
              (derived-mode-p 'magit-mode)
              (derived-mode-p 'xref--xref-buffer-mode))
    (symbol-overlay-mode t)))
(define-global-minor-mode global-symbol-overlay-mode ;;the name of the new global mode
  symbol-overlay-mode ;;the name of the minor mode
  enable-symbol-overlay-mode)
(global-symbol-overlay-mode);;enable it
(global-set-key (kbd "s-`") 'symbol-overlay-put)
(setq symbol-overlay-ignore-functions nil) ;; don't ignore keywords in various languages
(setq symbol-overlay-map (make-sparse-keymap)) ;; disable special cmds on overlays

(eval-after-load 'irony-mode
  '(progn
	 (irony-mode t)
	 ;;remove company-clang since company-irony the same thing but asynchronous
	 (setq company-backends (delete 'company-clang company-backends))
	 (add-to-list 'company-backends 'company-irony)))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(defun myfunc-text-mode ()
  (set (make-local-variable 'company-idle-delay) 0.3)
  (set (make-local-variable 'company-minimum-prefix-length) 3)
  )
(add-hook 'text-mode-hook 'myfunc-text-mode)

(defun myfunc-gud-gdb-mode ()
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (company-mode 0)
  (local-set-key (kbd "C-d") 'delete-char) ;; gdb rebinds the key
  )
(add-hook 'gud-mode-hook 'myfunc-gud-gdb-mode)
(add-hook 'gdb-mode-hook '(lambda () (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))
(put 'erase-buffer 'disabled nil)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

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
	(sgml-quote start end)
	(replace-regexp "^\\(.*\\)$" "\\1<br>" nil start end)))

(defun myactions-haskell-mode-hook ()
  (haskell-indent-mode)
  )
(add-hook 'haskell-mode-hook 'myactions-haskell-mode-hook)

(defun myfunc-shell-mode ()
  (flycheck-mode 1)
  )
(add-hook 'shell-mode-hook 'myfunc-shell-mode)

(defun myfunc-markdown-mode ()
  (set (make-local-variable 'company-idle-delay) 0.3);;delay before completition
  (setq case-fold-search t) ;; ignore case in search
  (setq dabbrev-upcase-means-case-search nil) ;;ignore case
  (flyspell-mode)
  (ispell-change-dictionary "english")
  (setq markdown-enable-math t) ;; enable latex delimiters
  )
(add-hook 'markdown-mode-hook 'myfunc-markdown-mode)

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
(global-set-key (kbd "M-<SPC>") 'just-one-space-region)

;; .m is octave mode
(setq auto-mode-alist (append '(("\\.m$" . octave-mode))
      auto-mode-alist))

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

;; ---------- color-identifiers specific setup

(add-hook 'after-init-hook 'global-color-identifiers-mode) ;; semantic highlight of variables

(defun myfunc-color-identifiers-mode-hook ()
  (let ((faces '(font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-builtin-face font-lock-preprocessor-face font-lock-constant-face)))
    (dolist (face faces)
      (face-remap-add-relative face '((:foreground "" :weight normal :slant normal)))))
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

;; ---------- end of color-identifiers specific setup

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

(require 'winum)
(winum-mode)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)
(global-set-key (kbd "M-9") 'winum-select-window-9)
(global-set-key (kbd "M-0") 'winum-select-window-0) ;; minibuf

(defun myfunc-diff-mode () ;; diff mode overrides the keys, undo that
  (define-key diff-mode-map (kbd "<M-DEL>") nil)
  (define-key diff-mode-map (kbd "M-1") nil)
  (define-key diff-mode-map (kbd "M-2") nil)
  (define-key diff-mode-map (kbd "M-3") nil)
  (define-key diff-mode-map (kbd "M-4") nil)
  (define-key diff-mode-map (kbd "M-5") nil)
  (define-key diff-mode-map (kbd "M-6") nil)
  (define-key diff-mode-map (kbd "M-7") nil)
  (define-key diff-mode-map (kbd "M-8") nil)
  (define-key diff-mode-map (kbd "M-9") nil)
  (define-key diff-mode-map (kbd "M-0") nil))
(add-hook 'diff-mode-hook 'myfunc-diff-mode)

(defun myfunc-magit-mode () ;; magit overrides the keys, undo that
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  (define-key magit-mode-map (kbd "M-5") nil)
  (define-key magit-mode-map (kbd "M-6") nil)
  (define-key magit-mode-map (kbd "M-7") nil)
  (define-key magit-mode-map (kbd "M-8") nil)
  (define-key magit-mode-map (kbd "M-9") nil)
  (define-key magit-mode-map (kbd "M-0") nil))
(add-hook 'magit-mode-hook 'myfunc-magit-mode)

;; START: enable irony-mode for c++
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun myactionsfor-irony-mode-hook ()
  (irony-cdb-autosetup-compile-options)
  (cl-assert (boundp 'company-backends)) ;; I always use company-mode
  (set (make-local-variable 'company-backends)
       '(company-irony company-etags company-dabbrev))
  )
(add-hook 'irony-mode-hook 'myactionsfor-irony-mode-hook)
;; END: enable irony-mode for c++


;; C++ regex to conver constructor args to initialization (assumes no types with spaces & commas)
;; \b[^,]+? \b\(.+?\)\b → \1(\1)

(defun smerge-next-safe ()
    "returns t on success, nil otherwise"
  (condition-case nil
      (not (smerge-next))
    ('error
     nil)))

(require 'vc)
(defun next-conflict ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (not (smerge-next-safe))
      (vc-find-conflicted-file)
      (if (eq buffer (current-buffer))
          (message "No conflicts found")
        (goto-char 0)
        (smerge-next-safe)))))

;;;; BUGS workarounds START
;;; as of today out of nowhere appeared problems that turned out to be bugs reported
;;; somewhere. Idk why I've never met these before. *sigh* I hate Emacs for this.

;; removes warning: backend company-capf error "Nothing to complete" with args (prefix)
(defun et/semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))
;;;; BUGS workarounds END

;;; START Transpose arguments in c-like mode. Credits: https://emacs.stackexchange.com/a/47934/2671
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

(defalias 'ss 'server-start)

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

;;
;;;;;; START: lsp-mode setup
(require 'lsp-mode)

;; I prefer default indentation functional
(setq lsp-enable-indentation nil)

(defun myactionsfor-lsp-mode-hook ()
  (set (make-local-variable 'company-backends)
       ;; lsp-mode provides company-capf. company-lsp they say not supported, Idk
       ;; why. Perhaps because last commit was at 2019, so it may be unmaintainted
       '(company-capf company-etags company-dabbrev))
  )
(add-hook 'lsp-mode-hook 'myactionsfor-lsp-mode-hook)

;; even if given python project has types utterly broken, the mypy still also
;; gives immensely useful syntax checking that is lacking otherwise with pyls.
(lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)
                                ("pyls.plugins.pyls_mypy.live_mode" nil t)))
;;;;;;; END: lsp-mode setup

;; make `rgrep' function skip binary files
(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nHI --null -e <R> \{\} +")

(defun window-swap-states-rev ()
  "window-swap-states that works in the reverse direction"
  (interactive)
  (window-swap-states (window-normalize-window nil t) (previous-window))
  )

;;;; highlight regions I work with in evil mode. Just fancies.
(evil-goggles-mode 1)
(setq evil-goggles-blocking-duration 0.05)
;; make some evil-goggle colors to look fancier than the default boring gray
(custom-set-faces
 '(evil-goggles-delete-face           ((t (:inherit diff-refine-removed))))
 '(evil-goggles-change-face           ((t (:inherit diff-refine-removed))))
 '(evil-goggles-paste-face            ((t (:inherit diff-refine-added))))
 '(evil-goggles-yank-face             ((t (:inherit diff-refine-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed)))))
