;; run garbage collection only when idle
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;;turn off the bars (I prefer to disable it in the very beginning ⇒ before Emacs starts loading
;;unneeded bars increasing a load time without a sense)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "C-x C-c") nil) ;; I never use it, but do accidantally press
(setq kill-ring-max 1) ;; I never use more than one entry anyway

(setq reb-re-syntax 'string) ;;standard syntax for re-builder

(add-to-list 'load-path "/home/constantine/.emacs.d/lisp")
(load-file "/home/constantine/.emacs.d/lisp/loc-additions.elc") ;;a local features

;;faces
(font-lock-add-keywords
 'c-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 95 :width normal))))
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight bold :height 95 :family "Ubuntu Mono"))))
 '(font-lock-doc-face ((t (:foreground "blue"))))
 '(font-lock-doc-string-face ((t (:foreground "medium blue" :slant italic :height 95 :family "Ubuntu Mono"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :underline t))))
 '(font-lock-type-face ((t (:foreground "ForestGreen" :height 105 :family "Ubuntu Mono"))))
 '(line-number ((t (:inherit (shadow default) :family "Ubuntu Mono"))))
 '(markdown-inline-code-face ((t (:inherit markdown-code-face :background "light blue"))))
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
 '(omnisharp-imenu-support t)
 '(package-selected-packages
   '(company company-ngram flycheck-rust php-mode htmlize csharp-mode meson-mode rust-mode flycheck surround ess minizinc-mode rainbow-delimiters atom-dark-theme highlight-numbers color-identifiers-mode company-anaconda anaconda-mode markdown-mode yasnippet wrap-region smartparens slime pretty-symbols paredit omnisharp lua-mode indent-guide idomenu highlight-parentheses helm-company ggtags flycheck-irony flycheck-haskell evil emms ctypes company-irony company-c-headers cmake-mode autopair))
 '(semantic-imenu-bucketize-file nil)
 '(semantic-imenu-bucketize-type-members nil)
 '(semantic-imenu-buckets-to-submenu nil)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25))

(setq-default display-line-numbers 'visual) ;; show the line numbers
(show-paren-mode 1) ;; show matching braces
;; (electric-indent-mode -1) ;; I don't like it, really

(setq-default tab-width 4) ;;set tab width

(require 'evil-surround)
(global-evil-surround-mode 1)
(semantic-mode 1)
(global-company-mode 1)
(add-to-list 'company-dabbrev-code-modes 'c++-mode)
(add-to-list 'company-dabbrev-code-modes 'c-mode)
(add-to-list 'company-dabbrev-code-modes 'php-mode)
(setq-default company-idle-delay 0.7);;delay before completition

;(add-to-list 'company-backends 'company-yasnippet) commented out due to killing company's completition

(require 'yasnippet)
(yas-global-mode)

(require 'minizinc-mode)
(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))

(defun sort-lines-nocase (reverse beg end)
  (let ((sort-fold-case t))
    (sort-lines reverse beg end)))

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
  (block replace-delimiters
	(let ((closing-point opening-point))
	  (setq closing-point (+ 1 opening-point))
	  (while (< closing-point end-point)
		(if (eq (char-after closing-point) ?\n) ;;no closing delimiter
			(progn
			  (print "Err: no closing delimiter")
			  (return-from replace-delimiters nil))
		  (when (eq (char-after closing-point) old-closing-char)
			(progn
			  (replace-char-after opening-point new-opening-char);;opening delimiter
			  (replace-char-after closing-point new-closing-char);;closing delimiter
			  (return-from replace-delimiters (+ 1 closing-point)))))
		(setq closing-point (+ closing-point 1))))))

(defun swap-<-and-quote-includes (beg end)
  "Swaps in the text between `beg' and `end' the matching «<» and
  «>» character to the \" quote, and vice versa. Mainly used
  before sorting to swap the order of these characters, next
  after the sort to restore the text."
  (block swap-<-and-quote-includes
	(let ((curr-point beg))
	  (while (< curr-point end)
		(setq curr-point (+ curr-point 1))
		;;first chack «"»
		(if (eq (char-after curr-point) ?\")
			(progn
			  (setq curr-point (replace-delimiters ?\" ?< ?> curr-point end))
			  (if (eq curr-point nil)
				  (return-from swap-<-and-quote-includes t)))
		  ;;else if «<»
		  (if (eq (char-after curr-point) ?<)
			  (progn
				(setq curr-point (replace-delimiters ?\> ?\" ?\" curr-point end))
				(if (eq curr-point nil)
					(return-from swap-<-and-quote-includes t)))))))))

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
	  (sort-lines-nocase nil beg end)
	  )))

(defun c-sort-includes ()
  "Sorts #include statements"
  (interactive)
  (save-excursion
	(let (beg end)
	  (goto-char (point-min))
	  (while (and (not (looking-at "#include "));;look for includes, if no then
				  (eq (forward-line 1) 0) ;;go one line down (if not EOF).
				  ))
	  (setq beg (point))
	  (while (and (looking-at "#include ")
				  (eq (forward-line 1) 0)));;to not hang cuz of EOF
	  (setq end (point))
	  (swap-<-and-quote-includes beg end);;swap characters < and > in includes
	  (sort-lines-nocase nil beg end) ;;sort
	  (swap-<-and-quote-includes beg end);;swap the characters  back
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
	  (sort-lines-nocase nil beg end))))

(defun haskell-sort-n-align-imports ()
  "Sorts and aligns Haskell imports"
  (interactive)
  (save-excursion
	(let (beg end)
	  (goto-char (point-min))
	  (while (and (not (looking-at "import "));;look for usings, if no then
				  (eq (forward-line 1) 0) ;;go one line down (if not EOF).
				  ))
	  ;; (setq beg (point))
	  ;; (while (and (looking-at "import ")
	  ;; 			  (eq (forward-line 1) 0)));;to not hang cuz of EOF
	  ;; (setq end (point))
	  (haskell-sort-imports)
	  ;; (let ((case-fold-search nil)) ;; to make sure [A-Z] works as expected
	  ;; 	(align-regexp beg end "\\(\\s-*\\)[A-Z].*$"))
	  ;; (haskell-align-imports)
	  )))

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

(modify-syntax-entry ?_ "w" text-mode-syntax-table) ;;make underscore part of a word

(require 'rust-mode)
(modify-syntax-entry ?_ "w" rust-mode-syntax-table) ;;make underscore part of a word

;;GNU Global mode for c/C++/Java/C#
(require 'ggtags)
(defun my-activate-ctypes () (require 'ctypes))
(defun myactionsfor-c-mode-common-hook ()
  (c-set-offset 'case-label '+)
  (fix-c-style-indentation)
  (turn-on-auto-fill) ;;auto fill mode for c modes.
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table);;make underscore part of a word
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'csharp-mode 'lua-mode)
    (ggtags-mode 1)
    (init-prettify-table-c-like)
    (prettify-symbols-mode 1)
    (my-activate-ctypes)
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table);;make underscore part of a word
    (modify-syntax-entry ?_ "w" c-mode-syntax-table);;make underscore part of a word
    (setq flycheck-clang-language-standard "c++17")
    (semantic-mode)
    (require 'flycheck)
    (add-to-list 'flycheck-clang-warnings '"-Wno-missing-braces")
    (add-to-list 'flycheck-clang-args "-frelaxed-template-template-args")
              ))
(add-hook 'c-mode-common-hook 'myactionsfor-c-mode-common-hook)

(defun myactionsfor-python-mode-common-hook ()
  (modify-syntax-entry ?_ "w" python-mode-syntax-table);;make underscore part of a word
  ;; (semantic-mode) — commented out: seems to be enabled automatically
  )
(add-hook 'python-mode-common-hook 'myactionsfor-python-mode-common-hook)

(defun myactionsfor-prog-mode-hook ()
  (modify-syntax-entry ?_ "w" prog-mode-syntax-table);;make underscore part of a word
  )
(add-hook 'prog-mode-hook 'myactionsfor-prog-mode-hook)

(add-hook  'csharp-mode-hook (lambda () (interactive)
								(flycheck-mode -1);;disable, it for some reason lags with C#
								(c-set-offset 'innamespace '+)
								(local-set-key (kbd "{") 'c-electric-brace)
								(set (make-local-variable 'company-backends)
									 '(company-omnisharp))
								(omnisharp-mode)
								))

;;in term-mode the «yas» have no a sense, plus causes a problem with <tab>. So disable it.
(add-hook 'term-mode-hook (lambda()
							(yas-minor-mode -1)
							;; (global-set-key (kbd "<RET>")
							;; 				'(lambda ()
							;; 				   (comint-truncate-buffer)
							;; 				   (term-send-input)
							;; 				   ))
							))


;; (require 'flycheck)
;; (global-flycheck-mode)
;; (setq flycheck-checker-error-threshold 2000)
(setq flycheck-clang-language-standard "C++11")

;;Enable Evil, and disable it's keybinds except for «ESC» in an insert mode
(require 'emvil)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(setq evil-jumps-cross-buffers nil)

(require 'ido)
(setq-default ido-case-fold t);;case insensistivity
(ido-mode)

(setq whitespace-style (list 'face 'tabs 'spaces 'space-before-tab 'empty 'space-mark 'tab-mark))
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
							(modify-syntax-entry ?- "w" lisp-mode-syntax-table);;make underscore part of a word
							))

;;set apropriate lenght of a line
(setq-default fill-column 86)

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
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
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
  (if (and current-input-method
		   (not (eq current-input-method  "russian-computer")))
	  (progn
		(set-input-method "russian-computer")
		(deactivate-input-method));;set to english
	(toggle-input-method)))
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
		(indent-according-to-mode);;indent the line
		(newline 2);;2 newlines
		(indent-according-to-mode);;indent the line
		(while (not (eq (char-after) ?\n))
		  (backward-char));;go back till newline «after»
		(indent-according-to-mode);;indent the line
		)
	(newline-and-indent);;else
	))

;;set copy/paste work as usual, i.e. not overwrite when delete a characters
;;(and a few another useful keybindings)
(setq select-enable-clipboard nil)
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "s-y") 'yank)
(require 'idomenu)
(global-set-key (kbd "s-i") 'idomenu)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<RET>") 'improved-newline-and-indent)
(global-set-key (kbd "<f11>") 'ff-find-other-file) ;switch between a corresponding c/c++ header and a file
(global-set-key (kbd "s-o") 'newline-without-break-of-line)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-decrease);set in wheel font decrease
(global-set-key (kbd "<C-mouse-5>") 'text-scale-increase);set in wheel font increase
(global-set-key (kbd "s-.") 'company-complete)

;;set apropriate lenght of a line
(setq-default fill-column 85)
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
(setq-default flyspell-issue-message-flag 'nil)
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
;; 				   (ispell-change-dictionary "russian")
;; 				   (flyspell-mode 1))));;enable for twittering-mode

(require 'smartparens-config)
(require 'sp-sublimelike) ;;sublime like behavior of smartparens
(smartparens-global-mode 1);;enable

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
(push '("melpa" . "http://melpa.milkbox.net/packages/")
	  package-archives)

(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode t) ;enable icons in the twittering mode

;deletes trailing whitespaces before saving the file and sort includes
(defun myfunc-before-save-hook () (interactive)
							   (delete-trailing-whitespace)
							   ;; (when (derived-mode-p 'c-mode 'c++-mode)
							   ;; 	 (c-sort-includes))
							   (when (derived-mode-p 'csharp-mode)
								 (csharp-sort-usings))
							   (when (derived-mode-p 'haskell-mode)
								 (haskell-sort-n-align-imports))
							   )
(add-hook 'before-save-hook 'myfunc-before-save-hook)

;; ;;CEDET loading
;; (load-file "~/Projects/cedet-1.1/common/cedet.el")
;; (global-ede-mode 1)
;; ;(semantic-load-enable-code-helpers) commented out due to the line below
;; (semantic-load-enable-excessive-code-helpers)
;; (global-srecode-minor-mode 1)
;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)
;; (setq-default imenu-create-index-function 'imenu-default-create-index-function)

;;enable and configure wrap-mode
;; (require 'wrap-region) → don't need beacuase of smart-parens, but I still leaved it for a safe case
;; (wrap-region-mode t)
;; (wrap-region-add-wrapper "«" "»")

;; ;;vertical guides
;; (require 'indent-guide)
;; (set 'indent-guide-char "¦") ;;character to show an indentation
;; (indent-guide-global-mode t)

(require 'highlight-symbol)
(global-set-key (kbd "s-`") 'highlight-symbol-at-point)
(define-global-minor-mode global-highlight-symbol-mode ;;the name of the new global mode
  highlight-symbol-mode ;;the name of the minor mode
  (lambda () (highlight-symbol-mode t)
	))
(global-highlight-symbol-mode);;enable it

;;moving between windows with «Shift + "Arrow"
(windmove-default-keybindings)
(setq windmove-wrap-around t)

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
  (set (make-local-variable 'company-idle-delay) 0)
  )
(add-hook 'text-mode-hook 'myfunc-text-mode)

(add-to-list 'company-backends 'company-c-headers)

(defun myfunc-gud-gdb-mode ()
  (modify-syntax-entry ?_ "w" gud-mode-syntax-table);;make underscore part of a word
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (company-mode 0)
  (local-set-key (kbd "C-d") 'c-electric-delete-forward) ;; gdb rebinds the key
  )
(add-hook 'gud-mode-hook 'myfunc-gud-gdb-mode)
(add-hook 'gdb-mode-hook '(lambda () (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))
(put 'erase-buffer 'disabled nil)

(defun myfunc-php-mode-hook ()
  ;; (setq c-basic-offset 2) ;;in my company indentation is two spaces
  (modify-syntax-entry ?_ "w" php-mode-syntax-table)
  )
(add-hook 'php-mode-hook 'myfunc-php-mode-hook)

(defun myfunc-web-mode-hook ()
  (myfunc-php-mode-hook))
(add-hook 'web-mode-hook 'myfunc-web-mode-hook)

;;fallback font (if the default have no some gliphs)
(set-fontset-font "fontset-default" 'unicode "Ubuntu Mono")

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
  (modify-syntax-entry ?_ "w" haskell-mode-syntax-table)
  )
(add-hook 'haskell-mode-hook 'myactions-haskell-mode-hook)

(defun myfunc-shell-mode ()
  (flycheck-mode 1)
  (modify-syntax-entry ?_ "w") ;; make underscore part of a word
  )
(add-hook 'shell-mode-hook 'myfunc-shell-mode)

(defun myfunc-markdown-mode ()
  (make-local-variable 'company-idle-delay);;delay before completition
  (setq company-idle-delay 0);;delay before completition
  (setq company-dabbrev-downcase t) ;; ignore case in completions
  (setq case-fold-search t) ;; ignore case in search
  (setq dabbrev-upcase-means-case-search nil) ;;ignore case
  (flyspell-mode)
  (ispell-change-dictionary "english")
  (modify-syntax-entry ?_ "w" markdown-mode-syntax-table);;make underscore part of a word
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

(defun myfunc-patch-mode ()
  (modify-syntax-entry ?_ "w");;make underscore part of a word
  )
(add-hook 'patch-mode-hook 'myfunc-patch-mode)
(split-window-right) ;; something I always do, let's automatize that
(load-file "~/.emacs.d/elpa/markdown-mode-20170712.1703/markdown-mode.elc")

;; heaader guards, source https://www.emacswiki.org/emacs/AutoInsertHeaderGuards
(global-set-key [f12]
                '(lambda ()
                   (interactive)
                   (if (buffer-file-name)
                       (let*
                           ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
                            (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
                            (begin (point-marker))
                            )
                         (progn
                           ;; Insert the Header Guard
                           (goto-char (point-min))
  			               (insert ifDef)
  			               (goto-char (point-max))
  			               (insert "\n#endif" " //" fName "_H")
  			               (goto-char begin))
  			             )
                                        ;else
  		             (message (concat "Buffer " (buffer-name) " must have a filename"))
  		             )
  		           )
  		        )

(defun my-isearch-yank-clipboard ()
  (interactive)
  (isearch-yank-string (or (gui-get-selection 'PRIMARY)
                           (gui-get-selection 'CLIPBOARD)
                           "")))

(setq compile-command "ninja -C build")
