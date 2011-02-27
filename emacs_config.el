(provide 'emacs_config)

(require 'cl) ;; Common Lisp

;(load "~/emacs/emacs-tiny-tools/lisp/tiny/tinypath")

(defvar emacs-root (if (or (eq system-type 'cygwin)
			   (eq system-type 'gnu/linux)
			   (eq system-type 'linux)
			   (eq system-type 'darwin))
		           "/home/romanshestakov/"
		       "c:/home/romanshestakov/")
"My home directory - root of my personal emacs load-path.")


;;add all the elisp
(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p)))))

(add-to-list 'load-path "~/emacs/Include") ;;variouse el files
(add-to-list 'load-path "~/emacs/erlang") ;; Configuration for Erlang mode
;;(add-to-list 'load-path "~/emacs/flymake") ;; Flymake syntax checker
(add-to-list 'load-path "~/emacs/flymake-erl") ;; Flymake syntax checker
(add-to-list 'load-path "~/emacs/STk") ;; Scheme intepreter
(add-to-list 'load-path "~/emacs/js2") ;; js2 editing
;(add-to-list 'load-path "~/emacs/git-emacs") ;; Git
(add-to-list 'load-path "~/emacs/magit") ;; Git
(add-to-list 'load-path "~/emacs/esense-1.12") ;; Esense , erlang code completion
(add-to-list 'load-path "~/emacs/perl") ;; perl mode
;(add-to-list 'load-path "~/emacs/emacs-tiny-tools/lisp/tiny") ;; tiny-tools package
(add-to-list 'load-path "~/Development/Reia/Emacs") ;; Reia mode
(add-to-list 'load-path "~/emacs/textmate.el") ;; Textmate mode
;;(add-to-list 'load-path "~/emacs/ruby") ;; ruby mode
(add-to-list 'load-path "~/emacs/pabbrev") ;; pabbrev
(add-to-list 'load-path "~/emacs/org-6.28e/lisp") ;; org-mode
;(add-path "emacs/erlang") ;; Configuration for Erlang mode
;(add-path "flymake") ;; Flymake syntax checker
;(add-path "STk") ;; Scheme intepreter
;(add-path "js2") ;; js2 editing
;(add-path "git-emacs") ;; Git
;(add-path "esense-1.12") ;; Esense
(add-to-list 'load-path "~/emacs/color-theme") ;;color
(add-to-list 'load-path "~/emacs/J") ;;J

(add-to-list 'load-path "~/emacs/nitrogen-mode") ;; nitrogen-mode

;;color-theme-jonadabian

(require 'color-theme)
(require 'erlang_mode_config) ;; Loading Erlang mode
(require 'flymake_config) ;; Loading flymake
(require 'stk_mode_config) ;; Loading Scheme
(require 'js2_mode_config) ;; Loading JS2
;(require 'git-emacs) ;; Loading Git
(require 'magit) ;;loading magit
(require 'esense_config) ;; Loading Esense
(require 'perl_config) ;; Loading Perl
;(require 'tinypath) ;; Loading tinylisp
;(require 'tinylisp) ;; Loading tinylisp
;(require 'tramp) ;; Loading tramp
;(require 'reia) ;; Loading reia
;(require 'textmate) ;; Loading textmate
;(textmate-mode)
;(require 'ruby-mode-config) ;; Loading ruby
;;(require 'whole-line-or-region) ;; work on the line as if it was a region
(require 'other) ;; load my custom useful functions
(require 'j_config) ;;load J config
(require 'nitrogen-mode)


(require 'org-install) ;; load org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(require 'pabbrev) ;; code completion
(global-pabbrev-mode)


(setq tramp-default-method "scp")


;; redefine c-x c-e to <f9>
;;(global-set-key (kbd "<f9>") 'eval-last-sexp ) ;; use F9 instead of Ctrl-X Ctrl-E

;; bind tinylisp-error-find-1 to Ctrl-L, same key as GS secdb lint
(global-set-key (kbd "C-l") 'tinylisp-error-find-1 ) 

;; bind magit-status to c-x g
(global-set-key (kbd "\C-x g") 'magit-status )

;; make pager more
(setenv "PAGER" "more")


;; set autosave optioin
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))



;; set color-theme-rshestakov
(defun color-theme-rshestakov ()
  "Dark blue background.
Supports standard faces, font-lock, highlight-changes, widget and
custom."
  (interactive)
  (color-theme-install
   '(color-theme-rshestakov
     ((foreground-color . "#CCBB77")
      (cursor-color . "medium turquoise")
      (background-color . "#000055")
      (background-mode . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "cyan" :background "#007080"))))
     (modeline-buffer-id ((t (:foreground "cyan" :background "#007080"))))
     (modeline-mousable ((t (:foreground "cyan" :background "#007080"))))
     (modeline-mousable-minor-mode ((t (:foreground "cyan" :background "#007080"))))
     (underline ((t (:underline t))))
     (region ((t (:background "#004080"))))
     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))
     (font-lock-comment-face ((t (:foreground "grey50" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "#10D010"))))
     (font-lock-constant-face ((t (:foreground "indian red"))))
     (highlight-changes-face ((t (:background "navy"))))
     (highlight-changes-delete-face ((t (:foreground "red" :background "navy"))))
     (widget-field-face ((t (:foreground "black" :background "grey35"))))
     (widget-inactive-face ((t (:foreground "gray"))))
     (custom-button-face ((t (:foreground "yellow" :background "dark blue"))))
     (custom-state-face ((t (:foreground "mediumaquamarine"))))
     (custom-face-tag-face ((t (:foreground "goldenrod" :underline t))))
     (custom-documentation-face ((t (:foreground "#10D010"))))
     (custom-set-face ((t (:foreground "#2020D0")))))))

;(set-face-background 'region "#004080")
(color-theme-rshestakov)








;; (defun color-theme-example ()
;;   "Example theme. Carbon copy of color-theme-gnome contributed by Jonadab."
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-example
;;      ((foreground-color . "wheat")
;;       (background-color . "darkslategrey")
;;       (background-mode . dark))
;;      (default ((t (nil))))
;;      (region ((t (:foreground "cyan" :background "dark cyan"))))
;;      (underline ((t (:foreground "yellow" :underline t))))
;;      (modeline ((t (:foreground "dark cyan" :background "wheat"))))
;;      (modeline-buffer-id ((t (:foreground "dark cyan" :background "wheat"))))
;;      (modeline-mousable ((t (:foreground "dark cyan" :background "wheat"))))
;;      (modeline-mousable-minor-mode ((t (:foreground "dark cyan" :background "wheat"))))
;;      (italic ((t (:foreground "dark red" :italic t))))
;;      (bold-italic ((t (:foreground "dark red" :bold t :italic t))))
;;      (font-lock-comment-face ((t (:foreground "Firebrick"))))
;;      (bold ((t (:bold)))))))



;; (color-theme-example)



;; ;;;###autoload
;; (defun color-theme-blackboardkjh ()
;;   "Color theme by JD Huntington, based off the TextMate Blackboard theme, created 2008-11-27"
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-blackboard
;;      ((background-color . "#0C1021")
;;       (background-mode . dark)
;;       (border-color . "black")
;;       (cursor-color . "#A7A7A7")
;;       (foreground-color . "#F8F8F8")
;;       (mouse-color . "sienna1"))
;;      (default ((t (:background "#0C1021" :foreground "#F8F8F8"))))
;;      (blue ((t (:foreground "blue"))))
;;      (bold ((t (:bold t))))
;;      (bold-italic ((t (:bold t))))
;;      (border-glyph ((t (nil))))
;;      (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
;;      (font-lock-builtin-face ((t (:foreground "#F8F8F8"))))
;;      (font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
;;      (font-lock-constant-face ((t (:foreground "#D8FA3C"))))
;;      (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
;;      (font-lock-function-name-face ((t (:foreground "#FF6400"))))
;;      (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
;;      (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
;;      (font-lock-reference-face ((t (:foreground "SlateBlue"))))
 
;;      (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
;;      (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 
;;      (minibuffer-prompt ((t (:foreground "#5F5A60"))))
;;      (ido-subdir ((t (:foreground "#CF6A4C"))))
;;      (ido-first-match ((t (:foreground "#8F9D6A"))))
;;      (ido-only-match ((t (:foreground "#8F9D6A"))))
;;      (mumamo-background-chunk-submode ((t (:background "#222222"))))
 
;;      (font-lock-string-face ((t (:foreground "#61CE3C"))))
;;      (font-lock-type-face ((t (:foreground "#8DA6CE"))))
;;      (font-lock-variable-name-face ((t (:foreground "#FF6400"))))
;;      (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
;;      (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
;;      (region ((t (:background "#253B76"))))
;;      (mode-line ((t (:background "grey75" :foreground "black"))))
;;      (highlight ((t (:background "#222222"))))
;;      (highline-face ((t (:background "SeaGreen"))))
;;      (italic ((t (nil))))
;;      (left-margin ((t (nil))))
;;      (text-cursor ((t (:background "yellow" :foreground "black"))))
;;      (toolbar ((t (nil))))
;;      (underline ((nil (:underline nil))))
;;      (zmacs-region ((t (:background "snow" :foreground "ble")))))))



;; (color-theme-blackboardkjh)


(setq mac-allow-anti-aliasing t)  

;;(set-default-font "Monaco-13")


(ido-mode 'buffer)
;;(setq ido-enable-flex-matching t)
