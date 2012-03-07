(provide 'emacs_config)

(require 'cl) ;; Common Lisp

;switch off tool bar
(tool-bar-mode 0)

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
(add-to-list 'load-path "~/emacs/font") ;; fonts
(add-to-list 'load-path "~/emacs/erlang") ;; Configuration for Erlang mode
(add-to-list 'load-path "~/emacs/flymake-erl") ;; Flymake syntax checker
(add-to-list 'load-path "~/emacs/STk") ;; Scheme intepreter
;(add-to-list 'load-path "~/emacs/js2") ;; js2 editing
(add-to-list 'load-path "~/emacs/magit") ;; Git
;(add-to-list 'load-path "~/emacs/esense-1.12") ;; Esense , erlang code completion
(add-to-list 'load-path "~/emacs/perl") ;; perl mode
;(add-to-list 'load-path "~/emacs/emacs-tiny-tools/lisp/tiny") ;; tiny-tools package
;;(add-to-list 'load-path "~/emacs/ruby") ;; ruby mode
(add-to-list 'load-path "~/emacs/pabbrev") ;; pabbrev
;(add-to-list 'load-path "~/emacs/org-6.28e/lisp") ;; org-mode
(add-to-list 'load-path "~/emacs/scala") ;;scala mode
;(add-path "emacs/erlang") ;; Configuration for Erlang mode
;(add-path "esense-1.12") ;; Esense
(add-to-list 'load-path "~/emacs/color-theme") ;;color
;(add-to-list 'load-path "~/emacs/J") ;;J
;(add-to-list 'load-path "~/Development/kona") ;; k
(add-to-list 'load-path "~/emacs/nitrogen-mode") ;; nitrogen-mode
(add-to-list 'load-path "~/emacs/tuareg") ;;ocaml mode

;;color-theme-jonadabian
(require 'color-theme)
(require 'erlang_mode_config) ;; Loading Erlang mode
(require 'flymake_config) ;; Loading flymake
(require 'stk_mode_config) ;; Loading Scheme
;(require 'js2_mode_config) ;; Loading JS2
(require 'magit) ;;loading magit
;(require 'esense_config) ;; Loading Esense
;(require 'perl_config) ;; Loading Perl
;(require 'tinypath) ;; Loading tinylisp
;(require 'tinylisp) ;; Loading tinylisp
;(require 'tramp) ;; Loading tramp
;(require 'ruby-mode-config) ;; Loading ruby
;;(require 'whole-line-or-region) ;; work on the line as if it was a region
(require 'other) ;; load my custom useful functions
;(require 'j_config) ;;load J config
(require 'nitrogen-mode)
(require 'scala_mode_config) ;; scala 
;(require 'k-mode) ;; k mode
(require 'tuareg_mode_config) ;; ocaml 

;; start k session
(global-set-key (kbd "C-c i k") 'switch-to-k)
;(require 'org-install) ;; load org-mode
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
;(setq org-log-done t)

(require 'pabbrev) ;; code completion
(global-pabbrev-mode)
(setq tramp-default-method "scp")

;; bind magit-status to c-x g
(global-set-key (kbd "\C-x g") 'magit-status )

;; bind C-Z to undo (also C-_ or C-/)
(global-set-key (kbd "\C-z") 'undo )

;; highlight region between the point and the mark
(transient-mark-mode t)

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


;(set-default-font "Inconselata-13")

;(require 'carbon-font)
;(fixed-width-set-default-fontset "-apple-inconsolata-medium-r-normal--15-*-*-*-*-*-iso10646-1")

;;(set-default-font "Monaco-13")
(setq mac-allow-anti-aliasing t)  

;(set-default-font "-apple-inconsolata-medium-r-normal--15-130-72-72-m-130-iso10646-1")

;;set color-theme-rshestakov
(defun color-theme-rshestakov ()
  "Dark blue background.
Supports standard faces, font-lock, highlight-changes, widget and
custom."
  (interactive)
  (color-theme-install
   '(color-theme-rshestakov
     (
      (foreground-color . "#CCBB77")
      (cursor-color . "medium turquoise")
      (background-color . "#000243")
      (background-mode . dark))
     (default ((t (nil))))
     (underline ((t (:underline t))))
     (region ((t (:background "#004080"))))
     (font-lock-builtin-face ((t (:foreground "#00BBBB"))))
     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))
     (font-lock-comment-delimiter-face ((t (:foreground "grey30" :italic t))))
     (font-lock-comment-face ((t (:foreground "grey30" :italic t))))
     (font-lock-string-face ((t (:foreground "#10D010"))))
     (font-lock-constant-face ((t (:foreground "indian red"))))
     (font-lock-function-name-face ((t (:foreground "#69C4EB"))))
     (font-lock-variable-name-face ((t (:foreground "#CCBB77"))))
     (highlight-changes-face ((t (:background "navy"))))
     (highlight-changes-delete-face ((t (:foreground "red" :background "navy"))))
     )))

(color-theme-rshestakov)


(ido-mode 'buffer)
;;(setq ido-enable-flex-matching t)


;; ;;; Color theme based on Tango Palette. Created by danranx@gmail.com
;; (defun color-theme-tango ()
;;   "A color theme based on Tango Palette."
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-tango
;;      ((background-color . "#2e3436")
;;       (background-mode . dark)
;;       (border-color . "#888a85")
;;       (cursor-color . "#fce94f")
;;       (foreground-color . "#eeeeec")
;;       (mouse-color . "#8ae234"))
;;      ((help-highlight-face . underline)
;;       (ibuffer-dired-buffer-face . font-lock-function-name-face)
;;       (ibuffer-help-buffer-face . font-lock-comment-face)
;;       (ibuffer-hidden-buffer-face . font-lock-warning-face)
;;       (ibuffer-occur-match-face . font-lock-warning-face)
;;       (ibuffer-read-only-buffer-face . font-lock-type-face)
;;       (ibuffer-special-buffer-face . font-lock-keyword-face)
;;       (ibuffer-title-face . font-lock-type-face))
;;      (border ((t (:background "#888a85"))))
;;      (fringe ((t (:background "grey10"))))
;;      (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
;;      (region ((t (:background "#555753"))))
;;      (font-lock-builtin-face ((t (:foreground "#729fcf"))))
;;      (font-lock-comment-face ((t (:foreground "#888a85"))))
;;      (font-lock-constant-face ((t (:foreground "#8ae234"))))
;;      (font-lock-doc-face ((t (:foreground "#888a85"))))
;;      (font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
;;      (font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
;;      (font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
;;      (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
;;      (font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
;;      (font-lock-function-name-face ((t (:foreground "#edd400" :bold t :italic t))))
;;      (comint-highlight-input ((t (:italic t :bold t))))
;;      (comint-highlight-prompt ((t (:foreground "#8ae234"))))
;;      (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
;;      (isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
;;      (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
;;      (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
;;      (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
;;      (info-xref ((t (:foreground "#729fcf"))))
;;      (info-xref-visited ((t (:foreground "#ad7fa8"))))
;;      )))

;; (provide 'color-theme-tango)


