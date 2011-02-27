;; ruby                                                                         
;; based on http://www.rubygarden.org/Ruby/page/show/InstallingEmacsExtensions  
;;                                                                              
(provide 'ruby-mode-config)


(add-to-list 'load-path "~/emacs/ruby")

;(require 'ruby-mode)
;(require 'ruby-electric)

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")

;;(add-to-list ‘auto-mode-alist ‘(”\\.rb$” . ruby-mode))
;;(add-to-list ‘auto-mode-alist ‘(”\\.rhtml$” . html-mode)) 

(autoload 'run-ruby "inf-ruby" "Run an inderior Ruby Process")

(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
 	  '(lambda ()
 	     (inf-ruby-keys)))

(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-complete-file-name
	try-expand-dabbrev))
(require 'rails)

;; rdebug keys
(add-to-list 'load-path "~/emacs/ruby/ruby-debug/trunk/emacs")
(require 'rdebug)
;;(autoload ‘rdebug “rdebug” “Ruby debugging support.” t)
;(require 'gud)
(load-library "rdebug")
;(global-set-key (kbd "<f9>") ‘gud-step)
;(global-set-key (kbd "<f10>") ‘gud-next)
;(global-set-key (kbd "<f11>") ‘gud-cont)
;;(global-set-key “\C-c\C-d” ‘rdebug)
(global-set-key (kbd "\C-C\C-d") 'rdebug )

;;  (autoload 'ruby-mode "ruby-mode"
;;      "Mode for editing ruby source files")
;;  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;;  (autoload 'run-ruby "inf-ruby"
;;      "Run an inferior Ruby process")
;;  (autoload 'inf-ruby-keys "inf-ruby"
;;      "Set local key defs for inf-ruby in ruby-mode")
;;  (add-hook 'ruby-mode-hook
;;      '(lambda ()
;;          (inf-ruby-keys)))
;;  ;; If you have Emacs 19.2x or older, use rubydb2x                              
;;  (autoload 'rubydb "rubydb3x" "Ruby debugger" t)
;;  ;; uncomment the next line if you want syntax highlighting                     
;;  (add-hook 'ruby-mode-hook 'turn-on-font-lock)

;; ;; add ruby-electric
;; (add-hook 'ruby-mode-hook
;;           (lambda()
;;             (add-hook 'local-write-file-hooks
;;                       '(lambda()
;;                          (save-excursion
;;                            (untabify (point-min) (point-max))
;;                            (delete-trailing-whitespace)
;;                            )))
;;             (set (make-local-variable 'indent-tabs-mode) 'nil)
;;             (set (make-local-variable 'tab-width) 2)
;;             (imenu-add-to-menubar "IMENU")
;;             (define-key ruby-mode-map "C-m" 'newline-and-indent) ;Not sure if this line is 100% right but it works!
;;             (require 'ruby-electric)
;;             (ruby-electric-mode t)
;;             ))

;; Ruby debugging.
;(add-to-list ‘load-path “~/emacs/ruby/emacs-rdebug”)
;(autoload ‘rdebug “rdebug” “Ruby debugging support.” t)
;(global-set-key [f9] ‘gud-step)
;(global-set-key [f10] ‘gud-next)
;(global-set-key [f11] ‘gud-cont)
;(global-set-key “\C-c\C-d” ‘rdebug)
