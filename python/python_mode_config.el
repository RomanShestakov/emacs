(provide 'python_mode_config)

;; from http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/

;; http://hide1713.wordpress.com/2009/01/30/setup-perfect-python-environment-in-emacs/

;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;(require 'python)

(require 'ipython)
;(setq python-python-command "ipython")
(setq py-python-command-args '( "-colors" "Linux"))
;(setq py-python-command-args '( "--colors" "Linux"))
;(setq py-python-command-args '("-colors" "Linux"))
;(setq py-python-command-args '("--colors=linux"))
;(setq py-python-command-args '("-pylab" "-colors" "LightBG"))

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; node unit test integration
(require 'nose)
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key "\C-T" 'nosetests-all)
	    ;; (local-set-key "\C-cm" 'nosetests-module)
	    ;; (local-set-key "\C-c." 'nosetests-one)
	    ;; (local-set-key "\C-cpa" 'nosetests-pdb-all)
	    ;; (local-set-key "\C-cpm" 'nosetests-pdb-module)
	    ;; (local-set-key "\C-cp." 'nosetests-pdb-one)
	    ))

;; autocomplete
;; http://stackoverflow.com/questions/2855378/ropemacs-usage-tutorial
;(add-to-list 'load-path "~/emacs/auto-complete-1.3.1"); This may not be appeared if you have already added
;(add-to-list 'ac-dictionary-directories "~/emacs/auto-complete-1.3.1/ac-dict")
;(require 'auto-complete-config)
;(ac-config-default)

(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;; switch off pabbrev
(put 'python-mode 'pabbrev-global-mode-excluded-modes t)
(put 'ipython 'pabbrev-global-mode-excluded-modes t)
(put 'comint-mode 'pabbrev-global-mode-excluded-modes t)

;; flymake init
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; run python script with f9 instead of C-C C-C
(define-key py-mode-map [f9] 'py-execute-buffer)




;; (require 'anything-ipython)
;; (define-key py-mode-map (kbd "M-") 'anything-ipython-complete)
;; (define-key py-shell-map (kbd "M-") 'anything-ipython-complete)
;; (define-key py-mode-map (kbd "C-c M") 'anything-ipython-import-modules-from-buffer)

;; (require 'comint)
;; (define-key comint-mode-map (kbd "M-") 'comint-next-input)
;; (define-key comint-mode-map (kbd "M-") 'comint-previous-input)
;; (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
;; (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;; (autoload 'pylookup-lookup "pylookup")
;; (autoload 'pylookup-update "pylookup")
;; (setq pylookup-program "~/emacs/python/pylookup.py")
;; (setq pylookup-db-file "~/emacs/python/pylookup.db")
;; (global-set-key "\C-ch" 'pylookup-lookup)

;; (autoload 'autopair-global-mode "autopair" nil t)
;; (autopair-global-mode)
;; (add-hook 'lisp-mode-hook
;;           #'(lambda () (setq autopair-dont-activate t)))

;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (push '(?' . ?')
;;                     (getf autopair-extra-pairs :code))
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))

;; #sudo easy_install pep8
(require 'python-pep8)
(require 'python-pylint)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; activate yasnippet
(add-to-list 'load-path "~/emacs/yasnippet")
(require 'yasnippet)
(yas/initialize)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))



;; (yas/global-mode 1)

;; http://chrispoole.com/project/ac-python/
;(require 'ac-python)


;; ;; ;; Initialize Pymacs

;; pymacs
(add-to-list 'load-path "~/emacs/Pymacs")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; (eval-after-load "pymacs"
;;  (add-to-list 'pymacs-load-path "~/emacs/Pymacs"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; (ac-ropemacs-initialize)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;; 	    (add-to-list 'ac-sources 'ac-source-ropemacs)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Auto-completion
;; ;;;  Integrates:
;; ;;;   1) Rope
;; ;;;   2) Yasnippet
;; ;;;   all with AutoComplete.el
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;       (setq value (cons (format "%s%s" prefix element) value))))))
;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")
;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))
;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;                  (auto-complete-mode 1)
;;                  (set (make-local-variable 'ac-sources)
;;                       (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;                  (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;                  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;                  (set (make-local-variable 'ac-auto-start) nil)))


;; ;;Ryan's python specific tab completion
;; (defun ryan-python-tab ()
;;   ; Try the following:
;;   ; 1) Do a yasnippet expansion
;;   ; 2) Do a Rope code completion
;;   ; 3) Do an indent
;;   (interactive)
;;   (if (eql (ac-start) 0)
;;       (indent-for-tab-command)))

;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))

;(define-key python-mode-map "\t" 'ryan-python-tab)



;; calc-mode more comfortable
;(global-set-key (kbd "M-c") 'calc-dispatch)


;; ;;Ryan's python specific tab completion
;;   ; Try the following in order:
;;   ; 1) Try a yasnippet expansion without autocomplete
;;   ; 2) If at the beginning of the line, indent
;;   ; 3) If at the end of the line, try to autocomplete
;;   ; 4) If the char after point is not alpha-numerical, try autocomplete
;;   ; 5) Try to do a regular python indent.
;;   ; 6) If at the end of a word, try autocomplete.
;; (define-key python-mode-map "\t" 'yas/expand)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'yas/trigger-fallback) 'ryan-python-expand-after-yasnippet)))
;; (defun ryan-indent ()
;;   "Runs indent-for-tab-command but returns t if it actually did an indent; nil otherwise"
;;   (let ((prev-point (point)))
;;     (indent-for-tab-command)
;;     (if (eql (point) prev-point)
;;         nil
;;       t)))
;; (defun ryan-python-expand-after-yasnippet ()
;;   (interactive)
;;   ;;2) Try indent at beginning of the line
;;   (let ((prev-point (point))
;;         (beginning-of-line nil))
;;     (save-excursion
;;       (move-beginning-of-line nil)
;;       (if (eql 0 (string-match "\\W*$" (buffer-substring (point) prev-point)))
;;           (setq beginning-of-line t)))
;;     (if beginning-of-line
;;         (ryan-indent)))
;;   ;;3) Try autocomplete if at the end of a line, or
;;   ;;4) Try autocomplete if the next char is not alpha-numerical
;;   (if (or (string-match "\n" (buffer-substring (point) (+ (point) 1)))
;;           (not (string-match "[a-zA-Z0-9]" (buffer-substring (point) (+ (point) 1)))))
;;       (ac-start)
;;     ;;5) Try a regular indent
;;     (if (not (ryan-indent))
;;         ;;6) Try autocomplete at the end of a word
;;         (if (string-match "\\W" (buffer-substring (point) (+ (point) 1)))
;;             (ac-start)))))

;; ;; End Tab completion


;; (defun add-py-debug ()
;;       "add debug code and move line down"
;;     (interactive)
;;     (move-beginning-of-line 1)
;;     (insert "import pdb; pdb.set_trace();\n"))
;; ;(local-set-key (kbd "<f12>") 'add-py-debug)
;; ;(define-key py-mode-map (kbd "<Shift-f1>") 'add-py-debug)
;; (define-key py-mode-map (kbd "C-c C-t") 'add-py-debug)

;; (defun remove-py-debug ()
;;   "remove py debug code, if found"
;;   (interactive)
;;   (let ((x (line-number-at-pos))
;;     (cur (point)))
;;     (search-forward-regexp "^[ ]*import pdb; pdb.set_trace();")
;;     (if (= x (line-number-at-pos))
;;     (let ()
;;       (move-beginning-of-line 1)
;;       (kill-line 1)
;;       (move-beginning-of-line 1))
;;       (goto-char cur))))
;; ;(local-set-key (kbd "<f12>") 'remove-py-debug)
;; ;(define-key py-mode-map (kbd "<Shift-f1>") 'remove-py-debug)
;; (define-key py-mode-map (kbd "C-c C-t") 'remove-py-debug)

;; set annotations for debugger
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

;; (defun add-py-debug ()
;;       "add debug code and move line down"
;;     (interactive)
;;     (move-beginning-of-line 1)
;;     (insert "import pdb; pdb.set_trace();\n"))
;; ;(local-set-key (kbd "<f12>") 'add-py-debug)
;; ;(define-key py-mode-map (kbd "<Shift-f1>") 'add-py-debug)
;; (define-key py-mode-map (kbd "C-c C-t") 'add-py-debug)


;; set breakpoints wiht C-C C-t
(defun python-add-breakpoint ()
  (interactive)
  ;(py-newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (py-newline-and-indent)
  (highlight-lines-matching-regexp "^[ 	]*import ipdb; ipdb.set_trace()"))
(define-key py-mode-map (kbd "C-c C-t") 'python-add-breakpoint)
