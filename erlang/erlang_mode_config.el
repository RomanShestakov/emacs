(provide 'erlang_mode_config)

;;(setq erlang-root-dir "/usr/local/Cellar/erlang/R14B03/lib/erlang")
(setq erlang-root-dir "/usr/local/lib/erlang/lib")
(setq load-path  (cons "/usr/local/lib/erlang/lib/tools-2.6.6.6/emacs" load-path))
(setq exec-path (cons "/usr/local/lib/erlang/lib/bin" exec-path))
(require 'erlang-start)


(let ((distel-dir "~/emacs/distel/elisp"))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
      (lambda ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-name" "emacs@127.0.0.1"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")
        ;;(define-key erlang-mode-map [f5] 'compile)
        ;;(define-key erlang-mode-map (kbd "\C-c\C-dH") 'erlang-man-function)
))

(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@rs")))
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
       ;(car (split-string (shell-command-to-string "hostname"))))))


;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))


(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/local/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

(global-set-key [(f6)] (lambda () (interactive) (get-erl-man)))
(global-set-key [(f14)] (lambda () (interactive)(edb-toggle-interpret)))
;;(add-hook 'erlang-shell-mode-hook
          ;;(lambda()
            ;; start erlang shell
;;        (erlang-shell-display))

;;erlang compilation
;;http://www.rsaccon.com/2007/10/erlang-compilation-with-emacs.html
(defun my-erlang-compile()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (save-excursion
    (let ((thisdir default-directory))
      (setq src-file-name buffer-file-name)
      (set-buffer (get-buffer-create "*erl-output*"))
      (setq special-display-buffer-names (cons "*erl-output*" special-display-buffer-names))
      (setq default-directory thisdir)
      (erase-buffer)
      (compilation-mode)
      (toggle-read-only nil)
      (setq compilation-current-error nil)
      (display-buffer (current-buffer))
      (erl-spawn
        (erl-send-rpc (erl-target-node)
                      'distel
                      'eval_expression
                      (list (format "myapp_make:all(%S)." src-file-name)))
        (erl-receive ()
            ((['rex ['ok string]]
              (insert string))
             (['rex ['error reason]]
              (insert reason))
             (other
              (message "Unexpected: %S" other))))))))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))

  (define-key erlang-mode-map [f9]
    (lambda()
      (interactive)
      (progn
        (erlang-compile)))))



;; (let ((distel-dir "/Users/romanshestakov/src/distel4/elisp"))
;;   (unless (member distel-dir load-path)
;;     (setq load-path (append load-path (list distel-dir)))))
;; (require 'distel)
;; (distel-setup)

;; ;; Some Erlang customizations
;; (add-hook 'erlang-mode-hook
;;           (lambda ()
;;             ;; when starting an Erlang shell in Emacs, default in the node name
;;             (setq inferior-erlang-machine-options '("-sname" "emacs"))
;;             ;; add Erlang functions to an imenu menu
;;             (imenu-add-to-menubar "imenu")))

;; (setq erl-nodename-cache
;;       (make-symbol
;;        (concat
;;         "emacs@rs")))
;; ;; Mac OS X uses "name.local" instead of "name", this should work
;; ;; pretty much anywhere without having to muck with NetInfo
;; ;; ... but I only tested it on Mac OS X.
;;                                         ;(car (split-string (shell-command-to-string "hostname"))))))


;; ;; A number of the erlang-extended-mode key bindings are useful in the shell too
;; (defconst distel-shell-keys
;;   '(("\C-\M-i"   erl-complete)
;;     ("\M-?"      erl-complete)
;;     ("\M-."      erl-find-source-under-point)
;;     ("\M-,"      erl-find-source-unwind)
;;     ("\M-*"      erl-find-source-unwind)
;;     )
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             ;; add some Distel bindings to the Erlang shell
;;             (dolist (spec distel-shell-keys)
;;               (define-key erlang-shell-mode-map (car spec) (cadr spec)))))


;; (add-hook 'erlang-shell-mode-hook
;;           (lambda()
;;             ;;start erlang shell
;; 	    (setq newdir (concat default-directory ".."))
;; 	    (message "current: %S" newdir)
;; 	    (setq default-directory newdir)
;; 	    (erlang-shell-display)))
	  
;;erlang compilation
;;http://www.rsaccon.com/2007/10/erlang-compilation-with-emacs.html
(defun my-erlang-compile()
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (save-excursion
    (let ((thisdir default-directory))
      (setq src-file-name buffer-file-name)
      (set-buffer (get-buffer-create "*erl-output*"))
      (setq special-display-buffer-names (cons "*erl-output*" special-display-buffer-names))
      (setq default-directory thisdir)
      (erase-buffer)
      (compilation-mode)
      (toggle-read-only nil)
      (setq compilation-current-error nil)
      (display-buffer (current-buffer))
      (erl-spawn
        (erl-send-rpc (erl-target-node)
                      'distel
                      'eval_expression
                      (list (format "myapp_make:all(%S)." src-file-name)))
        (erl-receive ()
            ((['rex ['ok string]]
              (insert string))
             (['rex ['error reason]]
              (insert reason))
             (other
              (message "Unexpected: %S" other))))))))

;; (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
;; (defun my-erlang-mode-hook ()
;; ;; when starting an Erlang shell in Emacs, default in the node name
;;   (setq inferior-erlang-machine-options '("-sname" "emacs"))


;;(global-set-key (kbd "C-l") 'tinylisp-error-find-1 ) 
;;[f13]
;; (define-key erlang-mode-map [f13]
;;   (lambda()
;;     (interactive)
;;     (progn
;;       (my-erlang-compile)))))


;;create new erlang project
(defun create-project-structure(dir &optional parents)
  "Create directory structure for erlang project"

(interactive
   (list (read-file-name "Make directory: " default-directory default-directory
                         nil nil)
         t))


;;make directory structure
(make-directory dir parents)
   (setq dir (expand-file-name dir))
   (make-directory (concat dir "/ebin") parents)
   (make-directory (concat dir "/include") parents)
   (make-directory (concat dir "/src") parents)
   (make-directory (concat dir "/priv") parents)
   (create-emakefile dir))

(defun create-emakefile (dir)
  "Create Emakefile for new Erlang project"
  (setq contents "\{\"src/*\", [debug_info, {outdir, \"ebin\"}, {i,\"include\"}]}. ")
  (write-string-to-file (concat (expand-file-name dir) "/Emakefile") contents))


(defun write-string-to-file (string file)
   (with-temp-buffer
     (insert string)
     (when (file-writable-p file)
       (write-region (point-min)
                     (point-max)
                     file))))


(global-set-key [(M-f8)]
  (lambda ()
    (interactive)
    (funcall (quote tempo-template-function-header))))


;;assosiate cfg files with erlang mode
(setq auto-mode-alist (cons '(".cfg" . erlang-mode) auto-mode-alist))

;; http://jamesaimonetti.com/2010/02/03/more-erlangemacs/
(defun erl-shell-with-flags (flags)
  "Start an erlang shell with flags"
  (interactive (list (read-string "Flags: ")))
  (set 'inferior-erlang-machine-options (split-string flags))
  (erlang-shell))
 
;; map Ctrl-c Ctrl-z to the new function
(global-set-key "\C-c\C-x" 'erl-shell-with-flags)


;; template for func headers
(tempo-define-template "function-header"
   '(" 
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec"  ))
