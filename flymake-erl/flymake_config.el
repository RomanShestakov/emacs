(provide 'flymake_config)

(require 'flymake)

;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-intemp))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list "~/emacs/flymake/bin/eflymake" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;; 	     '("\\.erl\\'" flymake-erlang-init))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as FILE-NAME. For the use of PREFIX see that function. Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on relative paths to other files \(for the type of checks flymake makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))




(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-intemp))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "~/emacs/flymake-erl/flymake-erl" (list buffer-file-name local-file))))

(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.erl\\'" flymake-erlang-init))

(add-hook 'find-file-hook 'flymake-find-file-hook)




;; (require 'flymake)
;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name temp-file
;; 		(file-name-directory buffer-file-name))))
;;     (list "~/emacs/flymake-erl/flymake-erl" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)




;; (defun flymake-erlang-init ()
;;   (list "~/emacs/flymake-erl/flymake-erl" (list buffer-file-name)))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
