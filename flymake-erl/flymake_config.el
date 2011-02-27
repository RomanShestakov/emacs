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
