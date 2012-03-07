(provide 'tuareg_mode_config)

(setq load-path  (cons "~/emacs/tuareg/tuareg-mode-1.45.7" load-path))

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(global-set-key "\C-c\C-s" 'tuareg-run-caml)

;; ;; flymake mode
;; (defun flymake-ocaml-init ()
;;           (flymake-simple-make-init-impl
;;             'flymake-create-temp-with-folder-structure nil nil
;;             (file-name-nondirectory buffer-file-name)
;;             'flymake-get-ocaml-cmdline))
;;     (defun flymake-get-ocaml-cmdline (source base-dir)
;;        (list "ocaml_flycheck.pl"
;;             (list source base-dir)))
    
;;     (push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
;;           flymake-allowed-file-name-masks)
;;     (push
;;       '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;        1 2 3 4) flymake-err-line-patterns)

;;     ;; optional setting
;;     ;; if you want to use flymake always, then add the following hook.
;;     (add-hook
;;      'tuareg-mode-hook
;;      '(lambda ()
;;         (if (not (null buffer-file-name)) (flymake-mode))))

;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:background "LightYellow" :underline "OrangeRed"))))
;;  '(flymake-warnline ((((class color)) (:background "LightBlue2" :underline "Yellow")))))

;; (global-set-key [(f9)] 'compile)


;; (global-set-key [(f9)]
;;   (lambda ()
;;     (interactive)
;;     (funcall (quote compile "make -k"))))

