(provide 'scala_mode_config)

(add-to-list 'load-path "~/emacs/scala")
(require 'scala-mode-auto)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (scala-mode-feature-electric-mode)
	     ))


;(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path "/usr/local/lib/ensime/elisp/")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(push "/usr/local/lib/scala/bin/" exec-path)
(push "/usr/local/lib/sbt/" exec-path)


(global-set-key "\C-c\C-z" 'scala-run-scala) ;;run scala interpreter
