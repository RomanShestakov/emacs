;;  .emacs

;;(load "~/elisp/packages/tiny-tools/lisp/tiny/tinypath")
;;(require 'tinylisp)

(setq make-backup-files t)

;; The following variables control how backup files are made, and are
;; only used if `make-backup-files' is non-nil.
;;
;; Backup files are created when a file is saved for the first time (and
;; the file already exists on disk).  Backup files can be created by
;; renaming the original file or by copying.
;;
;; Renaming means that Emacs renames the existing file so that it is a
;; backup file, then writes the buffer into a new file.  Any other names
;; that the old file had will now refer to the backup file.  The new
;; file is owned by you and its group is defaulted.  Note that this
;; method CAN CHANGE the ownerships of a file.  The variables
;; `backup-by-copying-when-linked' and `backup-by-copying-when-mismatch'
;; control whether or not the ownerships can change.
;;
; Copying means that Emacs copies the existing file into the backup
;; file, then writes the buffer on top of the existing file.  Any other
;; names that the old file had will now refer to the new (edited) file.
;; The file's owner and group are unchanged.  However, if you edit very
;; large file, backing up by copying can take a long time.
;;
;; The choice of renaming or copying is controlled by the variables
;; backup-by-copying, backup-by-copying-when-linked and
;; backup-by-copying-when-mismatch.  For most people, these variables
;; should have the following values:
;;
;;	backup-by-copying			nil
;;	backup-by-copying-when-linked		t
;;	backup-by-copying-when-mismatch		t
;;
;; If you want to backup by renaming, set the variable
;; `backup-by-copying' to nil; if you want to backup by copying, set the
;; variable `backup-by-copying' to non-nil.

(setq backup-by-copying nil)

;; If you want to use copying to create backups for files with multiple
;; names, set `backup-by-copying-when-linked' to non-nil.  This causes
;; the alternate names to refer to the latest version as edited.  This
;; variable is relevant only if backup-by-copying is nil.

(setq backup-by-copying-when-linked t)

;; If you want to create backups by copying if this preserves owner or
;; group, set `backup-by-copying-when-mismatch' to non-nil.  Renaming
;; may still be used (subject to control of other variables) when it
;; would not result in changing the owner or group of the file; that is,
;; for files which are owned by you and whose group matches the default
;; for a new file created there by you.  This variable is relevant only
;; if backup-by-copying is nil.

(setq backup-by-copying-when-mismatch t)


;; You can set `require-final-newline' to one of the following to
;; control newlines at the end of a file when the file is saved:
;;
;;	t		Silently place a newline at the end of the file
;;			when the file is saved.  This is done only if
;;			the file does not already end with a newline.
;;	nil		Don't add newlines.
;;	(anything else) Ask the user what to do.

(setq require-final-newline t)


;; Set the following to `t' to create numbered backup files.  Set it to
;; `nil' to make numbered backup files only for those files that already
;; have them.  Set it to `never' to never make numbered backup files
;; (i.e., use "(setq version-control 'never)").

(setq version-control nil)


;; Set the following variable to `t' or `nil', depending on whether or
;; not you want Emacs to auto-save your files.  It is strongly suggested
;; that auto-save-default be set to `t' (enable auto-save) to prevent
;; much work from being lost in the event of a power failure or system
;; crash.

(setq auto-save-default t)

;; auto-save-interval is the number of keystrokes between auto-saves.
;; If it is set to zero, autosaving is disabled.

(setq auto-save-interval 1000)


;; If default-truncate-lines is non-nil, continuation lines are not
;; displayed; each line of text is given one and only one screen line.
;; In this case, lines longer than the screen/window width have to be
;; viewed using the scroll-left and scroll-right functions.
;; It is recommended that default-truncate-lines be set to `nil'.

(setq default-truncate-lines nil)
(setq truncate-partial-width-windows default-truncate-lines)


;; scroll-step is the number of lines to try scrolling a window when
;; point tries to move outside of a window.  If that fails to bring the
;; point back onto the screen the point is centered in the window
;; instead.  If scroll-step is zero, the point is always centered after
;; it moves outside of a window.

(setq scroll-step 0)


;; next-screen-context lines contains the number of lines of continuity
;; when scrolling a window.

(setq next-screen-context-lines 1)

;;**** Key-binding changes

(global-set-key "\eg" 'goto-line)   
(global-set-key "\eW" 'copy-region-as-kill) 
;; (global-set-key "\^w" 'backward-kill-word)
;; (global-set-key "\eq" 'query-replace)
;; (global-set-key "\eQ" 'query-replace-regexp)
;; (global-set-key "\^x\^e" 'compile)
;; (global-set-key [S-return] 'newline-and-indent)

;; Disable C-x n n, ^z, C-x C-l, C-x C-u

(global-unset-key "\^z")
(global-unset-key "\^xnn")
;; (global-unset-key "\^x\^l")
;; (global-unset-key "\^x\^u")


;; Marking regions

;; (setq transient-mark-mode t)

;; Line numbers

(line-number-mode 1)

;; Printing

(load "lpr")

(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) 
		  (append lpr-switches (lpr-make-buffer-title)) t))

(defun lpr-make-buffer-title ()
  (list (concat "-b" (user-login-name) ":" (buffer-name) "<Emacs-buffer>")))

(setq lpr-command "enscript-stdin")

; Set lpr-headers-switches rather than lpr-switches to get 
; around an odd bug in print-region-1.
(setq lpr-headers-switches "-2rG")

;; Set degree of noviceness

;; Don't complain about ESC ESC,  [commented out b/c emacs tutorial says so!]
;; (put 'eval-expression 'disabled nil)

(defvar abbreviated-buffer-file-name nil
   "A shortened version of the buffer's file name, if any (buffer local).")

(make-variable-buffer-local 'abbreviated-buffer-file-name)

(setq find-file-hooks 
      (append find-file-hooks '(set-abbreviated-file-name)))
(setq write-file-hooks 
      (append write-file-hooks '(set-abbreviated-file-name)))

(defun set-abbreviated-file-name ()
  (let* ((name (abbreviate-file-name buffer-file-name))
	 (suffix-index (string-match "\\(/[^/]*/[^/]*/[^/]*\\)$" name)))
    (setq abbreviated-buffer-file-name
	  (if (and suffix-index (> suffix-index 3))
	      (concat "..." (substring name suffix-index))
	    name))
    nil))

(setq-default mode-line-format
  '("EMACS {" host-name ": " mode-line-buffer-identification
    "%1*%1+" (line-number-mode " L%l") "}   "
    "%[(" mode-name minor-mode-alist "%n" mode-line-process
    ")%]   "
    (buffer-file-name ("{ " abbreviated-buffer-file-name " }   "))
    (-3 . "%p") "   " global-mode-string))

(set-default (quote mode-line-buffer-identification) (quote ("%b")))

;; Initial windows: Reminder file (if any) and Scheme.

; (shell)
; (call-interactively 'run-scheme)
; (split-window-vertically nil)
; (other-window 1)
; (switch-to-buffer "*shell*")
; (other-window 1)



(raise-frame (car (car (cdr (current-frame-configuration)))))

;; (if (eq window-system 'x)
;;     (let ((remfile (substitute-in-file-name "~$MASTER/adm/reminder"))
;; 	  frame-window)
;;       (if (file-readable-p remfile)
;; 	  (progn
;; 	    (save-window-excursion
;; 	      (find-file remfile)
;; 	      (let ((frame (make-frame '((minibuffer . nil) 
;; 					 (height . 50)
;; 					 (width . 80)
;; 					 (name . "Class Notices. (Click here to put in the background.)")))))
;; 		(raise-frame frame)
;; 		(setq frame-window (frame-selected-window frame))))
;; 	    (set-buffer "reminder")
;; 	    (goto-char (point-max))
;; 	    (beginning-of-line (- 3 (window-height frame-window)))
;; 	    (set-window-start frame-window (point))
;; 	    (set-buffer-modified-p nil)
;; 	    (toggle-read-only 1)))))
	      
	    
;; Miscellaneous adjustments

; Turn off the confounded tool bar (PNH 8/20/2003)
(tool-bar-mode 0)

; Set readable menubar face [code copied from cus-face.el] (PNH 8/20/2003)
(require 'cus-face)
(let ((face 'menu)
      (spec '((((type x-toolkit)) (:width normal :family "Courier"))))
      (now nil)
      (comment nil))
  (put face 'saved-face spec)
  (put face 'saved-face-comment comment)
  (when now
    (put face 'force-face t))
  (when (or now (facep face))
    (put face 'face-comment comment)
    (make-empty-face face)
    (face-spec-set face spec)))


(defun set-program-coloring (arg)
  (interactive "P")
  (show-paren-mode arg)
  (global-font-lock-mode arg))

(setq comment-style 'indent)

;;;######################################################################
;; Place any additional customization after the "load" command for 
;; this file.
;;;######################################################################

;; define emacs root


;; load erlang configuration
(add-to-list 'load-path "~/emacs")

(require 'emacs_config)

;http://www.emacswiki.org/cgi-bin/wiki/AquamacsFAQ#toc4
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'meta)


(setq-default ispell-program-name "aspell")

(defalias 'qrr 'query-replace-regexp)


;;tab mode http://www.emacswiki.org/cgi-bin/wiki/TabBarMode
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
     
    (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
      `(defun ,name (arg)
         (interactive "P")
         ,do-always
         (if (equal nil arg)
             ,on-no-prefix
           ,on-prefix)))
     
    (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
    (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
     
    (global-set-key [(control tab)] 'shk-tabbar-next)
    (global-set-key [(control shift tab)] 'shk-tabbar-prev)

(put 'narrow-to-region 'disabled nil)







(put 'erase-buffer 'disabled nil)
