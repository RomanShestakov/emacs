;;  -*-Emacs-Lisp-*-
;;;;
;;;;
;;;
;;; Modified from gdb.el by Pertti Kellom\"aki, pk@cs.tut.fi
;;; Modified from psd.el by Paul Hilfinger, Hilfinger@cs.berkeley.edu
;;;

;;; Run stkdb under GNU Emacs 
;;; Copyright (C) 1992 Pertti Kellom\"aki.
;;; Modifications Copyright (C) 2003 by Paul N. Hilfinger

;;; stkdb is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; stkdb is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with stkdb; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Paul N. Hilfinger, Univ. of California at Berkeley.
;;; Adapted from the stk.el written by
;;;    Pertti Kellom\"aki, Tampere University of Technology, Finland
;;;    pk@cs.tut.fi

;;; Description of stkdb interface:

;;; A facility is provided for the simultaneous display of the source code
;;; in one window, while using stkdb to step through a function in the
;;; other.  Highlighting or a small arrow in the source window 
;;; indicates the current subexpression or line.

;;; Starting up:

;;; In order to use this facility, start up an inferior scheme
;;; interpreter. Then give the command "M-x stkdb-mode". This will load
;;; the stkdb system into your interpreter. The command "M-x
;;; stkdb-debug-file", usually bound to "C-c d" will instrument and load a
;;; Scheme file into the interpreter. The procedures in the file are
;;; instrumented so that executing them invokes the debugger. 

;;; To communicate with Emacs, the Scheme portion of the stkdb facility,
;;; when running in "Emacs mode", outputs special strings having one of the
;;; following formats (^Z is a control-Z character):

;;; a. "^Z^Zb:FILE:LINE1:COL1:LINE2:COL2:"
;;;    Encountered a breakpoint (or end of a 'step' or 'next') on the
;;;    expression stretching from line #LINE1 and column #COL1 to
;;;    line #LINE2 and column #COL2 in FILE.
;;; b. "^Z^Zr:FILE:LINE1:COL1:LINE2:COL2:"
;;;    Stopped after finishing evaluation of an expression (as a result
;;;    of a 'step', 'next', or 'finish').  The expression is denoted as
;;;    for case a.
;;; c. "^Z^Zf:FILE:LINE1:COL1:LINE2:COL2:"
;;;    Just did an 'up' or 'down' command that did not leave us at the
;;;    the top level.   The expression to be displayed is denoted as for
;;;    case a.
;;; d. "^Z^Ze:FILE:LINE1:COL1:LINE2:COL2:"
;;;    Stopped as a result of an error.  The expression being evaluated
;;;    is denoted as for case a.
;;; e. "^Z^ZR::::::"
;;;    Unhighlight or unmark the last expression indicated by one of 
;;;    the preceding cases.
;;; f. "^Z^Zt:FILE:LINE:"FUNC":FRAME:CURRENT:
;;;    Add entry to the current back-trace in file FILE at line LINE, 
;;;    indicating that the enclosing named function is FUNC. List this as
;;;    frame number FRAME (0 is top).  CURRENT is the number of the currently
;;;    displayed (highlighted) frame.  If FILE, LINE, FUNC, and FRAME are
;;;    all empty, this just revises which frame in the existing back-trace
;;;    is current.  When frame FRAME is displayed, any subsequent existing
;;;    frames should be considered invalid (and deleted).
;;; g. "^Z^ZV:FRAME:NAME^ZVALUE^Z"
;;;    Indicates that local variable NAME has value VALUE in frame number
;;;    FRAME (the numbering is as for the back-trace).  
;;; h. "^Z^ZV:FRAME:^Z^Z"
;;;    Clear all local variable displays for frame FRAME.

(require 'cl)
(require 'comint)
(require 'scheme)
(require 'cmuscheme)

;; Where stkdb resides. Edit this to suit your installation.  It is prefixed
;; to the front of file names that need to be loaded, so if it is simply a
;; directory, be sure that it ends with "/".
(defvar stkdb-vicinity nil
  "Path of the directory that contains stkdb.")

(setq stkdb-vicinity
      (or stkdb-vicinity
	  (getenv "STKDB_LIBRARY_PATH")
	  (and (getenv "SCHEME_LIBRARY_PATH")
	       (concat (getenv "SCHEME_LIBRARY_PATH") "stkdb/"))
	  "/Applications/STk/lib/stkdb/"))

(defvar stkdb-mode-hook '()
  "Hook to run upon entering stkdb-mode")

(defvar stkdb-marker-regexp 
  "^\032\032\\([ebrfR]\\):\\([^:]*\\):\\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\):"
  "Regular expression describing marker sent from stkdb to Emacs.")

(defvar stkdb-stack-trace-regexp 
  "^\032\032\\t:\\([^:]*\\):\\([0-9]*\\):\"\\([^\"]*\\)\":\\([0-9]*\\):\\([0-9]*\\):"
  "Regular expression describing stack-trace frame sent from stkdb to Emacs.")

(defvar stkdb-var-list-regexp 
  "^\032\032V:\\([0-9]+\\):\\([^\032]*\\)\032\\([^\032]*\\)\032"
  "Beginning of a variable-value message from stkdb to Emacs.")

(defun stkdb-choose-background-spec (color-list)
  "A face spec (in format used by defface) specifying a background color
(for all displays) that is the first defined color on COLOR-LIST.  Empty
spec if none defined."
  (cond ((null color-list) '((t)))
	((x-color-defined-p (car color-list)) 
	 `((t (:background ,(car color-list)))))
	(t (stkdb-choose-background-spec (cdr color-list)))))

(defface stkdb-bpt-face 
  (stkdb-choose-background-spec '("PaleGreen" "PaleGreen1"))
  "Face used to highlight a breakpoint stop.")

(defface stkdb-return-face 
  (stkdb-choose-background-spec '("LightBlue" "SkyBlue1"))
  "Face used to highlight a completed expression.")

(defface stkdb-show-stack-frame-face 
  (stkdb-choose-background-spec '("Plum" "magenta1"))
  "Face used to highlight an expression in the midst of computation.")

(defface stkdb-error-face 
  (stkdb-choose-background-spec '("OrangeRed" "OrangeRed1"))
  "Face used to highlight an expression that has caused an error.")

;; Add stkdb into the minor modes.
(defvar stkdb-mode nil "Indicator for stkdb-mode")

(or (assq 'stkdb-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(stkdb-mode " Stkdb") minor-mode-alist)))

;; The temporary files that are used for sending stuff to stkdb.
(defvar stkdb-tmp-source-file (make-temp-name "/tmp/stkdb1"))

(defvar stkdb-highlight-start (make-marker)
  "Marker for beginning of s-expression highlighting.")

(defvar stkdb-highlight-end (make-marker)
  "Marker for end of s-expression highlighting.")

(defvar stkdb-highlighted-buffers nil
  "List of buffers that may contain stkdb highlighting.")

(defvar stkdb-inferior-minor-mode-map nil
  "Key bindings for Scheme execution buffers when in stkdb mode.")

(defvar stkdb-settings-menu-map nil
  "Menu bindings for the 'Settings' submenu.")

(defvar stkdb-auto-backtrace "#f"
  "Indicates the type of automatic backtrace and display of 
local variables that has been requested at each stop (#f = none).")

(defvar stkdb-keep-tail-recursion-p nil
  "When true, indicates that values returned from tail-recursive calls should
be caught by the debugger.")

(defvar stkdb-show-all-returned-values-p nil
  "When true, stepping mode will print all values yielded by expressions.")

(defun stkdb-define-menu-item (map sym label command)
  "Add local binding [menu-bar debug SYM] with label LABEL bound to COMMAND in
MAP."
  (let* ((bindings (and command (symbolp command) 
			(where-is-internal command map 'non-ascii)))
	 (keys (and (vectorp bindings) 
		    (> (length bindings) 0)
		    (not (eq (elt bindings 0) 'menu-bar))
		    bindings)))
    (define-key-after (lookup-key map [menu-bar debug]) 
      `[,sym] 
      (cond (keys
	     `(menu-item ,label ,command :key-sequence ,keys))
	    ((keymapp command) (cons label command))
	    (command `(menu-item ,label ,command))
	    (t `(menu-item ,label)))
      t)))

(defun stkdb-define-menu-button-item (map sym label var &optional commands)
  "In MAP, add local binding [SYM] with label LABEL as menu button toggling
variable VAR, and also executing COMMANDS (an optional list of commands)."
  (define-key-after map `[,sym] 
    `(menu-item ,label (lambda () 
			 (interactive)
			 (setq ,var (not ,var))
			 ,@commands)
		:button (:toggle . ,var))
    t))

(defun stkdb-define-radio-button-item (map sym label var val 
					   &optional commands)
  "In MAP, add local binding [SYM] with label LABEL as menu button toggling
variable VAR, and also executing COMMANDS (an optional list of commands)."
  (define-key-after map `[,sym] 
    `(menu-item ,label (lambda () 
			 (interactive)
			 (setq ,var ',val)
			 ,@commands)
		:button (:radio . (equal ,var ',val)))
    t))

(defun stkdb-to-t/f (x)
  "Map true => string #t, false => string #f"
  (if x "#t" "#f"))

(defun stkdb-to-f/t (x)
  "Map true => string #f, false => string #t"
  (stkdb-to-t/f (not x)))

(defun stkdb-define-menu-button-send-item (map sym label var opt 
					       &optional val-trans)
  "In MAP, add local binding [SYM] with label LABEL as menu button toggling
variable VAR, and also sending a command to the inferior scheme process
to set debugger option OPT, using function VAL-TRANS to convert the value of
VAR to an option value.  VAL-TRANS defaults ot stkdb-to-t/f."
  (stkdb-define-menu-button-item 
   map sym label var 
   `((force-stkdb-mode)
     (send-string (scheme-proc) 
		  (concat "(stkdb:set-options! " ,(symbol-name opt) 
			  " " (,(or val-trans 'stkdb-to-t/f) ,var) ")\n")))))
			  
(defun stkdb-define-radio-button-send-item (map sym label var val send-val 
						opt )
  (stkdb-define-radio-button-item
   map sym label var val
   `((force-stkdb-mode)
     (send-string (scheme-proc)
		  (concat "(stkdb:set-options! " ,(symbol-name opt)
			  " " ,send-val ")\n")))))

(if (null stkdb-settings-menu-map)
    (let ((map (make-keymap)))
      (setq stkdb-settings-menu-map map)
      (stkdb-define-radio-button-send-item
       map 'no-bt-mode "No Auto Backtrace" 
       'stkdb-auto-backtrace "#f" "#f" :auto-backtrace)
      (stkdb-define-radio-button-send-item 
       map 'bt-mode "Auto-Display Backtrace" 
       'stkdb-auto-backtrace "'bt" "'bt" :auto-backtrace)
      (stkdb-define-radio-button-send-item 
       map 'bt-top-mode "Auto-Display Backtrace/Top Locals" 
       'stkdb-auto-backtrace "'bt-and-top" "'bt-and-top" :auto-backtrace)
      (stkdb-define-radio-button-send-item 
       map 'bt-locals-mode "Auto-Display Backtrace/Locals" 
       'stkdb-auto-backtrace "'bt-and-locals" "'bt-and-locals" :auto-backtrace)
      (define-key-after map [separator-eval] '(menu-item "--") t)
      (stkdb-define-menu-button-item 
       map 'keep-tail-recursive "Keep Tail Recursion" 
       'stkdb-keep-tail-recursion-p)
      (stkdb-define-menu-button-send-item 
       map 'show-all-returns "Show All Returned Values"
       'stkdb-show-all-returned-values-p :show-returns)))

(if (null stkdb-inferior-minor-mode-map)
    (let ((map (make-keymap)))
      (setq stkdb-inferior-minor-mode-map map)
      (mapc '(lambda (x)
	       (let ((func (intern (concat "stkdb-" (cdr x)))))
		 (if (stringp (car x))
		     (progn
		       (define-key map (concat "\C-c" (car x)) func)
		       (define-key scheme-mode-map 
			 (concat "\C-x\C-a" (car x)) func))
		   (define-key map (car x) func)
		   (define-key scheme-mode-map (car x) func))
		 (eval 
		  `(defun ,func ()
		     (interactive)
		     (send-string (scheme-proc)
				  (concat "(stkdb) *quiet* " ,(cdr x) "\n"))))))
	    '(([f3] . "up")
	      ("<" . "up")
 	      ([f4] . "down")
 	      (">" . "down")
	      ([f5] . "step")
	      ("\C-s" . "step")
	      ([f6] . "next")
	      ("\C-n" . "next")
	      ([f7] . "finish")
	      ("\C-f" . "finish")
	      ([f8] . "cont")
	      ("\C-r" . "cont")))

      (define-key map [menu-bar debug]
	(cons "Debugging" (make-sparse-keymap "Debugging")))
      (stkdb-define-menu-item map 'continue "Continue" 'stkdb-cont)
      (stkdb-define-menu-item map 'finish "Finish Function" 'stkdb-finish)
      (stkdb-define-menu-item map 'next "Step Over" 'stkdb-next)
      (stkdb-define-menu-item map 'step "Step Into" 'stkdb-step)
      (stkdb-define-menu-item map 'down "View Callee" 'stkdb-down)
      (stkdb-define-menu-item map 'up "View Caller" 'stkdb-up)
      (stkdb-define-menu-item map 'locals "See Local Variables" 'stkdb-locals)
      (stkdb-define-menu-item map 'bt "Backtrace" 'stkdb-backtrace)
      (stkdb-define-menu-item map 'intr "Interrupt" 'comint-interrupt-subjob)
      (stkdb-define-menu-item map 'pop "Abandon Expression" 'stkdb-pop)
      (stkdb-define-menu-item map 'reset "Exit" 'stkdb-reset)

      (stkdb-define-menu-item map 'separator-eval "--" nil)

      (stkdb-define-menu-item map 'settings "Settings..." 
			      stkdb-settings-menu-map)
      (stkdb-define-menu-item map 'help "Help" 'stkdb-help)

      (rplacd
       (or (assq 'stkdb-mode minor-mode-map-alist)
	   (car (setq minor-mode-map-alist 
		      (cons (cons 'stkdb-mode nil) minor-mode-map-alist))))
       stkdb-inferior-minor-mode-map)))


(defun stkdb-mode (&optional arg)
  "Toggle stkdb-mode; with argument turn on stkdb-mode.

Stkdb-mode is a minor mode for interacting with a stkdb running in an
inferior Scheme buffer. Stkdb is a Scheme debugger that debugs the
program by instrumenting it.

The command `stkdb-debug-file', which is bound to \\[stkdb-debug-file]
prepares a Scheme file for debugging and loads it into the Scheme 
interpreter.

The command `stkdb-set-breakpoint' or `C-x SPC' sets a breakpoint in
current line when given in a Scheme buffer.

Entering stkdb-mode also loads stkdb into the Scheme interpreter.

If the debugger does not seem to work properly, try the command ``M-x
stkdb-reset'', which will clear breakpoints and restore the debugger
into its initial state."
  (interactive "p")
  (make-local-variable 'stkdb-mode)
  (if (and (<= (or arg 1) 1)
	   stkdb-mode)

      ;; turn off stkdb-mode
      (let ((proc (get-buffer-process (current-buffer))))
	(stkdb-unhighlight-all)
	(setq stkdb-filter-accumulator nil)
	(if proc 
	    (progn 
	      (set-process-filter proc nil)
	      (set-process-sentinel proc nil)))
	(setq stkdb-mode nil))

    ;; otherwise set up stkdb-mode
    (setq stkdb-mode t)
    (make-local-variable 'stkdb-filter-accumulator)
    (setq stkdb-filter-accumulator nil)
    (set-process-filter (get-buffer-process (current-buffer))
			'stkdb-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'stkdb-sentinel)
    
    (send-string
     (scheme-proc)
     (concat "(begin "
	     "(define stkdb-vicinity \""
	     stkdb-vicinity
	     
	     "\") (load \"" (concat stkdb-vicinity "stkdb.scm") "\")"
	     "(import stk-debugger) "
	     "(stkdb:set-options! " (stkdb-debugging-options-state) ")"
	     " 'stkdb-mode-initialized)\n"))
    (run-hooks 'stkdb-mode-hook)))

;; Additional process-communication commands in the scheme-mode keymap.

(define-key scheme-mode-map "\C-cd" 'stkdb-debug-file)
(define-key scheme-mode-map "\C-x " 'stkdb-set-breakpoint)

;; Debugging menu in Scheme mode

(if (not (lookup-key scheme-mode-map [menu-bar debug]))
    (define-key scheme-mode-map [menu-bar debug]
      (cons "Debugging" (make-sparse-keymap "Debugging"))))
(stkdb-define-menu-item scheme-mode-map 'debug-file "Debug File" 
			'stkdb-debug-file)
(stkdb-define-menu-item scheme-mode-map 'send-def "Debug Definition"
			'stkdb-send-definition)
(stkdb-define-menu-item scheme-mode-map 'send-def-go "Debug Definition & Go"
			'stkdb-send-definition-and-go)
(stkdb-define-menu-item scheme-mode-map 'set-breakpoint "Set Breakpoint" 
			'stkdb-set-breakpoint)
(stkdb-define-menu-item scheme-mode-map 'clear-breakpoint
			"Clear Breakpoint" 'stkdb-clear-breakpoint)
(stkdb-define-menu-item scheme-mode-map 'clear-all-breakpoints
			"Clear All Breakpoints" 'stkdb-clear-all-breakpoints)
(stkdb-define-menu-item scheme-mode-map 'conditionalize
			"Condition Breakpoint" 'stkdb-condition-breakpoint)

(stkdb-define-menu-item scheme-mode-map 'separator-eval "--" nil)

(stkdb-define-menu-item scheme-mode-map 'continue "Continue" 'stkdb-cont)
(stkdb-define-menu-item scheme-mode-map 'finish "Finish Function" 
			'stkdb-finish)
(stkdb-define-menu-item scheme-mode-map 'next "Step Over" 'stkdb-next)
(stkdb-define-menu-item scheme-mode-map 'step "Step Into" 'stkdb-step)
(stkdb-define-menu-item scheme-mode-map 'down "View Callee" 'stkdb-down)
(stkdb-define-menu-item scheme-mode-map 'up "View Caller" 'stkdb-up)
(stkdb-define-menu-item scheme-mode-map 'locals "See Local Variables" 
			'stkdb-locals)
(stkdb-define-menu-item scheme-mode-map 'bt "Backtrace" 'stkdb-backtrace)

(stkdb-define-menu-item scheme-mode-map 'separator-eval "--" nil)

(stkdb-define-menu-item scheme-mode-map 'settings "Settings..." 
			stkdb-settings-menu-map)
(stkdb-define-menu-item scheme-mode-map 'help "Help" 'stkdb-help)



(defun stkdb-filter (proc string)
  "Insert output from inferior Scheme process into its buffer, after
 removing source-file-position annotations and (if possible) displaying
 the source they indicate.  Arguments are the Scheme process PROC and the
 output STRING."
  (with-current-buffer (process-buffer proc)
    (let (stop-posn)
      (stkdb-filter-scan-input (concat stkdb-filter-accumulator string)))))

(defun stkdb-filter-scan-input (string)
  "Process output STRING from inferior Scheme process proc, assuming that
 the current buffer is proc's buffer.  Insert all of STRING into the 
 inferior buffer, except for source-position markers, which are interpreted
 and stored in variable stop-posn.  If STRING ends with an incomplete 
 source-position marker, store it in stkdb-filter-accumulator."
  (setq stkdb-filter-accumulator nil)
  (let ((start (string-match "\032" string)))
    (cond ((null start)
	   (stkdb-filter-insert proc string stop-posn))
	  ((> start 0)
	   (stkdb-filter-insert proc (substring string 0 start) stop-posn)
	   (stkdb-filter-scan-input (substring string start)))
	  ((string-match stkdb-marker-regexp string)
	   (let* ((filename (match-string 2 string))
		  (type (match-string 1 string))
		  (line1 (string-to-int (match-string 3 string)))
		  (col1  (string-to-int (match-string 4 string)))
		  (line2 (string-to-int (match-string 5 string)))
		  (col2 (string-to-int (match-string 6 string)))
		  (rest (match-end 0))
		  buffer)
	     (setq buffer (and (not (string= type "R"))
			       (file-exists-p filename)
			       (find-file-noselect filename)))
	     (setq stop-posn (list type buffer line1 col1 line2 col2))
	     (stkdb-filter-scan-input (substring string rest))))
	  ((string-match stkdb-stack-trace-regexp string)
	   (let* ((filename (match-string 1 string))
		  (line1 (string-to-int (match-string 2 string)))
		  (func-name (match-string 3 string))
		  (raw-stack-frame-num (match-string 4 string))
		  (stack-frame-num (string-to-int raw-stack-frame-num))
		  (current-stack-frame (string-to-int (match-string 5 string)))
		  (rest (match-end 0)))
	     (if (string= raw-stack-frame-num "")
		 (stkdb-update-current-stack-frame current-stack-frame)
	       (stkdb-insert-trace-line filename line1 func-name 
					stack-frame-num current-stack-frame))
	     (stkdb-filter-scan-input (substring string rest))))
	  ((string-match stkdb-var-list-regexp string)
	   (let ((stack-frame-num (string-to-int (match-string 1 string)))
		 (var-name (match-string 2 string))
		 (var-value (match-string 3 string))
		 (rest (match-end 0)))
	     (stkdb-update-local-var-display 
	      stack-frame-num var-name var-value)
	     (stkdb-filter-scan-input (substring string rest))))
	  ((string-match "\n" string);; Apparently a bad marker
	   (stkdb-filter-insert proc string stop-posn))
	  (t (setq stkdb-filter-accumulator string)))))

(defun stkdb-filter-insert (proc string stop-posn)
  "Insert into the buffer for inferior Scheme process PROC the string STRING
 and display the source position STOP-POSN, if appropriate."
  (let ((output-after-point (< (point) (process-mark proc))))
    (comint-output-filter proc string)
    (stkdb-display-source-posn stop-posn 
			       (and (not output-after-point)
				    (get-buffer-window (current-buffer))))))

(defun stkdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)));; buffer killed
	 (stkdb-erase-previous-marking)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 (stkdb-erase-previous-marking)
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (with-current-buffer (process-buffer proc)
	   (unwind-protect
	       (progn
		 (and stkdb-mode (stkdb-mode 1))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc)))))))


(defun stkdb-display-source-posn (stop-posn force-visible)
  "Set source marking as indicated by STOP-POSN.  Nil means display
 nothing.  Otherwise, STOP-POSN has the format
     (type buffer start-line start-column end-line end-column).  
 See stkdb-type-to-face for the meaning of 'type'.
 If FORCE-VISIBLE, display the marked spot in a visible buffer."
  (if stop-posn
      (progn (stkdb-erase-previous-marking)
	     (stkdb-mark-line-or-region stop-posn force-visible))))

(defun stkdb-line-and-col-to-pos (line col)
  (save-excursion
    (goto-line line)
    (move-to-column (- col 1))
    (point)))

(defun stkdb-mark-line-or-region (source-posn force-visible)
  "Visibly mark the region indicated by SOURCE-POSN 
 (cf. stkdb-display-source-posn).  If FORCE-VISIBLE, display the marked 
 spot in a visible buffer."
  (if (and source-posn (cadr source-posn))
      (let* ((type (car source-posn))
	     (buffer (cadr source-posn))
	     (line1 (car (cddr source-posn)))
	     (col1 (cadr (cddr source-posn)))
	     (line2 (car (cddddr source-posn)))
	     (col2 (cadr (cddddr source-posn)))
	     pos)
	(with-current-buffer buffer
	  (save-restriction
	    (widen)
	    (if col1
		(progn
		  (setq pos (stkdb-line-and-col-to-pos line1 col1))
		  (stkdb-highlight pos (stkdb-line-and-col-to-pos line2 col2)
				   (stkdb-type-to-face type)))
	      (progn 
		(setq pos (stkdb-line-and-col-to-pos line1 1))
		(setq overlay-arrow-string "=>")
		(or overlay-arrow-position
		    (setq overlay-arrow-position (make-marker)))
		(set-marker overlay-arrow-position (point) (current-buffer)))))
	  (if (or (< pos (point-min)) (> pos (point-max))) (widen))
	  (goto-char pos))
	(if force-visible
	    (set-window-point (display-buffer buffer t) pos)))))

(defun stkdb-highlight (start end face)
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only nil)
	(after-change-functions nil))
    (add-to-list 'stkdb-highlighted-buffers (current-buffer))
    (set-marker stkdb-highlight-start start nil)
    (set-marker stkdb-highlight-end end nil)
    (facemenu-add-face face start end)
    (set-buffer-modified-p modified-p)))

(defun stkdb-erase-previous-marking ()
  (setq overlay-arrow-position nil)
  (if (marker-buffer stkdb-highlight-start)
      (save-excursion
	(set-buffer (marker-buffer stkdb-highlight-start))
	(let ((modified-p (buffer-modified-p))
	      (buffer-read-only nil))
	  (facemenu-remove-face-props 
	   (marker-position stkdb-highlight-start)
	   (marker-position stkdb-highlight-end))
	  (set-marker stkdb-highlight-start nil)
	  (set-marker stkdb-highlight-end nil)
	  (set-buffer-modified-p modified-p)))))

(defun stkdb-unhighlight-all () 
  (save-excursion
    (dolist (x stkdb-highlighted-buffers)
      (if (buffer-live-p x)
	  (let ((modified-p (buffer-modified-p x))
		(buffer-read-only nil))
	    (set-buffer x)
	    (facemenu-remove-face-props (point-min) (point-max))
	    (set-buffer-modified-p modified-p)))))
  (setq stkdb-highlighted-buffers nil))

(defun stkdb-type-to-face (type)
  (case (intern type)
    ((b) 'stkdb-bpt-face)
    ((r) 'stkdb-return-face)
    ((f) 'stkdb-show-stack-frame-face)
    (t 'stkdb-error-face)))

(defun stkdb-update-current-stack-frame (current-stack-frame)
  (let ((buffer (get-buffer "*Scheme Backtrace*")))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (while (search-forward-regexp "^\\*\\[" nil t)
	    (let ((buffer-read-only nil))
	      (beginning-of-line 1)
	      (delete-char 1)
	      (insert " ")))
	  (goto-char (point-min))
	  (if (search-forward-regexp 
	       (format "^[* ]\\[%d\\]" current-stack-frame) nil t)
	      (let ((buffer-read-only nil))
		(beginning-of-line 1)
		(delete-char 1)
		(insert "*")))))))

(defun stkdb-insert-trace-line (filename line1 func-name stack-frame-num
					 current-stack-frame)
  (let ((buffer (get-buffer-create "*Scheme Backtrace*")))
    (stkdb-make-buffer-visible buffer scheme-buffer)
    (if (not buffer)
	(let ((pop-up-frames t))
	  (setq buffer (get-buffer-create "*Scheme Backtrace*"))
	  (display-buffer buffer t)))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(if (> stack-frame-num 0)
	    (search-forward-regexp (format "^[* ]\\[%d\\]" stack-frame-num) 
				   nil 'limit))
	(beginning-of-line 1)
	(delete-region (point) (point-max))
	(insert (format "%s[%s] %s:%s (%s)\n"
			(if (= stack-frame-num current-stack-frame) "*" " ")
			stack-frame-num filename line1 func-name))))))

(defun stkdb-update-local-var-display (stack-frame-num var-name var-value)
  (let ((buffer (get-buffer "*Scheme Backtrace*")))
    (if (and buffer (not (string= var-name "")))
	(save-excursion 
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (search-forward-regexp (format "^[* ]\\[%d\\]" stack-frame-num) 
				 nil 'limit)
	  (beginning-of-line 2)
	  (let ((start (point))
		(buffer-read-only nil))
	    (search-forward-regexp "^.\\[" nil 'limit)
	    (beginning-of-line 1)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char (point-min))
	      (if (search-forward-regexp (concat "^  +" (regexp-quote var-name)
						 ": ")
					 nil t)
		  (progn
		    (zap-to-char 1 ?\n)
		    (insert var-value "\n"))
		(insert "    " var-name ": " var-value "\n"))))))))

(defun stkdb-make-buffer-visible (buffer default-buffer)
  "Make BUFFER visible in some window without hiding any currently visible
buffer, if possible.  If that is not possible and DEFAULT-BUFFER is visible,
split DEFAULT-BUFFER and display BUFFER in one half.  Otherwise, pop up a new
frame to show BUFFER."
  (let ((default-window (and default-buffer 
			     (get-buffer-window default-buffer 'visible)))
	(pop-up-frames t))
    (cond ((get-buffer-window buffer 'visible) nil)
	  ((get-buffer-window buffer t) 
	   (display-buffer buffer nil t))
	  (default-window
	    (set-window-buffer (split-window default-window nil nil) buffer))
	  (t (display-buffer buffer t t)))))
		
(defun stkdb-debug-file-option-string () 
  "Options to send to inferior when instrumenting a function."
  (concat " :keep-tail-recursion " 
	  (stkdb-to-t/f  stkdb-keep-tail-recursion-p)))

(defun stkdb-debug-file (file-name &optional no-switch start end)
  "Save current buffer, if needed, instrument it for debugging, and load 
it into the Scheme interpreter.  If the current buffer is not in Scheme
mode, prompts for a file name.  When called from program, takes arguments
FILE-NAME, optional argument NO-SWITCH, which if true means to keep
point in current (source) buffer, and optional arguments START and END which
bound the line numbers of code that is to be replaced by this definition."
  (interactive 
   (or (and (eq major-mode 'scheme-mode)
	    (buffer-file-name (current-buffer))
	    (list (buffer-file-name (current-buffer))))
       (comint-get-source "Debug Scheme file: "
			  scheme-prev-l/c-dir/file
			  scheme-source-modes t))) ; T because LOAD 
					; needs an exact name
  (comint-check-source file-name)	; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (force-stkdb-mode)
  (send-string (scheme-proc) (concat "(begin (stkdb:debug-file \""
				     file-name "\""
				     (if start
					 (format " :start-line %d :end-line %d"
						 start end)
				       "")
				     (stkdb-debug-file-option-string)
				     ") (stkdb) 'ok)\n"))
  (if no-switch
      nil
    (switch-to-buffer-other-window scheme-buffer)
    (switch-to-scheme t)))

;;;
;;; Write a Scheme definition into a file, instrument it with stkdb and
;;; load it into the interpreter. Use #line directives for informing
;;; stkdb where the definition originally came from.
;;; 

(defun stkdb-send-definition ()
  "Instrument a definition and load it into Scheme."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((beginning (point))
	  (line (1+ (count-lines 1 (line-beginning-position))))
	  (char (1+ (current-column)))
	  (file (buffer-file-name))
	  end)
      (end-of-defun)
      (save-excursion
	(search-backward ")" beginning)
	(setq end (1+ (count-lines 1 (line-beginning-position)))))
      (copy-region-as-kill beginning (point))
      (find-file stkdb-tmp-source-file)
      (erase-buffer)
      (insert "#line \"" file "\" "
	      (int-to-string line) " "
	      (int-to-string char) " #\n")
      (yank)
      (save-buffer 0)
      (kill-buffer (current-buffer))
      (stkdb-debug-file stkdb-tmp-source-file t line end))))

(defun stkdb-send-definition-and-go ()
  "Instrument a definition and load it into Scheme. Switches to the
Scheme buffer."
  (interactive)
  (stkdb-send-definition)
  (switch-to-scheme t))

(defun stkdb-set-breakpoint ()
  "Set a breakpoint in current line. This command is ment to be used
in buffers containing Scheme source code." 
  (interactive)
  (send-string (scheme-proc) 
	       (format "(stkdb:set-bp! \"%s\" %d)\n"
		       (buffer-file-name (current-buffer))
		       (save-restriction
			 (save-excursion
			   (widen)
			   (beginning-of-line)
			   (1+ (count-lines 1 (point))))))))

(defun stkdb-clear-breakpoint ()
  "Clear any breakpoints in current line. This command is meant to be used
in buffers containing Scheme source code." 
  (interactive)
  (send-string (scheme-proc) 
	       (format "(stkdb:clear-bp \"%s\" %d)\n"
		       (buffer-file-name (current-buffer))
		       (save-restriction
			 (save-excursion
			   (widen)
			   (beginning-of-line)
			   (1+ (count-lines 1 (point))))))))

(defun stkdb-clear-all-breakpoints ()
  "Clear all stkdb breakpoints on all Scheme source files."
  (interactive)
  (send-string (scheme-proc) "(stkdb:clear-all-bps!)\n"))

(defun stkdb-break (name)
  "Enable break on entry to a named procedure."
  (interactive "sBreak on entry to procedure: ")
  (send-string (scheme-proc) (concat "(stkdb:set-bp-func! '"
				     name
				     ")\n")))

(defun stkdb-condition-breakpoint (expr)
  "Set condition EXPR on breakpoint at cursor.  Prompts for breakpoint 
condition in the minibuffer."
  (interactive "sEnter condition expression: ")
  (let ((proc (scheme-proc)))
    (force-stkdb-mode)
    (send-string (scheme-proc) 
	       (format "(stkdb) condition %s:%d %s\n"
		       (buffer-file-name (current-buffer))
		       (save-restriction
			 (save-excursion
			   (widen)
			   (beginning-of-line)
			   (1+ (count-lines 1 (point)))))
		       expr))))

(defun stkdb-reset ()
  "Reset the stkdb runtime clearing all breakpoints and resetting the
runtime system into the initial state."
  (interactive)
  (stkdb-erase-previous-marking)
  (stkdb-unhighlight-all)
  (setq stkdb-auto-backtrace "#f"
	stkdb-show-all-returned-values-p nil
	stkdb-keep-tail-recursion-p nil)
  (send-string (scheme-proc) 
	       (concat "(stkdb:reset #t)"
		       "(stkdb:set-options! " (stkdb-debugging-options-state) 
		       ")\n")))

(defun stkdb-pop () 
  "Instruct the debugger to abandon execution of the current expression, 
returning to the state where the user asked that it be evaluated, or to
plain STk if there was no expression being evaluated."
  (interactive)
  (stkdb-erase-previous-marking)
  (stkdb-unhighlight-all)
  (send-string (scheme-proc) "(stkdb)quit\n"))

(defun stkdb-backtrace ()
  "Request a backtrace from current program point."
  (interactive)
  (send-string (scheme-proc) "where\n"))

(defun stkdb-debugging-options-state () 
  "String of arguments to stkdb:set-options!, based on current variable 
settings."
  (concat " :auto-backtrace " stkdb-auto-backtrace
	  " :show-returns " (stkdb-to-t/f stkdb-show-all-returned-values-p)
	  " :emacs #t"))

(defun stkdb-locals ()
  "Display values of local variables and parameters for current stack frame."
  (interactive)
  (send-string (scheme-proc) "info locals\n"))

(defun scheme-active-p ()
  (get-buffer-process scheme-buffer))

(defun force-stkdb-mode ()
  (if (not (scheme-active-p))
      (save-window-excursion 
	(run-scheme scheme-program-name)))
  (with-current-buffer scheme-buffer
    (or stkdb-mode (stkdb-mode 4))))

(defun stkdb-help ()
  (interactive)
  (info "/Applications/STk/lib/emacs/info/stkdb.info"))

(provide 'stkdb)
