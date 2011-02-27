(provide 'other)

(defun mark-line ()
  "Mark a line as a region"
 (interactive)
 (move-beginning-of-line nil)
 (push-mark (point) t t)
 (end-of-line))

(global-set-key "\C-c\C-l" 'mark-line)
