;; =============================================================================
;; Init multiple-cursors 
;; =============================================================================
;; Swap for package install

;; Add the plugin to the search path
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path 
       '("multiple-cursors.el")))

;; Enable multiple cursor
(require 'multiple-cursors)

;;========================== Extra MC functions ================================
;; Based on mc/insert-numbers

(defun mc/insert-dec-numbers (arg)
  "Insert decreasing numbers for each cursor, starting at number
of cursors - 1 or ARG."
  (interactive "P")
  (setq mc--insert-numbers-number (or arg (1- (mc/num-cursors))))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--insert-number-and-decrease cursor)))

(defun mc--insert-number-and-decrease ()
  (interactive)
  (insert (number-to-string mc--insert-numbers-number))
  (setq mc--insert-numbers-number (1- mc--insert-numbers-number)))

(defun mc/insert-same-numbers-per-line (arg)
  "Insert increasing numbers for each cursor that are on a separate
line, cursors on the same line will insert the same number, starts at
0 or ARG"
(interactive "P")
(setq mc--current-line nil
      mc--insert-numbers-number (or arg 0) )
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-number-and-increase-for-diff-lines
				     cursor)))

(defun mc/insert-dec-same-numbers-per-line (arg)
  "Insert decreasing numbers for each cursor that are on a
separate line, cursors on the same line will insert the same
number, starts at number of cursors - 1 or ARG"
(interactive "P")
(setq mc--current-line nil
      mc--insert-numbers-number (or arg (1- (mc/num-cursors))) )
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-number-and-decrease-for-diff-lines
				     cursor)))

(defun mc--insert-number-and-increase-for-diff-lines ()
(interactive)
(mc--insert-number-and-change-for-diff-lines '1+))

(defun mc--insert-number-and-decrease-for-diff-lines ()
(interactive)
(mc--insert-number-and-change-for-diff-lines '1-))

(defun mc--insert-number-and-change-for-diff-lines (change)
  (interactive)
  (if (not mc--current-line) 
      ;; If first time init mc--current-line
      (progn (setq mc--current-line (line-number-at-pos))
	     (insert (number-to-string mc--insert-numbers-number)))
    ;; Else compare lines and set accordingly
    (progn  
      ;; If current-line and the line is it on are different change
      ;; and insert.
      (if (not (= mc--current-line (line-number-at-pos)))
	  (progn 
	    (setq mc--current-line (line-number-at-pos)
		  mc--insert-numbers-number 
		  (funcall change mc--insert-numbers-number))
	    (insert (number-to-string mc--insert-numbers-number)))
	;; Else insert number.
	(insert (number-to-string mc--insert-numbers-number)) ))))

(defun mc/insert-characters (char)
  "Insert increasing character for each cursor, it starts from
the user specified character"
(interactive  "cSpecify letter to start from")
(setq mc--insert-chars-char char)
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-char-and-increase
				     cursor)))

(defun mc/insert-dec-characters (char)
  "Insert decreasing character for each cursor, it starts from
the user specified character"
(interactive  "cSpecify letter to start from")
(setq mc--insert-chars-char char)
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-char-and-increase
				     cursor)))

(defun mc--insert-char-and-increase ()
  (interactive)
  (mc--insert-char-and-change '1+))

(defun mc--insert-char-and-decrease ()
  (interactive)
  (mc--insert-char-and-change '1-))

(defun mc--insert-char-and-change (change)
  (interactive)
  (insert mc--insert-chars-char)
  (setq mc--insert-chars-char (funcall change mc--insert-chars-char)))


(defun mc/insert-same-chars-per-line (char)
  "Insert increasing character for each cursor that are on a
separate line, cursors on the same line will insert the same
character, it starts from the user specified character."
(interactive  "cSpecify letter to start from")
(setq mc--current-line nil
      mc--insert-chars-char char )
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-char-and-increase-for-diff-lines
				     cursor)))

(defun mc/insert-dec-same-chars-per-line (char)
  "Insert decreasing character for each cursor that are on a
separate line, cursors on the same line will insert the same
character, it starts from the user specified character."
(interactive  "cSpecify letter to start from")
(setq mc--current-line nil
      mc--insert-chars-char char )
(mc/for-each-cursor-ordered
 (mc/execute-command-for-fake-cursor 'mc--insert-char-and-decrease-for-diff-lines
				     cursor)))

(defun mc--insert-char-and-increase-for-diff-lines ()
  (interactive)
  (mc--insert-char-and-change-for-diff-lines '1+))

(defun mc--insert-char-and-decrease-for-diff-lines ()
  (interactive)
  (mc--insert-char-and-change-for-diff-lines '1-))

(defun mc--insert-char-and-change-for-diff-lines ( change )
  (interactive)
  (if (not mc--current-line) 
      ;; If first time init mc--current-line
      (progn (setq mc--current-line (line-number-at-pos))
	     (insert mc--insert-chars-char))
    ;; Else compare lines and set accordingly.
    (progn  
      ;; If current-line and the line is it on is different increment
      ;; and insert.
      (if (not (= mc--current-line (line-number-at-pos)))
	  (progn (setq mc--current-line (line-number-at-pos)
		       mc--insert-chars-char 
		       (funcall change mc--insert-chars-char))
		 (insert mc--insert-chars-char))
	;; Else insert char.
	(insert mc--insert-chars-char)) )))

;; ============================= Key bindings ==================================
;; Multiple cursor key bindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
