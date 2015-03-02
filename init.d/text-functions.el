;; =============================================================================
;; Text Functions:
;; lisp functions that manipulate text
;; =============================================================================

;; ----------------------------- Camel score -----------------------------------
;; Function from http://www.emacswiki.org/emacs/CamelCase
;; Modified to be able to call it from emacs
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))
(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore (s)
  (cond ((string-match-p "\:"s)	(camelcase s))
	((string-match-p "-" s) (colonize s))
	((string-match-p "_" s)	(dasherize s))
	(t                      (underscore s)) ))
(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
	 (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	 (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	 (txt (buffer-substring beg end))
	 (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;; ----------------------------- Dupe line -------------------------------------
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs,
;; Author mk-fg
(defun duplicate-line ()
    "Clone line at cursor, leaving the latter intact."
    (interactive)
    (save-excursion
    	(let ((kill-read-only-ok t) deactivate-mark)
    		(toggle-read-only 1)
    		(kill-whole-line)
    		(toggle-read-only 0)
    		(yank))))

;; ----------------------------- Uniquify --------------------------------------
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
  (let ((end (copy-marker end)))
    (while
	(progn
	  (goto-char start)
	  (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
      (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

;; --------------------------- Move text ---------------------------------------
;; ============================= Key bindings ==================================

;; Key bindings for cycling between camelCase, underscore, dasherize and colonize
(global-set-key (kbd "C-;") 'camelscore-word-at-point ) 

;; Move line up and down. Install move-text
(global-set-key [C-S-up] 'move-text-up)
(global-set-key [C-S-down] 'move-text-down)
