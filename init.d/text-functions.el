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

(defun camelscore (str)
  "Transform STR between different type. ForExampleThisText -> 
for_example_this_text -> for-example-this-text -> For::Example::This::Text -> ForExampleThisText. 
Known bug: It doesn't handle ThisIsASample ->  this_is_asample -> this-is-asample -> This::Is::Asample -> ThisIsAsample"
  (interactive)
  (cond ((string-match-p "\:"str) (camelcase str))
	((string-match-p "-" str) (colonize str))
	((string-match-p "_" str) (dasherize str))
	(t                      (underscore str)) ))
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

;; --------------------------- Split at Delim ----------------------------------

(defun split-at (&optional delim)
"Split region/line at DELIM, if there are multiple matches it
will split each one. DELIM will default to \",\" if no delim is
given."
(interactive "sSpecify delimiter: ")
(when (or (string= delim "") (not delim)) (setq delim ","))
(let ((start (if (use-region-p) (region-beginning) (point-at-bol)))
      (end (if (use-region-p) (region-end) (point-at-eol)))
      (regex delim))
  (goto-char start)
 
  (while (search-forward-regexp regex end t)
    (insert "\n")
    (setq end (1+ end))
    )
  (indent-region start end)
  (goto-char start)
  )
)

(defun split-at-comma ()
"wrapper for split-at for use with key command"
(interactive)
(split-at ",")
)
;; ============================= Key bindings ==================================

;; Key bindings for cycling between camelCase, underscore, dasherize and colonize
;; (global-set-key (kbd "C-;") 'camelscore-word-at-point ) 

;; Move line up and down. Install move-text
(global-set-key [C-S-up] 'move-text-up)
(global-set-key [C-S-down] 'move-text-down)
(global-set-key (kbd "C-,") 'split-at-comma)
