;;; DD-newfile --- Functions to setup a new file at work.

;;; Commentary:

;;; Code:

;; Function from http://www.emacswiki.org/emacs/CamelCase
;; Modified to be able to call it from emacs
(defun dd-split-name (s)
"Split string S."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun dd-camelcase (s)
"Transfrom string S to camelcase."
(mapconcat 'capitalize (dd-split-name s) ""))
(defun dd-underscore (s)
"Transfrom string S to underscore."
(mapconcat 'downcase (dd-split-name s) "_"))

;; ---------------------------- Boilerplate ------------------------------------
(defun dd-insert-boilerplate()
  "Insert DD's boilerplate. Reads from file to avoid copyright issues."
  (interactive)
  (let ((current_pos (point) )
	end )
    (insert-file-contents "/dd-dept/software/users/fredriks/boilerplate.txt")
    (setq end (point-max))
    ;; Insert current year
    (while (re-search-forward "::date::" end t )
      (replace-match (format-time-string "%Y" (current-time))))
    (goto-char current_pos)))
;; ---------------------------- Description ------------------------------------

(defun dd-insert-description ()
  "Insert time when it was created, the author and name of the file."
  (interactive)
  (insert
   (mapconcat 'identity
	      (list "/**"
		    ( concat "* \\file   " (buffer-name))
		    "*"
		    "* \\author Fredrik Salomonsson (fredriks@d2.com)"
		    "*"
		    ( concat "* \\date   " (format-time-string "%B %Y") )
		    "*/" )
	      "\n")))
;; ------------------------------ Header ---------------------------------------
(defun dd-insert-header()
  "Insert boilerplate and description"
  (interactive)
  (dd-insert-boilerplate)
  (goto-char (point-max))
  (insert "\n\n")
  (dd-insert-description))

;; ------------------------------ Namespace ------------------------------------
(defun dd-insert-namespace ( extra_namespaces )
  "Insert namespace based on the location on the file.
Insert extra namespaces using the variable EXTRA_NAMESPACES."
  (interactive "sAdd extra namespace (separated by space): ")
  (let* ((insert-namespace-devdir "swdevl")
	 (path (pwd))
	 (end (- (length path) 1))
	 (start (string-match insert-namespace-devdir path))
	 ;; Remove everything until swdevl and replace CoreLibs with DD if it exist
	 (path (substring path start end))
	;; Remove split it into a list and remove swdevl
	 (path_list)
	 (houdini)
	 (extra_list))

    (setq path_list ( cdr ( split-string  path "/") ) )
    ;; Handle exceptions
    ;; Remove CoreLibs, Common and Utility if they exist
    (setq path_list (remove "CoreLibs" (remove "Common" (remove "Utility" path_list))))
    
    ;; Captitalize houdini if it exist
    (when (setq houdini (member "houdini" path_list))
      (setcar houdini (capitalize (car houdini))) )

    ;; Add extra namespaces
    (setq extra_list (split-string extra_namespaces ) )
    (setq path_list (append path_list extra_list))

    ;; Remove all entrys that starts with a lowercase
    (setq path_list
	  (let ((case-fold-search nil)
		(check))
	    (cl-remove-if (lambda (x)
			 (when (setq check (string-match-p "[a-z]+" x))
			   (= check 0)))
		       path_list )))
  ;; Add DD as the first namespace
  (push "DD" path_list)
  (let ((is_header
	 (string-equal "h" (substring (file-name-extension (buffer-name)) 0 1))))
    (when is_header
	(let ((include_guard (mapconcat 'upcase path_list "_")))
	  ;; Create include gaurd name
	  (setq include_guard (concat include_guard
				      "_"
				      (upcase
				       (dd-underscore
					(dd-camelcase (buffer-name))))))
	  ;; Insert all into buffer
	  (insert "#ifndef " include_guard "\n" "#define " include_guard "\n\n")
	  ))

    (dolist (x path_list) (insert "namespace " x " {\n} // namespace " x "\n")
	    (search-backward "{")
	    (forward-char 2))
    (if is_header
	(progn
	  (insert "\n")
	  (forward-line (length path_list))
	  (insert "#endif")
	  (forward-line (- (length path_list) 1)))
      (progn (insert "\n") (forward-line -1) )
      )))) ;; insert-namespace

(defun dd-setup-newfile (args)
"Add boilerplate, description and namespaces.
ARGS are passed on to insert-namespace"
(interactive "sAdd extra namespace (separated by space): ")
(dd-insert-header)
(insert "\n\n")
(dd-insert-namespace args)
)

(provide 'dd-newfile)
;;; dd-newfile.el ends here
