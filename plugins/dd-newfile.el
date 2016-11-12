;;; DD-newfile --- Functions to setup a new file at work.

;;; Commentary:

;;; Code:
;; ---------------------------- Boilerplate ------------------------------------
(defun dd/insert-boilerplate()
"Insert DD's boilerplate. Reads from file to avoid copyright issues."
(interactive)
(setq current_pos (point) )
(insert-file-contents "/dd/dept/software/users/fredriks/boilerplate.txt")
;; Insert current year
(replace-regexp "::date::" (setq year (format-time-string "%Y" (current-time))))
(goto-char current_pos)
)
;; ---------------------------- Description ------------------------------------
(defun dd/insert-description ()
"Insert description of when the file was created who's the author
and the name of the file."
(interactive)
;; (setq user (getenv "USER"))
;; (setq full_name (shell-command-to-string 
;; 		 (concat "phone " user" |"
;; 			 " sed -En 's/.*Full Name: ([a-zA-Z ]+)/\1/p'") ) )
(insert (mapconcat 'identity 
		   ( list "/**"
			  ( concat "* \\file   " (buffer-name))
			  "*"
			  "* \\author Fredrik Salomonsson (fredriks@d2.com)"
			  "*"
			  ( concat "* \\date   " (format-time-string "%B %Y") )
			  "*/" ) 
		   "\n") )
)
;; ------------------------------ Header ---------------------------------------
(defun dd/insert-header()
"Insert boilerplate and description"
(interactive)
(dd/insert-boilerplate)
(goto-char (point-max))
(insert "\n\n")
(dd/insert-description)
)

;; ------------------------------ Namespace ------------------------------------
(setq insert-namespace-devdir "swdevl")
(defun dd/insert-namespace ( extra_namespaces )
"Insert namespace based on the location on the file"
(interactive "sAdd extra namespace (separated by space): ")
(setq path (pwd))
(setq end (- (length path) 1))
(setq start (string-match insert-namespace-devdir path))
;; Remove everything until swdevl and replace CoreLibs with DD if it exist
(setq path (substring path start end) )
;; Remove split it into a list and remove swdevl
(setq path_list ( cdr ( split-string  path "/") ) )
;; Handle exceptions
;; Remove CoreLibs, Common and Utility if they exist
(setq path_list (remove "CoreLibs" path_list))
(setq path_list (remove "Common" path_list))
(setq path_list (remove "Utility" path_list))
;; Captitalize houdini if it exist
(if (setq houdini (member "houdini" path_list))
    (setcar houdini (capitalize (car houdini)))
  nil )

;; Add extra namespaces
(setq extra_list (split-string extra_namespaces ) )
(setq path_list (append path_list extra_list))

;; Remove all entrys that starts with a lowercase
(setq path_list (let ((case-fold-search nil))
		  (remove-if (lambda (x) 
			       (if (setq check (string-match-p "[a-z]+" x) ) 
				   (= check 0) 
				 nil)
			       )
			     path_list )))
;; Add DD as the first namespace
(push "DD" path_list)
(let ((is_header (string-equal "h" (substring (file-name-extension (buffer-name)) 0 1))))
  (if is_header 
      (progn
	;; Create include gaurd name
	(setq include_guard (mapconcat 'upcase path_list "_"))
	(setq include_guard (concat include_guard 
				    "_" 
				    (upcase 
				     (underscore 
				      (camelcase (buffer-name))
				      )
				     )
				    ) 
	      )
	;; Insert all into buffer
	(insert "#ifndef " include_guard "\n" "#define " include_guard "\n\n")
	) nil)

  (dolist (x path_list) (insert "namespace " x " {\n} // namespace " x "\n")
	  (search-backward "{")
	  (forward-char 2))
  (if is_header 
      (progn
	(insert "\n")
	(next-line (length path_list))
	(insert "#endif")
	(forward-line (- (length path_list) 1)))
    (progn (insert "\n") (previous-line) )
    )
)
) ;; insert-namespace

(defun dd/setup-newfile (args)
"Add boilerplate, description and namespaces.
ARGS are passed on to insert-namespace"
(interactive "sAdd extra namespace (separated by space): ")
(dd/insert-header)
(insert "\n\n")
(dd/insert-namespace args)
)

(provide 'dd-newfile)
;;; dd-newfile.el ends here
