;;; DD-newfile --- Functions to setup a new file at work.

;;; Commentary:

;;; Code:
(require 'cl-lib)

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
    (insert-file-contents "/dd/dept/software/users/fredriks/boilerplate.txt")
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

(defun dd-find-workspace (path)
  "Find the path to the last WORKSPACE file, in the PATH.
For absolute paths it will return the root of the path if not
found.  For relative it will return nil."

  (let ((found-path-p nil))
    (while (and (not (string-equal path "/")) (not found-path-p))
      (setq found-path-p (member "WORKSPACE" (directory-files path)))
      (setq path (file-name-directory (directory-file-name path)))))
  path)

;; ------------------------------ Namespace ------------------------------------
(defun dd-insert-namespace (&optional modify_namespaces )
  "Insert namespace based on the location on the file.
The MODIFY_NAMESPACES can be used to change, remove or add
namespaces.  If MODIFY_NAMESPACE is a string entries are
separated by whitespace.  The syntax '!REGEX' will remove
matching namespaces from the result.  REGEX=REP will replace
matching namespaces with REP.  Rest will be added as extra
namespaces.

For example:

Say cwd is \"/path/to/workspace/Math/Geometry/test/\".

Called with with \"(dd-insert-namespace)\" the final namespace of
the file will be DD::Math::Geometry.

With \"(dd-insert-namespace \"!DD Geometry=Geometry3D Test\")\".
It will remove the DD namespace, change Geometry to Geometry3D
and add Test.  So the final namespace of the file will be
Math::Geometry3D::Test."

  (interactive "sAdd extra namespace: ")
  (let* ((path (cadr (split-string (pwd))))
         (workspace_root (dd-find-workspace path))
	 (path (directory-file-name (string-remove-prefix workspace_root path)))
	 (path_list (split-string path "/")))

    ;; Remove entries in the blacklist if they exist
    (let ((blacklist '("CoreLibs" "Utility" "Common")))
      (setq path_list (cl-remove-if (lambda (x) (member x blacklist)) path_list)))

    ;; Captitalize houdini if it exist
    (let ((houdini (member "houdini" path_list)))
      (when houdini (setcar houdini (capitalize (car houdini)))))

    ;; Remove all entrys that starts with a lowercase and or .
    (setq path_list
	  (let ((case-fold-search nil))
	    (cl-remove-if (lambda (x)
                            (string-match-p "^[.a-z]+" x))
                          path_list)))

    ;; Add DD as the first namespace
    (if (not (string-equal "DD" (car path_list)))
        (push "DD" path_list))

    ;; Customize namespaces
    (when modify_namespaces
      (let ((modify_namespaces_list (if (stringp modify_namespaces)
                                        (split-string modify_namespaces)
                                      modify_namespaces))
            (value))
        ;; Modify the path_list
        (dolist (value modify_namespaces_list)
          (cond
           ;; Modify namespaces
           ((string-match "\\(.*?\\)=\\(.*\\)" value)
            (let ((regex (match-string 1 value))
                  (replace (match-string 2 value)))
              (setq path_list
                    (mapcar (lambda (x)
                              (replace-regexp-in-string regex replace x))
                            path_list))))
           ;; Remove namespaces
           ((string-match "^!\\(.*\\)" value)
            (let ((remove (match-string 1 value)))
              (setq path_list
                    (cl-remove-if (lambda (x) (string-equal x remove)) path_list))))
           ;; Prepend namespace
           ((string-match "^\\^\\(.*\\)" value)
            (push (match-string 1 value) path_list))
           ;; Append namespace
           (t (setq path_list (nconc path_list (list value))))))))

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
	  (insert "#ifndef " include_guard "\n" "#define " include_guard "\n\n")))

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
ARGS are passed on to insert-namespace.  Extra namespaces in ARGS
are separated by whitespace.  If any entry contains 'REGEX=REP'
it will replace all namespaces matching REGEX with REP."
(interactive "sAdd extra namespace: ")
(dd-insert-header)
(insert "\n\n")
(dd-insert-namespace args)
)

(provide 'dd-newfile)
;;; dd-newfile.el ends here
