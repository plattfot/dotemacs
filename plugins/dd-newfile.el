;;; DD-newfile --- Functions to setup a new file at work.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'subr-x)

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
(defun dd-insert-boilerplate (&optional boilerplate)
  "Insert DD's BOILERPLATE.  Read from file to avoid copyright issues."
  (interactive)
  (let ((current_pos (point) )
	end )
    (insert-file-contents (if boilerplate
                              boilerplate
                            "~/.emacs.d/boilerplates/dd.txt"))
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
(defun dd-insert-header (&optional boilerplate)
  "Insert BOILERPLATE and description."
  (interactive)
  (dd-insert-boilerplate boilerplate)
  (goto-char (point-max))
  (insert "\n\n")
  (dd-insert-description))

(defun dd-find-workspace (path)
  "Find the path to the last WORKSPACE file, in the PATH.
For absolute paths it will return the root of the path if not
found.  For relative it will return nil."

  (let ((found-path-p nil))
    (while (and (not (string-equal path "/")) (not found-path-p))
      (setq found-path-p (directory-files path nil "WORKSPACE"))
      (setq path (file-name-directory (directory-file-name path)))))
  path)

(defun dd-insert-include-guard (namespaces filename)
  "Insert a #ifdef-include guard based on NAMESPACES and FILENAME."
  (let ((include_guard (mapconcat 'upcase namespaces "_"))
        (return_pos))
    ;; Create include gaurd name
    (setq include_guard
          (concat include_guard "_" (upcase (dd-underscore (dd-camelcase filename)))))
    ;; Insert all into buffer
    (insert "#ifndef " include_guard "\n" "#define " include_guard "\n")
    (setq return_pos (point))
    (goto-char (point-max))
    ;; Jump to the end and close the ifdef
    (insert (concat "\n#endif // " include_guard "\n"))
    (goto-char return_pos)))

;; ------------------------------ Namespace ------------------------------------
(defun dd-insert-namespace-raw (&optional modify_namespaces workspace_root path)
  "Insert namespace based on the location of the buffer.

The MODIFY_NAMESPACES can be used to change, remove or add
namespaces.  If MODIFY_NAMESPACE is a string entries are
separated by whitespace.  All regex are case sensitive.
Options supported are:

- !REGEX remove matching namespaces from the result.
- REGEX=REP will replace matching namespaces with REP.
- NAMESPACE append to the namespaces.
- ^NAMESPACE prepend to the namespaces

WORKSPACE_ROOT is the path to the root of the project.  This path
will be removed from PATH if the PATH is absolute.

PATH to the where the buffer is located, default is to use the
\"default-directory\"."

  (interactive "sAdd/Remove/Modify namespaces: ")
  (let* ((path (if (not path) default-directory path))
         (path (if (and workspace_root (file-name-absolute-p path))
                   (progn
                     (if (not (file-name-absolute-p workspace_root))
                         (error "Workspace root '%s' is not an absolute path"
                                workspace_root))
                     (if (string-prefix-p workspace_root path)
                         (setq path (string-remove-prefix workspace_root path))
                       (error "Path '%s' is not in workspace root '%s'"
                              path workspace_root)))
                 path))
         (path_list (split-string (directory-file-name path) "/")))

    ;; If path is still absolute path_list will have an empty entry at
    ;; the beginning.
    (when (file-name-absolute-p path)
      (setq path_list (cdr path_list)))

    ;; Customize namespaces
    (when modify_namespaces
      (let ((modify_namespaces_list (if (stringp modify_namespaces)
                                        (split-string modify_namespaces)
                                      modify_namespaces))
            (value)
            (case-fold-search nil))
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
                    (cl-remove-if (lambda (x) (string-match-p remove x)) path_list))))
           ;; Prepend namespace
           ((string-match "^\\^\\(.*\\)" value)
            (push (match-string 1 value) path_list))
           ;; Append namespace
           (t (setq path_list (nconc path_list (list value))))))))

    (let ((is_header (string-match-p "^h" (file-name-extension (buffer-name)))))
      (when is_header
        (dd-insert-include-guard path_list (buffer-name))
        (insert "\n"))
      (dolist (x path_list) (insert "namespace " x " {\n} // namespace " x "\n")
              (search-backward "{")
              (forward-char 2))
      (insert "\n")
      (forward-line -1)))) ;; insert-namespace

(defun dd-insert-namespace (&optional modify_namespaces)
  "Insert namespace based on the location of the file.

Prefixed with namespace DD and remove all directories starting
with lowercase or a dot (.).  MODIFY_NAMESPACES can be use to
tweak the namespaces, see dd-insert-namespace-raw for the syntax.

Limitations: The DD namespace can only be at the beginning (and
or end if you append it yourself).  As it will first remove all
DD namespaces from the list then prepend DD.  As that is the only
way right now to avoid duplicates at the beginning if the source
tree has a directory called DD."
  (interactive "sAdd/Remove/Modify namespaces: ")

  (let* ((blacklist '("!^CoreLibs$" "!^Utility$" "!^Common$" "!^[.a-z]+"))
         (prefix_dd '("!^DD$" "^DD"))
         (replace '("^houdini$=HOUDINI"))
         (user_args (if modify_namespaces
                        (if (stringp modify_namespaces)
                            (split-string modify_namespaces))))
         (modify_namespaces (append prefix_dd replace blacklist user_args))
         (workspace_root (dd-find-workspace default-directory)))
    (dd-insert-namespace-raw modify_namespaces workspace_root)))

(defun dd-setup-newfile (args &optional boilerplate)
"Add boilerplate, description and namespaces.
ARGS are passed on to insert-namespace.  Extra namespaces in ARGS
are separated by whitespace.  If any entry contains 'REGEX=REP'
it will replace all namespaces matching REGEX with REP.

If BOILERPLATE is empty it will look for the boilerplate in
'~/.emacs.d/boilerplates/dd.txt'."
(interactive "sAdd extra namespace: ")
(dd-insert-header boilerplate)
(insert "\n\n")
(dd-insert-namespace args))

(defun dd-include-guard-from-namespaces ()
  "Insert #ifdef include guard at point.
Based on the main namespaces in the file."
  (interactive)
  (let ((namespaces '())
        (position (point)))
    (goto-char (point-min))
    (while (re-search-forward "^namespace[ ]+\\([[:alpha:]]+\\)[ ]+{" nil t)
      (setq namespaces (cons (match-string-no-properties 1) namespaces)))
    (goto-char position)
    (dd-insert-include-guard (reverse namespaces) (buffer-name)))
)

(provide 'dd-newfile)
;;; dd-newfile.el ends here
