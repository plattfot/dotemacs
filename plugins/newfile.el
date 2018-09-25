;;; newfile --- Functions to setup a new source file.

;;; Commentary:
;;; At the moment only supports c++ files.
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defun nf-setup-newfile (modify_namespaces
                         author
                         email
                         &optional
                         boilerplate
                         workspace_root
                         path )
  "Add header, namespaces and include guard.

MODIFY_NAMESPACES are passed on to 'nf-insert-namespace'.  See
'nf-insert-namespace' for the description on syntax it accepts.

The header consist of the AUTHOR, EMAIL and optionally the
BOILERPLATE if defined.

WORKSPACE_ROOT and PATH are optional arguments that will be
passed to 'nf-insert-namespace'.  Again see description on that
function.

'~/.emacs.d/boilerplates/dd.txt'."
  (nf-insert-header author email boilerplate)
  (insert "\n\n")
  (let ((include_guard_position (point))
        (namespaces (nf-insert-namespace modify_namespaces workspace_root path))
        (is_header (string-match-p "^h" (file-name-extension (buffer-file-name)))))
    (when is_header
      (goto-char include_guard_position)
      (nf-insert-include-guard namespaces (buffer-name))
      (insert "\n")
      (forward-line (length namespaces)))))

(defun nf-include-guard-from-namespaces ()
  "Insert #ifdef include guard at point.
Based on the main namespaces in the file."
  (interactive)
  (let ((namespaces '())
        (position (point)))
    (goto-char (point-min))
    (while (re-search-forward "^namespace[ ]+\\([[:alpha:]]+\\)[ ]+{" nil t)
      (setq namespaces (cons (match-string-no-properties 1) namespaces)))
    (goto-char position)
    (nf-insert-include-guard (reverse namespaces) (buffer-name))))

(defun nf-insert-header (author email &optional boilerplate)
  "Insert header to the buffer.
With the AUTHOR name and EMAIL.
If BOILERPLATE is not nil the header will consist of the
boilerplate and the description.  Else it will just contain the
description."
  (when boilerplate
    (nf-insert-boilerplate boilerplate)
    (goto-char (point-max))
    (insert "\n\n"))
  (nf-insert-description author email))

(defun nf-insert-boilerplate (boilerplate)
  "Insert boilerplate from file BOILERPLATE.
Read from file to avoid copyright issues.
Places with ::date:: will be replaced with current year."
  (interactive "fPath to boilerplate: ")
  (let ((current_pos (point) )
	end )
    (insert-file-contents boilerplate)
    (setq current_pos (point))
    (setq end (point-max))
    ;; Insert current year
    (while (re-search-forward "::date::" end t )
      (replace-match (format-time-string "%Y" (current-time))))
    (goto-char current_pos)))

(defun nf-insert-description (author email)
  "Insert description of the file.
Which is name of the file, the AUTHOR and EMAIL to the
author.  Last the date it was created (B Y),"
  (interactive (list (read-string "Author: ")
                     (read-string "email: ")))
  (insert
   (mapconcat 'identity
	      (list "/**"
		    ( concat " * \\file   " (file-name-nondirectory
                                             (buffer-file-name)))
		    " *"
		    ( concat " * \\author " author  " (" email ")")
		    " *"
		    ( concat " * \\date   " (format-time-string "%B %Y") )
		    " */" )
	      "\n")))

(defun nf-insert-namespace (&optional modify_namespaces workspace_root path)
  "Insert namespace based on the location of the buffer.

The MODIFY_NAMESPACES can be used to change, remove or add
namespaces.  If MODIFY_NAMESPACE is a string entries are
separated by whitespace.  All regexs are case sensitive and the
options are order dependent.

Options supported are:

- !REGEX remove matching namespaces from the result.
- REGEX=REP will replace matching namespaces with REP.
- NAMESPACE append to the namespaces.
- ^NAMESPACE prepend to the namespaces

WORKSPACE_ROOT is the path to the root of the project.  This path
will be removed from PATH if the PATH is absolute.

PATH to the where the buffer is located, default is to use the
\"default-directory\".

Return the namespaces it inserted into the buffer."

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

    (setq path_list (cl-remove-if (lambda (x) (string-equal x "")) path_list))

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

    (dolist (x path_list)
      (insert "namespace " x " {\n} // namespace " x "\n")
      (search-backward "{")
      (forward-char 2))
    (insert "\n")
    (forward-line -1)
    path_list)) ;; insert-namespace

(defun nf-insert-include-guard (namespaces filename)
  "Insert a #ifdef-include guard based on NAMESPACES and FILENAME."
  (let ((include_guard (mapconcat 'upcase namespaces "_"))
        (return_pos))
    ;; Create include gaurd name
    (setq include_guard
          (concat include_guard "_" (upcase (nf-underscore (nf-camelcase filename)))))
    ;; Insert all into buffer
    (insert "#ifndef " include_guard "\n" "#define " include_guard "\n")
    (setq return_pos (point))
    (goto-char (point-max))
    ;; Jump to the end and close the ifdef
    (insert (concat "\n#endif // " include_guard "\n"))
    (goto-char return_pos)))

;; Function from http://www.emacswiki.org/emacs/CamelCase
;; Modified to be able to call it from emacs
(defun nf-split-name (s)
"Split string S."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun nf-camelcase (s)
"Transfrom string S to camelcase."
(mapconcat 'capitalize (nf-split-name s) ""))
(defun nf-underscore (s)
"Transfrom string S to underscore."
(mapconcat 'downcase (nf-split-name s) "_"))

(provide 'newfile)
;;; newfile.el ends here
