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

MODIFY_NAMESPACES are passed on to `nf-insert-namespace'.  See
'nf-insert-namespace' for the description on syntax it accepts.

The header consist of the AUTHOR, EMAIL and optionally the
BOILERPLATE if defined.

WORKSPACE_ROOT and PATH are optional arguments that will be
passed to `nf-insert-namespace'.  Again see description on that
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
    (nf-insert-include-guard (mapcar 'list (reverse namespaces))
                             (file-name-nondirectory (buffer-file-name)))))

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
namespaces.  If MODIFY_NAMESPACE is a string, entries are
separated by whitespace.

See `nf-modify-namespaces' for the syntax for modifying the
namespaces.

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
    ;; namspaces is in the form of an assoc list to handle keywords
    (let ((namespaces (mapcar 'list path_list)))
      ;; Customize namespaces
      (when modify_namespaces
        (let ((modify_namespaces_list (if (stringp modify_namespaces)
                                          (split-string modify_namespaces)
                                        modify_namespaces)))
          (setq namespaces (nf-modify-namespaces modify_namespaces_list namespaces))))

      (let (x)
        (dolist (x namespaces)
          (let ((namespace (car x))
                (keyword (cdr x)))
            (if keyword
                (insert (format "%s namespace %s {\n} // %s namespace %s\n"
                                keyword namespace keyword namespace))
              (insert (format "namespace %s {\n} // namespace %s\n"
                              namespace namespace))))
          (search-backward "{")
          (forward-char 2)))
      (insert "\n")
      (forward-line -1)
      namespaces))) ;; insert-namespace

(defun nf-modify-namespaces (modify_list namespaces)
  "Using the options in MODIFY_LIST to modify the alist NAMESPACES.
MODIFY_LIST should contain a list of strings with the following
syntax:

- NAMESPACE Append NAMESPACE to NAMESPACES.
- ^NAMESPACE Prepend to NAMESPACES.
- REGEX>NAMESPACE Add NAMESPACE after first matching REGEX.
- REGEX<NAMESPACE Add NAMESPACE before first matching REGEX.
- REGEX>>NAMESPACE Add NAMESPACE after last matching REGEX.
- REGEX<<NAMESPACE Add NAMESPACE before last matching REGEX.
- REGEX->KEYWORD Add KEYWORD to the cdr of matching namespaces.
- !REGEX Remove matching namespaces from NAMESPACES.
- REGEX=REP Replace matching namespaces with REP.

All regexs are case sensitive and the options are order
dependent."

  (let ((case-fold-search nil)
        value)
    (dolist (value modify_list)
      (cond
       ;; Add keyword to cdr, replacing the old cdr value.
       ((string-match "\\(.*?\\)->\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (keyword (match-string 2 value)))
          (setq namespaces
                (mapcar (lambda (x)
                          (if (string-match regex (car x))
                              (cons (car x) keyword)
                            x))
                        namespaces))))
       ;; Modify namespaces
       ((string-match "\\(.*?\\)=\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (replace (match-string 2 value)))
          (setq namespaces
                (mapcar
                 (lambda (x)
                   (cons (replace-regexp-in-string regex replace (car x)) (cdr x)))
                 namespaces))))
       ;; Remove namespaces
       ((string-match "^!\\(.*\\)" value)
        (let ((remove (match-string 1 value)))
          (setq namespaces
                (cl-remove-if (lambda (x) (string-match-p remove (car x))) namespaces))))
       ;; Prepend namespace
       ((string-match "^\\^\\(.*\\)" value)
        (push (list (match-string 1 value)) namespaces))

       ;; Insert after last namespace matching regex
       ((string-match "\\(.*?\\)>>\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (namespace (match-string 2 value)))
          (let ((idx (nf-last-index-of regex namespaces)))
            (when idx
              (setq namespaces (nf-insert-after (list namespace) idx namespaces))))))

       ;; Insert after first namespace matching regex
       ((string-match "\\(.*?\\)>\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (namespace (match-string 2 value)))
          (let ((idx (nf-first-index-of regex namespaces)))
            (when idx
              (setq namespaces (nf-insert-after (list namespace) idx namespaces))))))

       ;; Insert before first namespace matching regex
       ((string-match "\\(.*?\\)<<\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (namespace (match-string 2 value)))
          (let ((idx (nf-first-index-of regex namespaces)))
            (when idx
              (setq namespaces (nf-insert-before (list namespace) idx namespaces))))))

       ;; Insert before first namespace matching regex
       ((string-match "\\(.*?\\)<\\(.*\\)" value)
        (let ((regex (match-string 1 value))
              (namespace (match-string 2 value)))
          (let ((idx (nf-first-index-of regex namespaces)))
            (when idx
              (setq namespaces (nf-insert-before (list namespace) idx namespaces))))))

       ;; Append namespace
       (t (setq namespaces `(,@namespaces ,(list value))))))
    namespaces))

(defun nf-insert-after (element idx list)
  "Insert ELEMENT after IDX in a copy of LIST."
  (nf-insert-before element (+ 1 idx) list))

(defun nf-insert-before (element idx list)
  "Insert ELEMENT before IDX in a copy of LIST."
  (when (not (integerp idx))
    (error "IDX must be an integer!"))
  (when (or (> idx (length list)) (< idx 0))
    (error "IDX is out of bounds!"))

  (let ((new_list (copy-tree list)))
    (if (equal idx 0 )
        (push element new_list)
      (setcdr (nthcdr (- idx 1) new_list) (cons element (nthcdr idx new_list))))
    new_list))

(defun nf-first-index-of (regex list)
"Find the first index matching the REGEX in LIST."

(let ((count 0)
      value)
  (catch 'index
    (dolist (x list)
      (setq value (if (listp x) (car x) x))
      (when (string-match regex value)
        (throw 'index count))
      (setq count (+ 1 count))))))

(defun nf-last-index-of (regex list)
"Find the last index matching the REGEX in LIST."

(let ((count 0)
      idx
      value)
  (dolist (x list)
    (setq value (if (listp x) (car x) x))
    (when (string-match regex value)
      (setq idx count))
    (setq count (+ 1 count)))
  idx))

(defun nf-insert-include-guard (namespaces filename)
  "Insert a #ifdef-include guard based on NAMESPACES and FILENAME.
Will ignore namespaces that have a keyword assign to them."
  (let* ((include_guard (nf-generate-include-guard namespaces filename))
         return_pos)
    ;; Insert all into buffer
    (insert "#ifndef " include_guard "\n" "#define " include_guard "\n")
    (setq return_pos (point))
    (goto-char (point-max))
    ;; Jump to the end and close the ifdef
    (insert (concat "\n#endif // " include_guard "\n"))
    (goto-char return_pos)))

(defun nf-generate-include-guard (namespaces filename)
  "Create name for #ifdef-include guard based on NAMESPACES and FILENAME.
Will ignore namespaces that have a keyword assign to them."
  (let* ((only_namespaces (mapcar 'car (cl-remove-if (lambda (x) (cdr x)) namespaces)))
         (include_guard (mapconcat 'upcase only_namespaces "_")))
    (concat include_guard "_" (upcase (nf-underscore (nf-camelcase filename))))))

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
