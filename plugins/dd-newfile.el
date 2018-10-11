;;; DD-newfile --- Functions to setup a new file at work.

;;; Commentary:

;;; Code:
(require 'newfile)

(defgroup dd-newfile nil
  "Functions to setup a new file at DD"
  :group 'tools)

(defcustom dd-author-name user-full-name
  "Author's full name to use when creating the header."
  :type 'string
  :group 'dd-newfile)

(defcustom dd-author-mail user-mail-address
  "Author's mail address to use when creating the header."
  :type 'string
  :group 'dd-newfile)

(defcustom dd-boilerplate "~/.emacs.d/boilerplates/dd.txt"
  "File to use or the DD's boilerplate."
  :type 'string
  :group 'dd-newfile)

(defcustom dd-setup-newfile-default nil
  "Default options for `dd-setup-newfile'.
By default this is empty."
  :type 'string
  :group 'dd-newfile)

(defvar dd-newfile-history nil
  "History for dd-setup-newfile.")

(defun dd-setup-newfile (modify_namespaces)
  "Add boilerplate, description, namespaces and include guard.
MODIFY_NAMESPACES are passed on to insert-namespace.  Extra
namespaces in MODIFY_NAMESPACES are separated by whitespace.  See
'nf-insert-namespaces' for the syntax on MODIFY_NAMESPACES.

It will use the 'dd-boilerplate' as the boilerplate.
It will use the 'dd-author-name' as the author name for the header.
It will use the 'dd-author-mail' as the author's mail address for the header."

  (interactive
   (let ((prompt "Add/Remove/Modify namespaces"))
     (list (read-regexp
            (if (and dd-setup-newfile-default
                     (not (string-empty-p dd-setup-newfile-default)))
                (format "%s (default %s): " prompt dd-setup-newfile-default)
              (format "%s: " prompt))
            dd-setup-newfile-default
            'dd-newfile-history))))
  
  (let* ((blacklist '("!^CoreLibs$" "!^Utility$" "!^Common$" "!^[.a-z]+"))
         (prefix_dd '("!^DD$" "^DD"))
         (replace '("^houdini$=Houdini"))
         (user_args (if modify_namespaces
                        (if (stringp modify_namespaces)
                            (split-string modify_namespaces))))
         (modify_namespaces (append prefix_dd replace blacklist user_args))
         (workspace_root (dd-find-workspace default-directory)))
    (nf-setup-newfile modify_namespaces
                      dd-author-name
                      dd-author-mail
                      dd-boilerplate
                      workspace_root)))
  
(defun dd-find-workspace (path)
  "Find the path to the last WORKSPACE file, in the PATH.
For absolute paths it will return the root of the path if not
found.  For relative it will return nil."

  (let ((found-path-p nil)
        (abs_path (expand-file-name path)))
    (while (and (not (string-equal abs_path "/")) (not found-path-p))
      (setq found-path-p (directory-files abs_path nil "WORKSPACE"))
      (setq abs_path (file-name-directory (directory-file-name abs_path))))
    (when (not found-path-p)
      (error "Workspace not found!"))
    abs_path))

(provide 'dd-newfile)
;;; dd-newfile.el ends here
