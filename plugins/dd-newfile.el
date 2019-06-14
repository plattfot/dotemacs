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

(defun dd-setup-newfile (modify-namespaces)
  "Add boilerplate, description, namespaces and include guard.
MODIFY-NAMESPACES are passed on to insert-namespace.  Extra
namespaces in MODIFY-NAMESPACES are separated by whitespace.  See
'nf-insert-namespaces' for the syntax on MODIFY-NAMESPACES.

It will use the 'dd-boilerplate' as the boilerplate.
It will use the 'dd-author-name' as the author name for the header.
It will use the 'dd-author-mail' as the author's mail address for the header."

  (interactive (list (dd--input-modify-namespaces)))
  (let* ((blacklist '("!^CoreLibs$" "!^Utility$" "!^Common$" "!^[.a-z]+"))
         (prefix_dd '("!^DD$" "^DD"))
         (replace '("^houdini$=Houdini"))
         (user_args (if modify-namespaces
                        (if (stringp modify-namespaces)
                            (split-string modify-namespaces))))
         (modify-namespaces (append prefix_dd replace blacklist user_args))
         (workspace_root (dd-find-workspace default-directory)))
    (nf-setup-newfile modify-namespaces
                      dd-author-name
                      dd-author-mail
                      dd-boilerplate
                      workspace_root)))

(defun dd--input-modify-namespaces ()
  "Parse the input for modify-namespaces.

Return a string with the namespaces."
  (let ((prompt "Add/Remove/Modify namespaces"))
    (read-regexp
     (if (and dd-setup-newfile-default
              (not (string-empty-p dd-setup-newfile-default)))
         (format "%s (default %s): " prompt dd-setup-newfile-default)
       (format "%s: " prompt))
     dd-setup-newfile-default
     'dd-newfile-history)))

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

(defun dd-rename-namespaces ()
  "Update the outer namespaces in the file to the new location.

Outer namespaces are classified as top namespaces that are
clumped together. For example

namespace DD {
namespace Maelstrom {
namespace Geometry3D {
namespace Remesh {
namespace Voxelize {
...
}
}
}
}
}

All of them are classified as outer namespaces and will be
replaced.

namespace DD {
  namespace Maelstrom {
    namespace Geometry3D {
      namespace Remesh {
        namespace Voxelize {
        ...
        }
      }
    }
  }
}

Only the DD namespace will be replaced."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-buffer)
    ;; (while (re-search-forward "^namespace [[:graph:]]+ {" nil t))
    (re-search-forward "\\(?:^namespace [[:graph:]]+ {\n\\)+")
    (re-search-backward "{")
    (mark-sexp)
    (let ((code (buffer-substring (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (while (re-search-backward "^namespace [[:graph:]]+ {" nil t)
        (re-search-forward "{")
        (backward-char)
        (mark-sexp)
        (delete-region (region-beginning) (region-end)))
      (delete-region (point-at-bol) (point-at-eol))
      (let ((blacklist '("!^CoreLibs$" "!^Utility$" "!^Common$" "!^[.a-z]+"))
            (prefix_dd '("!^DD$" "^DD"))
            (replace '("^houdini$=Houdini"))
            (workspace_root (dd-find-workspace default-directory)))
        (nf-insert-namespace `(,@prefix_dd ,@blacklist ,@replace) workspace_root)
        (re-search-backward "{")
        (mark-sexp)
        (delete-region (region-beginning) (region-end))
        (insert code)))))

(defun dd-rename-include-guard ()
  "Rename the include guard to match the namespaces in the file."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-buffer)
    (re-search-forward "^#ifndef [[:graph:]]+\n#define [[:graph:]]+\n")
    (set-mark (point))
    (previous-line 2)
    (delete-region (point-at-bol) (region-end))
    (let ((curr-point (point)))
      (end-of-buffer)
      (re-search-backward "^#endif")
      (delete-region (point-at-bol) (point-at-eol))
      (goto-char curr-point))
    (nf-include-guard-from-namespaces)))

(provide 'dd-newfile)
;;; dd-newfile.el ends here
