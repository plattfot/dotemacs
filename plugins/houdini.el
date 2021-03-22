;;; houdini.el --- Collection of functions to help with houdini at work
;;; Commentary:

;;; Code:
(require 'cpreproc)

(defun hou-version-to-hex (version-string)
  "Convert VERSION-STRING to hex.
Instead of looking up in a table better just to compute it."
  ;; Split the string into parts, filtering out the pre
  (let ((vers_parts (delete "" (split-string version-string "[.]\\|pre[0-9]+"))))
    ;; If it's not three parts, i.e major, minor and build version throw error
    (when (not (eq (length vers_parts) 3))
      (throw 'hou-tag (format "%s is not a valid version string" version-string)))

    ;; Convert the parts to integers and then print them out in the
    ;; format houdini is using.
    (let ((vers_int (mapcar #'(lambda (x) (string-to-number x)) vers_parts)))
      (format "0x%02x%02x%04x" (nth 0 vers_int) (nth 1 vers_int) (nth 2 vers_int))
      )))

(defun hou-insert-latest-version (version-re)
  "Insert the latest version matching VERSION-RE at point."
  (interactive "sHoudini version regex: ")
  (insert (car (sort (hou-matching-versions version-re) 'string> ))))

(defun hou-insert-version-id (version)
  "Insert the houdini version id for VERSION."
  (interactive "sHoudini version: ")
  (let ((version_hex (cpreproc-semver-to-hex version))) (insert version_hex)))

(defun hou-insert-if (version comp)
  "Insert an #if-else clause.
The specified VERSION in int format with the COMP being either
==, <=, >=, !=, < or >.  If a region is active it will place that
in both the ifdef and else clause.  Example:
`(hou-insert-ifdef \"14.0.173\" \"==\")' will insert this
#if( UT_VERSION_INT == 0x0e0000ad ) // 14.0.173
<Cursor>
#else
#endif

(hou-insert-ifdef \"14.0.173\" \">=\") will insert this
#if( UT_VERSION_INT >= 0x0e0000ad ) // 14.0.173 or later
<Cursor>
#else
#endif

(hou-insert-ifdef \"14.0.173\" \"<=\") will insert this
#if( UT_VERSION_INT <= 0x0e0000ad ) // 14.0.173 or earlier
<Cursor>
#else
#endif"
(interactive "sHoudini version: \nsCompare operator: ")
(let ((version_hex (cpreproc-semver-to-hex version)))
  (cpreproc-insert-if "UT_VERSION_INT" comp version_hex version current-prefix-arg)))

;; Following the same naming convention as gtest
;; eq -> ==
;; ne -> !=
;; lt -> <
;; le -> <=
;; gt -> >
;; ge -> >=

(defun hou-insert-if-eq (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and ==."
  (interactive "sHoudini version: ")
  (hou-insert-if version "=="))

(defun hou-insert-if-ne (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and !=."
  (interactive "sHoudini version: ")
  (hou-insert-if version "!="))

(defun hou-insert-if-lt (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and <."
  (interactive "sHoudini version: ")
  (hou-insert-if version "<"))

(defun hou-insert-if-le (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and <=."
  (interactive "sHoudini version: ")
  (hou-insert-if version "<="))

(defun hou-insert-if-gt (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and >."
  (interactive "sHoudini version: ")
  (hou-insert-if version ">"))

(defun hou-insert-if-ge (version)
  "Wrapper for calling hou-insert-if with arguments VERSION and >=."
  (interactive "sHoudini version: ")
  (hou-insert-if version ">="))

(defun hou-get-version (&optional version-offset)
  "Return the houdini version that is installed in /tools/package.
If VERSION-OFFSET is 1 (default) it will return the latest
version, if 2 it will return the next latest and so on.  If the
offset is less than 1 or greater than the number of installed
versions it will return the oldest version."
  (interactive "p")
  ;; Calling this from the mini-buffer or global-keys will by
  ;; default set the numeric-prefix to 1. Hence the switch to
  ;; using 1 as the default.
  (when (not version-offset) (setq version-offset 1))
  ;; Sorted by time, newest first. The last element will be empty
  ;; hence the nbutlast command to remove it.
  (let* ((houdini-versions (nbutlast (split-string (shell-command-to-string
						    "ls -t /tools/package/houdini")
						   "\n")))
	 (total (length houdini-versions))
	 (offset (1- version-offset)))
    (if (and (> version-offset 0) (< version-offset total))
	(nth offset houdini-versions)
      (car(last houdini-versions))
      )))

(defun hou-insert-version (&optional version-offset)
  "Insert the houdini version where the cursor is located.
If VERSION-OFFSET is 1 (default) it will insert the latest
installed houdini version, 2 the next latest and so on.  If the
offset is less than 1 or greater than the number of installed
versions it will return the oldest version."
  (interactive "p")
  (insert (hou-get-version version-offset)))

(defun hou-open-other-version (version)
  "Will open the same hdk file but for the version specified by VERSION.
Note only works if the open buffer is an hdk file."
(interactive "sEnter houdini version: ")
(let ((file-path (buffer-file-name))
      (hou-regexp "\\(.*?/houdini/\\)[0-9]+\\.[0-9]+\\.[0-9]+\\(/.*\\)+"))
  (if file-path
      (progn (if (string-match hou-regexp file-path)
		 (progn (find-file (concat
				    (match-string 1 file-path)
				    version
				    (match-string 2 file-path)))
			(rename-buffer (concat (file-name-nondirectory file-path)
					       "<" version ">")))
	       (message "File is not a houdini file")))
    (message "File isn't saved to disk"))))

(provide 'houdini)
;;; houdini.el ends here
