;;; svn-log --- Functions to parse svn xml logs

;;; Commentary:

;;; Code:
(cl-defstruct svn-logentry revision author date paths msg name)
(cl-defstruct svn-path prop-mods text-mods kind action name)

(defun svn-parse-log (svn-xml-log)
  "Parse SVN-XML-LOG file and return a list of svn-logentry.

To get a svn log in xml use 'svn log --xml --verbose'."
  (with-temp-buffer
    (insert-file-contents svn-xml-log)
    (let ((log (libxml-parse-xml-region (point-min) (point-max)))
          (name (file-name-base svn-xml-log)))
      (mapcar (lambda (logentry-xml)
                (let* ((fourth-entry (nth 4 logentry-xml))
                       (paths)
                       (msg))
                  ;; paths are only included if svn log is run with --verbose
                  (if (eq (car fourth-entry) 'paths)
                      (setf paths (svn-parse-paths (nthcdr 2 fourth-entry))
                            msg (nth 2 (nth 5 logentry-xml)))
                    (setf paths '()
                          msg (nth 2 (nth 4 logentry-xml))))
                  (make-svn-logentry
                   :revision (cdr (assoc 'revision (nth 1 logentry-xml)))
                   :author (nth 2 (nth 2 logentry-xml))
                   :date (date-to-time (nth 2 (nth 3 logentry-xml)))
                   :paths paths
                   :msg msg
                   :name (if paths (svn-name-from-paths-naive paths) name))))
              (nthcdr 2 log)))))

(defun svn-parse-paths (paths-xml)
  "Extract the paths in the list PATHS-XML as svn-path structs."
  (mapcar (lambda (path-xml)
            (let ((metadata (nth 1 path-xml)))
              (make-svn-path
               :prop-mods (string-equal "true" (cdr (assoc 'prop-mods metadata)))
               :text-mods (string-equal "true" (cdr (assoc 'text-mods metadata)))
               :kind (cdr (assoc 'kind metadata))
               :action (cdr (assoc 'action metadata))
               :name (nth 2 path-xml))))
          paths-xml))

(defun svn-name-from-paths-naive (paths)
  "Pick out a name based on file PATHS.

This assumes all files touched are local to one repository.
Therefore it will just pick the directory before trunk|branches|tags."
  (svn-repository-name-from-path (svn-path-name (car paths))))

(defun svn-repository-name-from-path (path)
  "Return repository name from PATH.
Where PATH is a string.

Will pick the directory directly after trunk, branches or tags.
If no directory is found it returns nil"
  (let* ((path (directory-file-name path))
         (parent (svn-parent-path path)))
    ;; Check that we aren't at the root.
    (when (not (string-equal path parent))
      (if (string-match "^\\(trunk\\|branches\\|tags\\)$"
                        (file-name-base path))
          (file-name-base parent)
        (svn-repository-name-from-path parent)))))

(defun svn-parent-path (path)
  "Return the parent path of PATH.
PATH must be string.
Examples:
/foo/bar/baz -> /foo/bar
/foo/bar/  -> /foo/bar
/ -> /"
  (directory-file-name (file-name-directory path)))

(defun svn-combine-logs (&rest logs)
  "Combine the svn LOGS, sorted by date.

The LOGS must contain a list of lists containing svn-logentry."
  (sort (copy-tree (apply 'append logs))
        (lambda (logentry-a logentry-b)
          (time-less-p (svn-logentry-date logentry-a)
                       (svn-logentry-date logentry-b)))))

(defun svn-summary-list (svn-log)
  "Return a summary of SVN-LOG as a string.
Each entry in the log will be a line in the string."
  (mapconcat
   (lambda (logentry)
     (format "%s: [%s] %s"
             (format-time-string "%Y %b %d T%k:%M" (svn-logentry-date logentry))
             (svn-logentry-name logentry)
             (car (split-string (svn-logentry-msg logentry) "\n"))))
   svn-log
   "\n"))

(defun svn-summary-day (svn-log)
  "Return a summary of SVN-LOG as a string.
This clump log entries per day. Assume SVN-LOG is sorted by date."
  (let ((prev-day ""))
    (mapconcat
     (lambda (logentry)
       (let* ((date (svn-logentry-date logentry))
              (header (car (split-string (or (svn-logentry-msg logentry) "") "\n")))
              (day (format-time-string "%d" date))
              (prefix (if (string-equal day prev-day)
                          ""
                        (format "\n%s\n" (format-time-string "%A %Y %b %d" date)))))
         (setf prev-day day)
         (format "%s%s: [%s] %s"
                 prefix
                 (format-time-string "%k:%M" date)
                 (svn-logentry-name logentry)
                 header)))
     svn-log
     "\n")))

(defun svn-show-summary-in-buffer (svn-xml-log-path buffer-name)
"Print out a summar of SVN-XML-LOG-PATH file(s) to BUFFER-NAME.
Supports wildcards for combining multiple logs into one summary."
  (interactive "FSVN xml log(s): \nBbuffer to print the summary to: ")
  (let ((buffer (get-buffer-create buffer-name))
        (logs (file-expand-wildcards svn-xml-log-path)))

    (unless logs
      (error "File path does not exist"))

    (switch-to-buffer buffer)
    (insert (svn-summary-day
             (apply 'svn-combine-logs (mapcar 'svn-parse-log logs))))))

(provide 'svn-log)
;;; svn-log.el ends here
