;;; svn-log --- Functions to parse svn xml logs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'dash)
(require 's)

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
             (car (s-lines (svn-logentry-msg logentry)))))
   svn-log
   "\n"))

(defun svn-summary-day (svn-log)
  "Return a summary of SVN-LOG as a string.
This clump log entries per day. Assume SVN-LOG is sorted by date."
  (let ((prev-day ""))
    (mapconcat
     (lambda (logentry)
       (let* ((date (svn-logentry-date logentry))
              (header (car (s-lines (or (svn-logentry-msg logentry) ""))))
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

(defun svn-summary-org (svn-log)
  "Return a summary of SVN-LOG as a string.
This format the string as an org buffer. Assume SVN-LOG is sorted by date."
  (let ((prev-day "")
        (prev-month "")
        (prev-year ""))
    (mapconcat
     (lambda (logentry)
       (let* ((date (svn-logentry-date logentry))
              (msg-lines (split-string (or (svn-logentry-msg logentry) "") "\n"))
              (header (car msg-lines))
              (body (svn-trim-leading-and-trailing-newlines
                     (string-join (or (cdr msg-lines) "") "\n")))
              (year (format-time-string "%Y" date))
              (month (format-time-string "%Y%m" date))
              (day (format-time-string "%Y%m%d" date))
              (prefix-year (if (string-equal year prev-year)
                               ""
                             (format "* %s\n" year)))
              (prefix-month (if (string-equal month prev-month)
                              ""
                            (format "** %s\n" (format-time-string "%B" date))))
              (prefix-day (if (string-equal day prev-day)
                              ""
                            (format "*** %s\n" (format-time-string "%A %d" date)))))
         (setf prev-year year)
         (setf prev-month month)
         (setf prev-day day)
         (format "%s%s%s%s - %s :%s:%s"
                 prefix-year
                 prefix-month
                 prefix-day
                 (format-time-string "**** %k:%M" date)
                 header
                 (svn-logentry-name logentry)
                 (if (not (string-empty-p body)) (format "\n%s" body) ""))))
     svn-log
     "\n")))

(defun svn-trim-leading-and-trailing-newlines (body)
  "Remove newline from the beginning and end of BODY."
  (svn-trim-trailing-newlines (svn-trim-leading-newlines body)))

(defun svn-trim-leading-newlines (body)
  "Remove newline from the beginning of BODY."
  (save-match-data
    (let* ((start (progn (string-match "^\n*" body) (match-end 0))))
      (substring body start))))

(defun svn-trim-trailing-newlines (body)
  "Remove newline from the end of BODY."
  (save-match-data
    (let* ((end (- (length body)
                   (progn (string-match "^\n*" (reverse body)) (match-end 0)))))
      (substring body 0 end))))

(defmacro svn-stats-increment-count! (key stats)
  "Increment KEY in STATS with one.
If NAME doesn't exist in stats, add it and increment it by one.
STATS is assumed to be an alist with the cells (key . count)."
  `(let ((cell (assoc-string ,key ,stats)))
     (if cell (progn (setcdr cell (+ 1 (cdr cell)))
                     ,stats)
       (push (cons ,key 1) ,stats))))

(defun svn-show-summary-in-org-buffer (svn-xml-log-path buffer-name)
"Print out a summar of SVN-XML-LOG-PATH file(s) to BUFFER-NAME.
Supports wildcards for combining multiple logs into one summary.
This will generate the summary in a org buffer"
  (interactive "FSVN xml log(s): \nBbuffer to print the summary to: ")
  (let ((buffer (get-buffer-create buffer-name))
        (logs (file-expand-wildcards svn-xml-log-path)))

    (unless logs
      (error "File path does not exist"))

    (switch-to-buffer buffer)
    (insert (svn-summary-org
             (apply 'svn-combine-logs (mapcar 'svn-parse-log logs))))
    (org-mode)))

(provide 'svn-log)
;;; svn-log.el ends here
