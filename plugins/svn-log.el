;;; svn-log --- Functions to parse svn xml logs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'dash)
(require 's)
(require 'cl-lib)

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
Therefore it will just pick the directory before
trunk|branches|tags. If no name can be found it will just take
the base name of the path."
  (or (svn-repository-name-from-path (svn-path-name (car paths)))
      (file-name-base (directory-file-name (svn-path-name (car paths))))))

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
                        (file-name-nondirectory path))
          (file-name-base parent)
        (svn-repository-name-from-path parent)))))

(defun svn-repository-path (path)
  "Return repository path from PATH.
Where PATH is a string.

Will pick the path directly after trunk, branches + name or tags + name.
If no directory is found it returns nil"
  (let* ((path-prefix (directory-file-name path))
         (parent (svn-parent-path path-prefix))
         (child nil)
         (basename nil)
         (found nil)
         (repo-path nil))
    (while (and (not found) (not (string-equal path-prefix parent)))
      (setf basename (file-name-nondirectory path-prefix))
      (cond
       ((string-match "^trunk$" basename)
        (setf found t
              repo-path (s-chop-prefix path-prefix path)))
       ((string-match "^\\(branches\\|tags\\)$" basename)
        (setf found t
              repo-path (s-chop-prefix
                         (if child
                             (concat (file-name-as-directory path-prefix) child)
                           path-prefix)
                         path)))
       (t
        (setf path-prefix parent
              parent (svn-parent-path parent)
              child basename))))
    (unless (s-blank? repo-path) repo-path)))

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

(defun svn-summary-org (svn-log
                        stats-func
                        stats-print
                        stats-merge-op
                        text-transform)
  "Return a summary of SVN-LOG as a string.
Statistics are updated by a call to STATS-FUNC, it takes two
arguments logentry and stats.

STATS-PRINT function to convert the stats alist to string.

STATS-MERGE-OP function use to combine two stats values.

TEXT-TRANSFORM is called on the header and body before they are
added to the output.

This format the string as an org buffer. Assume SVN-LOG is sorted
by date."
  (let ((prev-day "")
        (prev-month "")
        (prev-year "")
        (stats-day '())
        (stats-month '())
        (stats-year '()))
    (concat
     (mapconcat
      (lambda (logentry)
        (let* ((date (svn-logentry-date logentry))
               (year (format-time-string "%Y" date))
               (month (format-time-string "%Y%m" date))
               (day (format-time-string "%Y%m%d" date))
               (same-year? (string-equal year prev-year))
               (same-month? (string-equal month prev-month))
               (same-day? (string-equal day prev-day))
               (prefix-stats-year "")
               (prefix-stats-month "")
               (prefix-stats-day ""))

          (setf prev-year year
                prev-month month
                prev-day day)

          (if (and (not same-day?) stats-day)
              (progn
                (setf stats-month (svn-merge-stats stats-month stats-day stats-merge-op))
                (unless same-month?
                  (setf stats-year (svn-merge-stats
                                    stats-year stats-month stats-merge-op)
                        prefix-stats-month
                        (format "*** Stats for the month\n%s\n"
                                (funcall stats-print stats-month))
                        stats-month '())
                  (unless same-year?
                    (setf prefix-stats-year
                          (format "** Stats for the year\n%s\n"
                                  (funcall stats-print stats-year))
                          stats-year '())))
                (setf prefix-stats-day (format "**** Stats for the day\n%s\n"
                                               (funcall stats-print stats-day))
                      stats-day '())))
          (setf stats-day (funcall stats-func logentry stats-day))
          (svn-summary-generate-entry logentry
                                      prefix-stats-day
                                      prefix-stats-month
                                      prefix-stats-year
                                      same-year?
                                      same-month?
                                      same-day?
                                      text-transform)))
      svn-log
      "\n")
     (if stats-day
         (format "\n**** Stats for the day\n%s\n" (funcall stats-print stats-day))
       "")
     (if stats-month
         (format "\n*** Stats for the month\n%s\n" (funcall stats-print stats-month))
       "")
     (if stats-year
         (format "\n** Stats for the year\n%s\n" (funcall stats-print stats-year))
       ""))))

(defun svn-summary-generate-entry (logentry
                                   prefix-stats-day
                                   prefix-stats-month
                                   prefix-stats-year
                                   same-year?
                                   same-month?
                                   same-day?
                                   text-transform)
  "Generate the entry from LOGENTRY.
PREFIX-STATS-DAY will be added first to the entry, then
PREFIX-STATS-MONTH, then PREFIX-STATS-YEAR. After that it will
add a year entry if SAME-YEAR? is true. Then a month entry if
SAME-MONTH? is true. Then a day entry if the SAME-DAY? is true.
Before the commit message is added it passes through the
TEXT-TRANSFORM."
  (let* ((date (svn-logentry-date logentry))
         (msg-lines (s-lines (or (svn-logentry-msg logentry) "")))
         (header (funcall text-transform (car msg-lines)))
         (body (funcall text-transform
                        (svn-trim-leading-and-trailing-newlines
                         (s-join "\n" (or (cdr msg-lines) ""))))))
    (concat
     prefix-stats-day
     prefix-stats-month
     prefix-stats-year
     (if same-year? "" (format-time-string "* %Y\n" date))
     (if same-month? "" (format-time-string "** %B\n" date))
     (if same-day? "" (format-time-string "*** %A %d\n"  date))
     (format "**** %s - %s :%s:"
             (format-time-string "%k:%M" date)
             header
             (svn-logentry-name logentry))
     (if (not (s-blank? body)) (format "\n%s" body) ""))))


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

(defmacro svn-stats--append! (key value stats &optional add)
  "Increment KEY in STATS with VALUE.
If KEY doesn't exist in STATS, add (KEY . VALUE). Otherwise add
VALUE to KEY cell using function ADD. STATS is assumed to be an
alist with the cells (key . count). This will modify STATS in place."

  `(let ((cell (assoc-string ,key ,stats)))
     (if cell (progn (setcdr cell (funcall (or ,add '+) ,value (cdr cell)))
                     ,stats)
       (push (cons ,key ,value) ,stats))))

(defun svn-stats--append (key value stats &optional add)
  "Increment KEY in STATS with VALUE.
If KEY doesn't exist in STATS, add (KEY . VALUE). Otherwise add
VALUE to KEY cell using function ADD. STATS is assumed to be an
alist with the cells (key . count). This is non-destructive,
STATS will not change."
  (let* ((stats-mod (copy-alist stats))
         (cell (assoc-string key stats-mod)))
    (if cell (progn (setf (cdr cell) (funcall (or add '+) value (cdr cell)))
                    stats-mod)
      (push (cons key value) stats-mod))))

(defmacro svn-stats--increment-count! (key stats)
  "Increment KEY in STATS with one.
If NAME doesn't exist in stats, add it and increment it by one.
STATS is assumed to be an alist with the cells (key . count).
This will modify STATS in place."
  `(svn-stats--append! ,key 1 ,stats))

(defun svn-stats--increment-count (key stats)
  "Increment KEY in STATS with one.
If NAME doesn't exist in stats, add it and increment it by one.
STATS is assumed to be an alist with the cells (key . count).
This is non-destructive, STATS will not change."
  (svn-stats--append key 1 stats))

(defun svn-stats-pretty-print-count (stats)
  "Return a string containing the percentages of the keys in STATS.
The keys are sorted in descending order based on the
percentage."
  (svn-stats--to-string (svn-stats--sort (svn-stats--compute-percentage stats))))

(defun svn-stats--compute-percentage (stats)
  "Compute the percentage of each key in STATS.
Where STATS is an alist with the cells (key . count).
Return an alist with count replaced with percentage as a float."
  (when stats
    (let ((total (--reduce-from (+ acc (cdr it)) 0 stats)))
      (--map (cons (car it) (* (/ (float (cdr it)) total) 100)) stats))))

(defun svn-stats--convert-value (stats convert-func)
  "Convert the value in STATS using CONVERT-FUNC.

Retun a new alist with (KEY . (CONVERT-FUNC VALUE))"
  (when stats (--map (cons (car it) (funcall convert-func (cdr it))) stats)))

(defun svn-stats--sort (stats)
  "Sort STATS in descending order based on the value."
  (--sort (> (cdr it) (cdr other)) stats))

(defun svn-stats--to-string (stats)
  "Convert STATS to string."
  (mapconcat (lambda (x) (format "%s: %#.2f%%" (car x) (cdr x))) stats "\n"))

;; Modified version of https://stackoverflow.com/a/10061513
(defun svn-merge-stats (stats1 stats2 &optional add)
  "Merge alists STATS1 and STATS2 using ADD.
Default for ADD is +."
  (let ((ac (copy-alist stats1))
        (add (or add '+)))
    (dolist (x stats2)
      (let ((r (assoc-string (car x) ac)))
        (if (null r)
            (push x ac)
          (setf (cdr r) (funcall add (cdr x) (cdr r))))))
    ac))

(defun svn-stats-name-count (logentry stats &optional value add)
  "Increment the LOGENTRY's name in STATS with VALUE using ADD.
Default for VALUE is 1 and + for ADD."
  (svn-stats--append (svn-logentry-name logentry) (or value 1) stats add))

(defun svn-stats-path-count (logentry stats &optional value add)
  "Increment the LOGENTRY's paths in STATS with VALUE using ADD.
Default for VALUE is 1 and + for ADD."
  (let ((stats-mod (copy-alist stats))
        (val (or value 1)))
    (--each (svn-logentry-paths logentry)
      (svn-stats--append! (svn-path-name it) val stats-mod add))
    stats-mod))

(defun svn-stats-filename-count (logentry stats &optional value add)
  "Increment the LOGENTRY's filenames in STATS with VALUE using ADD.
Default for VALUE is 1 and + for ADD."
  (let ((stats-mod (copy-alist stats))
        (val (or value 1)))
    (--each (svn-logentry-paths logentry)
      (svn-stats--append!
       (file-name-nondirectory (svn-path-name it)) val stats-mod add))
    stats-mod))

(defun svn-stats-repo-path-count (logentry stats &optional value add)
  "Increment the LOGENTRY's repo paths in STATS with VALUE using ADD.
Default for VALUE is 1 and + for ADD.
Repo paths means removing repository prefix."
  (let ((stats-mod (copy-alist stats))
        (val (or value 1)))
    (--each (svn-logentry-paths logentry)
      (svn-stats--append!
       (let* ((path (svn-path-name it))
              (repo-path (svn-repository-path path)))
         (format "[%s]%s"
                 (svn-logentry-name logentry)
                 (if repo-path
                     repo-path
                   (file-name-nondirectory path))))
       val stats-mod add))
    stats-mod))

(defun svn-stats-repo-path-dir-only-count (logentry stats &optional value add)
  "Increment the LOGENTRY's repo paths in STATS with VALUE using ADD.

This will only count the directory names, not the files. Use
`svn-stats-repo-path-count' for that. Default for VALUE is 1 and
+ for ADD. Repo paths means removing repository prefix."
  (let ((stats-mod (copy-alist stats))
        (val (or value 1)))
    (--each (svn-logentry-paths logentry)
      (svn-stats--append!
       (let* ((path (svn-path-name it))
              (repo-path (svn-repository-path path)))
         (format "[%s]%s"
                 (svn-logentry-name logentry)
                 (if repo-path
                     (file-name-directory repo-path)
                   (file-name-nondirectory path))))
       val stats-mod add))
    stats-mod))

(defun svn-show--parse-interactive-input (&optional no-optional)
  "Parse the input to `svn-show-summary-in-org-buffer' or similar.

Functitons that takes a svn xml log path, buffer name, stats
function and optionally a text-transform.

If NO-OPTIONAL is true it will skip reading the optional
arguments."
  (let ((path (read-file-name "SVN xml log(s): ")))
    (list path
          (read-buffer "Write summary to buffer: "
                       (let ((default (file-name-base path)))
                         (if (or (null default) (string-equal "*.xml" default))
                             (current-buffer)
                           (format "*%s*" default))))
          (intern (completing-read  "Statistics function: "
                                    '(svn-stats-name-count
                                      svn-stats-path-count
                                      svn-stats-filename-count)
                                    'fboundp
                                    t))
          nil
          (when (and (null no-optional) current-prefix-arg)
            (intern
             (completing-read  "Text transform function: "
                               obarray 'fboundp t  nil nil 'identity ))))))

(defun svn-show--org-buffer-skeleton (svn-xml-log-path
                                      buffer-name
                                      stats-func
                                      stats-print
                                      stats-merge-op
                                      text-transform)
  "Print out a summar of SVN-XML-LOG-PATH file(s) to BUFFER-NAME.
Supports wildcards for combining multiple logs into one summary.
This will generate the summary in a org buffer.

Stats will be gathered by calling the STATS-FUNC with a logentry
and an alist.

STATS-MERGE-OP operator use to merge two stats cells.

STATS-PRINT is the function called when converting the stats to a
string.

TEXT-TRANSFORM is called on the header and body of the commit
message. And the result is added to the buffer."

  (let ((buffer (get-buffer-create buffer-name))
        (logs (file-expand-wildcards svn-xml-log-path)))

    (unless logs
      (error "File path does not exist"))

    (switch-to-buffer buffer)
    (insert (svn-summary-org
             (apply 'svn-combine-logs (mapcar 'svn-parse-log logs))
             stats-func
             (or stats-print 'svn-stats-pretty-print-count)
             stats-merge-op
             (or text-transform 'identity)))
    (org-mode)))

(defun svn-show-summary-in-org-buffer (svn-xml-log-path
                                       buffer-name
                                       stats-func
                                       &optional
                                       stats-print
                                       text-transform)
  "Print out a summar of SVN-XML-LOG-PATH file(s) to BUFFER-NAME.
Supports wildcards for combining multiple logs into one summary.
This will generate the summary in a org buffer.

Stats will be gathered by calling the STATS-FUNC with a logentry
and an alist.

STATS-PRINT is the function called when converting the stats to a
string. Default is `svn-stats-pretty-print-count'.

TEXT-TRANSFORM is called on the header and body of the commit
message. And the result is added to the buffer. By default this
is `identity'."
  (interactive (svn-show--parse-interactive-input))
  (svn-show--org-buffer-skeleton
   svn-xml-log-path
   buffer-name
   stats-func
   (or stats-print 'svn-stats-pretty-print-count)
   '+
   (or text-transform 'identity)))

(defun time-greater-p (t1 t2)
  "Return t if T1 is greater than T2."
  (let ((greater nil))
    (while (and t1 t2 (not greater))
      (let ((x1 (car t1))
            (x2 (car t2)))
        (setf greater (when (not (= x1 x2)) (> x1 x2))
              t1 (cdr t1)
              t2 (cdr t2))))
    greater))

(defun svn-show-summary-in-org-buffer-time (svn-xml-log-path
                                            buffer-name
                                            stats-func
                                            &optional
                                            stats-print
                                            text-transform)
  "Print out a summar of SVN-XML-LOG-PATH file(s) to BUFFER-NAME.
Supports wildcards for combining multiple logs into one summary.
This will generate the summary in a org buffer.

Stats will be gathered by calling the STATS-FUNC with a logentry
and an alist.

STATS-PRINT is the function called when converting the stats to a
string. Default is `svn-stats-pretty-print-count'. Note that time
will be converted to seconds before this is called.

TEXT-TRANSFORM is called on the header and body of the commit
message. And the result is added to the buffer. By default this
is `identity'."
  (interactive (svn-show--parse-interactive-input))

  (svn-show--org-buffer-skeleton
   svn-xml-log-path
   buffer-name
   (let ((prev-time nil))
     #'(lambda (logentry stats)
         (let* ((time (svn-logentry-date logentry))
                (time-spent))
           (when (or (null prev-time)
                     (not (string-equal (format-time-string "%Y%m%d" time)
                                        (format-time-string "%Y%m%d" prev-time))))
             ;; Set it to the start of the day
             (setf prev-time (date-to-time (format-time-string "%Y-%m-%d 09:00" time))))
           (setf time-spent (time-subtract time prev-time))

           (let ((time-start-lunch
                  (date-to-time (format-time-string "%Y-%m-%d 13:00" time)))
                 (time-end-lunch
                  (date-to-time (format-time-string "%Y-%m-%d 14:00" time)))
                 (time-hour (seconds-to-time (* 60 60))))
             (when (and (time-less-p prev-time time-start-lunch)
                        (time-greater-p time time-end-lunch))
               (setf time-spent (time-subtract time-spent time-hour))))
           (funcall stats-func logentry stats time-spent 'time-add))))
   #'(lambda (stats)
       (funcall (or stats-print 'svn-stats-pretty-print-count)
        (svn-stats--convert-value stats 'time-to-seconds)))
   'time-add
   (or text-transform 'identity)))

(provide 'svn-log)
;;; svn-log.el ends here
