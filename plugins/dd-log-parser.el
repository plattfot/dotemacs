;; From http://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)
;; Based on http://stackoverflow.com/questions/3243035/define-average-in-lisp
(defun average (ns) (/ (apply '+ ns) (length ns)))

;; From
;; http://stackoverflow.com/questions/2289883/emacs-copy-matching-lines
;; by Trey Jackson
(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer 
      (erase-buffer))
    (save-match-data 
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position) 
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun dd/log-extract-info (re)
"Extracts all lines matching the regexp RE, putting them in a
buffer named *matching* and strips the DD_LOG header."
(interactive "sEnter regexp to match: ")
(copy-lines-matching-re re)
(beginning-of-buffer)
(replace-regexp "[| ]*\\(\\+-\\)*\\[.*\\]:" "") ;; Strip DD_LOG header
)

(defun dd/log-build-table-with-methods (method table first_header compute_avg)
  "build the table based on the data in TABLE"
  (org-mode)
  (insert "# Method: " method ".\n")
  (setq avg_list '())
  (setq num_cols 0) ;; number of columns of the table
  (setq num_rows 0) ;; number of rows of the table
  (maphash (lambda (key values) 
	     ;; Compute the number if columns
	     (setq num_cols (max num_cols (length values)))
	     ;; Count rows
	     (setq num_rows (1+ num_rows))
	     (if compute_avg
		 (push (number-to-string (average (mapcar 'string-to-number values)))
		       avg_list)
	       nil)
	     (insert "|" key "|" (mapconcat 'identity (nreverse values) "|"))
	     (newline)
	     ) table)
  (org-table-align)
  ;; Go to the beginning of the table
  (forward-line (- (1+ num_rows)))
  (end-of-line)
  ;; Create the header
  (newline)
  (insert "||")
  (org-table-align)
  ;; Place the cursor at the right spot
  (beginning-of-line)
  (org-table-next-field)
  ;; Populate header
  (insert first_header)
  (setq count 0)
  (while (< count num_cols)
    (org-table-next-field)
    (insert " run " (int-to-string count))
    (setq count (1+ count))
  )
  ;; Add column for the average value at the end of the table
  (if compute_avg
      (progn
	(end-of-line)
	(insert " avg|")
	(org-table-align)
	;; Move to correct field to insert avg values
	(org-table-previous-field)
	;; Reverse list since we will begin at the top
	(setq avg_list (nreverse avg_list))
	(while avg_list 
	  (org-table-next-row)
	  (insert (pop avg_list))
	  )
	;; Go back to the beginning
	(forward-line (- num_rows))
	) nil)
  ;; Add divider, reason for not doing this with the header is that
  ;; navigation becomes much harder.
  (end-of-line)
  (newline)
  (insert "|-+-|")
  (org-table-align)
  (setq begin (point))
  (forward-line num_rows)
  (end-of-line)
  (setq end (point))
  (sort-lines nil begin end)
  (newline)
)

(defun dd/log-generate-table-with-methods ( name_row name_col methods 
					    &optional first_header compute_avg) 
"Call from the buffer that contains the dd log. Generates
table/tables from that log by extracting the regexp matcing the
NAME_ROW, NAME_COL and METHODS. Place paranthesis \\(\\) around
the thing you want to be in the each cell i.e Frame
\\([0-9]+\\). Note that the regexp for each must only match one
thing per iteration.\n\n NAME_ROW specifies which row of the
table it should place data, for example frame number or number of
secondary grids used. The value inside the parenthesis must be
unique for each row and it will be placed in the first column.\n\n
NAME_COL specifies what to put in the columns.\n\n METHODS
specify each method it should look for, for example running the
fluid sim with reseeding and split/merge a second time. Note that
the method must be unique for the key, value set and can only be
on one line. For example running the fluid sim first with
reseeding, then with split/merge and then with both will not
work."
(interactive (list (read-string "String that classifies each iteration: ")
		   (read-string "String to match the result: ")	
		   (read-string "Enter name for each method, separated by comma: ")
		   (read-string "Enter name for the first column: ")
		   (y-or-n-p "Compute average?"))
)
;; Process arguments
(setq methods (split-string methods ",") )
(setq methods (sort methods 'string<))
;; Remove whitespaces from beginning and end.
(setq methods (mapcar 'chomp methods))
;: Prepare for regexp string
(setq methods_pad (mapcar (lambda (a) (format "\\(%s\\)" a)) methods))
;; Regexp for extracting the data
(setq re_types (mapconcat 'identity methods_pad "\\|"))
(setq re_value  (concat "\\(" name_col "\\)") )
(setq re_key (concat "\\(" name_row "\\)") )
;; Extract all lines matching the three variables into a separate buffer
(dd/log-extract-info (mapconcat 'identity (list re_value re_key re_types) "\\|"))
;; Setup hash table to put the data in.
(define-hash-table-test 'str-hash 'string-equal 'sxhash)
(setq num_methods (length methods))
;; Will contain all the tables where each table is mapped to the method
(setq tables (make-hash-table :test 'str-hash :size (length methods) ))
;; populate tables with a hash table for each method
(dolist (method methods)
  ;; Extract method name
  (setq method (substring method 
			  (1+ (string-match "(" method)) 
			  (string-match "\\\\)" method)) )
  (puthash method (make-hash-table :test 'str-hash ) tables)
  )
(beginning-of-buffer)
;; Remove value duplicates.
;; Based on http://www.emacswiki.org/emacs/DuplicateLines
(while (re-search-forward (concat "^\\(.*" name_col ".*\n\\)\\1+") (point-max) t)
  (replace-match "\\1"))
(beginning-of-buffer)
(while (not (= (line-beginning-position) (line-end-position)))
  ;; Need to read in region, parse the row, col and method (if it exist)
  ;; and add that to the correct table
  (setq begin (line-beginning-position))
  (setq end (point-at-eol 3))
  ;; Read line from buffer
  (setq input (buffer-substring-no-properties 
	       begin end ))
  (string-match name_row input)
  (setq row (match-string 1 input))
  (string-match name_col input) 
  (setq col (match-string 1 input))
  (setq method_used "")
  (dolist (method methods)
    (if (string-match method input) 
	(progn (setq method_used (match-string 1 input)) (return)) 
      nil))
  (if (> (string-width method_used) 0)
      (progn 
	;; Get the hash table associated with the method name
       (setq table (gethash method_used tables))
       ;; Get the row for that table
       (setq bucket (gethash row table))
       ;; Add the column value to that row
       (push col bucket)
       ;; Update the row in the table
       (puthash row bucket table)
       ) nil)
  (delete-region begin end)
  (kill-line)
)
;; Iterate over the tables.
(maphash (lambda (key table) (dd/log-build-table-with-methods key table first_header compute_avg ))
	 tables)
)
;; Functions that uses dd/log-generate-table to extract info
(defun extract-particle-count ()
"Extracts the particle count from a fluid sim by parsing the dd log"
(interactive)
(dd/log-generate-table-with-methods
 "Float frame: \\([0-9]+\\)"
 "total number of particles: \\([0-9]+\\)"
 "Starting process Get input \\(fluid\\)"
 "Frame")
)

(defun extract-volumetrics-info () 
"Extracts the result from a DD log when using inheritance or template
specialization with rasterizing."
  (interactive)
  (dd/log-generate-table-with-methods 
   "Number of Secondary Grids to raster \\([0-9]+\\)"
   "Finished process Volume rasterization in \\([0-9]+\\.[0-9]+\\)s"
   "Using \\(template specialization\\)\\., Using \\(inheritance\\)\\."
   "SG"
   t) ;; SG = Secondary Grids
)
;; Work in progress
(defun dd/log-parse-layout (layout type default) 
"Parse the input and returns the layout in a list. For example
layout = \"I(1,2) R(Test,1)\" type = I, will return (1 2), type = R will return (\"Test\" 1) if it fails to make sense of
the input it will return the default"
(interactive)
(setq re "\\s-*\\([a-zA-Z0-9 ]+?\\)\\s-*")

(setq matcher (concat type "(" (mapconcat 'identity (list re re) ",") ")") )

(if (string-match matcher layout)
    (progn 
      (setq first (match-string 1 layout)
	    second (match-string 2 layout))
      (setq first_digi (string-to-number first) 
	    second_digi (string-to-number second))
      (if (not (= first_digi 0))
	  (setq first first_digi) nil)
      (if (not (= second_digi 0)) 
	  (setq second second_digi) nil)
      (list first second)
      )
  default )
)

(defun dd/log-const_extractor (const input )
  "Returns what's in CONST"
  (interactive)
  const
)

(defun dd/log-extractor (group input )
  "Returns the match from GROUP"
  (interactive)
  (match-string group input)
)

(defun dd/log-extract-func (input)
"returns the const-extractor if the input is an int else the extractor"
(interactive)
(if (stringp input) 'dd/log-const_extractor 'dd/log-extractor)
)

(defun dd/convert-to-re (layout)
"Convert integers to re groups. I.e 1 -> \\1"
(mapcar (lambda (l) (if (numberp l) (concat "\\" (number-to-string l)) 
		      (replace-regexp-in-string "^\\([0-9]+\\)$" "\\\\1" l))) layout)
)

(defun dd/log-generate-table-last ( iter_re result_re &optional match_layout) 
"Same as log-generate-table except this one sets CELL-OPERATOR to dd/last. Which mean that if there are more than one entry per iteration the last encounter will be stored"
(interactive (list (read-string "String that classifies each iteration: ")
		   (read-string "String to match the result: ")	
		   (read-string "Enter layout (default I(Iteration,1) R(Result,1)): "))
)
(dd/log-generate-table iter_re result_re match_layout 'dd/last )
)

(defun dd/log-generate-table ( iter_re result_re &optional match_layout cell-operator) 
"Call from the buffer that contains the dd log. Generates a table
from that log by extracting the regexp matcing the NAME_ITER and
RESULT_RE.\nThe ITER_RE should be something that is unique for
each iteration and also mark the begin on each iteration. A good
example is matching the frame number i.e frame \\([0-9]+\\) since
it's usually in the beginning of the iteration and it's unique.\n
RESULT_RE should match everything you want to extract during the
iteration. An example is to extract all Finished process. Both
regexp strings expects atleast one group and that's for what to
put in the table and another can be used for the name of that
column.\n In order to know which one is which an optional
parameter MATCH_LAYOUT specifies the layout and expects the
following syntax I/R(x,y) separated by whitespace. The I specify
the the layout for ITER_RE and R for RESULT_RE. x is what group
the name of the result is in could be a number or a str. If it's
a string it will use that as the name. y can only be an integer
specifying the group of what to put in the table. It will default
to I(Iteration,1) R(Result,1)"
(interactive (list (read-string "String that classifies each iteration: ")
		   (read-string "String to match the result: ")	
		   (read-string "Enter layout (default I(Iteration,1) R(Result,1)): "))
)
;; Process arguments
( setq iter_layout (list "Iteration" 1)
       result_layout (list "Result" 1) )
(if (not cell-operator) (setq cell-operator 'dd/add-numbers))

(if match_layout
    (progn (setq iter_layout (dd/log-parse-layout match_layout "I" iter_layout)
	   result_layout (dd/log-parse-layout match_layout "R" result_layout ))) 
    nil)

;; Regexp for extracting the data
(setq re_value  (concat "\\(" iter_re "\\)") )
(setq re_key (concat "\\(" result_re "\\)") )
;; Extract all lines matching the three variables into a separate buffer
(copy-lines-matching-re (mapconcat 'identity (list re_value re_key) "\\|"))

;; Setup hash table to put the data in.
(define-hash-table-test 'str-hash 'string-equal 'sxhash)
(setq table (make-hash-table :test 'str-hash ))

(beginning-of-buffer)
(setq start (point))
(search-forward-regexp iter_re )
(beginning-of-line)
(setq end (point))
(kill-region start end)

;; ;; Remove value duplicates.
;; ;; Based on http://www.emacswiki.org/emacs/DuplicateLines
;; (while (re-search-forward (concat "^\\(.*" name_col ".*\n\\)\\1+") (point-max) t)
;;   (replace-match "\\1"))
;; (beginning-of-buffer)

(setq iter_first_func (dd/log-extract-func (car iter_layout))
      iter_second_func (dd/log-extract-func (cdr iter_layout))
      result_first_func (dd/log-extract-func (car result_layout))
      result_second_func (dd/log-extract-func (cdr result_layout))
)

(setq iter_layout_re (dd/convert-to-re iter_layout)
      result_layout_re (dd/convert-to-re result_layout)
)
;; Get the header for the iteration
(beginning-of-buffer)
(setq row_nr 0)
(while (search-forward-regexp iter_re (point-max) t) 

  (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (string-match iter_re line)
  (setq col_nr (funcall iter_first_func (car iter_layout) line)
	data (funcall iter_second_func (nth 1 iter_layout) line)
	col (gethash col_nr table))

  (if (not col) 
      ;; column doesn't exist create it first
      (setq col (make-hash-table :test 'equal))
    ) ;; if col

  (puthash row_nr data col)
  (puthash col_nr col table)

  (beginning-of-line)
  (kill-whole-line)
  
  (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (while (string-match result_re line)

    (setq col_nr (funcall result_first_func (car result_layout) line)
	  data (funcall result_second_func (nth 1 result_layout) line)
	  col (gethash col_nr table))

    (if (not col) 
	;; column doesn't exist create it first
      	(setq col (make-hash-table :test 'equal))
      ) ;; if col

    (setq cell (gethash row_nr col))
    ;; If cell already has a value add the data to it
    (if cell (setq cell (funcall cell-operator cell data))
      (setq cell data))
    ;; update cell 
    (puthash row_nr cell col)
    
    ;; update col
    (puthash col_nr col table)
    ;; Move to the next line and read that line in and continue
    (beginning-of-line)
    (kill-whole-line)
    (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
    ) ;; while result
  (setq row_nr (1+ row_nr))
  ) ;; while iter
;; allocate space in buffer for table
(insert (make-string (1+ row_nr) ?\n))
(dd/log-build-table table row_nr)
)

(defun dd/add-numbers (a b)
  "Add two numbers together"
  (number-to-string (+ (string-to-number a) 
		       (string-to-number b)))
  )

(defun dd/last (a b) "Returns b" b)

(defun dd/log-build-table ( table num_rows)
"Build table from the nested hash map TABLE"
;; Save the state of next-line-add-nelines
(beginning-of-buffer)

(maphash (lambda (key column) (dd/log-build-column key column num_rows))
	 table)
(beginning-of-buffer)
(replace-regexp "^\\(.\\)" "|\\1")
(org-mode)
(org-table-align)
)

(defun dd/log-build-column ( name column num_rows )
"Build the column"
  (end-of-line)
  (insert name "|")
  (forward-line 1)
  (end-of-line)
  (insert "-+-|")
  (let ((r 0)
      value)
    (while (< r num_rows)
      (forward-line)
      (end-of-line)
      (setq value (gethash r column " "))
      (insert  value "|")
      (setq r (1+ r))
      ))
  (beginning-of-buffer)
)

(defun dd/convert-to-seconds ( start-time )
  (setq start_s (float-time (date-to-time (replace-regexp-in-string "-" " " start-time ))))
  (setq curr_s (float-time (date-to-time (replace-regexp-in-string "-" " " (match-string 1) ))))
  (- curr_s start_s)
)

(defun dd/extract-fluid-info () 
"Extracts the result from a DD fluid log."
  (interactive)
  (dd/log-generate-table 
   "Float frame: \\([0-9]+\\)"
   "Finished process \\(.*\\) in \\([0-9]+\\.[0-9]+\\)s"
   "I(Frame,1) R(1,2)")
)

;; (defun dd/extract-fluid-info ()
;;   (copy-lines-matching-re "\\(Float frame: [0-9]+\\)\\|\\(Finished process\\)")
;;   (replace-regexp ".*?Float frame: \([0-9]+\)" "Frame |\1|")
;;   (replace-regexp ".*?Finished process \(.*?\) in \([0-9]+\.[0-9e\-]+s\)" "\1 \2")
;;   (replace-regexp "s\n" "s ")
;;   (replace-regexp "s Frame" "s\nFrame ")

;; )
(defun dd/to-simple-table (name value)
"Convert NAME = VALUE to a table, it expect the buffer to only contain values you want to replace."

;;(interactive "sName to match: \nsValue to match: ")
(interactive 
       (list (read-regexp "Name to match: " "volume") 
	     (read-regexp "Value to match: " "\[0-9.\]+")))

;; "sName to match: \nsValue to match: ")
(let ((pos (point-at-bol) ))
  (goto-char pos)
  (replace-regexp (concat ".*?\\(" name "\\).*?\\(" value "\\).*?") "||\\2|")
  (goto-char pos)
  (insert (concat "#+PLOT: title:\"" name "\" ind:1 type:2d with:lines\n|Frame|" name "|\n|-+-|\n"))
  (let ((count 1))
    (while (search-forward-regexp "^||" (point-max) t)
      (goto-char (+ 1 (point-at-bol)))
      (insert (number-to-string count))
      (setq count (1+ count))
      )
    ;; (org-mode)
    ;; (org-table-align)
    ))
)

(provide 'dd-log-parser)