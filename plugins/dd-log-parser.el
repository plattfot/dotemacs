;;; dd-log-parser --- Functions for parsing the dd log
;;; Commentary:

;;; Code:

;; From http://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun dd-log--chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)
;; Based on http://stackoverflow.com/questions/3243035/define-average-in-lisp
(defun dd-log--average (ns)
    "Average the values in the list NS."
    (/ (apply '+ ns) (length ns)))

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

(defun dd-log-extract-info (re)
  "Extracts all lines matching the regexp RE.
It will place them in a buffer named *matching* and strip the DD_LOG header."
(interactive "sEnter regexp to match: ")
(copy-lines-matching-re re)
(goto-char (point-min))
;; Strip DD_LOG header
(while (re-search-forward "[| ]*\\(\\+-\\)*\\[.*\\]:" (point-max) t)
  (replace-match "")))

(defun dd-log-build-table-with-methods (method table first_header compute_avg)
  "Build the table METHOD based on the data in TABLE.
FIRST_HEADER is as the name applies the first header in the
table.  If COMPUTE_AVG is true (t) it will compute the average
for each row."
  (org-mode)
  (insert "# Method: " method ".\n")
  (let ((avg_list '())
	(num_cols 0)  ;; number of columns of the table
	(num_rows 0)) ;; number of rows of the table
    
    (maphash (lambda (key values)
	       ;; Compute the number if columns
	       (setq num_cols (max num_cols (length values)))
	       ;; Count rows
	       (setq num_rows (1+ num_rows))
	       (when compute_avg
		 (push (number-to-string
			(dd-log--average (mapcar 'string-to-number values)))
		       avg_list))
	       (insert "|" key "|" (mapconcat 'identity (nreverse values) "|"))
	       (newline))
	     table)

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
    
    (let ((count 0))
      (while (< count num_cols)
	(org-table-next-field)
	(insert " run " (int-to-string count))
	(setq count (1+ count))))
    
    ;; Add column for the average value at the end of the table
    (when compute_avg
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
	(forward-line (- num_rows))))

    ;; Add divider, reason for not doing this with the header is that
    ;; navigation becomes much harder.
    (end-of-line)
    (newline)
    (insert "|-+-|")
    (org-table-align)
    (let ((begin (point))
	  end)
      (forward-line num_rows)
      (end-of-line)
      (setq end (point))
      (sort-lines nil begin end))
    (newline)))

(defun dd-log-generate-table-with-methods ( name_row name_col methods 
					    &optional first_header compute_avg) 
  "Generate a table for each method.
Call from the buffer that contains the dd log.  Generates
table/tables from that log by extracting the regexp matcing the
NAME_ROW, NAME_COL and METHODS.  Place paranthesis \\(\\) around
the thing you want to be in the each cell i.e Frame \\([0-9]+\\).
Note that the regexp for each must only match one thing per
iteration.\n\n NAME_ROW specifies which row of the table it
should place data, for example frame number or number of
secondary grids used.  The value inside the parenthesis must be
unique for each row and it will be placed in the first
column.\n\n NAME_COL specifies what to put in the columns.\n\n
METHODS specify each method it should look for, for example
running the fluid sim with reseeding and split/merge a second
time.  Note that the method must be unique for the key, value set
and can only be on one line.  For example running the fluid sim
first with reseeding, then with split/merge and then with both
will not work.

Optional arguments are FIRST_HEADER, which should be the main
row.  COMPUTE_AVG, set to true and it will compute the average
for each row."
  (interactive (list (read-string "String that classifies each iteration: ")
		     (read-string "String to match the result: ")	
		     (read-string "Enter name for each method, separated by comma: ")
		     (read-string "Enter name for the first column: ")
		     (y-or-n-p "Compute average? ")))
  ;; Process arguments
  (setq methods (split-string methods ",") )
  (setq methods (sort methods 'string<))
  ;; Remove whitespaces from beginning and end.
  (setq methods (mapcar 'dd-log--chomp methods))
					;: Prepare for regexp string

  ;; Regexp for extracting the data
  (let* ((methods_pad (mapcar (lambda (a) (format "\\(%s\\)" a)) methods))
	 (re_types (mapconcat 'identity methods_pad "\\|"))
	 (re_value  (concat "\\(" name_col "\\)") )
	 (re_key (concat "\\(" name_row "\\)") ))
    ;; Extract all lines matching the three variables into a separate buffer
    (dd-log-extract-info (mapconcat 'identity (list re_value re_key re_types) "\\|")))

  ;; Setup hash table to put the data in.
  (define-hash-table-test 'str-hash 'string-equal 'sxhash)

  ;; Will contain all the tables where each table is mapped to the method
  (let ((tables (make-hash-table :test 'str-hash :size (length methods) )))
    ;; populate tables with a hash table for each method
    (dolist (method methods)
      ;; Extract method name
      (setq method (substring method 
			      (1+ (string-match "(" method)) 
			      (string-match "\\\\)" method)) )
      (puthash method (make-hash-table :test 'str-hash ) tables))
    (goto-char (point-min))
    ;; Remove value duplicates.
    ;; Based on http://www.emacswiki.org/emacs/DuplicateLines
    (while (re-search-forward (concat "^\\(.*" name_col ".*\n\\)\\1+") (point-max) t)
      (replace-match "\\1"))
    
    (goto-char (point-min))
      (while (not (= (line-beginning-position) (line-end-position)))
	(let* ((begin (line-beginning-position))
	       (end (point-at-eol 3))
	       (input (buffer-substring-no-properties begin end ))
	       (method_used "")
	       (row) (col))
	  ;; Need to read in region, parse the row, col and method (if it exist)
	  ;; and add that to the correct table
	  
	  ;; Read line from buffer
	  (string-match name_row input)
	  (setq row (match-string 1 input))
	  (string-match name_col input) 
	  (setq col (match-string 1 input))
	  (setq method_used "")

	  (dolist (method methods)
	    (when(string-match method input) 
		(progn (setq method_used (match-string 1 input)) (return))))

	  (when (> (string-width method_used) 0)
	      ;; Get the hash table associated with the method name
	      (let* ((table (gethash method_used tables))
		     (bucket (gethash row table))) ;; Get the row for that table
		;; Add the column value to that row
		(push col bucket)
		;; Update the row in the table
		(puthash row bucket table)))
	  (delete-region begin end)
	  (kill-line)))

      ;; Iterate over the tables.
      (maphash (lambda (key table) 
		 (dd-log-build-table-with-methods key table first_header compute_avg ))
	       tables)))

;; Functions that uses dd-log-generate-table to extract info
(defun dd-log-extract-particle-count ()
"Extracts the particle count from a fluid sim by parsing the dd log."
(interactive)
(dd-log-generate-table-with-methods
 "Float frame: \\([0-9]+\\)"
 "total number of particles: \\([0-9]+\\)"
 "Starting process Get input \\(fluid\\)"
 "Frame")
)

(defun dd-log-extract-volumetrics-info () 
  "Extract info about volumetricss from the DD logger."
  (interactive)
  (dd-log-generate-table-with-methods 
   "Number of Secondary Grids to raster \\([0-9]+\\)"
   "Finished process Volume rasterization in \\([0-9]+\\.[0-9]+\\)s"
   "Using \\(template specialization\\)\\., Using \\(inheritance\\)\\."
   "SG"
   t) ;; SG = Secondary Grids
)

;; Work in progress
(defun dd-log-parse-layout (layout type default) 
  "Parse the input and return the layout in a list. 
For example LAYOUT = \"I(1,2) R(Test,1)\" type = I, will
return (1 2), TYPE = R will return (\"Test\" 1) if it fails to
make sense of the input it will return the DEFAULT"
  (interactive)
  (let* ((re "\\s-*\\([a-zA-Z0-9 ]+?\\)\\s-*")
	 (matcher (concat type "(" (mapconcat 'identity (list re re) ",") ")") ))

    (if (string-match matcher layout)
	(let* ((first (match-string 1 layout))
	       (second (match-string 2 layout))
	       (first_digi (string-to-number first))
	       (second_digi (string-to-number second)))
	  (when (not (= first_digi 0)) (setq first first_digi))
	  (when (not (= second_digi 0)) (setq second second_digi))
	  (list first second))
      default )))

(defun dd-log-const_extractor (const input )
  "Return what is in CONST and skips INPUT."
  (interactive)
  const)

(defun dd-log-extractor (group input )
  "Return the match from GROUP in INPUT."
  (interactive)
  (match-string group input))

(defun dd-log-extract-func (input)
"Return the const-extractor if the INPUT is an int else the extractor."
(interactive)
(if (stringp input)
    'dd-log-const_extractor
  'dd-log-extractor))

(defun dd-convert-to-re (layout)
  "Convert integers to re groups in the LAYOUT.
For example 1 -> \\1"
  (mapcar (lambda (l)
	    (if (numberp l) (concat "\\" (number-to-string l)) 
	      (replace-regexp-in-string "^\\([0-9]+\\)$" "\\\\1" l))) 
	  layout))

(defun dd-log-generate-table-last ( iter_re result_re &optional match_layout) 
  "Same as log-generate-table except this one set CELL-OPERATOR to dd-last.

Which mean that if there are more than one entry per iteration
the last encounter will be stored. 

ITER_RE regexp expression that classifies each iteration.
RESULT_RE regexp expression that match the result.
MATCH_LAYOUT Specifies the layout for the table for the iteration and result type."
  (interactive 
   (list (read-string "String that classifies each iteration: ")
	 (read-string "String to match the result: ")	
	 (read-string "Enter layout (default I(Iteration,1) R(Result,1)): ")))
  (dd-log-generate-table iter_re result_re match_layout 'dd-last ))

(defun dd-log-generate-table ( iter_re result_re &optional match_layout cell-operator) 
  "Generate a table from a DD log buffer.

Call from the buffer that contains the dd log.  Generates a table
from that log by extracting the regexp matcing the ITER_RE and
RESULT_RE.\nThe ITER_RE should be something that is unique for
each iteration and also mark the begin on each iteration.  A good
example is matching the frame number i.e frame \\([0-9]+\\) since
it's usually in the beginning of the iteration and it's unique.\n
RESULT_RE should match everything you want to extract during the
iteration.  An example is to extract all Finished process.  Both
regexp strings expects atleast one group and that's for what to
put in the table and another can be used for the name of that
column.\n In order to know which one is which an optional
parameter MATCH_LAYOUT specifies the layout and expects the
following syntax I/R(x,y) separated by whitespace.  The I specify
the the layout for ITER_RE and R for RESULT_RE.  x is what group
the name of the result is in could be a number or a str.  If it's
a string it will use that as the name.  y can only be an integer
specifying the group of what to put in the table.  It will
default to I(Iteration,1) R(Result,1).  CELL-OPERATOR determine
how it will deal with collisions in each cell.  Options are to
pick the last one, average or sum."
  (interactive
   (list (read-string "String that classifies each iteration: ")
	 (read-string "String to match the result: ")	
	 (read-string "Enter layout (default I(Iteration,1) R(Result,1)): ")))
  
  ;; Process arguments
  (let ((iter_layout (list "Iteration" 1))
	(result_layout (list "Result" 1) ))
    (when (not cell-operator) (setq cell-operator 'dd-add-numbers))

    (when match_layout
      (setq iter_layout (dd-log-parse-layout match_layout "I" iter_layout)
	    result_layout (dd-log-parse-layout match_layout "R" result_layout )))

    ;; Regexp for extracting the data
    (let ((re_value  (concat "\\(" iter_re "\\)") )
	  (re_key (concat "\\(" result_re "\\)") ))
      ;; Extract all lines matching the three variables into a separate buffer
      (copy-lines-matching-re (mapconcat 'identity (list re_value re_key) "\\|")))

    ;; Setup hash table to put the data in.
    (define-hash-table-test 'str-hash 'string-equal 'sxhash)
    (let ((table (make-hash-table :test 'str-hash )))
      
      (let (start end)
	(goto-char (point-min))
	(setq start (point))
	(search-forward-regexp iter_re )
	(beginning-of-line)
	(setq end (point))
	(kill-region start end))

      (let ((iter_first_func (dd-log-extract-func (car iter_layout)))
	    (iter_second_func (dd-log-extract-func (cdr iter_layout)))
	    (result_first_func (dd-log-extract-func (car result_layout)))
	    (result_second_func (dd-log-extract-func (cdr result_layout)))
	    (row_nr 0)
	    col_nr
	    col
	    data)

	;; Get the header for the iteration
	(goto-char (point-min))
	(while (search-forward-regexp iter_re (point-max) t) 
	  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	    (string-match iter_re line)
	    (setq col_nr (funcall iter_first_func (car iter_layout) line)
		  data (funcall iter_second_func (nth 1 iter_layout) line)
		  col (gethash col_nr table)))

	  ;; column doesn't exist create it first
	  (when (not col) (setq col (make-hash-table :test 'equal))) 

	  (puthash row_nr data col)
	  (puthash col_nr col table)

	  (beginning-of-line)
	  (kill-whole-line)
	  
	  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	    (while (string-match result_re line)

	      (setq col_nr (funcall result_first_func (car result_layout) line)
		    data (funcall result_second_func (nth 1 result_layout) line)
		    col (gethash col_nr table))

	      ;; column doesn't exist create it first
	      (when (not col) (setq col (make-hash-table :test 'equal))) 
	      
	      (let ((cell (gethash row_nr col)))
		;; If cell already has a value add the data to it
		(if cell
		    (setq cell (funcall cell-operator cell data))
		  (setq cell data))

		;; update cell 
		(puthash row_nr cell col))
	      
	      ;; update col
	      (puthash col_nr col table)
	      ;; Move to the next line and read that line in and continue
	      (beginning-of-line)
	      (kill-whole-line)
	      (setq line (buffer-substring-no-properties
			  (point-at-bol) (point-at-eol)))))
	  (setq row_nr (1+ row_nr)))

	;; allocate space in buffer for table
	(insert (make-string (1+ row_nr) ?\n))
	(dd-log-build-table table row_nr)))))

(defun dd-add-numbers (a b)
  "Add the numbers A and B together."
  (number-to-string (+ (string-to-number a) (string-to-number b))))

(defun dd-last (a b) "Ignore A, return B." b)

(defun dd-log-build-table ( table num_rows)
  "Build table from the nested hash map TABLE. 
NUM_ROWS specifies number of rows in the table"
;; Save the state of next-line-add-nelines
(goto-char (point-min))

(maphash (lambda (key column) (dd-log-build-column key column num_rows)) table)
(goto-char (point-min))

(while (re-search-forward "^\\(.\\)" (point-max) t)
      (replace-match "|\\1"))

(org-mode)
(org-table-align))

(defun dd-log-build-column ( name column num_rows )
  "Build the column specified by the input.  
NAME will be the header for the column.  COLUMN is the if for the
column.  NUM_ROWS specifies number of rows in the column."
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
      (setq r (1+ r))))
  (goto-char (point-min)))

(defun dd-convert-to-seconds ( start-time )
  "Convert START-TIME to seconds."
  (let* ((start_s (float-time (date-to-time 
			       (replace-regexp-in-string "-" " " start-time ))))
	 (curr_s (float-time (date-to-time 
			      (replace-regexp-in-string "-" " " (match-string 1) )))))
    (- curr_s start_s)))

(defun dd-extract-fluid-info () 
  "Extracts the result from a DD fluid log."
  (interactive)
  (dd-log-generate-table 
   "Float frame: \\([0-9]+\\)"
   "Finished process \\(.*\\) in \\([0-9]+\\.[0-9]+\\)s"
   "I(Frame,1) R(1,2)")
)

;; (defun dd-extract-fluid-info ()
;;   (copy-lines-matching-re "\\(Float frame: [0-9]+\\)\\|\\(Finished process\\)")
;;   (replace-regexp ".*?Float frame: \([0-9]+\)" "Frame |\1|")
;;   (replace-regexp ".*?Finished process \(.*?\) in \([0-9]+\.[0-9e\-]+s\)" "\1 \2")
;;   (replace-regexp "s\n" "s ")
;;   (replace-regexp "s Frame" "s\nFrame ")

;; )
(defun dd-to-simple-table (name value)
  "Convert NAME = VALUE to a table.  
It expect the buffer to only contain values you want to replace."

;;(interactive "sName to match: \nsValue to match: ")
(interactive 
 (list (read-regexp "Name to match: " "volume") 
       (read-regexp "Value to match: " "\[0-9.\]+")))

;; "sName to match: \nsValue to match: ")
(let ((pos (point-at-bol) )
      (re (concat ".*?\\(" name "\\).*?\\(" value "\\).*?")))
  (goto-char pos)

  (while (re-search-forward re (point-max) t) (replace-match "|\\2"))

  (goto-char pos)
  (insert (concat "#+PLOT: title:\"" name 
		  "\" ind:1 type:2d with:lines\n|Frame|" name "|\n|-+-|\n"))
  (let ((count 1))
    (while (search-forward-regexp "^||" (point-max) t)
      (goto-char (+ 1 (point-at-bol)))
      (insert (number-to-string count))
      (setq count (1+ count))))))

(defun dd-generate-solver-info-table ()
"Generate table for how long the preconditioner took for each frame."
(interactive)
(let ((types '("\\(?1:Max iteration\\) = \\(?2:[0-9]+\\)"
	       "\\(?1:best residual\\) = \\(?2:[0-9]+\\.[0-9e-]+\\)"
	       "\\(?1:Number of cells\\): \\(?2:[0-9]+\\)"
	       "Finished process \\(?1:Initialize preconditioner\\) in \\(?2:[0-9]+\\.[0-9]+\\)s"
	       "Finished process \\(?1:Solve linear system\\) in \\(?2:[0-9]+\\.[0-9]+\\)s"
	       "Finished process \\(?1:Dimension-Reduced pressure solver\\) in \\(?2:[0-9]+\\.[0-9]+\\)s"
	       )))
  (dd-log-generate-table-last 
   "Float frame: \\([0-9]+\\)"
   (concat "\\(?:" (mapconcat 'identity types "\\|")"\\)")
   "I(Frame,1) R(1,2))")))

(provide 'dd-log-parser)
;;; dd-log-parser ends here
