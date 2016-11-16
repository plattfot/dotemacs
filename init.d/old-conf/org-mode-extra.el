(defun org/add-column (line key table)
"Add the second column to the table"
(when (string-match key line)
  (let* ((col_name (match-string 1 line))
  	 (col (gethash col_name table))
  	 (data (chomp(buffer-substring-no-properties 
  	 	      (search-forward "|") 
  	 	      (1- (search-forward "|")))))
	 )
 
     ;; Add data to column
    (push data col)
    ;; Update the column in the table
    (puthash col_name col table)
    )
  )
)
;; From http://stackoverflow.com/questions/17066169/retrieve-keys-from-hash-table-sorted-by-the-values-efficiently
(defun org/hash-table-keys (hash-table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun org-table-build (table)
  "Build a table from TABLE, assume that TABLE consist of a
hash to each row that in turn has a hash to each column"
  (let ((header nil)
	(start-header (point)))
    (maphash (lambda (row cols)
	       (insert "|" row "|")
	       ;; Sort the keys 
	       (let ((keys (sort (org/hash-table-keys cols)
				 (lambda (k0 k1) (string<  k0 k1)))))
		 ;; Select the row with most columns to be the header
		 (when (> (length  keys) (length  header)) (setq header keys))
		 ;; Iterate over the row and insert the columns
		 (dolist (key keys)
		   (insert (car (gethash key cols)) "|")
		   )
		 )
	       ;; 
	       (newline)
	       ) table)
    (newline)

    ;; Create Header
    (goto-char start-header)
    (insert "| |")
    (dolist (col header) (insert col "|"))
    (newline)
    (insert "|-+-|")
    (newline)
    (goto-char start-header)
    (org-table-align)
    )
  )

(defun org-table-build-simple (table)
  "Build a table from TABLE assume that it has a hash to each row
and the hash contain a list of the columns" 
  (let ((start_build (point))
	(end))
    (maphash (lambda (row cols) 
	       (insert "|" row "|") 
	       (insert (mapconcat 'identity (nreverse cols) "|") "|")
	       (newline)) 
	     table )
    (goto-char start_build)
    (org-table-align)
    (setq end (org-table-end))
    (goto-char end)
  )
)

(defun org/get-row-key (row_name col_re)
  "Get the key associated with the ROW_NAME, i.e remove the column key from the row"
  (replace-regexp-in-string col_re "" row_name)
  )

(defun org-table-pack (rows col_re)
  "Packs a Nx2 table into a RxC table where R is the unique rows
   matching the entries in ROWS_IN and C is the columns matching the
   COL_RE. ROWS can contain regexp for each entry. But not a comma
   since that's use to sepparate the entries. It expects the
   layout of the Nx2 table to be: 
   |<row name and col identifier>|<data>|

   for example:
   This command:
   (org/pack-table \"r1,r2\" \"c[0-9]+\")
   
   On this table:
   | r1c1| 0x1 |
   | r1c0| 0x0 |
   | r2c0| 0x2 |
   | c1r2| 0x3 |
   | r2c2| 0x4 |
   
   Will give:
   |    |  c0 |  c1 |  c2 |
   |----+-----+-----+-----|
   | r1 | 0x0 | 0x1 |     |
   | r2 | 0x2 | 0x3 | 0x4 |"
  
  (interactive  
   (list (read-regexp "Enter name for each entry: ")
	 (read-regexp "Enter regex for identifying columns: " )))

  ;; Switch to org-mode 
  (org-mode)
  (define-hash-table-test 'str-hash 'string-equal 'sxhash)
 
  (let* ((start (if (use-region-p) (region-beginning) (point)))
	 (end (if (use-region-p) (region-end) (point-max)))
	 (rows (mapcar 'chomp (split-string rows ",")))
	 (col_re (format "\\(%s\\)" col_re))
	 ;; Create the hash table
	 (table (make-hash-table :test 'str-hash ))
	 ;; Create the regex for identifying the rows
	 (row_re (concat "\\(" (mapconcat 'identity rows "\\|")"\\)")))

    ;; Go to the beginning of the region
    (goto-char start)
    ;; Populate the table by searching each line
    (while (search-forward-regexp col_re end t)
      (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	(when (string-match row_re line)
	  (let* ((row_key (org/get-row-key (match-string 1 line) col_re)) 
		 ;; Get the hash table for the row, create it if it doesn't exist 
		 (row (if (gethash row_key table) 
			  (gethash row_key table)
			(progn 
			  (puthash row_key (make-hash-table :test 'str-hash) table)
			  (gethash row_key table)))))
	    (org/add-column line col_re row)
	    )
	  )
	)
      )
    (goto-char start)
    (org-table-build table)
    (goto-char start)
    )
)

(defun org-table-copy-field-to-string ()
    "Copy field to string"
  (buffer-substring-no-properties 
   (1+ (search-backward-regexp "[|+]")) 
   (1- (search-forward-regexp "[|+]" nil t 2)))
)
;; Modified org-table-goto-column to include |-----+ cells
(defun org-table-goto-column-inclusive (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line, including separator fields.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
		(or (search-forward-regexp "[|+]" (point-at-eol) t)
		    (and force
			 (progn (end-of-line 1)
				(skip-chars-backward "^|")
				(insert " | ")
				t)))))
    (when (and force (not (looking-at ".*|")))
      (save-excursion (end-of-line 1) (insert " | ")))
    (if on-delim
	(backward-char 1)
      (if (looking-at " ") (forward-char 1)))))

(defun org-table-merge (col &optional ind)
  " Merge column specified by COL from all the tables in the file.
IND specifies what column to use as reference, it defaults to the first column (0),
Note that the entries used as keys, the ind column, are case sensitive.

For example:
(org-table-merge 1)

On these tables:
|    |  c0 |  c1 |
|----+-----+-----|
| r2 | 0x0 | 0x1 |
| r1 | 0x2 | 0x3 |
| r3 |     | 0x4 | 

|    |  c0 |  c1 |
|----+-----+-----|
| r1 | 0x7 | 0x8 |
| r2 | 0x5 | 0x6 |
| r3 | 0x9 | 0x1 |

Will give
|    |  c0 |  c0 |
|----+-----+-----|
| r2 | 0x0 | 0x5 |
| r1 | 0x2 | 0x7 |
| r3 |     | 0x9 |"

  (interactive (list (read-string "Specify column to merge: ")))

  (when (not ind) (setq ind 1))

  ;; Switch to org mode
  (org-mode)
  ;; Setup the hash table
  (define-hash-table-test 'str-hash 'string-equal 'sxhash)
  ;; let* evaluate at point
  (let* ((start (if (use-region-p) (region-beginning) (point)))
         (curr start)
	 (end (if (use-region-p) (region-end) (point-max)))
         (col (string-to-int col))
	 ;; Create the hash table
	 (table (make-hash-table :test 'str-hash )))
    ;; Iterate from current position to eof
    (while (search-forward-regexp "^[ ]*|" end t)
      ;; Jump to the index field
      (org-table-goto-column-inclusive ind)
       ;;      Get the key that identify the row.
      (let* ((key (chomp (org-table-copy-field-to-string)))
	     ;; Get the row form the table
	     (row (gethash key table))
	     ;; Specify local variable for the data
	     (data))
	;; Move to the column specified by the user
	(org-table-goto-column-inclusive col)
	;; Copy the data from that field
	(setq data (org-table-copy-field-to-string))
	;; Add data to row
	(push data row)
	;; Update the row in the table
	(puthash key row table)
	)
      )
    (goto-char start)
    (org-table-build-simple table)
    )
)

(defun org-table-create-header-regexp (regex match-group)
  "Create header from matching regex"
  (interactive (list (read-string "Regex for header: ")
		     (read-string "Group to match (default 0): " nil "0")))
  (let ((header nil)
	(start (point))
	(match-group (string-to-int match-group)))
    (while (search-forward-regexp regex (point-max) t)
       (push (match-string-no-properties match-group) header)
    )

    (goto-char start)
    (insert "|")
    (insert (mapconcat 'identity (nreverse header) "|"))
    (insert "|\n|-+-|")
    (goto-char start)
    (org-table-align)
  )
)

;; (defun org-table-zip (&optional ind)
;; "Zip tables together. IND is optional and specifies which column to use as header.
;; It assumes that the columns matches up.

;; For example:
;; (org-table-zip 1)

;; On these tables:
;; | h0 |  c0 |  c1 |
;; |----+-----+-----|
;; | r2 | 0x0 | 0x1 |
;; | r1 | 0x2 | 0x3 |
;; | r3 |     | 0x4 | 

;; | h1 |  c0 |  c1 |
;; |----+-----+-----|
;; | r1 | 0x7 | 0x8 |
;; | r2 | 0x5 | 0x6 |
;; | r3 | 0x9 | 0x1 |

;; Will give
;; | r1 |  c0 |  c1 |
;; |----+-----+-----|
;; | h0 | 0x2 | 0x3 |
;; | h1 | 0x7 | 0x8 |

;; | r2 |  c0 |  c1 |
;; |----+-----+-----|
;; | h0 | 0x0 | 0x1 |
;; | h1 | 0x5 | 0x6 |

;; | r3 |  c0 |  c1 |
;; |----+-----+-----|
;; | h0 |     | 0x4 | 
;; | h1 | 0x9 | 0x1 |
;; "
;; (interactive "p")
;; (when (not ind) (setq ind 1))

;; ;; Switch to org mode
;; (org-mode)
;; ;; ;; Setup the hash table
;; ;; (define-hash-table-test 'str-hash 'string-equal 'sxhash)
;; ;; ;; let* evaluate at point
;; ;; (let* ((start (if (use-region-p) (region-beginning) (point)))
;; ;;        (curr start)
;; ;;        (end (if (use-region-p) (region-end) (point-max)))
;; ;;        (col (string-to-int col))
;; ;;        ;; Create the hash table
;; ;;        (table (make-hash-table :test 'str-hash )))
;; ;;   ;; Iterate from current position to eof
;; ;;   (while (search-forward-regexp "^[ ]*|" end t)
;; ;;     ;; Jump to the index field
;; ;;     (org-table-goto-column-inclusive ind)
;; ;;     ;;      Get the key that identify the row.
;; ;;     (let* ((key (chomp (org-table-copy-field-to-string)))
;; ;; 	   ;; Get the row form the table
;; ;; 	   (row (gethash key table))
;; ;; 	   ;; Specify local variable for the data
;; ;; 	   (data))
;; ;;       ;; Move to the column specified by the user
;; ;;       (org-table-goto-column-inclusive col)
;; ;;       ;; Copy the data from that field
;; ;;       (setq data (org-table-copy-field-to-string))
;; ;;       ;; Add data to row
;; ;;       (push data row)
;; ;;       ;; Update the row in the table
;; ;;       (puthash key row table)
;; ;;       )
;; ;;     )
;; ;;   (goto-char start)
;; ;;   (org-table-build-simple table)
;; ;;   )
;; ;; let* evaluate at point
;; (let* ((start (if (use-region-p) (region-beginning) (point)))
;;        (end (if (use-region-p) (region-end) (point-max)))
;;        (curr start)
;;        (key))
;;   (goto-char start)
;;   ;; Goto the first line
;;   (search-forward-regexp "^[ ]*|" end t)
;;   (goto-char (org-table-begin))
;;   (org-table-goto-column-inclusive ind)
;;   (setq key (chomp (org-table-copy-field-to-string)))
;;   (while (org-table-ne))
  
;;   )
;; )

