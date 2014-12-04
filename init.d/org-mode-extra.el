(defun org/add-column (line key table)
"Add the second column to the table"
(when (string-match key line)
  (let* ((col_name (match-string 1 line))
  	 (col (gethash col_name table))
  	 (data (chomp(buffer-substring-no-properties 
  	 	      (1+ (search-forward "|")) 
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
  "Build a table from the hash"
  (maphash (lambda (row cols)
	     (insert "|" row "|")
	     ;; Sort the keys 
	     (let ((keys (sort (org/hash-table-keys cols) (lambda (k0 k1) (string<  k0 k1)))))
	       (dolist (key keys)
		 (insert (car (gethash key cols)) "|")
		 )
	       )
	     (newline)
	     ) table)
)

(defun org-table-pack (rows col_re)
  "Packs a Nx2 table into a RxC table where R is the unique rows
   matching the entries in ROWS and C is the columns matching the
   COL_RE. It expects the layout of the Nx2 table to be:
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
   |r1| 0x0 | 0x1 |     |
   |r2| 0x2 | 0x3 | 0x4 |"
  
  (interactive  
   (list (mapcar 'chomp (split-string (read-regexp "Enter name for each entry: ") ","))
	 (format "\\(%s\\)" (read-regexp "Enter regex for identifying columns: " ))))

  ;; Switch to org-mode 
  (org-mode)
  (define-hash-table-test 'str-hash 'string-equal 'sxhash)
 
  (let ((start (point))
	;; Create the hash table
	(table (make-hash-table :test 'str-hash ))
	;; Create the regex for identifying the rows
	(row_re (concat "\\(" (mapconcat 'identity rows "\\|")"\\)")))
    
    ;; Add hash table for each row
    (dolist (row rows) (puthash row (make-hash-table :test 'str-hash) table))
 
    ;; Populate the table by searching each line
    (while (search-forward-regexp col_re (point-max) t)
      (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	(when (string-match row_re line)
	  (let* ((row_name (match-string 1 line)) 
		(row (gethash row_name table)))
	    (org/add-column line col_re row)
	    )
	  )
	)
      )
    (goto-char start)
    (org-table-build table)    
    )
)
