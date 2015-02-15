
(defun hou-open-other-version (version)
"Will open the same hdk file but for the version specified by
VERSION, only works if the open buffer is an hdk file." 
(interactive "sEnter houdini version: ")
(let ((file-path (buffer-file-name))
      (hou-regexp "\\(.*?/houdini/\\)[0-9]+\\.[0-9]+\\.[0-9]+\\(/.*\\)+"))
  (if file-path 
      (progn (if (string-match hou-regexp file-path)
		 (progn (find-file (concat
				    (match-string 1 file-path)
				    version
				    (match-string 2 file-path)
				    ))
			(rename-buffer (concat (file-name-nondirectory file-path)
					       "<" version ">"))
			)
	       (message "File is not a houdini file")))
    (message "File isn't saved to disk")
    )
  )
)
