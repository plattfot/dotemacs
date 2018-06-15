;;; reverse-deps.el --- Trace dependencies on elf binaries
;;; Commentary:

;;; Code:
(require 'ffap)

(defvar rd-tracer "/lib64/ld-linux-x86-64.so.2"
  "Executable for listing all dependencies.")

(defvar rd-direct-deps "readelf -d"
  "Executable for listing direct dependencies.")

(defun rd-deps-tree (&optional filename)
  "Print the dependency tree from the elf FILENAME."
  (interactive)
  (if (and (called-interactively-p 'interactive)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (let (current-prefix-arg)		; we already interpreted it
	(call-interactively ffap-file-finder))
    (or filename (setq filename (ffap-prompter)))
    (let* ((buff (get-buffer-create "*rd-trace*"))
	   (deps (split-string
		  (shell-command-to-string
		   (concat "LD_TRACE_LOADED_OBJECTS=1 " rd-tracer " " filename))
		  "\n")))
      (let ((paths))
	;; Create an alist for the libs to look up their path
	(dolist (dep deps)
	  (if (string-match
	       "\\([[:graph:]]+\\)[ ]+=>[ ]+\\(.+?\\)[ ]+(0x[[:xdigit:]]+)"
	       dep)
	      (add-to-list 'paths (cons (match-string 1 dep)
					(match-string 2 dep)))))
	(switch-to-buffer buff)
	(erase-buffer)
	;; Look up the direct dependencies
	(rd-traverse-deps filename paths)
	;; Remove duplicates
	(sort-lines nil (point-min) (point-max))
	(delete-duplicate-lines (point-min) (point-max))
	;; Add graph header
	(goto-char (point-min))
	(insert (format "digraph \"%s\" {\n" (file-name-nondirectory filename)))
	;; Add closing bracket
	(goto-char (point-max))
	(insert "}")
	))
    ))

(defun rd-traverse-deps (filename paths)
  "Recursively print the dependencies of FILENAME.
Using PATHS to look up the path to the dependencies.
It will print directly into the current buffer."
  (let ((direct-deps
	 (split-string
	  (shell-command-to-string (concat rd-direct-deps " " filename)) "\n")))

    (dolist (dep direct-deps)
      (if (string-match "(NEEDED).*?\\[\\(.*?\\)\\]" dep)
	  (let ((lib (match-string 1 dep)))
	    (insert (format "\"%s\" ->\"%s\"\n" (file-name-nondirectory filename) lib))
	    ;; If it has valid path keep going
	    (let ((path (cdr (assoc lib paths))))
	     (if path (rd-traverse-deps path paths))))))))

(provide 'reverse-deps)
;;; reverse-deps.el ends here
