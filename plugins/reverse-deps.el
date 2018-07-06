;;; reverse-deps.el --- Trace dependencies on elf binaries
;;; Commentary:

;;; Code:
(require 'ffap)
(require 's)

(defvar rd-tracer "/lib64/ld-linux-x86-64.so.2"
  "Executable for listing all dependencies.")

(defvar rd-direct-deps "readelf -d"
  "Executable for listing direct dependencies.")

(defun rd-deps-tree (&optional elf)
  "Print the dependency tree from the ELF binary."
  (interactive)
  (if (and (called-interactively-p 'interactive)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (let (current-prefix-arg)		; we already interpreted it
	(call-interactively ffap-file-finder))
    (or elf (setq elf (ffap-prompter)))
    (let* ((buff (get-buffer-create "*rd-trace*")))
      (let ((paths (rd-compute-paths elf)))
	(switch-to-buffer buff)
	(erase-buffer)
	;; Look up the direct dependencies
	(rd-traverse-deps elf paths)
	;; Remove duplicates
	(sort-lines nil (point-min) (point-max))
	(delete-duplicate-lines (point-min) (point-max))
	;; Add graph header
	(goto-char (point-min))
	(insert (format "digraph \"%s\" {\n" (file-name-nondirectory elf)))
	;; Add closing bracket
	(goto-char (point-max))
	(insert "}")
	))
    ))

(defun rd-compute-paths (elf)
  "Compute the path for each dependency of ELF."
  ;; Create an alist for the libs to look up their path
  (let ((paths)
	(deps (s-split "\n"
		       (shell-command-to-string
			(concat "LD_TRACE_LOADED_OBJECTS=1 " rd-tracer " " elf)))))
    ;; Create an alist for the libs to look up their path
    (dolist (dep deps)
      (let ((match (s-match
		    "\\([[:graph:]]+\\)[ ]+=>[ ]+\\(.+?\\)[ ]+(0x[[:xdigit:]]+)"
		    dep)))
	(if match (add-to-list 'paths (cons (nth 1 match) (nth 2 match))))))
    paths
    ))

(defun rd-traverse-deps (elf paths)
  "Recursively print the dependencies of ELF.
Using PATHS to look up the path to the dependencies.
It will print directly into the current buffer."
  (let ((direct-deps
	 (s-split "\n"
	  (shell-command-to-string (concat rd-direct-deps " " elf)))))

    (dolist (dep direct-deps)
      (let ((match (s-match "(NEEDED).*?\\[\\(.*?\\)\\]" dep)))
      (if match
	  (let ((lib (nth 1 match)))
	    (insert (format "  \"%s\" ->\"%s\"\n" (file-name-nondirectory elf) lib))
	    ;; If it has valid path keep going
	    (let ((path (cdr (assoc lib paths))))
	     (if path (rd-traverse-deps path paths)))))))))

(provide 'reverse-deps)
;;; reverse-deps.el ends here
