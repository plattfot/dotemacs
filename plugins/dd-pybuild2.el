;;; dd-pybuild2 --- Helper functions for pb2

;;; Commentary:

;;; Code:
(defun dd/pybuild2-project-path (root-project)
  "Prints the project path.
Where ROOT-PROJECT defines the root."
  (let* ((dir (pwd))
	 (start (string-match root-project dir))
	 (end (length dir)))
    ;; Remove "directory " from the path
    (setq dir (substring dir start end))
    (concat "^" dir)
    ))


(defun dd/pybuild2-new-project (&optional root-project)
"Create a new template PROJECT file.
Where ROOT-PROJECT specifies the name of the project."
(interactive "sSpecify root package: ")
;; notify emacs to use python-mode for the PROJECT file
(if (> (length root-project) 0)
    (insert (concat "# -*- python -*-\n\n"
		    "Project(\n"
		    "  includes = ['^" root-project "/BUILD.conf'],\n"
		    "  dependencies = [\n"
		    "    '^Make'\n"
		    "  ],\n"
		    "  tests = ['tests']\n"
		    ")\n"))
  (insert "# -*- python -*-\n\n"
	  "Project(\n"
	  "  dependencies = [\n"
	  "    '^Make'\n"
	  "  ],\n"
	  "  tests = ['tests']\n"
	  ")\n"))
;; Change to python mode
(python-mode)
)

(defun dd/pybuild2-new-virtual-project ()
"Create a new template PROJECT file, containing a virtual project."
(interactive)
;; notify emacs to use python-mode for the PROJECT file
(insert "# -*- python -*-\n\n"
	"VirtualProject(\n"
	"  dependencies = [\n"
	"  ]\n"
	")\n")
;; Change to python mode
(python-mode)
)
(provide 'dd-pybuild2)
;;; dd-pybuild2 ends here
