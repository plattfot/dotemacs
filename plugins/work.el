;;; work.el --- Work related functions
;;; Commentary:

;;; Code:

(require 'highlight-extra)
(require 'multi-term)

;; ============================= Functions ===================================
(defun work-goc ()
"Equivalent to typing go cyclone rd 1 work in the terminal."
(interactive)
(cd "/dd/shows/CYCLONE/RD/0001/user/work.fredriks")
)

(defun work-insert-eigen-pretty-printer ()
"Insert command to add eigen pretty printer for gdb."
(interactive )
(insert "python execfile(\"/dd/dept/software/users/fredriks/swdevl"
	"/CoreLibs/src/gdb/EigenPrettyPrinter.py\")")
)

;; Functions for quickly set up the work environment
(defun work-setup ()
  "Splits the window into three equivalent buffers and set the cwd to swdevl."
  (interactive)
  (cd "~/fredriks/swdevl")
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (toggle-frame-fullscreen))

(defun work-setup-build-fun (terminal-type)
  "Spawns multiple TERMINAL-TYPE.
With the names 3ps, release, cyclone, build and misc"
  (delete-other-windows)
  (split-window-horizontally)
  (cd "/dd/dept/software/users/fredriks/swdevl/3ps")
  (funcall terminal-type)
  (rename-buffer "3ps")
  (highlight-build)
  (cd "/dd/dept/software/users/fredriks/release")
  (funcall terminal-type)
  (rename-buffer "release")
  (highlight-build)
  (cd "/dd/dept/software/users/fredriks/swdevl")
  (funcall terminal-type)
  (rename-buffer "cyclone")
  (highlight-build)
  (highlight-gtest)
  (funcall terminal-type)
  (rename-buffer "misc")
  (highlight-build)
  (funcall terminal-type)
  (rename-buffer "build")
  (highlight-build)
  (highlight-gtest)
  (toggle-frame-maximized))

(defun work-setup-build-term()
  "Spawns multiple multi-terms called cyclone, build and misc."
  (interactive)
  (work-setup-build-fun #'multi-term) ;; #'x short for (function x)
)

(defun work-setup-build()
  "Spawns multiple shells.  Called cyclone, build, misc, release
and git.  Using shell instead of multi-term for all except git."
  (interactive)
  ;; shell doesn't handle git's diff functions therefore I'm using
  ;; multi-term for that.
  (cd "/dd/dept/software/users/fredriks/swdevl")
  (multi-term)
  (rename-buffer "git")
  (work-setup-build-fun #'shell) ;; #'x short for (function x)
)

(defun work-run-emacs-shell-command (command)
  "Runs the COMMMAND in a emacs shell.
Works only if the current buffer is a shell."
  (let ((process (get-buffer-process (current-buffer))))
    (unless process
      (error "No process in %s" buffer-or-name))
    (goto-char (process-mark process))
    (insert command)
    (comint-send-input nil t )))

(defun work-setup-houdini ()
  "Spawns four shells at the cyclone test show.
Using shell instead of multi-term."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (cd "/dd/shows/CYCLONE/RD/0001/user/work.fredriks")

  (shell)
  (rename-buffer "hou-misc")
  (work-run-emacs-shell-command "go cyclone rd 1 =fx work")

  (shell)
  (rename-buffer "hou-beta")
  (work-run-emacs-shell-command "go cyclone rd 1 =fx_beta work")

  (shell)
  (rename-buffer "hou-devl")
  (work-run-emacs-shell-command "go CYCLONE RD 1 =fx work")

  (shell)
  (rename-buffer "hou-test")
  (work-run-emacs-shell-command "go cyclone rd 1 =fx work")

  (toggle-frame-maximized))

;; --------------------------- Source BuildConfig ----------------------------
(defun work-get-version-from-config (name file)
  "Gets the version from a file.
Where NAME is the name of the package you want the version for
and FILE the config to search in."
  ;; Pick the first in the list
  (car
   ;; Remove newlines and convert the string to a list if there's more
   ;; than one version.
   (split-string
    (shell-command-to-string
     ;; Look for the string matching the name.
     (concat "grep -iE " name "_version " file
	     ;; Extract only the version number from the string and
	     ;; pick the last one if there are multiple.
	     " | cut -d = -f 2 | tail -n1"
	     )
     ))))

(defun work-get-version-from-build-config (name path)
  "Gets the version from the BuildConfig file.
Where NAME is the name of the package you want the version for
and PATH is where the BUILD.conf file is located."
  ;; Pick the first in the list
  (work-get-version-from-config  name (concat path "/BUILD.conf"))
)

;; pk
(defun work-pk-project (name)
  "Parse pk files an replace {{ package.project }} with NAME."
  (interactive "sSpecify name of project: ")
  (while (search-forward-regexp "{{[ ]*package\.project.*}}" (point-max) t )
    (let* ((start (match-beginning 0))
	   (end (match-end 0))
	   (str (buffer-substring-no-properties start end))
	   (project (if (string-match "capitalize" str) (capitalize name)
		      (if (string-match "upper" str) (upcase name) name))))
      (kill-region start end)
      (insert project)
      )
    )
)

;; ============================ Registers ====================================
(set-register ?f (cons 'file
		       (concat "/dd/dept/software/users/fredriks/swdevl/"
			       "CoreLibs/Math/Geometry/include/VDB/"
			       "FieldImpl.hpp")))
(set-register ?o
	      (cons 'file
		    (concat "/tools/package/openvdb/"
		    	    (work-get-version-from-build-config
		    	     "openvdb"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone")
		    	    "/include/openvdb/")))

(set-register ?m
	      (cons 'file
		    (concat "/tools/package/openmesh/"
		    	    (work-get-version-from-build-config
		    	     "openmesh"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone")
		    	    "/include/OpenMesh/")))
(set-register ?c (cons 'file "/dd/shows/CYCLONE/RD/0001/user/work.fredriks/"))

(set-register ?h
	      (cons 'file
		    (concat "/tools/package/houdini/"
			    (work-get-version-from-build-config
		    	     "houdini"
			     "/dd/dept/software/users/fredriks/swdevl/cyclone")
			    "/toolkit/include")))
(set-register ?v
	      (cons 'file
		    (concat "/tools/package/eigen/"
		    	    (work-get-version-from-build-config
		    	     "eigen"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone")
		    	    "/include/eigen3/Eigen/src")))
(provide 'work)
;;; work.el ends here
