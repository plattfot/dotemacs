;; =============================================================================
;; Work specific stuff
;; =============================================================================

(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-to-load-path '("dd-log-parser")))

;; Enable dd-log-parser
(require 'dd-log-parser)

;; ============================= Functions ===================================
(defun goc ()
"Equivalent to typing go cyclone rd 1 work in the terminal"
(interactive)
(cd "/dd/shows/CYCLONE/RD/0001/user/work.fredriks")
)

(defun get-houdini-vers (&optional version-offset)
  "Return the houdini version that is installed in /tools/package
if VERSION-OFFSET is 1 (default) it will return the latest
version, if 2 it will return the next latest and so on. If the
offset is less than 1 or greater than the number of installed
versions it will return the oldest version."
  (interactive "p")
  ;; Calling this from the mini-buffer or global-keys will by
  ;; default set the numeric-prefix to 1. Hence the switch to
  ;; using 1 as the default.
  (when (not version-offset) (setq version-offset 1))
  ;; Sorted by time, newest first. The last element will be empty
  ;; hence the nbutlast command to remove it.
  (let* ((houdini-versions (nbutlast (split-string (shell-command-to-string
						    "ls -t /tools/package/houdini")
						   "\n")))
	 (total (length houdini-versions))
	 (offset (1- version-offset)))
    (if (and (> version-offset 0) (< version-offset total))
	(nth offset houdini-versions)
      (car(last houdini-versions))
      )
  ))

(defun insert-houdini-vers (&optional version-offset)
  "Insert the houdini version where the cursor is located. if
VERSION-OFFSET is 1 (default) it will insert the latest installed
houdini version, 2 the next latest and so on. If the offset is
less than 1 or greater than the number of installed versions
it will return the oldest version"
  (interactive "p")
  (insert (get-houdini-vers version-offset)))

;; Functions for quickly set up the work environment
(defun setup-work ()
  "Splits the window into three equivalent buffers and set the cwd to swdevl"
  (interactive)
  (cd "~/fredriks/swdevl")
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (toggle-frame-fullscreen))
(defun setup-build-fun (terminal-type)
"Spawns multiple TERMINAL-TYPES, with the names release, cyclone,
build and misc"
  (cd "~/fredriks/release")
  (funcall terminal-type)
  (rename-buffer "release")
  (highlight-build)
  (cd "~/fredriks/swdevl")
  (funcall terminal-type)
  (rename-buffer "cyclone")
  (highlight-build)
  (split-window-right)
  (funcall terminal-type)
  (rename-buffer "misc")
  (highlight-build)
  (funcall terminal-type)
  (rename-buffer "build")
  (highlight-build)
  (toggle-frame-maximized)
)

(defun setup-build-term()
  "Spawns multiple multi-terms called cyclone, build and misc"
  (interactive)
  (setup-build-fun #'multi-term) ;; #'x short for (function x)
)

(defun setup-build()
  "Spawns multiple shells called cyclone, build and misc"
  "Using shell instead of multi-term"
  (interactive)
  ;; shell doesn't handle git's diff functions therefore I'm using
  ;; multi-term for that.
  (cd "~/fredriks/swdevl")
  (multi-term)
  (rename-buffer "git")
  (setup-build-fun #'shell) ;; #'x short for (function x)
)

(defun create-term-and-go (name command)
  "Spawns a multi-term and rename it to NAME and then executes the COMMAND"
  (multi-term)
  (rename-buffer name)
  (run-emacs-term-command command)
  (term-resync-dirs)
  )

(defun setup-houdini-term ()
  "Spawns two multi-terms called h14 and h13 and move to the correct path for both."
  (interactive)
  (cd "~/fredriks/Houdini")
  (delete-other-windows)
  (create-term-and-go "h14" "go cyclone rd 1 =fx_h14 work")
  (create-term-and-go "h13" "goc")
  (toggle-frame-maximized)
  )

(defun setup-houdini ()
  "Spawns two shells called h14 and h13 and move to the correct path for both. "
  "Using shell instead of multi-term"
  (interactive)
  (cd "~/fredriks/Houdini")
  (delete-other-windows)
  (shell)
  (rename-buffer "h13")
  (run-emacs-shell-command "goc")
  ;; (shell-resync-dirs)
  (shell)
  (rename-buffer "h14")
  (run-emacs-shell-command "go cyclone rd 1 =fx_h14 work")

  ;; (shell-resync-dirs)
  (toggle-frame-maximized)
  )
;; ------------------------------ Parse logs ---------------------------------
(defun dd/generate-solver-info-table ()
"Generate table for how long the preconditioner took for each frame"
(interactive)
(let ((types '("\\(?1:Max iteration\\) = \\(?2:[0-9]+\\)"
	       "\\(?1:best residual\\) = \\(?2:[0-9]+\\.[0-9e-]+\\)"
	       "\\(?1:Number of cells\\): \\(?2:[0-9]+\\)"
	       "Finished process \\(?1:Initialize preconditioner\\) in \\(?2:[0-9]+\\.[0-9]+\\)s"
	       "Finished process \\(?1:Solve linear system\\) in \\(?2:[0-9]+\\.[0-9]+\\)s")))
      (dd/log-generate-table-last 
       "Float frame: \\([0-9]+\\)"
       (concat "\\(?:" (mapconcat 'identity types "\\|")"\\)")
       "I(Frame,1) R(1,2))")))

;; --------------------------- Source BuildConfig ----------------------------
(defun get-version-from-config (name file)
  "Gets the version from a file."
  ;; Pick the first in the list
  (car 
   ;; Remove newlines and convert the string to a list if there's more
   ;; than one version.
   (split-string 
    (shell-command-to-string 
     ;; Look for the string matching the name.
     (concat "grep -iE ^" name "_version " file
	     ;; Extract only the version number from the string.
	     " | cut -d = -f 2"
	     )
     ))))

(defun get-version-from-build-config (name path)
  "Gets the version from the BuildConfig file"
  ;; Pick the first in the list
  (get-version-from-config  name (concat path "/BuildConfig"))
)

;; pk
(defun pk-project (name)
  "Parse pk files an replace {{ package.project }} with NAME"
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
		    	    (get-version-from-build-config
		    	     "openvdb"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone/")
		    	    "/core/include/openvdb/")
;;		    "~/fredriks/swdevl/private/openvdb/core/include/openvdb"
		    )
	      )

(set-register ?m 
	      (cons 'file 
		    (concat "/tools/package/openmesh/"
		    	    (get-version-from-build-config
		    	     "openmesh"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone/")
		    	    "/include/OpenMesh/")
		    )
	      )
(set-register ?c 
	      (cons 'file 
		    "/dd/shows/CYCLONE/RD/0001/user/work.fredriks/"
		    )
	      )

(set-register ?h
	      (cons 'file 
		    (concat "/tools/package/houdini/"
			    (get-version-from-config
		    	     "houdini"
		    	     (concat "/dd/dept/software/users/fredriks/swdevl/"
				     "cyclone/houdini/projdeps.cent6_64"))
			    "/toolkit/include"
		    )))
(set-register ?v 
	      (cons 'file 
		    (concat "/tools/package/eigen/"
		    	    (get-version-from-build-config
		    	     "eigen"
		    	     "/dd/dept/software/users/fredriks/swdevl/cyclone/")
		    	    "/include/eigen3/Eigen/src")
		    )
	      )

;; ============================= Key bindings ==================================
(global-set-key (kbd "C-x j") 'insert-houdini-vers)
