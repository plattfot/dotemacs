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
if VERSION-OFFSET is zero (default) it will return the latest
version, if 1 it will return the next latest and so on. If the
offset is less than zero or greater than the number of installed
versions it will return the oldest version."
  (interactive "p")
  (when (not version-offset) (setq version-offset 0))
  ;; Sorted by time, newest first. The last element will be empty
  ;; hence the nbutlast command to remove it.
  (let* ((houdini-versions (nbutlast (split-string (shell-command-to-string
						    "ls -t /tools/package/houdini")
						   "\n")))
	 (total (length houdini-versions)))
    (if (and (>= version-offset 0) (< version-offset total))
	(nth version-offset houdini-versions)
      (last houdini-versions)
      )
  ))

(defun insert-houdini-vers (&optional version-offset)
  "Insert the houdini version where the cursor is located. if
VERSION-OFFSET is zero it will insert the latest installed
houdini version, 1 the next latest and so on. If the offset is
less than zero or greater than the number of installed versions
it will return the oldest version"
  (interactive "p")
  (insert (get-houdini-vers  version-offset)))

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
(defun setup-build()
  "Spawns multiple multi-terms called cyclone, build and misc"
  (interactive)
  (setup-build-fun #'multi-term) ;; #'x short for (function x)
)

(defun setup-build-shell()
  "Spawns multiple shells called cyclone, build and misc"
  "Using shell instead of multi-term"
  (interactive)
  (setup-build-fun #'shell) ;; #'x short for (function x)
)

(defun create-term-and-go (name command)
  "Spawns a multi-term and rename it to NAME and then executes the COMMAND"
  (multi-term)
  (rename-buffer name)
  (run-emacs-term-command command)
  (term-resync-dirs)
  )

(defun setup-houdini ()
  "Spawns two multi-terms called h14 and h13 and move to the correct path for both."
  (interactive)
  (cd "~/fredriks/Houdini")
  (delete-other-windows)
  (create-term-and-go "h14" "go cyclone rd 1 =fx_h14 work")
  (create-term-and-go "h13" "goc")
  (toggle-frame-maximized)
  )

(defun setup-houdini-shell ()
  "Spawns two shells called h14 and h13 and move to the correct path for both. "
  "Using shell instead of multi-term"
  (interactive)
  (cd "~/fredriks/Houdini")
  (delete-other-windows)
  (shell)
  (rename-buffer "h14")
  (run-emacs-shell-command "go cyclone rd 1 =fx_h14 work")
  ;; (shell-resync-dirs)
  (shell)
  (rename-buffer "h13")
  (run-emacs-shell-command "goc")
  ;; (shell-resync-dirs)
  (toggle-frame-maximized)
  )

;; --------------------------- Source BuildConfig ----------------------------
(defun get-version-from-build-config (name path)
  "Gets the version from the BuildConfig file"
  ;; Pick the first in the list
  (car 
   ;; Remove newlines and convert the string to a list if there's more
   ;; than one version.
   (split-string 
    (shell-command-to-string 
     ;; Look for the string matching the name.
     (concat "grep -iE ^" name "_version " path "BuildConfig.cent6_64"
	     ;; Extract only the version number from the string.
	     " | grep -oE '[0-9]+\.[0-9]+\.[0-9_a-z]+'"
	     )
     ))) )

;; pk
(defun pk-project (name)
  "Parse pk files an replace {{ package.project }} with NAME"
  (interactive "sSpecify name of project: ")
  (while (search-forward-regexp "{{[ ]*package\.project.*}}" (point-max) t )
    (let* ((start (match-beginning 0))
	   (end (match-end 0))
	   (str (buffer-substring-no-properties start end))
	   (project (if (string-match "capitalize" str) (capitalize name) name)))
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
		    ;; (concat "/tools/package/openvdb/"
		    ;; 	    (get-version-from-build-config
		    ;; 	     "openvdb"
		    ;; 	     "/dd/dept/software/users/fredriks/swdevl/Cyclone/")
		    ;; 	    "/core/include/openvdb/")
		    "~/fredriks/swdevl/private/openvdb/core/include/openvdb"
		    )
	      )

(set-register ?m 
	      (cons 'file 
		    (concat "/tools/package/openmesh/"
		    	    (get-version-from-build-config
		    	     "openmesh"
		    	     "/dd/dept/software/users/fredriks/swdevl/Cyclone/")
		    	    "/include/OpenMesh/")
		    )
	      )

;; ============================= Key bindings ==================================
(global-set-key (kbd "C-i") 'insert-houdini-vers)
