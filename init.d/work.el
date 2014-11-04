;; =============================================================================
;; Work specific stuff
;; =============================================================================

;; Determine if it's work or home
(defvar work/is-work (string= (getenv "USER") "fredriks") )

(when work/is-work
  (let ((default-directory "~/.emacs.d/plugins/"))
    (normal-top-level-add-to-load-path '("dd-log-parser")))

  ;; Enable dd-log-parser
  (require 'dd-log-parser)

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

  ;; ============================= Functions ===================================

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

  (defun setup-build()
    "Spawns multiple multi-terms called cyclone, build and misc"
    (interactive)
    (cd "~/fredriks/swdevl")
    (multi-term)
    (rename-buffer "cyclone")
    (highlight-build)
    (split-window-right)
    (multi-term)
    (rename-buffer "misc")
    (multi-term)
    (rename-buffer "build")
    (highlight-build)
    (toggle-frame-maximized))
(setq debug-on-error t)
  (defun setup-build-shell()
    "Spawns multiple shells called cyclone, build and misc"
    "Using shell instead of multi-term"
    (interactive)
    (cd "~/fredriks/swdevl")
    (shell)
    (rename-buffer "cyclone")
    (highlight-build)
    (split-window-right)
    (shell)
    (rename-buffer "misc")
    (shell)
    (rename-buffer "build")
    (highlight-build)
    (toggle-frame-maximized))

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
    (debug)
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
    (shell-resync-dirs)
    (shell)
    (rename-buffer "h13")
    (run-emacs-shell-command "goc")
    (shell-resync-dirs)
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

  )



