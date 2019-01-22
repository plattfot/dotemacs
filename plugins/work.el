;;; work.el --- Work related functions
;;; Commentary:

;;; Code:

(require 'highlight-extra)
(require 'multi-term)

;; ============================= Functions ===================================
(defun work-insert-eigen-pretty-printer ()
"Insert command to add eigen pretty printer for gdb."
(interactive )
(insert "python execfile(\"/dd/shows/DEV01/user/work.fredriks/swdevl"
	"/CoreLibs/src/gdb/EigenPrettyPrinter.py\")"))

;; Functions for quickly set up the work environment
(defun work-setup ()
  "Create two frames; Code and Shell.

Use 'universal-argument' before calling this function to not
delete all other frames.

Code is split into three windows, the cwd is set to DEV01's
swdevl, it's moved to the right screen and fullscreen is toggled.

Shell is split into two windows, multiple shells are spawned;
3ps, release, cyclone, build and misc.  It's moved to the left
screen and is maximized"

  (interactive)
  (select-frame (make-frame `((name . "Code")
			      (top . 28)
			      (left . ,(/ (display-pixel-width) 2)))))
  (when (not current-prefix-arg) (delete-other-frames) )
  (work-setup-code)
  (select-frame
   (make-frame '((name . "Shell") (top . 28) (left . 0))))
  (work-setup-build)
  (other-frame 1))

(defun work-setup-code ()
  "Split the frame into three windows and set cwd to DEV01's swdevl.

It also toggle the fullscreen for the frame."
  (interactive)
  (cd "/dd/shows/DEV01/user/work.fredriks/swdevl")
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (toggle-frame-fullscreen))

(defun work-setup-build ()
  "Split the window horizontally and spawns multiple shells.
With the names 3ps, cyclone, build and misc"
  (delete-other-windows)
  (split-window-horizontally)
  (cd "/dd/shows/DEV01/user/work.fredriks/swdevl")
  (let ((go-swdevl (lambda (name extra)
                     (shell name)
                     (work-run-emacs-shell-command "go dev01 work && cd swdevl")
                     (mapcar (lambda (x) (funcall x)) extra))))
    (funcall go-swdevl "cyclone" '(highlight-build highlight-gtest))
    (funcall go-swdevl "misc" '(highlight-build))
    (funcall go-swdevl "build" '(highlight-build highlight-gtest))
    (funcall go-swdevl "3ps" '(highlight-build)))
  (toggle-frame-maximized))

(defun work-run-emacs-shell-command (command)
  "Run the COMMAND in a Emacs shell.
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
  (cd "/dd/shows/DEV01/user/work.fredriks")

  (let ((go-hou (lambda (name role)
                  (shell name)
                  (work-run-emacs-shell-command (format "go dev01 =%s work" role)))))
    (funcall go-hou "hou-misc" "fx")
    (funcall go-hou "hou-beta" "fx_beta")
    (funcall go-hou "hou-devl" "fx")
    (funcall go-hou "hou-test" "fx"))
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
	     )))))

(defun work-get-version-from-build-config (name path)
  "Gets the version from the BuildConfig file.
Where NAME is the name of the package you want the version for
and PATH is where the BUILD.conf file is located."
  ;; Pick the first in the list
  (work-get-version-from-config
   name
   (concat (file-name-as-directory path) "BUILD.conf")))

;; ============================ Registers ====================================
(set-register ?o
              (cons 'file
                    (mapconcat 'file-name-as-directory
                               `("/tools/package/openvdb"
                                 ,(work-get-version-from-build-config
                                   "openvdb"
                                   "/dd/shows/DEV01/user/work.fredriks/swdevl/CoreLibs")
                                 "include/openvdb")
                               "")))
(set-register ?m
              (cons 'file
                    (mapconcat 'file-name-as-directory
                               `("/tools/package/openmesh"
                                 ,(work-get-version-from-build-config
                                   "openmesh"
                                   "/dd/shows/DEV01/user/work.fredriks/swdevl/CoreLibs")
                                 "include/OpenMesh")
                               "")))

(set-register ?d (cons 'file "/dd/shows/DEV01/user/work.fredriks/"))

(set-register ?h
              (cons 'file
                    (mapconcat 'file-name-as-directory
                               `("/tools/package/houdini"
                                 ,(work-get-version-from-build-config
                                   "houdini"
                                   "/dd/shows/DEV01/user/work.fredriks/swdevl/cyclone")
                                 "toolkit/include")
                               "")))
(set-register ?v
              (cons 'file
                    (mapconcat 'file-name-as-directory
                               `("/tools/package/eigen"
                                 ,(work-get-version-from-build-config
                                   "eigen"
                                   "/dd/shows/DEV01/user/work.fredriks/swdevl/CoreLibs")
                                 "include/eigen3/Eigen/src")
                               "")))
(provide 'work)
;;; work.el ends here
