;;; work.el --- Work related functions
;;; Commentary:

;;; Code:

(require 'highlight-extra)
(require 'string-inflection)
;; Load libyaml bindings if they exist.
(require 'libyaml nil t)
(require 'dash)
(require 'json)
(require 'cl-lib)

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
      (error "No process in %s" (buffer-name)))
    (goto-char (process-mark process))
    (insert command)
    (comint-send-input nil t )))

(defun work-setup-houdini ()
  "Spawns four shells at the cyclone test show."
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

(defun work-parse-manifest (manifest-file)
  "Read the manifest and return an alist with the entries.

Where MANIFEST-FILE is the path to the manifest file.

If libyaml isn't loaded it only supports the simple structure.
I.e key: [']value[']. No nesting.
Return a hash table."
  (if (featurep 'libyaml)
      (with-temp-buffer
        (insert-file-contents manifest-file)
        (yaml-read-from-string (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (insert-file-contents manifest-file)
      (setq case-fold-search t)
      (let ((manifest (make-hash-table :test 'equal))
            (type-value-re "^\\([[:alnum:]_]+\\):[ ']*\\([[:alnum:]_.]+\\)[ ']*"))
        (goto-char (point-min))
        (while (re-search-forward type-value-re nil t)
          (let ((type (string-inflection-lower-camelcase-function (match-string 1)))
                (value (match-string 2)))
            (puthash type value manifest)))
        manifest))))

(defun work-insert-sources (&optional directory regex)
  "Go to DIRECTORY and fetch all files matching the REGEX.

Will print them out as a meson source list. The files will be
relative to the `default-directory'."
  (interactive (list
                (read-directory-name "Source: ")
                (read-regexp "regex: ")))
  (let ((files (directory-files (or directory default-directory) t regex)))
    (--each files (insert (format "'%s',\n" (file-relative-name it))))))
;; --------------------------- Source BuildConfig ----------------------------
(cl-defun work-get-version-pk-lock (name file &key (recipe 'build) flavour)
  "Gets the version from a file.
Where NAME is the name of the package you want the version for
and FILE the config to search in.

RECIPE specifies what recipe it should fetch the VERSION from, default is build.

FLAVOUR specifies what flavour it should fetch from, default is the first one.
"
  (let* ((pk.lock (json-read-file file))
         (get-symbol (lambda (x) (if (symbolp x) x (intern x))))
         (version
          (-as-> (alist-get 'flavours pk.lock) x
                 (if flavour
                     (alist-get (funcall get-symbol flavour) x)
                   (cdar x))
                 (alist-get (funcall get-symbol recipe) x)
                 (alist-get 'packages x)
                 (alist-get (funcall get-symbol name) x)
                 (alist-get 'version x))))
    (unless version
      (error "No version found for %s" name))
    version))

(defun work-get-version-from-pk-lock (name path)
  "Gets the version from the pk lock file.
Where NAME is the name of the package you want the version for
and PATH is where the pk.lock file is located."
  ;; Pick the first in the list
  (work-get-version-pk-lock
   name
   (concat (file-name-as-directory path) "pk.lock")))

;; ============================ Registers ====================================
(defvar work-swdevl
  (substitute-in-file-name "$DD_SHOWS_ROOT/DEV01/user/work.$USER/swdevl")
  "Path to my default workspace at work.")

(defvar work-package-root (substitute-in-file-name "$DD_TOOLS_ROOT/$DD_OS/package")
  "Default location for packages at work.")

(defun work-pkg-path (name include &optional config use-config-verbatim)
  "Create include path to NAME.

INCLUDE is the relative path from the package root to the include
directory.  Default is \"include/NAME'\".

CONFIG is where it should look for the version.  It's
relative to `work-swdevl'.  By default it is CoreLibs.

If USE-CONFIG-VERBATIM is not nil it will just use the value
of CONFIG as the version."
  (directory-file-name
   (mapconcat
    'file-name-as-directory
    `(,work-package-root
      ,name
      ,(if use-config-verbatim
           (or config "")
         (work-get-version-from-pk-lock
          name
          (concat (file-name-as-directory work-swdevl) (or config "CoreLibs"))))
      ,include)
    "")))

(set-register ?d (cons 'file work-swdevl))

;; ============================== GDB ========================================
(define-prefix-command 'gdb-insert-map)

(defun work-insert-sourceme (name &optional config use-config-verbatim)
  "Insert path to NAME's sourceme file.

CONFIG is where it should look for the version.  It's
relative to `work-swdevl'.  By default it is CoreLibs.

If USE-CONFIG-VERBATIM is not nil it will just use the value
of CONFIG as the version.

Useful when debugging."
  (insert (work-pkg-path name "gdb/sourceme.py" config use-config-verbatim)))

(defun work-insert-spork-sourceme ()
  "Insert path to spork's sourceme file."
  (interactive)
  (work-insert-sourceme "spork_for_gdb" "0.7.0" t))

(defun work-insert-openmesh-sourceme ()
  "Insert path to spork's sourceme file."
  (interactive)
  (work-insert-sourceme "openmesh_for_gdb" "1.0.0" t))

(global-set-key (kbd "C-c i") 'gdb-insert-map)

(define-key gdb-insert-map (kbd "s") 'work-insert-spork-sourceme)
(define-key gdb-insert-map (kbd "m") 'work-insert-openmesh-sourceme)

(provide 'work)
;;; work.el ends here
