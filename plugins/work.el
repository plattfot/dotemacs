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

;;; Functions
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

(defun work-vfxplatform ()
  "Open vfxplatform.com in the *eww* buffer."
  (interactive)
  (eww "https://vfxplatform.com"))

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
  "Visit the DIRECTORY and fetch all files matching the REGEX.

Different to `work-insert-sources-recursively' is that this will
ignore subdirectories.

Will print them out as a meson source list. The files will be
relative to the `default-directory'."
  (interactive (list
                (read-directory-name "Source: ")
                (read-regexp "regex: ")))
  (let ((files (directory-files (or directory default-directory) t regex)))
    (--each files (insert (format "'%s',\n" (file-relative-name it))))))

(defun work-insert-sources-recursively (&optional directory regex)
  "Visit the DIRECTORY and its subdirs and fetch all files matching the REGEX.

Different to `work-insert-sources' is that this will traverse
into subdirectories and fetch those files as well.

Will print them out as a meson source list. The files will be
relative to the `default-directory'."
  (interactive (list
                (read-directory-name "Source: ")
                (read-regexp "regex: ")))
  (let ((files (directory-files-recursively (or directory default-directory) regex)))
    (--each files (insert (format "'%s',\n" (file-relative-name it))))))

;;; git
(defun work-git--fetch-version (change-re version-re error-msg)
  "Extract version change from a diff.

Search for change using CHANGE-RE regex. If it finds a change
extract it using VERSION-RE. And return it. The VERSION-RE must
contains one capture group.

If nothing is found it will error out with ERROR-MSG as the error
message."
  (save-mark-and-excursion
    (goto-char (point-min))
    (diff-beginning-of-hunk t)
    (if (re-search-forward change-re nil t)
        (let ((result (s-match version-re
                               (buffer-substring-no-properties
                                (point-at-bol)
                                (point-at-eol)))))
          (if result
              (nth 1 result)
            (error error-msg)))
      (error "No changes found"))))

(cl-defstruct (work-git--version (:constructor work-git--construct-version))
  "Structure contain a semver-ish version."
  (components nil)
  (pre-release nil)
  (revision nil))

(defun work-git--make-version (version)
  "Create a `work-git--version' from VERSION."
  (let* ((version-split (split-string (car (split-string version "_")) "-"))
         (version-parts (seq-map (lambda (v) (split-string v (rx "."))) version-split))
         (version-comp (car version-parts))
         (version-pre (cadr version-parts))
         (version-revision (or
                            (seq-filter (lambda (i) (string-prefix-p "DD" i)) version-comp)
                            (seq-filter (lambda (i) (string-prefix-p "DD" i)) version-pre))))
    (work-git--construct-version
     :components (apply 'vector (seq-remove (lambda (i) (string-prefix-p "DD" i)) version-comp))
     :pre-release (apply 'vector (seq-remove (lambda (i) (string-prefix-p "DD" i)) version-pre))
     :revision (if version-revision
                   (string-to-number (string-remove-prefix "DD" (car version-revision)))
                 0))))

(defun work-git--version-length (version)
  "Return the length of the components for VERSION.
Where VERSION is a `work-git--version'."
  (length (work-git--version-components version)))

(defun work-git--fetch-old-new-version ()
  "Return the old and new version using git diff."
  (with-temp-buffer
    (insert (shell-command-to-string "git diff manifest.yaml"))
    (diff-mode)

    (let* ((regex-fmt (rx bol "%sversion:" (* blank)
                          (zero-or-one (or "\"" "'"))
                          (group (+ (any alnum "_.-")))
                          (zero-or-one (or "\"" "'"))))
           (old-version-re (format regex-fmt (rx "-")))
           (new-version-re (format regex-fmt (rx "+")))
           (old-version (work-git--fetch-version (rx bol "-")
                                                 old-version-re
                                                 "Old version not found"))
           (new-version (work-git--fetch-version (rx bol "+")
                                                 new-version-re
                                                 "New version not found")))
      `(,old-version ,new-version))))

(defun work-git--version-commit-prefix-message (old-version new-version)
  "Return the first part of the version commit message.

The prefix is based on the changes between OLD-VERSION and
NEW-VERSION. Assuming the versions following version and
new-version > old-version."
  (let* ((old-version-c (work-git--make-version old-version))
         (old-version-l (work-git--version-length old-version-c))
         (new-version-c (work-git--make-version new-version))
         (new-version-l (work-git--version-length new-version-c))
         (check-component
          (lambda (c)
            (and (> old-version-l c)
                 (> new-version-l c)
                 (not (string-equal (elt (work-git--version-components old-version-c) c)
                                    (elt (work-git--version-components new-version-c) c)))))))
    (cond
     ((seq-difference (work-git--version-pre-release new-version-c)
                      (work-git--version-pre-release old-version-c))
      (format "Bump %s %s"
              (if (or (seq-empty-p (work-git--version-pre-release old-version-c))
                      (not (string-equal (elt (work-git--version-pre-release old-version-c) 0)
                                         (elt (work-git--version-pre-release new-version-c) 0))))
                  "to" "up")
              (elt (work-git--version-pre-release new-version-c) 0)))
     ((funcall check-component 0)
      "Bump up major")
     ((funcall check-component 1)
      "Bump up minor")
     ((funcall check-component 2)
      "Bump up patch")
     ((and (> (work-git--version-revision new-version-c) (work-git--version-revision old-version-c)))
      "Bump up revision")
     ((and (> old-version-l 3)
           (> new-version-l 3))
      "Version change")
     (t (error "Cannot generate a prefix message for %s -> %s" old-version new-version)))))

(defun work-git--version-commit-message ()
  "Return the version commit message.
In the form of \"`prefix: old -> new\""
  (with-temp-buffer
    (insert (shell-command-to-string "git diff manifest.yaml"))
    (diff-mode)

    (let ((check-changes
           (lambda (change-re)
             (goto-char (point-min))
             (diff-beginning-of-hunk t)
             (let ((changes 0))
               (while (re-search-forward change-re nil t)
                 (setf changes (+ 1 changes)))
               (when (> changes 1)
                 (error "Diff contains more changes than just version, aborting"))))))
      ;; Make sure there is only one remove and add
      (funcall check-changes (rx bol "-"))
      (funcall check-changes (rx bol "+")))
    (let* ((old-new-version (work-git--fetch-old-new-version))
           (old (car old-new-version))
           (new (cadr old-new-version)))
      (format "%s: %s -> %s"
              (work-git--version-commit-prefix-message old new) old new))))

(defun work-git-add-version ()
  "Add a commit with the version change.

Will give an error if something other than the version has
changed in the manifest.yaml."
  (interactive)
  (shell-command
   (format "git add manifest.yaml && git commit -m %S"
           (work-git--version-commit-message))))

;;; Registers
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

;;; GDB
(define-prefix-command 'gdb-insert-map)

(defun work-insert-gdb-sourceme (name &optional config use-config-verbatim)
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
  (work-insert-gdb-sourceme "spork_for_gdb" "0.7.0" t))

(defun work-insert-openmesh-sourceme ()
  "Insert path to spork's sourceme file."
  (interactive)
  (work-insert-gdb-sourceme "openmesh_for_gdb" "1.0.0" t))

(global-set-key (kbd "C-c i") 'gdb-insert-map)

(define-key gdb-insert-map (kbd "s") 'work-insert-spork-sourceme)
(define-key gdb-insert-map (kbd "m") 'work-insert-openmesh-sourceme)



(provide 'work)
;;; work.el ends here
