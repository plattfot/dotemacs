;;; dotemacs -- Load config files

;;; Commentary:
;; Load config files for init.el.

;;; Code:

(defun dotemacs-build-path (&rest sequence)
  "Combine the SEQUENCE into one path using `file-name-as-directory'.

It will leave a slash at the end so this can directly be used
with concat to combine with file. If the SEQUENCE is a path to a
file use `directory-file-name' to strip that away."
  (mapconcat 'file-name-as-directory sequence ""))


;; Load init files
;; url; http://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
;; author: seh
(defun dotemacs-load-user-file (file)
"Load and eval FILE in the init.d directory."
(interactive "f")
  (load-file (dotemacs-build-path user-emacs-directory "init.d")))

(defun dotemacs-hide-trailing-whitespace ()
  "Hides trailing whitespaces by setting `show-trailing-whitespace' to nil."
  (setq show-trailing-whitespace nil))

(defvar dotemacs-is-work (string= (getenv "USER") "fredriks")
  "Non-nil if I'm at work.")

(defvar dotemacs-guix-installed (file-directory-p "/var/guix")
  "Non-nil if guix is installed.")

(defun dotemacs-font-hidpi ()
  "Change the font size to be more readable on a 4k monitor."
  (interactive)
  (set-frame-font "Hack:pixelsize=20" nil nil))

(defun dotemacs-font-lowdpi ()
  "Change the font size to be more readable on a standard monitor."
  (interactive)
  (set-frame-font "Hack:pixelsize=12" nil nil))

(defun dotemacs-font-setup ()
  "Change the font size depending on if GDK_SCALE is defined.
If defined it will use the `dotemacs-font-hidpi' otherwise it
will use `dotemacs-font-lowdpi'."
  (interactive)
  (if (or (getenv "GDK_SCALE") (getenv "GDK_DPI_SCALE"))
      (dotemacs-font-hidpi)
    (dotemacs-font-lowdpi)))

(provide 'dotemacs)
;;; dotemacs.el ends here
