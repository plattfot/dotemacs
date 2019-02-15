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

(defvar dotemacs-is-work (string= (getenv "USER") "fredriks")
  "Non-nil if I'm at work.")

(defvar dotemacs-guix-installed (file-directory-p "/var/guix")
  "Non-nil if guix is installed.")

(provide 'dotemacs)
;;; dotemacs.el ends here
