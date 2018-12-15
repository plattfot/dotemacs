;;; dotemacs -- Load config files

;;; Commentary:
;; Load config files for init.el.

;;; Code:
;; Load init files
;; url; http://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
;; author: seh
(defun dotemacs-load-user-file (file)
"Load and eval FILE in the init.d directory."
(interactive "f")
  (load-file (expand-file-name file "~/.emacs.d/init.d")))

(defvar dotemacs-is-work (string= (getenv "USER") "fredriks")
  "Non-nil if I'm at work.")

(defvar dotemacs-guix-installed (file-directory-p "/var/guix")
  "Non-nil if guix is installed.")

(provide 'dotemacs)
;;; dotemacs.el ends here
