;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/init.d/configuration.org")

;; Config that hasn't been ported to org.
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path '("dotemacs")))

(require 'dotemacs)

;; Work
(defvar dotemacs/is-work (string= (getenv "USER") "fredriks") )
(if dotemacs/is-work
    (progn
      (dotemacs/load-user-file "work/work.el")
      (dotemacs/load-user-file "work/dd-newfile.el")
 ) nil )
