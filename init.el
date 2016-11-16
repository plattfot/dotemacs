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
      (dotemacs/load-user-file "work/dd-newfile.el")) 
nil )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (esup yasnippet yaml-mode sudo-edit string-inflection rtags org-bullets multi-term move-text mc-extras magit ledger-mode go-autocomplete gnuplot flycheck expand-region evil-numbers buffer-move ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
