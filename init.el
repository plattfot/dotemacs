;; To execute an lisp function in replace regexp do \,(<function>)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path '("dotemacs")))

(require 'dotemacs)
;; =========================== Load init files =================================
;; This must be loaded first.							
(dotemacs/load-user-file "init.el") ;; defines dotemacs/is-work variable
(dotemacs/load-user-file "package.el") ;; setup package and install missing packages

(dotemacs/load-user-file "auto-complete.el")
;;(dotemacs/load-user-file "company.el")

;; Functions
(dotemacs/load-user-file "text-functions.el")
(dotemacs/load-user-file "debug-functions.el")
(dotemacs/load-user-file "highlight-functions.el")
(dotemacs/load-user-file "buffer-functions.el")
(dotemacs/load-user-file "file-functions.el")
(dotemacs/load-user-file "misc-functions.el")

;; Work
(if dotemacs/is-work
    (progn
      (dotemacs/load-user-file "work.el")
      (dotemacs/load-user-file "dd-newfile.el")
      (dotemacs/load-user-file "gccsense.el"))
  (dotemacs/load-user-file "home.el")
)

;; Key bindings
(dotemacs/load-user-file "macros.el")
(dotemacs/load-user-file "keys.el")
(dotemacs/load-user-file "registers.el")

;; Packages 
(dotemacs/load-user-file "gnu-plot.el") ;; Needs dotemacs/is-work
(dotemacs/load-user-file "ack.el")
(dotemacs/load-user-file "expand-region.el")
(dotemacs/load-user-file "evil-numbers.el")
(dotemacs/load-user-file "multiple-cursors.el")
(dotemacs/load-user-file "multi-term.el") ;; Needs dotemacs/is-work
(dotemacs/load-user-file "yasnippet.el")
(dotemacs/load-user-file "tramp.el")
(dotemacs/load-user-file "houdini.el")
(dotemacs/load-user-file "org-mode-extra.el")
(dotemacs/load-user-file "string-inflection.el")
;;(dotemacs/load-user-file "rtags.el")
;;(dotemacs/load-user-file "gtags.el")
(dotemacs/load-user-file "flycheck.el")
;; Modes
(dotemacs/load-user-file "modes.el")

;; Language specific
(dotemacs/load-user-file "lisp.el")
(dotemacs/load-user-file "c-family.el")
(when (not dotemacs/is-work)
  (dotemacs/load-user-file "go.el")
  (dotemacs/load-user-file "arduino.el")
  (dotemacs/load-user-file "sudo-edit.el")
  (dotemacs/load-user-file "ledger.el"))

;; (when (not dotemacs/is-work)
;;   (dotemacs/load-user-file "haskell.el"))
(dotemacs/load-user-file "python.el")
(dotemacs/load-user-file "shell.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-week-start-day 1)
 '(custom-safe-themes
   (quote
    ("c882403a829de68ab34bd194264035de23e17012065631fcac29d5e05f7ebb5d" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" default)))
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode white-sand-theme use-package sudo-edit string-inflection rtags multi-term move-text mc-extras magit ledger-mode go-autocomplete gnuplot flycheck expand-region evil-numbers company cmake-mode buffer-move ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
