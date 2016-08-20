;; (use-package ledger-mode 
;; :ensure t
;; :init 
;; (setq ledger-clear-whole-transactions 1)
;; :mode "\\.dat"
;; )
(require 'ledger-mode)
(setq ledger-clear-whole-transactions 1)
;;; set the default mode for .dat files to ledger-mode
(add-to-list 'auto-mode-alist '("\\.dat" . ledger-mode))

