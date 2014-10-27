;; =============================================================================
;; Haskell
;; =============================================================================

;; Enable haskell stuff
(require 'inf-haskell)
(require 'haskell-mode-autoloads)
;;(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
 ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(autoload 'ghc-init "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; ============================= Key bindings ==================================
;;(define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer) ;; doesn't work
