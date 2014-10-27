;; =============================================================================
;; Miscellaneous modes:
;; =============================================================================

;; Add Cuda Syntax
(autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
(setq auto-mode-alist (append '(("/*.\.cu$" . cuda-mode)) auto-mode-alist))

;; Add cython
(autoload 'cython-mode "cython-mode.el" "Cython mode" t)
   ;;(autoload 'cython-mode "cython-mode" nil t)
   ;; (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
   ;;(add-to-list 'auto-mode-alist '("\\.pyd\\'" . cython-mode))
(autoload 'mel-mode "mel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
