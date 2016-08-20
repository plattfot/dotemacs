;; =============================================================================
;; Miscellaneous modes:
;; =============================================================================

;; GLSL mode -------------------------------------------------------------------
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.prog\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
;; -----------------------------------------------------------------------------

;; CUA mode --------------------------------------------------------------------
;; Annoying thing when cua-mode is enable. Highlighting with the mouse
;; becomes a pain. Due to that in cua-mode the highlight selection
;; follows the cursor when scrolling.
;; (cua-mode t) ;; 
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (setq cua-enable-cua-keys nil) ;; disable windows shortcuts
;; (cua-selection-mode nil )
(set-variable 'shift-select-mode t)
;; -----------------------------------------------------------------------------

;; ;; Init android mode -----------------------------------------------------------
;; (let ((default-directory "~/.emacs.d/plugins/"))
;;   (normal-top-level-add-to-load-path 
;;    '("android-mode")))


;; Enable android mode

;; (require 'android-mode)

;; (custom-set-variables '(android-mode-sdk-dir "/opt/android-sdk"))
;; -----------------------------------------------------------------------------

;; ;; Cuda mode -------------------------------------------------------------------
;; (autoload 'cuda-mode "cuda-mode.el" "Cuda mode." t)
;; (setq auto-mode-alist (append '(("/*.\.cu$" . cuda-mode)) auto-mode-alist))
;; ;; -----------------------------------------------------------------------------

;; ;; Cython mode -----------------------------------------------------------------
;; (autoload 'cython-mode "cython-mode.el" "Cython mode" t)
;;    ;;(autoload 'cython-mode "cython-mode" nil t)
;;    ;; (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
;;    ;;(add-to-list 'auto-mode-alist '("\\.pyd\\'" . cython-mode))
;; -----------------------------------------------------------------------------

;; Mel mode --------------------------------------------------------------------
(autoload 'mel-mode "mel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
;; -----------------------------------------------------------------------------

;; Org mode --------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (gnuplot . t) (sh . t) (C . t)))


