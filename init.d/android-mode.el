;; =============================================================================
;; Init android mode
;; =============================================================================
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-to-load-path 
   '("android-mode")))

;; Enable android mode
(require 'android-mode)

