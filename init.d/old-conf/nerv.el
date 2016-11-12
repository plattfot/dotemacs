;; =============================================================================
;; Init nerv 
;; =============================================================================

(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path 
       '("nerv")))

;; Enable nerv
(require 'nerv)
