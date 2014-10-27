;; =============================================================================
;; Init expand-region 
;; =============================================================================
;; Swap for package install

;; Add the plugin to the search path
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path 
       '("expand-region.el")))

;; Enable expand region
(require 'expand-region)

;; ============================= Key bindings ==================================
;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)
