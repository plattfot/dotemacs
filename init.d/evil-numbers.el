;; =============================================================================
;; Init evil-numbers 
;; =============================================================================
;; Swap for package install

;; Add the plugin to the search path
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path 
       '("evil-numbers")))

;; Enable evil numbers
(require 'evil-numbers)

;; ============================= Key bindings ==================================
;; In/decrementing numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
