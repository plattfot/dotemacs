
(require 'rtags)
(require 'rtags-ac)

;; ============================= Key bindings ==================================
(global-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
(global-set-key (kbd "M-,") 'rtags-find-references-at-point)

(global-set-key (kbd "M-[") 'rtags-location-stack-back)
(global-set-key (kbd "M-]") 'rtags-location-stack-forward)

