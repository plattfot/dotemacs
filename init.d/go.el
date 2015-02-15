(add-hook 'go-mode-hook 
  (lambda ()
    (setq-default) 
    (setq tab-width 2) 
    (setq standard-indent 2) 
    (setq indent-tabs-mode nil)))

(require 'go-autocomplete)
(require 'auto-complete-config)
