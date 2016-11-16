(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("c++" (mode . c++-mode))
	       ("dired" (mode . dired-mode))
	       ("ag" (mode . ag-mode ))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Completions\\*$")))
	       ))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
