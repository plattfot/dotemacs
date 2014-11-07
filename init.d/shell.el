;; =============================================================================
;; Shell
;; =============================================================================

;; Indent using spaces
(setq sh-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))

(add-hook 'shell-mode-hook 
	  (lambda ()
	    ;; Enable color in shell
	    (ansi-color-for-comint-mode-on)
	    ;; Change Color theme in shell
	    (setq ansi-color-names-vector
		  ["#4d4d4d" "#D81860" "#60FF60" "#f9fd75" "#4695c8" "#a78edb" "#43afce" "#f3ebe2"])
	    (setq ansi-color-map (ansi-color-make-color-map))
	    ;; Disable echo in shell/ Hangs dirs
	    ;; (setq comint-process-echoes t) 
	    ))
