;; =============================================================================
;; Multi-term
;; =============================================================================

;; Enable multi-term
(require 'multi-term)

( if work/is-work 
    (setq multi-term-program "/bin/tcsh")
    (setq multi-term-program "/bin/zsh")
    )

(add-hook 'term-mode-hook
          (lambda ()
	    ;; Set the buffer size of the terminal
            (setq term-buffer-maximum-size 10000)
	    ;; Disable yasnippet, since it interfere with tab-completion
            (yas-minor-mode -1)      
;; ============================= Key bindings ==================================
	    (add-to-list 'term-bind-key-alist '("C-c j" . term-char-mode))
	    (add-to-list 'term-bind-key-alist '("C-c k" . term-line-mode))     
	    ))
