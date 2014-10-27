;; =============================================================================
;; Multi-term
;; =============================================================================
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path '("multi-term")))

;; Enable multi-term
(require 'multi-term)

(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
	    ;; Set the buffer size of the terminal
            (setq term-buffer-maximum-size 10000)
	    ;; Disable yasnippet, since it interfere with tab-completion
            (yas-minor-mode -1)                   
	    ))
