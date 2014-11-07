;; =============================================================================
;; Multi-term
;; =============================================================================

;; Enable multi-term
(require 'multi-term)

( if work/is-work 
    (setq multi-term-program "/bin/tcsh")
    (setq multi-term-program "/bin/zsh")
    )

;; From: http://www.emacswiki.org/emacs/ShellDirtrackByProcfs
(defun term-resync-dirs ()
"resync dirs as shell-resync-dirs does"
(interactive)
(let ((directory (file-symlink-p
		  (format "/proc/%s/cwd"
			  (process-id
			   (get-buffer-process
			    (current-buffer)))))))
  (when (file-directory-p directory)
    (cd directory)))
)
;; From: http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(add-hook 'term-mode-hook
          (lambda ()
	    ;; Set the buffer size of the terminal
            (setq term-buffer-maximum-size 10000)
	    ;; Disable yasnippet, since it interfere with tab-completion
            (yas-minor-mode -1)      
;; ============================= Key bindings ==================================
	    ;;(add-to-list 'term-bind-key-alist '("C-c C-j" . jnm/term-toggle-mode))     
	    (define-key term-raw-map (kbd "C-y") 'term-paste)
	    (define-key term-raw-map (kbd"C-c C-j") 'jnm/term-toggle-mode)
	    ))
