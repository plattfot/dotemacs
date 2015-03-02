;; =============================================================================
;; Auto complete
;; =============================================================================

(require 'auto-complete-config)
(ac-config-default)

(define-key ac-completing-map "\t" 'ac-expand)
;; This will on occasion make emacs interpret S-c as C-c 
;; (define-key ac-completing-map "C-i" 'ac-expand)
(define-key ac-completing-map "\r" nil)

(add-to-list 'ac-modes 'makefile-gmake-mode)

(add-hook 'c++-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "//")))

(add-hook 'c-mode-common-hook 
	  '(lambda ()
	     
	     ;; ac-omni-completion-sources is made buffer local so
	     ;; you need to add it to a mode hook to activate on 
	     ;; whatever buffer you want to use it with.  This
	     ;; example uses C mode (as you probably surmised).

	     ;; auto-complete.el expects ac-omni-completion-sources to be
	     ;; a list of cons cells where each cell's car is a regex
	     ;; that describes the syntactical bits you want AutoComplete
	     ;; to be aware of. The cdr of each cell is the source that will
	     ;; supply the completion data.  The following tells autocomplete
	     ;; to begin completion when you type in a . or a ->

	     (add-to-list 'ac-omni-completion-sources
			  (cons "\\." '(ac-source-semantic)))
	     (add-to-list 'ac-omni-completion-sources
			  (cons "->" '(ac-source-semantic)))

;; 	     ;; ac-sources was also made buffer local in new versions of
;; 	     ;; autocomplete.  In my case, I want AutoComplete to use 
;; 	     ;; semantic and yasnippet (order matters, if reversed snippets
;; 	     ;; will appear before semantic tag completions).

;; 	     (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
 	     ))

;; ------------------------ Clang auto complete --------------------------------
;; (when (not dotemacs/is-work)
;;   (require 'auto-complete-clang-async)

;;   (defun ac-cc-mode-setup ()
;;     (setq ac-clang-complete-executable "~/.emacs.d/plugins/clang-complete/clang-complete")
;;     (setq ac-sources '(ac-source-clang-async))
;;     (setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
;;     (ac-clang-launch-completion-process))

;;   (defun my-ac-config ()
;;     (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;     (add-hook 'auto-complete-mode 'ac-common-setup)
;;     (global-auto-complete-mode t))

;;   (my-ac-config)
;;   )
