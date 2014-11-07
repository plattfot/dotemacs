;; =============================================================================
;; Init gccsense
;; =============================================================================
;; Better auto complete for c++

;; Add the plugin to the search path
(let ((default-directory "~/.emacs.d/plugins/"))
      (normal-top-level-add-to-load-path 
       '("gccsense")))

;; Enable gccsense
(require 'gccsense)

;; Set environment variables for gccsense
;; (setenv "PATH" (concat "/dd/dept/software/3ps/gccsense/bin" (getenv "PATH")))
;; (setq exec-path (append exec-path '("/dd/dept/software/3ps/gccsense/bin")))
;; (setenv "LD_LIBRARY_PATH" "/dd/dept/software/3ps/gccsense/lib")

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (flymake-mode)
;;             (gccsense-flymake-setup)))

;; ============================= Key bindings ==================================
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-gccsense)))
