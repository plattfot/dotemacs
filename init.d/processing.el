;; =============================================================================
;; Processing:
;; =============================================================================

;; Setup Processing
(setq processing-location "/usr/bin/processing-java")
(setq processing-application-dir "/usr/share/processing")
(setq processing-sketchbook-dir "~/sketchbook")

(defun processing-mode-init ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary ac-source-yasnippet))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary (append processing-functions
                                   processing-builtins
                                   processing-constants)))

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)
