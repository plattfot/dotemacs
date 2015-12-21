;; =============================================================================
;; Python
;; =============================================================================

;; Indentation
(setq python-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq python-indent 2))))
