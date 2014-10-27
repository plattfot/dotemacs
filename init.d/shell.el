;; =============================================================================
;; Shell
;; =============================================================================

;; Indent using spaces
(setq sh-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
