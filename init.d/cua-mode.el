;; =============================================================================
;; CUA mode
;; =============================================================================

;; Annoying thing when cua-mode is enable. Highlighting with the mouse
;; becomes a pain. Due to that in cua-mode the highlight selection
;; follows the cursor when scrolling.
;; (cua-mode t) ;; 
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (setq cua-enable-cua-keys nil) ;; disable windows shortcuts
;; (cua-selection-mode nil )
(set-variable 'shift-select-mode t)

