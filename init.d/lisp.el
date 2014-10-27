;; =============================================================================
;; Lisp
;; =============================================================================

;; Color hexvalues with their color, when in lisp-mode
(defvar hexcolour-keywords
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background 
				       (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'lisp-mode-hook 'hexcolour-add-to-font-lock)
