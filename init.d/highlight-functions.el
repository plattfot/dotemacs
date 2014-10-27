;; =============================================================================
;; Highlight functions
;; =============================================================================

;; ;; (custom-set-faces
;; ;;  ;; custom-set-faces was added by Custom.
;; ;;  ;; If you edit it by hand, you could mess it up, so be careful.
;; ;;  ;; Your init file should contain only one such instance.
;; ;;  ;; If there is more than one, they won't work right.
;; ;;  '(hi-blue ((t (:foreground "color-106"))))
;; ;;  '(hi-blue-b ((t (:foreground "color-245" :weight bold))))
;; ;;  '(hi-green ((t (:foreground "color-100"))))
;; ;;  '(hi-pink ((t (:foreground "color-94"))))
;; ;;  '(hi-yellow ((t (:foreground "color-130")))))
(defface hi-orange
  '((t (:foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-grey
  '((t (:foreground "brightblack")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun highlight-dd-logger() 
  "Highlight DD_LOGGER"
  (interactive)
  ( highlight-regexp "^\\+-.*\\]:" 'hi-blue )
  ( highlight-regexp "^\\(| \\)\\{1\\}\\+-.*\\]:" 'hi-green )
  ( highlight-regexp "^\\(| \\)\\{2\\}\\+-.*\\]:" 'hi-pink )
  ( highlight-regexp "^\\(| \\)\\{3\\}\\+-.*\\]:" 'hi-yellow )
  ( highlight-regexp "^\\(| \\)\\{3\\}" 'hi-pink )
  ( highlight-regexp "^\\(| \\)\\{2\\}" 'hi-green )
  ( highlight-regexp "^\\(| \\)\\{1\\}" 'hi-blue )
  ( highlight-regexp "[0-9]+\\.[0-9]+[a-z]+" 'hi-blue-b ))

(defun unhighlight-dd-logger() 
  "Unhighlight DD_LOGGER"
  (interactive)
  ( unhighlight-regexp "^\\+-.*\\]:" )
  ( unhighlight-regexp "^\\(| \\)\\{1\\}\\+-.*\\]:" )
  ( unhighlight-regexp "^\\(| \\)\\{2\\}\\+-.*\\]:" )
  ( unhighlight-regexp "^\\(| \\)\\{3\\}\\+-.*\\]:" )
  ( unhighlight-regexp "^\\(| \\)\\{3\\}" )
  ( unhighlight-regexp "^\\(| \\)\\{2\\}" )
  ( unhighlight-regexp "^\\(| \\)\\{1\\}" )
  ( unhighlight-regexp "[0-9]+\\.[0-9]+\\w" ))

(defun highlight-build() 
  "Highlight paranthesis, error, warning and 
const to easier find them when building."
  (interactive)
  ( highlight-regexp "[()]"    'hi-red-b )
  ( highlight-regexp "warning" 'hi-green-b )
  ( highlight-regexp "error"   'hi-red-b )
  ( highlight-regexp "const "  'hi-black-b )
  ( highlight-regexp "[a-zA-Z]+\.[a-zA-Z]+:[0-9]+" 'hi-orange ))

(defun highlight-versions( input )
  "Highlight important versions when building with pybuild."
  (interactive "sEnter name of packages to highlight (separated by space) ")
  (setq list (split-string input))
  (dolist (word list) 
    (highlight-regexp 
     (concat (upcase word) "_VERSION=[0-9]+\.[0-9]+\.[0-9_a-z]+") 
     'hi-green-b)
    )
  (dolist (word list) 
    (highlight-regexp 
     (concat (downcase word) "/[0-9]+\.[0-9]+\.[0-9_a-z]+") 
     'hi-green-b)
    )
  )

(defun unhighlight-versions(input)
  "Unhighlight versions that was highlighted with highlight-versions."
  (interactive "sEnter name of packages to unhighlight (separated by space) ")
  (setq list (split-string input))
  (dolist (word list) 
    (unhighlight-regexp 
     (concat (upcase word) "_VERSION=[0-9]+\.[0-9]+\.[0-9_a-z]+"))
    )
  (dolist (word list) 
    (unhighlight-regexp 
     (concat (downcase word) "/[0-9]+\.[0-9]+\.[0-9_a-z]+") )
    )
  )

(defun highlight-usual-vers()
  "Highlights the most common versions openvdb, boost and houdini"
  (interactive)
  (highlight-versions "openvdb boost houdini"))

;; (defface hi-red-b
;;   '((((min-colors 88)) (:weight bold :foreground "red1"))
;;     (t (:weight bold :foreground "red")))
;;   "Face for hi-lock mode."
;;   :group 'hi-lock-faces)

