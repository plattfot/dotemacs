;;; highlight-extra --- Extra functions for highlighting builds.
;;; Commentary:

;;; Code:
(defface hi-orange
  '((t (:foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-dark-orange
  '((t (:foreground "#dd7b3b")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gras
  '((t (:foreground "#99cf50")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-string-green
  '((t (:foreground "#65b042")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-grey
  '((t (:foreground "#666")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-crimson
  '((t (:foreground "#D80000")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow-b
  '((((min-colors 88)) (:weight bold :foreground "#f9fd75"))
    (t (:weight bold :foreground "#f9fd75")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta-b
  '((((min-colors 88)) (:weight bold :foreground "#ff00ff"))
    (t (:weight bold :foreground "#ff00ff")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun highlight-gtest()
  "Highlight gtest output"
  (interactive)
  ( highlight-regexp "[ ]\\{7\\}OK[ ]\\{1\\}" 'hi-string-green )
  ( highlight-regexp "[ ]\\{2\\}PASSED[ ]\\{2\\}" 'hi-string-green )
  ( highlight-regexp "[ ]\\{2\\}FAILED[ ]\\{2\\}" 'hi-crimson ))

(defun unhighlight-gtest()
  "Unhighlight gtest output"
  (interactive)
  ( unhighlight-regexp "[ ]\\{7\\}OK[ ]\\{1\\}" )
  ( unhighlight-regexp "[ ]\\{2\\}PASSED[ ]\\{2\\}" )
  ( unhighlight-regexp "[ ]\\{2\\}FAILED[ ]\\{2\\}" ))

(defun highlight-build()
  "Highlight flags, paranthesis, error, warning and const to
easier find them when building."
  (interactive)
  ( highlight-regexp "warning" 'hi-yellow-b )
  ( highlight-regexp "error"   'hi-red-b )
  ( highlight-regexp "const "  'hi-black-b )
  ( highlight-regexp "[[:alnum:]_-]+\\.[[:alpha:]]+:[[:digit:]]+" 'hi-orange)
  ( highlight-regexp "undefined reference to" 'hi-magenta-b ))

(defun unhighlight-build()
  "Like the function name applies remove the highlights set by highlight-build."
  (interactive)
  ( unhighlight-regexp "warning")
  ( unhighlight-regexp "error")
  ( unhighlight-regexp "const ")
  ( unhighlight-regexp "[[:alnum:]_-]+\\.[[:alpha:]]+:[[:digit:]]+" )
  ( unhighlight-regexp "undefined reference to"))

(defun highlight-versions( input )
  "Highlight important versions when building with pybuild."
  (interactive "sEnter name of packages to highlight (separated by space) ")
  (let ((list (split-string input)))
    (dolist (word list)
      (highlight-regexp
       (concat (upcase word) "_VERSION=[0-9]+\.[0-9]+\.[0-9_a-z]+")
       'hi-green-b))
    (dolist (word list)
      (highlight-regexp
       (concat (downcase word) "/[0-9]+\.[0-9]+\.[0-9_a-z]+")
       'hi-green-b))))

(defun unhighlight-versions(input)
  "Unhighlight versions that was highlighted with highlight-versions."
  (interactive "sEnter name of packages to unhighlight (separated by space) ")
  (let ((list (split-string input)))
    (dolist (word list)
      (unhighlight-regexp
       (concat (upcase word) "_VERSION=[0-9]+\.[0-9]+\.[0-9_a-z]+")))
    (dolist (word list)
      (unhighlight-regexp
       (concat (downcase word) "/[0-9]+\.[0-9]+\.[0-9_a-z]+") ))))

(defun highlight-usual-vers()
  "Highlights the most common versions openvdb, boost and houdini"
  (interactive)
  (highlight-versions "openvdb boost houdini"))

(provide 'highlight-extra)
;;; highlight-extra ends here

