;;; highlight-extra --- Extra functions for highlighting builds.
;;; Commentary:

;;; Code:
(defface hi-orange
  '((t (:foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-grey
  '((t (:foreground "#666")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun highlight-dd-logger()
  "Highlight DD_LOGGER."
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
  "Unhighlight DD_LOGGER."
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
  "Highlight flags, paranthesis, error, warning and const to
easier find them when building."
  (interactive)
  ( highlight-regexp "-\\{1,2\\}[a-zA-Z0-9_]+" 'hi-grey)
  ( highlight-regexp "[()]"    'hi-red-b )
  ( highlight-regexp "warning" 'hi-green-b )
  ( highlight-regexp "error"   'hi-red-b )
  ( highlight-regexp "const "  'hi-black-b )
  ( highlight-regexp "[a-zA-Z]+\.[a-zA-Z]+:[0-9]+" 'hi-orange ))

(defun unhighlight-build()
  "Like the function name applies remove the highlights set by highlight-build."
  (interactive)
  ( unhighlight-regexp "-\\{1,2\\}[a-zA-Z0-9_]+" )
  ( unhighlight-regexp "[()]" )
  ( unhighlight-regexp "warning")
  ( unhighlight-regexp "error")
  ( unhighlight-regexp "const ")
  ( unhighlight-regexp "[a-zA-Z]+\.[a-zA-Z]+:[0-9]+" ))

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

