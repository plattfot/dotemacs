;;; highlight-extra --- Extra functions for highlighting. -*- lexical-binding: t -*-
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

(let ((re-ok "[ ]\\{7\\}OK[ ]\\{1\\}")
      (re-passed "[ ]\\{2\\}PASSED[ ]\\{2\\}")
      (re-failed "[ ]\\{2\\}FAILED[ ]\\{2\\}"))

  (defun highlight-gtest()
    "Highlight gtest output"
    (interactive)
    (highlight-regexp re-ok 'hi-string-green)
    (highlight-regexp re-passed 'hi-string-green)
    (highlight-regexp re-failed 'hi-crimson))

  (defun unhighlight-gtest()
  "Unhighlight gtest output"
  (interactive)
  (unhighlight-regexp re-ok)
  (unhighlight-regexp re-passed)
  (unhighlight-regexp re-failed)))

(let ((re-warning "warning")
      (re-error "error")
      (re-const "const")
      (re-error-line "[[:alnum:]_-]+\\.[[:alpha:]]+:[[:digit:]]+")
      (re-undefined-reference "undefined reference to"))

  (defun highlight-build()
    "Highlight flags, paranthesis, error, warning and const to
easier find them when building."
    (interactive)
    (highlight-regexp re-warning 'hi-yellow-b)
    (highlight-regexp re-error   'hi-red-b)
    (highlight-regexp re-const  'hi-black-b)
    (highlight-regexp re-error-line 'hi-orange)
    (highlight-regexp re-undefined-reference 'hi-magenta-b))

  (defun unhighlight-build()
    "Like the function name applies remove the highlights set by highlight-build."
    (interactive)
    (unhighlight-regexp re-warning)
    (unhighlight-regexp re-error)
    (unhighlight-regexp re-const)
    (unhighlight-regexp re-error)
    (unhighlight-regexp re-undefined-reference)))

(let ((re-version "%s_VERSION=[[:alnum:]._]+")
      (re-version-path "[[:alnum:]._]+"))

  (defun highlight-versions(input)
    "Highlight important versions in INPUT when building with pybuild."
    (interactive "sEnter name of packages to highlight (separated by space) ")
    (let ((list (split-string input)))
      (dolist (word list)
        (highlight-regexp (format re-version (upcase word)) 'hi-green-b))
      (dolist (word list)
        (highlight-regexp
         (concat (file-name-as-directory (downcase word)) re-version-path)
         'hi-green-b))))

  (defun unhighlight-versions(input)
    "Unhighlight versions that was highlighted with highlight-versions."
    (interactive "sEnter name of packages to unhighlight (separated by space) ")
    (let ((list (split-string input)))
      (dolist (word list)
        (unhighlight-regexp (format re-version (upcase word))))
      (dolist (word list)
        (unhighlight-regexp
         (concat (file-name-as-directory (downcase word)) re-version-path))))))

(defun highlight-usual-vers()
  "Highlight the most common versions; openvdb, boost and houdini."
  (interactive)
  (highlight-versions "openvdb boost houdini"))

(provide 'highlight-extra)
;;; highlight-extra ends here
