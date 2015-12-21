(defvar hou/comp-format '(("==" . "// %s")
			  ("!=" . "// all but %s")
			  (">=" . "// %s or later")
			  ("<=" . "// %s or earlier")
			  (">"  . "// later than %s")
			  ("<"  . "// earlier than %s")
			  ))

(defun hou-version-to-hex (version-string)
  "Convert VERSION-STRNG to hex, instead of looking up in a table
better just to compute it."
  ;; Split the string into parts, filtering out the pre
  (let ((vers_parts (delete "" (split-string version-string "[.]\\|pre[0-9]+"))))
    ;; If it's not three parts, i.e major, minor and build version throw error
    (when (not (eq (length vers_parts) 3))
      (throw 'hou-tag (format "%s is not a valid version string" version-string)))
    
    ;; Convert the parts to integers and then print them out in the
    ;; format houdini is using.
    (let ((vers_int (mapcar #'(lambda (x) (string-to-number x)) vers_parts)))
      (format "0x%02x%02x%04x" (nth 0 vers_int) (nth 1 vers_int) (nth 2 vers_int))
      ))
  )

(defun hou-get-version-list (&optional root)
  "Search the path
ROOT/*/toolkit/include/{UT,SYS}/{UT,SYS}_Version.h for
UT_VERSION_INT or SYS_VERSION_INT and fetch the INT version."
  (when (not root) (setq root "/tools/package/houdini"))
  (shell-command-to-string
   (concat "grep -E \"SYS_VERSION_FULL_INT|UT_VERSION_INT\" "
	   root
	   "/*/toolkit/include/{SYS/SYS,UT/UT}_Version.h"
	   "| sed -En "
	   "'s:.*?/([0-9]+\.[0-9]+\.[0-9a-z]+)/.*?(0x[0-9a-f]+):"
	   "(\\1 . \\2):p'"))
  )

(defun hou-insert-version-id (version)
  "Insert the houdini version id for VERSION"
  (interactive "sHoudini version: ")
  (let ((version_hex (hou-version-to-hex version)))
    (insert version_hex)
  )
)

(defun hou-insert-if (version comp)
  "Insert an #if-else clause with the specified VERSION in int
format with the COMP being either ==, <=, >=, !=, < or > . If a
region is active it will place that in both the ifdef and else
clause.  Example:
(hou-insert-ifdef \"14.0.173\" \"==\") will insert this
#if( UT_VERSION_INT == 0x0e0000ad ) // 14.0.173
<Cursor>
#else
#endif

(hou-insert-ifdef \"14.0.173\" \">=\") will insert this
#if( UT_VERSION_INT >= 0x0e0000ad ) // 14.0.173 or later
<Cursor>
#else
#endif

(hou-insert-ifdef \"14.0.173\" \"<=\") will insert this
#if( UT_VERSION_INT <= 0x0e0000ad ) // 14.0.173 or earlier
<Cursor>
#else
#endif

" 
(interactive "sHoudini version: \nsCompare operator: ")
(let ((str))
  (if (region-active-p)
      (progn (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
	     (kill-region (region-beginning) (region-end)))
    (setq str "")
    )
  (let ((version_hex (hou-version-to-hex version))
	(format_assoc (assoc comp hou/comp-format))
	(done))
    (when (not format_assoc)
      (throw 'hou-tag (format "%s is not in comp list" comp)))

    (insert "#if( UT_VERSION_INT " comp " " version_hex " ) "
	    (format (cdr format_assoc) version) "\n")
    (let ((done (point)))
      (insert str "\n#else\n" str "\n#endif\n")
      (goto-char done))
    )
  )
)

;; Following the same naming convention as gtest
;; eq -> ==
;; ne -> !=
;; lt -> <
;; le -> <=
;; gt -> >
;; ge -> >=

(defun hou-insert-if-eq (version)
"Wrapper for calling hou-insert-if with arguments VERSION and =="
(interactive "sHoudini version: ")
(hou-insert-if version "==")
)

(defun hou-insert-if-ne (version)
"Wrapper for calling hou-insert-if with arguments VERSION and !="
(interactive "sHoudini version: ")
(hou-insert-if version "!=")
)

(defun hou-insert-if-lt (version)
"Wrapper for calling hou-insert-if with arguments VERSION and <"
(interactive "sHoudini version: ")
(hou-insert-if version "<")
)

(defun hou-insert-if-le (version)
"Wrapper for calling hou-insert-if with arguments VERSION and <="
(interactive "sHoudini version: ")
(hou-insert-if version "<=")
)

(defun hou-insert-if-gt (version)
"Wrapper for calling hou-insert-if with arguments VERSION and >"
(interactive "sHoudini version: ")
(hou-insert-if version ">")
)

(defun hou-insert-if-ge (version)
"Wrapper for calling hou-insert-if with arguments VERSION and >="
(interactive "sHoudini version: ")
(hou-insert-if version ">=")
)


