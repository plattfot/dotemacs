(defvar hou/version-list '(("14.0.132pre18" . "0x0e000084")
			   ("14.0.132pre29" . "0x0e000084")
			   ("14.0.173" . "0x0e0000ad")
			   ("14.0.201pre6" . "0x0e0000c9")
			   ("14.0.233" . "0x0e0000e9")
			   ("12.0.581" . "0x0c000245")
			   ("12.0.634" . "0x0c00027a")
			   ("12.0.670" . "0x0c00029e")
			   ("12.0.683" . "0x0c0002ab")
			   ("12.0.754" . "0x0c0002f2")
			   ("12.1.125" . "0x0c01007d")
			   ("12.1.185" . "0x0c0100b9")
			   ("12.1.42" . "0x0c01002a")
			   ("12.1.57" . "0x0c010039")
			   ("12.5.469" . "0x0c0501d5")
			   ("13.0.222" . "0x0d0000de")
			   ("13.0.260" . "0x0d000104")
			   ("13.0.376" . "0x0d000178")
			   ("13.0.524" . "0x0d00020c")
			   ("13.0.655" . "0x0d00028f")))


(defvar hou/comp-format '(("==" . "// %s")
			  ("!=" . "// all but %s")
			  (">=" . "// %s or later")
			  ("<=" . "// %s or earlier")
			  (">"  . "// earlier than %s")
			  ("<"  . "// later than %s")))

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
  (let ((version_assoc (assoc version hou/version-list))
	(format_assoc (assoc comp hou/comp-format))
	(done))
    (when (not version_assoc)
      (throw 'hou-tag (format "%s is not in version list" version)))
    (when (not format_assoc)
      (throw 'hou-tag (format "%s is not in comp list" comp)))

    (insert "#if( UT_VERSION_INT " comp " " (cdr version_assoc) " ) "
	    (format (cdr format_assoc) version) "\n")
    (setq done (point))
    (insert str "\n#else\n" str "\n#endif")
    (goto-char done)
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


