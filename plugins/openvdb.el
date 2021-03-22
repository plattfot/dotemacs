;;; openvdb.el --- Collection of functions to help with openvdb
;;; Commentary:

;;; Code:
(require 'cpreproc)

(defun vdb-insert-if (version comp)
  "Insert an #if-else clause.
The specified VERSION in int format with the COMP being either
==, <=, >=, !=, < or >.  If a region is active it will place that
in both the ifdef and else clause.  Example:
`(vdb-insert-if \"7.2.0\" \"==\")' will insert this
#if (OPENVDB_LIBRARY_VERSION_NUMBER == 0x07020000) // 7.2.0
<Cursor>
#else
#endif

(vdb-insert-if \"7.2.0\" \">=\") will insert this
#if (OPENVDB_LIBRARY_VERSION_NUMBER >= 0x07020000) // 7.2.0 or later
<Cursor>
#else
#endif

(vdb-insert-if \"7.2.0\" \"<=\") will insert this
#if (OPENVDB_LIBRARY_VERSION_NUMBER <= 0x07020000) // 7.2.0 or earlier
<Cursor>
#else
#endif"
(interactive "sOpenVDB version: \nsCompare operator: ")
(let ((version_hex (cpreproc-semver-to-hex version)))
  (cpreproc-insert-if "OPENVDB_LIBRARY_VERSION_NUMBER" comp version_hex version current-prefix-arg)))

;; Following the same naming convention as gtest
;; eq -> ==
;; ne -> !=
;; lt -> <
;; le -> <=
;; gt -> >
;; ge -> >=
(defun vdb-insert-if-eq (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and ==."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version "=="))

(defun vdb-insert-if-ne (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and !=."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version "!="))

(defun vdb-insert-if-lt (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and <."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version "<"))

(defun vdb-insert-if-le (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and <=."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version "<="))

(defun vdb-insert-if-gt (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and >."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version ">"))

(defun vdb-insert-if-ge (version)
  "Wrapper for calling `vdb-insert-if' with arguments VERSION and >=."
  (interactive "sOpenVDB version: ")
  (vdb-insert-if version ">="))
