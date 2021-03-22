;;; cpreproc.el --- Collection of functions to help with c/c++ development at work

;; Author: Fredrik Salomonsosn <plattfot@gmail.com>

;;; Commentary:

;;; Code:
(require 'subr-x)

(defvar cpreproc-comp-format '(("==" . "%s")
                               ("!=" . "all but %s")
                               (">=" . "%s or later")
                               ("<=" . "%s or earlier")
                               (">"  . "later than %s")
                               ("<"  . "earlier than %s")))

(defun cpreproc-insert-if (variable comp version &optional version-str no-else)
  "Insert an #if clause.
In the format of #if (VARIABLE COMP VERSION) // VERSION[-STR].

Where the VARIABLE specify the variable to compare against the
VERSION.

The VERSION type can be either a string or an integer.  In the
end it must be an integer the c preprocessor understands.
Recommended to use a string to keep the value intact.  As
\"0x0e0000ad\" will be inserted as is, where as #x0e0000ad will
evaluate to 234881197 before it's inserted.

The COMP being either ==, <=, >=, !=, < or >.

If specified the VERSION-STR is used for the comment instead of
VERSION, and should be seen as a more human readable description
of VERSION.  An example is houdini's UT_VERSION_INT which is a
packed hex value of the version number, for example 14.0.173 =>
0x0e0000ad.

If NO-ELSE is true it will not insert an else clause.

If a region is active it will place that in
both the ifdef and else clause.  Example:
`(cpreproc-insert-if \"__cplusplus\" \"201103L\" \"==\" nil \"c++11\")'
will insert

#if (__cplusplus == 201103L) // c++11
<Cursor>
#else
#endif

`(cpreproc-insert-if \"__cplusplus\" \"201103L\" \">=\")' will
insert
#if (__cplusplus >= 201103L t) // 201103L or later
<Cursor>
#endif"
(let ((str nil))
  (when (region-active-p)
    (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
    (delete-region (region-beginning) (region-end)))
  (unless version-str
    (setq version-str version))
  (let ((format_assoc (assoc comp cpreproc-comp-format)))
    (unless format_assoc
      (error (format "%s is not in cpreproc-comp-format." comp)))

    (insert (format "#if (%s %s %s) // %s\n"
                    variable comp version (format (cdr format_assoc) version-str)))

    (let ((done (point)))
      (insert (or str "\n"))
      (unless no-else
        (insert "#else\n" (or str "\n")))
      (insert "#endif\n")
      (goto-char done)))))

(defconst cpreproc-cplusplus
  '(("c++98" . "199711L")
    ("c++03" . "199711L")
    ("c++11" . "201103L")
    ("c++14" . "201402L")
    ("c++17" . "201703L")
    "Alist of what __cplusplus is set to for the different language standards."))

(defgroup cpreproc nil
  "Functions to insert macros in c/c++"
  :group 'tools)

(defcustom cpreproc-if-cplusplus-std-default nil
  "Default for the std in cpreproc-if-cplusplus."
  :type 'string
  :group 'cpreproc)

(defvar cpreproc-if-cplusplus-std-history nil
  "History for the std in cpreproc-if-cplusplus.")

(defvar cpreproc-if-cplusplus-comp-history nil
  "History for the comp in cpreproc-if-cplusplus.")

(defun cpreproc-if-cplusplus (std comp)
  "Insert preprocessor if clause for __cplusplus (c++ lang standard).
STD specifies the standard to compare against, e.g c++03, c++11,
c++17.  COMP is the compare opartor and must be one of ==, <=,
>=, !=, < or >.  If prefix argument is set it will not insert the
else clause."
(interactive
 (let ((std-prompt "C++ languange standard")
       (std-default (cond
                     ((and cpreproc-if-cplusplus-std-default
                           (not (string-empty-p cpreproc-if-cplusplus-std-default)))
                      cpreproc-if-cplusplus-std-default)
                     ((and cpreproc-if-cplusplus-std-history
                           (not (string-empty-p
                                 (car cpreproc-if-cplusplus-std-history))))
                      (car cpreproc-if-cplusplus-std-history)))))
   (list (read-string
          (if std-default
              (format "%s (default %s): " std-prompt std-default)
            (format "%s: " std-prompt))
          nil
          'cpreproc-if-cplusplus-std-history
          std-default)
         (read-string
          "Compare operator: "
          nil
          'cpreproc-if-cplusplus-comp-history))))
(let ((std-version (assoc std cpreproc-cplusplus)))
  (unless std-version
    (error (format "\"%s\" is not a supported/valid c++ language standard." std)))
  (cpreproc-insert-if "__cplusplus" comp (cdr std-version) std current-prefix-arg)))

(provide 'cpreproc)
;;; cpreproc.el ends here
