;;; cpreproc.el --- Easier create cpp macros for C/C++

;; Copyright (C) 2018-2021 Fredrik Salomonsson
;; Author: Fredrik Salomonsson <plattfot@posteo.net>
;; Created: 15 Feb 2020
;; Package-Requires: ((emacs "27.2") cl-lib seq subr-x)
;; Keywords: lisp
;; Version 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Contains function for wrapping a region in a #if macro for C/C++.

;; Convert a semver version to a hex value. As some c++ libraries tend
;; to pack their version string into an hex value, examples for this
;; are the houdini developer kit and openvdb.

;; Mapping c++ standards to the long int value defined in c++
;; preprocessor variable __cplusplus.

;;; Code:
(require 'subr-x)
(require 'seq)

(defgroup cpreproc nil
  "Functions to insert macros in c/c++."
  :group 'tools)

(defcustom cpreproc-if-cplusplus-std-default nil
  "Default for the std in `cpreproc-if-cplusplus'."
  :type 'string
  :group 'cpreproc)

(defun cpreproc-semver-to-hex (version-string)
  "Convert VERSION-STRING to hex.
Using what seems to be standard format 0x%02x%02x%04x."
  ;; Split the string into parts, filtering out the pre
  (let ((vers_parts (split-string version-string "[.]\\|pre[0-9]+" t)))
    (when (seq-empty-p vers_parts)
      (error "Failed to parse %s as a semantic version" version-string))

    ;; Convert the parts to integers and then print them out in the
    ;; somewhat standard format.
    (seq-let (major minor patch) (seq-map (lambda (x) (string-to-number x)) vers_parts)
      (format "0x%02x%02x%04x" major (or minor 0) (or patch 0)))))

(defvar cpreproc-comp-format '(("==" . "%s")
                               ("!=" . "all but %s")
                               (">=" . "%s or later")
                               ("<=" . "%s or earlier")
                               (">"  . "later than %s")
                               ("<"  . "earlier than %s")))

(defvar cpreproc-insert-if--variable-history nil
  "History for the std in `cpreproc-insert-if'.")

(defvar cpreproc-insert-if--comp-history nil
  "History for the comp in `cpreproc-insert-if'.")

(defvar cpreproc-insert-if--version-history nil
  "History for the version in `cpreproc-insert-if'.")

(defvar cpreproc-insert-if--version-str-history nil
  "History for the version-str in `cpreproc-insert-if'.")

;;;###autoload
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

\(cpreproc-insert-if \"__cplusplus\" \"201103L\" \"==\" \"c++11\" nil)
will insert

#if (__cplusplus == 201103L) // c++11
<Cursor>
#else
#endif

\(cpreproc-insert-if \"__cplusplus\" \"201103L\" \">=\" nil t)

will insert

#if (__cplusplus >= 201103L) // 201103L or later
<Cursor>
#endif

When called interactively, the universal argument (\\[universal-argument]) will set
NO-ELSE to t otherwise it will be nil."
  (interactive
   (let* ((in-variable (read-string "Variable: " nil 'cpreproc-insert-if--variable-history))
          (in-comp (read-string "Compare operator: " nil 'cpreproc-insert-if--comp-history))
          (in-version (read-string "Version: " nil 'cpreproc-insert-if--version-history))
          (in-version-str (read-string
                           (format "Version string (default %s): " in-version)
                           nil
                           'cpreproc-insert-if--version-str-history
                           in-version)))
     (list in-variable in-comp in-version in-version-str)))
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

      (save-mark-and-excursion
        (insert (or str "\n"))
        (unless no-else
          (insert "#else\n" (or str "\n")))
        (insert "#endif\n")))))

(defconst cpreproc-cplusplus
  '(("c++98" . "199711L")
    ("c++03" . "199711L")
    ("c++11" . "201103L")
    ("c++14" . "201402L")
    ("c++17" . "201703L")
    ("c++20" . "202002L")
    "Alist of what __cplusplus is set to for the different language standards."))

(defvar cpreproc-if-cplusplus--std-history nil
  "History for the std in `cpreproc-if-cplusplus'.")

(defvar cpreproc-if-cplusplus--comp-history nil
  "History for the comp in `cpreproc-if-cplusplus'.")

;;;###autoload
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
                     ((and cpreproc-if-cplusplus--std-history
                           (not (string-empty-p
                                 (car cpreproc-if-cplusplus--std-history))))
                      (car cpreproc-if-cplusplus--std-history)))))
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
