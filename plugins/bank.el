;;; bank --- Functions for dealing banking -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 's)
(cl-defun bank-fix-timestring (file)
  "Fixing time strings on statement files.

Expected file name to be PATH/XXXXX-YYYYMMMDD-YYYYMMMDD.EXT,
where XXXXX is the id for the account, YYYYMMMDD is the from date
e.g. 2020Feb15. and the other is the to date."
  (let* ((date-re
          (rx (group (= 4 (any digit)))
              (group (= 3 (any alpha)))
              (group (= 2 (any digit)))))
         (path (file-name-directory file))
         (name (file-name-base file))
         (ext (file-name-extension file))
         (parts (s-split "-" name)))
    (when (< (length parts) 3)
      (cl-return-from rbc-fix-timestring file))
    (let ((from (s-match date-re (nth 1 parts)))
          (to (s-match date-re (nth 2 parts)))
          (fix-date (lambda (match)
                      (format-time-string
                       "%Y%m%d"
                       (date-to-time (format "%s %s %s 00:00:00"
                                             (nth 1 match)
                                             (nth 2 match)
                                             (nth 3 match)))))))
      (if (and from to)
        (concat (file-name-as-directory path)
                (nth 0 parts)
                "-"
                (funcall fix-date from)
                "-"
                (funcall fix-date to)
                "." ext)
        file))))
(provide 'bank)
;;; bank.el ends here
