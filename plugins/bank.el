;;; bank --- Functions for dealing banking -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 's)
(require 'dash)
(require 'simple)
(require 'bfuture)
(require 'dired)

;;;###autoload
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

;;;###autoload
(cl-defun bank-us-to-iso-timestring (file)
  "Fixing time strings on statement files.

Expected file name to be \"PATH/MMMDDYY XXXXXX.EXT\",
where XXXXX is the id for the account, MMMDDYY is the date
e.g. 011221."
  (let* ((date-re
          (rx (group (= 2 (any digit)))
              (group (= 2 (any digit)))
              (group (= 2 (any digit)))
              ))
         (path (file-name-directory file))
         (name (file-name-base file))
         (ext (file-name-extension file))
         (parts (split-string name)))
    (when (< (length parts) 2)
      (cl-return-from rbc-fix-timestring file))
    (let ((date (s-match date-re (nth 0 parts)))
          (fix-date (lambda (match)
                      (format-time-string
                       "%Y-%m-%d"
                       (date-to-time (format "%s %s %s 00:00:00"
                                             (nth 3 match)
                                             (nth 1 match)
                                             (nth 2 match)))))))
      (if (and date)
        (concat (file-name-as-directory path)
                (funcall fix-date date)
                " "
                (nth 1 parts)
                "." ext)
        file))))

;;;###autoload
(defun bank-dired-map-rename (func)
  "Apply FUNC on each marked file to rename."
  (interactive "aFunction: ")
  (let ((procs
         (--map
          (bfuture-new "mv" it (funcall func it))
          (dired-get-marked-files))))
    (bfuture-result-when-all-done procs)))

(provide 'bank)
;;; bank.el ends here
