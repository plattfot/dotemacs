;;; training.el --- Training related functions
;;; Commentary:

;;; Code:
(require 'subr-x)
(require 'ledger-mode)

(cl-defstruct exercise name load volume rpe comments)

(defun tr-log-to-dat ()
"Convert old training log to something ledger can understand.

Will read the current buffer and output it to a buffer named training.dat."
  (interactive)
  (let ((exercises (make-hash-table :test 'equal)))
    (let* ((regionp (region-active-p))
           (beg (and regionp (region-beginning)))
           (end (and regionp (region-end)))
           (line)
           (exercise-name "unknown")
           (exercise-re (concat "^\\([0-9-.]+\\|bw\\|bts\\)"
                                "[ (]*\\(.*?\\)[) ]*"
                                "@\\([0-9]+\\)"
                                "[ ]*\\([0-9]+/[0-9]+/[0-9]+\\)"
                                "[ ]*\\(?:; \\(.*\\)\\)?")))
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq line (buffer-substring (point) (point-at-eol)))
          (unless (string-empty-p line)
            (if (string-match exercise-re line)
                (let ((exercise (make-exercise
                                 :name exercise-name
                                 :load (match-string 1 line)
                                 :volume (match-string 2 line)
                                 :rpe (match-string 3 line)
                                 :comments (or (match-string 5 line) "")))
                      (date (match-string 4 line)))
                  (let ((value (gethash date exercises)))
                    (if value
                        (puthash date (push exercise value) exercises)
                      (puthash date `(,exercise) exercises))))
              (setq exercise-name line)))
          (beginning-of-line 2))))

    (let ((buf.dat (generate-new-buffer "training.dat")))
      (switch-to-buffer buf.dat)
      (maphash (lambda (key value)
                 (insert (format "%s * \n" (tr-valid-date key)))
                 (mapc (lambda (exercise)
                         (insert (format "    Övning:%s  (%s) @%slbs ; RPE: %s %s\n"
                                         (exercise-name exercise)
                                         (tr-valid-volume (exercise-volume exercise))
                                         (string-to-number (exercise-load exercise))
                                         (exercise-rpe exercise)
                                         (exercise-comments exercise))))
                         value)
                 (insert "    Gym:Steve Nash\n\n"))
               exercises)
      (ledger-mode)
      (goto-char (point-min))
      (while (search-forward "AMRAP" nil t)
        (replace-match ":AMRAP:"))
      (ledger-mode-clean-buffer)
      (delete-trailing-whitespace)
      (goto-char (point-max)))))

(defun tr-valid-date (date)
  "Convert DATE from dd/mm/YY to 20YY/mm/dd."
  (concat "20" (string-join (reverse (split-string date "/")) "/")))

(defun tr-valid-volume (volume)
  "Convert VOLUME to a volume ledger can understand.

For example:
4x5 -> 4 reps * 5 sets.
5+5+4+5 -> 5 reps + 5 reps + 4 reps + 5 reps.
4 + 10x2 + 4 -> 4 reps 10 reps * 2 sets + 4 reps"
  ;; TODO: Find a better way that doesn't involve this many regex
  ;; searches.
  (replace-regexp-in-string
   "\\([0-9]+\\)\\($\\|[ ]+[^rs]\\)" "\\1 reps\\2"
   (replace-regexp-in-string
    "\\+\\([^ ]\\)" "+ \\1"
    (replace-regexp-in-string
     "\\([^ ]\\)\\+" "\\1 +"
     (replace-regexp-in-string
      "\\([0-9]+\\)sx\\([0-9]+\\)" "\\1s * \\2 sets"
      (replace-regexp-in-string
       "\\([0-9]+\\)x\\([0-9]+\\)" "\\1 reps * \\2 sets"
       volume))))))

(defun tr-exercise-history ()
  "Display temporary buffer with history of the exercise."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (search-forward-regexp "[^[:blank:]]")
    (let ((exercise-start (- (point) 1))
          (truncate-lines t))
      (search-forward-regexp "[ ]\\{2\\}")
      (occur (buffer-substring-no-properties exercise-start (- (point) 2))))))

(provide 'training)
;;; training.el ends here
