;;; tig.el --- Small library for helping with automate git tasks
;;; Commentary:
;; Right now only tested with sway
;;; Code:
(require 'eieio)
(require 's)
(require 'dash)

(defclass tig-entry ()
  ((xy :initarg :xy
       :initform ".."
       :type string
       :documentation "A 2 character field containing the staged
       and unstaged XY values. Unchanged value is indicated by
       \".\", see git status --help for more info.")
   (sub :initarg :sub
        :initform "...."
        :type string
        :documentation "A 4 character field describing the
        submodule state. \"N...\" when the entry is not a
        submodule.")
   (path :initarg :path
         :type string
         :documentation "The pathname. In a renamed/copied entry,
         this is the target path."))
  "Describe the basics of a git entry.")

(defclass tig-changed-entry (tig-entry)
  ((mode-head :initarg :mode-head
              :initform #o000000
              :type integer
              :documentation "The octal file mode in HEAD.")
   (mode-index :initarg :mode-index
               :initform #o000000
               :type integer
               :documentation "The octal file mode in the index.")
   (mode-worktree :initarg :mode-worktree
                  :initform #o000000
                  :type integer
                  :documentation "The octal file mode in the worktree.")
   (object-head :initarg :object-head
                :type string
                :documentation "The object name in HEAD.")
   (object-index :initarg :object-index
                 :type string
                 :documentation "The object name in the index."))
  "Describe a git entry that has been changed in some way")

(defclass tig-modified-entry (tig-changed-entry)
  ((type :initarg :type
         :type (member copy rename)
         :documentation "Specify what type of modification. The
         two types are copy or rename.")
   (score :initarg :score
          :type (integer 0 100)
          :documentation "The rename or copy score. Denoting the
   percentage of similarity between the path and original-path.")
   (original-path :initarg :original-path
                  :type string
                  :documentation "The pathname in the commit at
                  HEAD or in the index. I.e. tells where the
                  renamed/copied contents came from."))
  "Describe a git entry that has been copied or renamed.")

(defun tig--parse-status (output)
  "Parse OUTPUT and return a list of tig-entry objects.
Each line in OUTPUT is expected to be terminated with \0."
  (->> (s-split (char-to-string 0) output t)
       (--map (cond
               ((s-prefix? "1" it)
                (let ((fields (s-split-up-to " " it 8)))
                  (tig-changed-entry :xy (nth 1 fields)
                                         :sub (nth 2 fields)
                                         :mode-head (string-to-number (nth 3 fields) 8)
                                         :mode-index (string-to-number (nth 4 fields) 8)
                                         :mode-worktree (string-to-number (nth 5 fields) 8)
                                         :object-head (nth 6 fields)
                                         :object-index (nth 7 fields)
                                         :path (nth 8 fields))))
               ((s-prefix? "2" it)
                (let* ((fields (s-split-up-to " " it 9))
                       (paths (s-split "\t" (nth 9 fields))))
                  (tig-modified-entry :xy (nth 1 fields)
                                          :sub (nth 2 fields)
                                          :mode-head (string-to-number (nth 3 fields) 8)
                                          :mode-index (string-to-number (nth 4 fields) 8)
                                          :mode-worktree (string-to-number (nth 5 fields) 8)
                                          :object-head (nth 6 fields)
                                          :object-index (nth 7 fields)
                                          :type (if (s-prefix? "C" (nth 8 fields)) 'copy 'rename)
                                          :score (string-to-number (substring (nth 8 fields) 1 nil))
                                          :path (car paths)
                                          :original-path (cadr paths))))
               (t nil)))
       (-non-nil)))

(defun tig-git-status (&optional repo)
  "Run git status in REPO'.

If REPO is not specified it will use `default-directory'.

Return a list of tig-entry objects."
  (let ((default-directory (or repo default-directory)))
    (tig--parse-status (shell-command-to-string "git status -z --porcelain=2"))))

(provide 'tig)
;;; tig.el ends here
