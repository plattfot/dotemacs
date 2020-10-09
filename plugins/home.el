;;; home --- Functions for setting up home environment -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dotemacs)
(require 'f)

(defun home-setup ()
  "Splits the session into three frames."
  (interactive)
  (delete-other-frames)
  (delete-other-windows)
  (let ((frame-inherited-parameters '(font)))
    (make-frame)
    (make-frame)))

(defun home-mail-refresh ()
  "Run mbsync+notmuch to refresh mail."
  (interactive)
  (let ((buffer (get-buffer-create "*Mail Refresh*")))
    (message "Refreshing mail..." )
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    ;; TODO: filter the output for  to deal with long lines
    (make-process
     :name "mail-refresh"
     :buffer buffer
     :command `("mbsync" "-a"
                "-c" ,(f-join (getenv "XDG_HOME_CONFIG") "isync" "config"))
     :sentinel
     (lambda (process event)
       (when (eq (process-status process) 'exit)
         (make-process
          :name "notmuch-update"
          :buffer buffer
          :command '("notmuch" "new")
          :sentinel
          (lambda (process event)
            (when (eq (process-status process) 'exit)
              (with-current-buffer buffer
                (save-excursion
                  (goto-char (point-max))
                  (next-line -3)
                  (message "Mail synced: %s" (buffer-substring
                                              (point-at-bol)
                                              (point-at-eol)))))))))))))


(provide 'home)
;;; home.el ends here
