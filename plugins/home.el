;;; home --- Functions for setting up home environment -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dotemacs)

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
  (async-shell-command "mbsync -ac $XDG_HOME_CONFIG/isync/config; notmuch new")
  (set-process-sentinel (get-buffer-process "*Async Shell Command*")
                        (lambda (process event)
                          (when (eq (process-status process) 'exit)
                            (with-current-buffer "*Async Shell Command*"
                              (save-excursion
                                (goto-char (point-max))
                                (next-line -3)
                                (message "Mail synced: %s" (buffer-substring
                                                            (point-at-bol)
                                                            (point-at-eol)))))))))

(provide 'home)
;;; home.el ends here
