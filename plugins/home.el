;;; home --- Functions for setting up home environment -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defun home-setup ()
  "Splits the session into three frames."
  (interactive)
  (delete-other-frames)
  (delete-other-windows)
  (make-frame-command)
  (make-frame-command))

(provide 'home)
;;; home.el ends here
