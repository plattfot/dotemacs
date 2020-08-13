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

(provide 'home)
;;; home.el ends here
