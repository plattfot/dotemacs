;; <header>

(defconst dotemacs/path-to-dotemacs "<path-to-dotemacs>")
(defconst dotemacs/user-init-dir "<path-to-init.d>")

;; =============================================================================
;; Load init files 
;; url; http://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
;; author: seh
;; =============================================================================
(defun dotemacs/load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file dotemacs/user-init-dir)))				

(provide 'dotemacs)
