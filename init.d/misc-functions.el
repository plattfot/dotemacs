;; =============================================================================
;; Misc Functions:
;; 
;; =============================================================================

;; From:
;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; -----------------------------------------------------------------------------
(defun run-emacs-shell-command (command)
  "Runs the COMMMAND in a emacs shell. Works only if the current buffer is a shell."
  (let ((process (get-buffer-process (current-buffer))))
    (unless process
      (error "No process in %s" buffer-or-name))
    (goto-char (process-mark process))
    (insert command)
    (comint-send-input nil t ) ;; hit enter
    (term-s)
    )
  )
;; -----------------------------------------------------------------------------
(defun run-emacs-term-command (command)
  "Runs the COMMMAND in a emacs term. Works only if the current buffer is a term."
  (let ((process (get-buffer-process (current-buffer))))
    (unless process
      (error "No process in %s" buffer-or-name))
    (goto-char (process-mark process))
    (insert command)
    (term-send-input) ;; hit enter
    )
  )

;; -----------------------------------------------------------------------------
;; From http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; -----------------------------------------------------------------------------
(defun clear-shell ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))

;; -----------------------------------------------------------------------------
(defun clear-term ()
   (interactive)
   (run-emacs-term-command "clear")
   )

;; ============================= Key bindings ==================================

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
