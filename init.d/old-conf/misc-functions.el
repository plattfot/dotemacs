;; =============================================================================
;; Misc Functions:
;; 
;; =============================================================================
;; From https://www.emacswiki.org/emacs/Journal but modified it to use let
(defun yesterday-time ()
"Provide the date/time 24 hours before the time now in the format of current-time."
  (let* ((now-time (current-time))              ; get the time now
	 (hi (car now-time))                    ; save off the high word
	 (lo (car (cdr now-time)))              ; save off the low word
	 (msecs (nth 2 now-time))               ; save off the milliseconds
	 )

    (if (< lo 20864)                      ; if the low word is too small for subtracting
	(setq hi (- hi 2)  lo (+ lo 44672)) ; take 2 from the high word and add to the low
      (setq hi (- hi 1) lo (- lo 20864))  ; else, add 86400 seconds (in two parts)
      )
    (list hi lo msecs)                    ; regurgitate the new values
    ))

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
  "Delete everything in the buffer."
   (interactive)
   (delete-region (point-min) (point-max))
   (run-emacs-term-command "clear")
   )

;; ============================= Key bindings ==================================

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
