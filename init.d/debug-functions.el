;; =============================================================================
;; Debug Functions:
;; Lisp functions for debugging
;; =============================================================================

;; ----------------------------- PID Houdini -----------------------------------
(defun pid-houdini ()
"Get the pid for houdini, if more than one is running it returns
the first encounterd."
(interactive)
(car 
 (split-string 
  (shell-command-to-string
   (concat "ps aux | " ;; [h]oudini to not match itself
	   "sed -nE \"s/$USER\\s+([0-9]+).*?[h]oudini-bin/\\1/p\"")
   )))
)
;; ---------------------------- Attach Houdini ---------------------------------
(defun attach-houdini () 
"prints attach <proc number> into the buffer"
(interactive)
(insert-string (concat "attach " (pid-houdini) ))
)

;; ---------------------------- Kill Houdini -----------------------------------
(defun kill-houdini () 
"kills houdini, if more than one is running it will kill the
first on the ps list."
(interactive)
(shell-command (concat "kill -9 " (pid-houdini)))
)

;; ------------------------------ PID Maya -------------------------------------
(defun pid-maya ()
"Get the pid for maya, if more than one is running it returns
the first encounterd."
(interactive)
(car 
 (split-string 
  (shell-command-to-string
   (concat "ps aux | " ;; [h]oudini to not match itself
	   "sed -nE \"s/$USER\\s+([0-9]+).*?[m]aya\.bin/\\1/p\"")
   )))
)

;; ----------------------------- Attach Maya -----------------------------------
(defun attach-maya () 
"prints attach <proc number> into the buffer"
(interactive)
(insert-string (concat "attach " (pid-maya) ))
)

;; ------------------------------ Kill Maya ------------------------------------
(defun kill-maya () 
"kills maya, if more than one is running it will kill the
first on the ps list."
(interactive)
(shell-command (concat "kill -9 " (pid-maya)))
)
