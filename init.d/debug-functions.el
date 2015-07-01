;; =============================================================================
;; Debug Functions:
;; Lisp functions for debugging
;; =============================================================================

;; ----------------------------- PID Houdini -----------------------------------
(defun pid-houdini (&optional index )
"Get the pid for houdini, if more than one is running it returns
the one at INDEX. Where INDEX starts from 0 and up"

(interactive)
(when (not index) (setq index 0))
(nth index 
 (split-string 
  (shell-command-to-string
   (concat "ps aux | " ;; [h]oudini to not match itself
	   "sed -nE \"s/$USER\\s+([0-9]+).*?[h]oudini-bin/\\1/p\"")
   )))
)
;; ---------------------------- Attach Houdini ---------------------------------
(defun attach-houdini (&optional index) 
"prints attach <pid> into the buffer. INDEX is use to select
which one if there are multiple instances running, INDEX counts from 1."
(interactive"p")

;; The default for index is one.
(when (< index 1) (setq index 1))
(insert-string (concat "attach " (pid-houdini (- index 1)) ))
)

;; ---------------------------- Kill Houdini -----------------------------------
(defun kill-houdini () 
"kills houdini, if more than one is running it will kill the
first on the ps list."
(interactive)
(shell-command (concat "kill -9 " (pid-houdini)))
)

;; ------------------------------ PID Maya -------------------------------------
(defun pid-maya (&optional index)
"Get the pid for maya, if more than one is running it returns
the one at INDEX. Where INDEX starts from 0 and up"
(interactive)
(when (not index) (setq index 0))
(nth index
 (split-string 
  (shell-command-to-string
   (concat "ps aux | " ;; [h]oudini to not match itself
	   "sed -nE \"s/$USER\\s+([0-9]+).*?[m]aya\.bin/\\1/p\"")
   )))
)

;; ----------------------------- Attach Maya -----------------------------------
(defun attach-maya (&optional index) 
"prints attach <pid> into the buffer. INDEX is use to select
which one if there are multiple instances running, INDEX counts from 1."
(interactive"p")
;; The default for index is one.
(when (< index 1) (setq index 1))
(insert-string (concat "attach " (pid-maya index) ))
)

;; ------------------------------ Kill Maya ------------------------------------
(defun kill-maya () 
"kills maya, if more than one is running it will kill the
first on the ps list."
(interactive)
(shell-command (concat "kill -9 " (pid-maya)))
)

;; ------------------------------ Preprocess -----------------------------------
(defun preprocess-fix-macros ()
"Fix expanded macros when running only the preprocess on a file,
i.e. g++ <flags> -E <file>. Since they are expanded into a single
line which makes them hard to debug."
(interactive)
 (let* ((start (if (use-region-p) (region-beginning) (point)))
	(end (if (use-region-p) (region-end) (point-max)))
	(regex-map '(":[ ]" ";" "{" "}[ ]"))
	(regex (mapconcat (lambda (x) (format "\\(%s\\)" x)) regex-map "\\|")))
   (goto-char start)
   (while (search-forward-regexp regex end t)
     (newline)
     (setq end (1+ end)))
   (indent-region start (point))
   (goto-char start)))
