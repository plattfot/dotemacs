;; =============================================================================
;; Debug Functions:
;; Lisp functions for debugging
;; =============================================================================
;; ---------------------------- PID of process ---------------------------------
(defun pid (regex &optional index)
"Get the pid of REGEX, if more than one is running it returns one
at INDEX. Where INDEX starts from 0 and up"
(interactive)
(when (not index) (setq index 0))
(nth index 
 (split-string 
  (shell-command-to-string
   (concat "ps aux | " ;; wrap first character in [ ] to not match itself
	   "sed -nE \"s/$USER\\s+([0-9]+).*?"
	   (concat "[" (substring regex 0 1) "]" (substring regex 1))"/\\1/p\"")
   )))
)
;; ----------------------------- PID Houdini -----------------------------------
(defun pid-houdini (&optional index )
"Get the pid for houdini, if more than one is running it returns
the one at INDEX. Where INDEX starts from 0 and up"

(interactive)
(when (not index) (setq index 0))
(pid "houdini-bin" index)
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
(pid "maya\\.bin" index)
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

;; ------------------------------ PID SMEAT ------------------------------------
(defun pid-smeat (&optional index)
"Get the pid for smeat, if more than one is running it returns
the one at INDEX. Where INDEX starts from 0 and up"
(interactive)
(when (not index) (setq index 0))
;; the ^= is to ignore houdini/python commands e.g houdini --with smeat=...
(pid "smeat(:?[^=]+|$$)" index)
)

;; ---------------------------- Attach Smeat -----------------------------------
(defun attach-smeat (&optional index) 
"prints attach <pid> into the buffer. INDEX is use to select
which one if there are multiple instances running, INDEX counts from 1."
(interactive"p")

;; The default for index is one.
(when (< index 1) (setq index 1))
(insert-string (concat "attach " (pid-smeat (- index 1)) ))
)

;; ---------------------------- Abort Smeat ------------------------------------
;; Not quite working.
(defun smeat-abort (&optional index )
"Sends USR1 signal to houdini which aborts the smeat client,
INDEX is used to select which houdini instance to send to if
multiple instances exist."
(interactive"p")
(when (< index 1) (setq index 1))
(let ((hou-pid (pid-houdini index)))
  (shell-command (concat "kill -s USR1 " hou-pid))
)
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
