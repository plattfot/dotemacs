;; Run user specific code when running the mode
(defvar houdini-wiki-mode-hook nil )

(defconst houdini-wiki-font-lock-keywords-1
  (list
   '("\\(@[:ascii:]+\\)". font-lock-builtin-face )
   '("^[:ascii:]+:" . font-lock-variable-name-face))
"Minimal highlighting expressions for Houdini wiki mode")

(defvar wpdl-font-lock-keywords wpdl-font-lock-keywords-1
  "Default highlighting expressions for HOudini wiki mode")
