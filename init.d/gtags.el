(require 'gtags)

;; https://www.emacswiki.org/emacs/CyclingGTagsResult
(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (forward-line)
           (gtags-select-it nil))
          ) ))

;; (global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or
C-M-,
(global-set-key "M-." 'gtags-find-tag) ;; M-. finds tag
(global-set-key "C-M-." 'gtags-find-rtag)   ;; C-M-. find all references of tag
(global-set-key "C-M-," 'gtags-find-symbol) ;; C-M-, find all usages of symbol.
