;; =============================================================================
;; C family:
;; Specific for C, C++ and other in the c family
;; =============================================================================

;; (setq c-default-style "linux"
;;       c-basic-offset 2)
(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))
(setq objc-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 2))))


;;; set the default mode for .h files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; Enable subword-mode which makes it easier to work with camelCase words.
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))

;; ;;; Enable <> to be seen by the paranthesis matching
;; (modify-syntax-entry ?< "(>" c-mode-syntax-table)

;; ============================= Functions =====================================

;; --------------------------- Insert comment ----------------------------------
(defun insert-function-comment ()
  (interactive)
  (insert "/**")  (indent-according-to-mode)
  (insert "\n* ") (indent-according-to-mode)
  (insert "\n*/") (indent-according-to-mode)
  (previous-line 1)
  (end-of-line)
  )

;; --------------------------- Replace define ----------------------------------
(defun replace-define()
"Place cursor on a #define <var> <content> and execute this command and it will 
 replace all <var> with <content> in the file. 
 Basically evaluating the define variable"
(interactive)
(setq line (split-string (thing-at-point 'line) ))
(if (equal (car line) "#define") 
    (progn 
      ;; save current position
      (setq curr-pos (point))
      ;; Jump to the end of line
      (end-of-line)
      ;; Replace the first with the second.
      (replace-regexp (concat "\\_<"(nth 1 line)"\\_>") (nth 2 line) )
      ;; return to the same position
      (goto-char curr-pos)
      ;; move to the end of the line to indicate that it's done.
      (end-of-line) )
    ( message "Not a #define directive!" )
  )
)

(defun replace-define-undo()
"Place cursor on a #define <var> <content> and execute this command and it will 
 replace all <content> with <var> in the file. 
 Basically evaluating the define variable"
(interactive)
(setq line (split-string (thing-at-point 'line) ))
(if (equal (car line) "#define") 
    (progn 
      ;; save current position
      (setq curr-pos (point))
      ;; Jump to the end of line
      (end-of-line)
      ;; Replace the second with the first
      (replace-string (nth 2 line) (nth 1 line) )
      ;; return to the same position
      (goto-char curr-pos)
      ;; move to the end of the line to indicate that it's done.
      (end-of-line) )
    ( message "Not a #define directive!" )
  )
)

;; ============================= Key bindings ==================================
;; Hide/Show code blocks
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; Switch between header and implementation
(setq ff-search-directories
      '("." "../src" "../include"))
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "<backtab>") 'ff-find-other-file)))

;; (add-hook 'c-mode-common-hook
;;   (lambda() 
;;     (local-set-key  (kbd "\C-c \C-x") 'uncomment-region)))

;; Insert doxygen C based comment
(global-set-key (kbd "C-c i") 'insert-function-comment)
