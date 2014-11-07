;; =============================================================================
;; General key bindings
;; =============================================================================

;; ============================= Key bindings ==================================
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Key bindings
;; GUI
(global-set-key (kbd "<f5>") 'menu-bar-mode)
(global-set-key (kbd "<f6>") 'tool-bar-mode)

;; Clear shell
(global-set-key (kbd "<f8>") 'clear-shell)

;; Compile
(global-set-key (kbd "<f12>") 'compile) ; compile
(global-set-key (kbd "<f11>") 'recompile) ; recompile

;; Navigation
(global-set-key [M-left] 'windmove-left)   ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up)       ; move to upper window
(global-set-key [M-down] 'windmove-down)   ; move to downer window

(global-set-key (kbd "M-g") 'goto-line)

;; Text search. Swap places for normal search and regexp search
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Text edit
(global-set-key (kbd "M-r") 'replace-regexp)

;; Misc
;; Align 
;;; Cannot run gdb when this keybinding is set.
;; (global-set-key (kbd "C-x C-a") 'align-regexp)
(global-set-key (kbd "C-x a") 'align)

;; ------ Make emacs behave more lite unix ------
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)

;; Map backspace to Ctrl-h and ctrl+backspace to meta+h
;; (global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; This also maps open-line to enter
;;(global-set-key (kbd "C-m") 'open-line)


