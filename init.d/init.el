;; =============================================================================
;; Init file:
;; Load this file first since it initialize stuff that might be needed
;; by other init files. For example the package manager.
;; =============================================================================

;; Add path to plugins
(add-to-list 'load-path "~/.emacs.d/plugins/")

;; =============================== Variables ===================================
(defconst dotemacs/path-to-dotemacs "~/projects/dotemacs/dotemacs")
(defconst dotemacs/path-to-init.d "~/projects/dotemacs/init.d")
;; ================================ Package ====================================
(require 'package)
;; Add melpa to the package repo
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; ================================= Theme =====================================
;; Load sunburst theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(if window-system (load-theme 'sunburst t) (load-theme 'sunburst_term t))

;;============================= Backup/autosave ================================
;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;; ============================= Emacs behaviour ===============================
;; Reuse frames if the buffer is already open.
(setq-default display-buffer-reuse-frames t)
;; Set cursor color to white
(set-cursor-color "#ffffff")
;; Dissmiss startup screen
(setq inhibit-splash-screen t)

;; Highlight matching brackets
(show-paren-mode 1)

;; ;; turn off automatically add a newline in files.
;; (setq require-final-newline nil) 

;; ;; Hyper key in gtk and in an xterm!
;; (define-key key-translation-map [8711] 'event-apply-hyper-modifier)

;; Create new line if end of buffer when pressing C-n
(setq next-line-add-newlines nil)

;;=========================== Cosmetics ========================================
;; arg >= 1 enable the menu bar. Menu bar is the File, Edit, Options,
;; Buffers, Tools, Emacs-Lisp, Help

;; Disable menu bar mode
(menu-bar-mode 0)

;; Disable tool bar mode
(tool-bar-mode 0)

;; Disable scroll bar mode
(scroll-bar-mode 0)

;;enable column-number-mode
(column-number-mode 1)

;; Disable echo in shell
(setq comint-process-echoes t)

;;; height 88 <- my default value
(set-face-attribute 'default nil :height 92)

;; Use ibuffer instead of list-buffers
(defalias 'list-buffers 'ibuffer)
