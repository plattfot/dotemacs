;; =============================================================================
;; GNU plot
;; =============================================================================

(add-to-list 'Info-default-directory-list "/dd/home/fredriks/.emacs.d/plugins/gnuplot")

;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like
(setq load-path (append (list "/dd/home/fredriks/.emacs.d/plugins/gnuplot") load-path))

;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; ============================= Key bindings ==================================

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)
