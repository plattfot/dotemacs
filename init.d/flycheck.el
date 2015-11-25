;;; flycheck --- setuo flycheck
;;; Commentary:
;;; Code:
(require 'flycheck )

(add-hook 'after-init-hook #'global-flycheck-mode)

; Disable clang check, gcc check works better
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(c/c++-clang)))
;; Specify language standard in dir locals file: .dir-locals.el 
;; ((c++-mode
;;   (flycheck-clang-language-standard . "c++14")
;;   (flycheck-gcc-language-standard . "c++14")))
