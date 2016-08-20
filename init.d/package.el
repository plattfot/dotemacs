

(require 'package)
; list the packages you want
(setq package-list 
      '(ag
	async
	auto-complete
	buffer-move
	evil-numbers
	expand-region
	flycheck
	git-commit
	gnuplot
	go-autocomplete
	ledger-mode
	magit
	magit-popup
	mc-extras
	move-text
	multi-term
	multiple-cursors
	popup
	rtags
	string-inflection
	sudo-edit
	with-editor
	yaml-mode
	yasnippet ))

;; Add melpa to the package repo
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
