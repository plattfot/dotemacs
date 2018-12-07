;;; dd-make.el --- Collection of functions to help with Makefile at work.

;; Author: Fredrik Salomonsosn <plattfot@gmail.com>

;;; Commentary:

;;; Code:
(require 'subr-x)

(defun dd-make-module-wrap-libraries-undefined (&optional module-name)
  "Wrap region in an ifeq clause.

That checks if the variable `MODULE-NAME'_LIBRARIES is undefined.

If the MODULE-NAME is not specified it will derive it form the
file name for the current buffer, stripped of -[0-9]+."

  (interactive (if current-prefix-arg
                   (list (read-string "Specify module name: "))
                 nil))

  (when (region-active-p)
    (if (not module-name)
      (let ((filename (file-name-nondirectory (buffer-file-name))))
        (if filename
            (if (string-match "^\\(.*?\\)\\(?:-[[:digit:]]+\\|$\\)$" filename)
                (setq module-name (upcase (match-string 1 filename)))
              (error "Not a valid module name %s" filename))
          (error "Buffer %s doesn't point to a file" (buffer-name))))
      (setq module-name (upcase module-name)))

    ;; Get all libs listed, note the let arguments are order depedent!
    (let ((default-libraries
            (dd-make-fetch-default-libraries module-name))
          (has-static-libs (dd-make-module-has-static-libs-p module-name))
          (text (delete-and-extract-region (region-beginning) (region-end)))
          (begin (point)))

      (unless default-libraries
        (error "No default libraries found"))

      (let ((static-line
             (if has-static-libs
                 (format
                  (concat ". They will be static if %s_USE_STATIC_LIBS "
                          "is set to YES. "
                          "If %s_LIBRARIES")
                  module-name module-name)
               ". If the variable"))
            (num-libs (length default-libraries)))
        (insert (format "# List of the default libraries that
# will be linked against%s is defined (empty or not) no default
# libraries will be added to it, and instead use the ones already
# listed. The %s %s.
#
# To disable all default libraries, simply add '%s_LIBRARIES=' to
# your makefile."
                        static-line
                        (if (equal num-libs 1) "default is" "defaults are")
                        (if (> num-libs 1)
                            (format "%s and %s"
                                    (string-join
                                     (butlast (reverse default-libraries))
                                     ", ")
                                    (car (last (reverse default-libraries))))
                          (car default-libraries))
                        module-name)))

      (fill-individual-paragraphs begin (point))
      ;; Replace # with #:
      (let ((end (point)))
        (goto-char begin)
        (while (re-search-forward "#\\([^[:graph:]]\\)" end t)
          (replace-match "#:\\1")
          (setq end (+ 1 end)))
        (goto-char (point-at-eol)))

      (let ((indented-text (replace-regexp-in-string "\\(^.*$\\)" "  \\1" text)))
        (insert (format "
#%s_LIBRARIES =
ifeq ($(origin %s_LIBRARIES),undefined)
%s
endif"
                        module-name
                        module-name
                        indented-text))))))

(defun dd-make-module-has-static-libs-p (module-name &optional start end)
  "Check if MODULE-NAME_HAS_STATIC_LIBS is defined in the buffer.
Where MODULE-NAME is the name of the module.

If START and END are defined it will just search in that region.

For example if MODULE-NAME is foo it will look if
FOO_HAS_STATIC_LIBS is defined."
  (let ((current-point (point))
        (start-point (if start start (point-min)))
        (end-point end)
        (has-static-libs nil))
    (goto-char start-point)
    (setq has-static-libs
          (re-search-forward (format "%s_USE_STATIC_LIBS" module-name) end-point t))
    (goto-char current-point)
    has-static-libs))

(defun dd-make-fetch-default-libraries (module-name &optional start end)
  "Search the buffer for default libraries.

It will begin with `MODULE-NAME'_LIBRARIES.

If START and END are efined it will just search in that region.

If that fails, i.e. that variable is set to either dynamic
libraries or static libraries. It will search for first variable
containing dynamic libs. If that fails it will search for static
libs. If that fails it will just return nil."


  (let ((default-libraries
          (dd-make--parse-variable (format "%s_LIBRARIES" module-name) start end)))

    (unless default-libraries
      (error "No %s_LIBRARIES found" module-name))

    (when (string-match "^\\$([[:graph:]]+)$" (car default-libraries))
      (setq default-libraries (dd-make--parse-variable
                               (format (concat "%s_DYNAMIC_\\(?:"
                                               (string-join '("LIBRARIES"
                                                              "LAYERED_LIBS"
                                                              "LIBS")
                                                            "\\|")
                                               "\\)")
                                       module-name)
                               start end))
      (unless default-libraries
        (setq default-libraries (dd-make--parse-variable
                                 (format (concat "%s_STATIC_\\(?:"
                                                 (string-join
                                                  '("LIBRARIES" "LIBS")
                                                  "\\|")
                                                 "\\)")
                                         module-name)
                                 start end))))
    default-libraries))

;; TODO: use a make parser instead
(defun dd-make--parse-variable (var-re start end)
  "Search for variables matching VAR-RE.

Extract whats on the +=/= side of it. The VAR-RE must not contain
any groups!

This is super naive way of parsing, it doesn't not take into
account if it's assigning or appending to a variable."
  (let ((current-point (point))
        (start-point (if start start (point-min)))
        (values-re (concat var-re "[ ]+[+]*=[ ]+\\(.*\\)"))
        (values nil))
    (goto-char start-point)
    (while (re-search-forward values-re end t)
      (setq values `(,@(split-string (match-string 1)) ,@values)))
    (goto-char current-point)
    values))
(provide 'dd-make)
;;; dd-make.el ends here
