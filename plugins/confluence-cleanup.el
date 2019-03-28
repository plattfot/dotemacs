;;; confluence-cleanup.el --- Prep confluence html pages for pandoc

;; Author: Fredrik Salomonsosn <plattfot@gmail.com>

;;; Commentary:

;;; Code:

(defun confluence-escape-html (str)
  "Escape the special characters in html in STR."
  (let ((start 0)
        (escaped-str str))
    (save-match-data
      (while (string-match "\\(<\\|>\\|&\\)" escaped-str start)
        (cond ((string-equal (match-string 1 escaped-str) "&")
               (setq escaped-str (replace-match "&amp;" t t escaped-str 1)))
              ((string-equal (match-string 1 escaped-str) "<")
               (setq escaped-str (replace-match "&lt;" t t escaped-str 1)))
              ((string-equal (match-string 1 escaped-str) ">")
               (setq escaped-str (replace-match "&gt;" t t escaped-str 1))))
        (setq start (match-end 0))))
    escaped-str))

(defun confluence-cleanup ()
  "Convert all the ac:structured-macro to normal html tags."
  (interactive)
  (save-mark-and-excursion
   (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
          (end (when (region-active-p) (region-end)))
          (block-text-re "\\([$()\\\" [:blank:][:graph:][:cntrl:]]+?\\)")
          (code-block-lang-re (concat "<ac:structured-macro ac:name=\"code\" "
                                 "ac:schema-version=\"1\" "
                                 "ac:macro-id=\"[[:xdigit:]-]+\">"
                                 "<ac:parameter ac:name=\"language\">"
                                 "\\([[:alnum:]]+\\)"
                                 "</ac:parameter>"
                                 "<ac:plain-text-body>"
                                 "<!\\[CDATA\\["
                                 block-text-re
                                 "\\]\\]>"
                                 "</ac:plain-text-body>"
                                 "</ac:structured-macro>"))
          (code-block-text-re (concat "<ac:structured-macro ac:name=\"code\" "
                                      "ac:schema-version=\"1\" "
                                      "ac:macro-id=\"[[:xdigit:]-]+\">"
                                      "<ac:plain-text-body>"
                                      "<!\\[CDATA\\["
                                      block-text-re
                                      "\\]\\]>"
                                      "</ac:plain-text-body>"
                                      "</ac:structured-macro>"))
          (info-block-re (concat "<ac:structured-macro "
                                 "ac:name=\"\\(info\\|note\\|warning\\)\" "
                                 "ac:schema-version=\"1\" "
                                 "ac:macro-id=\"[[:xdigit:]-]+\">"
                                 "<ac:rich-text-body>"
                                 block-text-re
                                 "</ac:rich-text-body>"
                                 "</ac:structured-macro>"))
          (header-anchor-re (concat "<h\\([[:digit:]]+\\)>"
                                    "<ac:structured-macro ac:name=\"anchor\" "
                                    "ac:schema-version=\"1\" "
                                    "ac:macro-id=\"[[:xdigit:]-]+\">"
                                    "<ac:parameter ac:name=\"\">"
                                    block-text-re
                                    "</ac:parameter>"
                                    "</ac:structured-macro>")))
     (goto-char begin)
     (while (re-search-forward code-block-lang-re end t)
       (replace-match (format "<pre class=\"\\1\"><code>%s</code></pre>"
                              (confluence-escape-html (match-string 2)))))
     (goto-char begin)
     (while (re-search-forward code-block-text-re end t)
       (replace-match (format "<pre class=\"text\"><code>%s</code></pre>"
                              (confluence-escape-html (match-string 1)))))
     (goto-char begin)
     (while (re-search-forward info-block-re end t)
       (replace-match (concat "<div class=\"\\1\">"
                              "\\2"
                              "</div>")))
     (goto-char begin)
     (while (re-search-forward header-anchor-re end t)
       (replace-match (concat "<h\\1 id=\"\\2\">"))))))
;;; confluence-cleanup.el ends here
