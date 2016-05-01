;;; package --- Summary
;;; Commentary:

;;; Code:
(defun convert-mywallet-to-ledger ()
"Convert a buffer from mywallet csv to ledger dat."
(interactive)
(while (re-search-forward "\\(.*?\\)\";\"\";\"\\(.*?\\)\";\"\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\";\"\\(.*?\\)\";\"\\(.*?\\)\";\"\\(.*?\\)\";\\(.*?\\)\")")
  (replace-match "\\3/\\4/\\5 * \\8\n\\2:\\6:\\7   \\9  \\1")
)
;;(replace-regexp   "\\3/\\4/\\5 * \\8\n\\2:\\6:\\7   \\9  \\1")
)

(provide 'mywallet-to-ledger)
;;; mywallet-to-ledger ends here
