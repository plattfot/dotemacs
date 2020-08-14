;;; tile.el --- Utilites for tiling window managers
;;; Commentary:
;; Right now only tested with sway
;;; Code:
(require 'json)
(require 'dash)
(require 's)
(require 'cl-lib)

(cl-defstruct tile-window-properties class instance title)
(cl-defstruct tile-node id name type shell app-id window-properties num output)
(cl-defstruct tile-pretty-print-column header get)

(defun tile-sway-application-info ()
  "Show all applications and what shell they're using in sway."
  (interactive)
  (tile--application-info (tile--shell-command-to-json "swaymsg -t get_tree")))

(defun tile--shell-command-to-json (command)
  "Run shell COMMAND and parse it as json."
  (json-read-from-string (shell-command-to-string command)))

(defun tile--application-info (tile-tree)
  "Show all applications and what shell they're using in TILE-TREE.

Where TILE-TREE is a json object."
  (with-output-to-temp-buffer "*tile info*"
    (->> tile-tree
         (tile--collect-nodes)
         (-flatten)
         (-filter 'tile-node-shell)
         (tile-pretty-print-node
          `(,(make-tile-pretty-print-column
              :header "Shell"
              :get 'tile-node-shell)
            ,(make-tile-pretty-print-column
              :header "Class/App ID"
              :get (lambda (x)
                     (cond
                      ((tile-node-app-id x) (tile-node-app-id x))
                      ((tile-node-window-properties x)
                       (tile-window-properties-class (tile-node-window-properties x)))
                      (t "Unknown"))))
            ,(make-tile-pretty-print-column
              :header "Name"
              :get 'tile-node-name))))))

(defun tile-pretty-print-node (columns tile-nodes)
  "Pretty print COLUMNS for TILE-NODES.

Where COLUMNS is a list of `tile-pretty-print-column and
TILE-NODES is a sequence of `tile-node'.'

Example output:
Shell      Class/App ID  Name
xdg_shell  Alacritty     ~
xwayland   Emacs-27.0.91 tile.el"
  (let* ((padding (--map (length (tile-pretty-print-column-header it)) columns)))
    ;; TODO: update in place
    (--each tile-nodes
      (setf padding
            (-map
             (lambda (pad-column)
               (max (car pad-column)
                    (length (funcall (tile-pretty-print-column-get (cdr pad-column)) it))))
             (-zip padding columns))))
    (let ((fmt-str (->> padding
                        (--map (format "%%-%ds" it))
                        (-drop-last 1)
                        (s-join "  ")
                        (s-append "  %s\n"))))
      (princ (apply 'format fmt-str (--map (tile-pretty-print-column-header it) columns)))
      (--each tile-nodes
        (princ (apply 'format
                      fmt-str
                      (-map (lambda (column)
                              (funcall (tile-pretty-print-column-get column) it))
                            columns)))))))

(defun tile--collect-nodes (tile-tree-json)
  "Recursively extract the nodes in the TILE-TREE-JSON.

Return a nested list of `tile-node'."
  (let ((nodes (alist-get 'nodes tile-tree-json)))
    (when nodes
      (-map (lambda (node)
              (cons (make-tile-node
                     :name (alist-get 'name node)
                     :id (alist-get 'id node)
                     :shell (alist-get 'shell node)
                     :type (alist-get 'type node)
                     :num (alist-get 'num node)
                     :output (alist-get 'output node)
                     :app-id (alist-get 'app_id node)
                     :window-properties
                     (let ((window-props (alist-get 'window_properties node)))
                       (when window-props
                         (make-tile-window-properties
                          :class (alist-get 'class window-props)
                          :instance (alist-get 'instance window-props)
                          :title (alist-get 'title window-props)))))
                    (tile--collect-nodes node)))
            nodes))))

(provide 'tile)
;;; tile.el ends here
