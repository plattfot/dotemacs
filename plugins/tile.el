;;; tile.el --- Utilites for tiling window managers
;;; Commentary:
;; Right now only tested with sway
;;; Code:
(require 'json)
(require 'dash)
(require 's)
(require 'cl-lib)

(cl-defstruct tile-window-properties class instance title)
(cl-defstruct tile-node id name type shell app-id window-properties num output marks)
(cl-defstruct tile-pretty-print-column header get)
(cl-defstruct tile-output
  id
  name
  orientation
  active
  dpms
  primary
  make
  model
  serial
  scale
  scale-filter
  transform
  adaptive-sync-status
  current-workspace
  current-mode)
(cl-defstruct tile-current-mode width height refresh)

;;;###autoload
(defun tile-sway-application-info ()
  "Show all applications and what shell they're using in sway."
  (interactive)
  (tile--application-info (tile--shell-command-to-json "swaymsg -t get_tree")))

;;;###autoload
(defun tile-sway-output-info ()
  "Show all output that sway knows about."
  (interactive)
  (tile--output-info (tile--shell-command-to-json "swaymsg -t get_outputs")))

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
              :get 'tile-node-name)
            ,(make-tile-pretty-print-column
              :header "Marks"
              :get 'tile-node-marks))))))

(defun tile--output-info (tile-output)
  "Show all outputs  in TILE-OUTPUT.

Where TILE-OUTPUT is a json object."
  (with-output-to-temp-buffer "*tile info*"
    (->> tile-output
         (tile--collect-outputs)
         (tile-pretty-print-node
          `(,(make-tile-pretty-print-column
              :header "Name"
              :get 'tile-output-name)
            ,(make-tile-pretty-print-column
              :header "Make"
              :get 'tile-output-make)
            ,(make-tile-pretty-print-column
              :header "Model"
              :get 'tile-output-model)
            ,(make-tile-pretty-print-column
              :header "Serial"
              :get 'tile-output-serial))))))
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
                     :marks (alist-get 'marks node)
                     :window-properties
                     (let ((window-props (alist-get 'window_properties node)))
                       (when window-props
                         (make-tile-window-properties
                          :class (alist-get 'class window-props)
                          :instance (alist-get 'instance window-props)
                          :title (alist-get 'title window-props)))))
                    (tile--collect-nodes node)))
            nodes))))

(defun tile--collect-outputs (output-json)
  "Extract outputs form OUTPUT-JSON.

Return a list of `tile-output'."
  (--map (make-tile-output
          :id (alist-get 'id it)
          :name (alist-get 'name it)
          :orientation (alist-get 'orientation it)
          :active (alist-get 'active it)
          :dpms (alist-get 'dpms it)
          :primary (alist-get 'primary it)
          :make (alist-get 'make it)
          :model (alist-get 'model it)
          :serial (alist-get 'serial it)
          :scale (alist-get 'scale it)
          :scale-filter (alist-get 'scale-filter it)
          :transform (alist-get 'transform it)
          :adaptive-sync-status (alist-get 'adaptive-sync-status it)
          :current-workspace (alist-get 'current-workspace it)
          :current-mode (let ((current-mode (alist-get 'current-mode it)))
                          (when current-mode
                            (make-tile-current-mode
                             :height (alist-get 'height current-mode)
                             :width (alist-get 'width current-mode)
                             :refresh (alist-get 'refresh current-mode)
                             ))))
         output-json))
(provide 'tile)
;;; tile.el ends here
