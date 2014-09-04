;;; This is currently under heavy development.  Don't expect it to do
;;; anything yet.

;; * Definition hyperlinking
;; ** Put braces around a word or phrase and later occurrences will
;;    link back to it
;; * Quick jumping to sections by number
;; * Different colors for different levels
;; * Support for "quoted" or "verbatim" nodes that don't wrap their
;;   contents
;; * Support for inserting images/figures
;; * Support for TeX math mode
;; ** Need some way to specify options/packages
;; *** Should be per-file, but might be handy to have a per-directory
;;     specification as well
;;
;; * Outline modification operations
;; ** new-sibling
;; ** increase-indent
;; *** If the predecessor of the current node has a lower level than
;;     the current node, introduce a new parent (or do nothing).  If
;;     the predecessor of the current node has the same level as the
;;     current node, than make this node's parent its current
;;     predecessor.  If the predecessor of the current node has a
;;     higher level than the current node, then make the current node
;;     the last child of its current previous sibling.
;; ** decrease-indent
;; *** May delete bodyless parents to be the inverse of
;;     increase-indent
;; ** transpose-nodes
;; * Meanings of other actions
;; ** Backspacing at the beginning of a node should delete the node,
;;    merge any contents it has with its predecessor, and make any
;;    children it has children of its predecessor
;; ** Enter should remain in the same node and just insert hard
;;    newlines into the body of the node (M-enter or some-such should
;;    be new-sibling)
;; * Motion operations
;; ** jump-to-section
;; ** backwards-node
;; ** forwards-node
;; ** jump-to-parent
;;
;; * Implementation
;; ** Tracking relationships
;; *** Could use overlays on nodes to store tree relations to other
;;     node overlays
;; **** If this gets out of sync with the buffer contents, all hell
;;      breaks loose, but it's easy to reconstruct the correct buffer
;;      contents given this (or to checks its correctness)
;; *** Could examine buffer indentation
;; **** Certainly can't get out of sync with the actual buffer
;;      contents, but if something weird happens to the indentation,
;;      again, all hell breaks loose
;; *** Track indentation level in overlays and dynamically compute
;;     tree relationships
;; **** Most operations are probably easier on this
;; **** Probably won't get badly out of sync with the buffer contents
;;      and as long as the tree manipulations are written robustly,
;;      any failure should be isolated
;; **** Have a function that reconstructs the buffer contents for a
;;      region based on its node overlays

(require 'cl)

;;
;; Overlay helpers
;;

(defun notes-overlay-at (pos prop)
  (let ((overlays (overlays-at pos))
        result)
    (while (and overlays
                (not result))
      (if (overlay-get (car overlays) prop)
          (setq result (car overlays))
        (setq overlays (cdr overlays))))
    result))

(defun notes-walk-overlays (proc prop &optional start end)
  (let ((start (or start (point-min)))
        (end (or end (point-max)))
        seen)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((overlays (overlays-at (point))))
          (dolist (overlay overlays)
            (message "Considering %S" overlay)
            (when (and (overlay-get overlay prop)
                       (not (memq overlay seen)))
              (push overlay seen)
              (save-excursion
                (funcall proc overlay)))))
        (goto-char (next-overlay-change (point)))))))

;;
;; Node abstraction
;;

(defun notes-overlay-to-node (overlay)
  (list (overlay-get overlay 'notes-node)
        (overlay-start overlay)
        (overlay-end overlay)
        overlay))

(defun notes-node-level (node)
  (first node))

(defun notes-node-start (node)
  (second node))

(defun notes-node-end (node)
  (third node))

(defun notes-node-overlay (node)
  (fourth node))

(defun notes-create-node (start end level)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'notes-node level)))

(defun notes-delete-node (node)
  (delete-overlay (notes-node-overlay node)))

(defun notes-walk-nodes (proc &optional start end)
  (let ((real-proc proc))               ; Stupid dynamic scoping
    (notes-walk-overlays
     (lambda (overlay)
       (funcall real-proc (notes-overlay-to-node overlay)))
     'notes-node
     start
     end)))

(defun notes-node-at (pos)
  (let ((overlay (notes-overlay-at pos 'notes-node)))
    (if overlay
        (notes-overlay-to-node overlay))))

(defmacro with-notes-narrow-to-node (node &rest body)
  `(save-restriction
     (narrow-to-region (notes-node-start node)
                       (notes-node-end node))
     ,@body))

;;
;; Decoding
;;

(defconst notes-new-indent-re "\\(\\*+\\) ")
(defconst notes-zero-indent-re "[^* ]")

(defun notes-decode ()
  "Creates node overlays

A node overlay extends from the beginning of the first line of the
node to just after the newline that terminates it (ie, in a continuous
run of nodes, there are no characters in between nodes that aren't
assigned to some node overlay and no character is assigned to multiple
node overlays)"

  (save-excursion
    ;; Nuke existing notes overlays
    (notes-walk-nodes
     (lambda (node)
       (notes-delete-node node)))

    ;; Start decoding
    (goto-char (point-min))
    (let (regions
          (node-start (point))
          (current-level 0)
          (expected-prefix ""))
      (while (not (eobp))
        ;; Determine indentation type and level
        (let ((new-indent (if (looking-at notes-new-indent-re)
                              (length (match-string 1))
                            (if (looking-at notes-zero-indent-re)
                                0))))
          (if new-indent
              (progn
                (push (list node-start (point) current-level)
                      regions)
                (setq node-start (point))
                (setq current-level new-indent)
                (setq expected-prefix
                      (if (= new-indent 0)
                          ""
                        (make-string (+ current-level 1) ? ))))
            (if (not (looking-at expected-prefix))
                ;; Bad formatting, drop to level 0
                (progn
                  (push (list node-start (point) current-level)
                        regions)
                  (setq node-start (point))
                  (setq current-level 0)
                  (setq expected-prefix "")))))
        (forward-line))
      ;; Get last region if appropriate
      (unless (= node-start (point))
        (push (list node-start (point) current-level) regions))
      ;; Put regions in the right order
      (setq regions (reverse regions))

      ;; Create overlays
      (dolist (region regions)
        (let ((start (first region))
              (end (second region))
              (level (third region)))
          (notes-create-node start end level))))))

;;
;; Formatting
;;

(defun notes-format (&optional start end)
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (notes-walk-overlays
     (lambda (overlay) (delete-overlay overlay))
     'notes-format
     start end)
    (message "Bar")
    (notes-walk-nodes #'notes-format-node
                      start end)))

(defun notes-format-node (node)
   (message "Formatting %S" node)
   (goto-char (notes-node-start node))
   (when (looking-at notes-new-indent-re)
     (let* ((overlay (make-overlay (max
                                    (- (match-beginning 0) 1)
                                    (point-min))
                                   (match-end 0)))
            (str (concat (make-string
                          (max (- (length (match-string 1)) 1) 0)
                          ? )
                         (char-to-string
                          (make-char 'latin-iso8859-1 #XB7))
                         " "))
            (str2 (concat "\n"
                          str
                          (make-string
                           (max (- (length (match-string 0))
                                   (length str))
                                0)
                           ? ))))
       (message "Creating %S" overlay)
       (overlay-put overlay 'notes-format t)
       (overlay-put overlay 'display str2)
       (overlay-put overlay 'intangible t))
     (forward-line))
   (let* ((level (notes-node-level node))
          (width (if (= level 0) 0 (+ level 1))))
     (while (and (< (point) (notes-node-end node))
                 (not (eobp)))
       (let ((overlay (make-overlay (max
                                      (- (point) 1)
                                      (point-min))
                                    (+ (point) width))))
         (overlay-put overlay 'notes-format t)
         (overlay-put overlay 'intangible t))
       (forward-line))))

;;
;; Notes mode
;;

(define-derived-mode notes-mode text-mode "Notes"
  "Major mode for editing notes"

  (notes-decode)
  (notes-format))
