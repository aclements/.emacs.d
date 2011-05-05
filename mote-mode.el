;; Ideas
;; * Collapse all but the current mote.  Perhaps have collapsing under
;;   more manual control so you can have more than one mote visible at
;;   once.  Keep the last n motes open?  Have a key to pin a mote to
;;   keep it visible and another key to unpin all motes?
;; ** If the mark is active, all motes spanned by the region should be
;;    visible.
;; * isearch should search across all motes and temporarily uncollapse
;;   the mote with the current result in it (perhaps all results?)
;; * Tags with ability to filter based on tags.  Makes non-matching
;;   motes completely invisible and prevents isearch into them.
;;   isearch-open-invisible provides the perfect mechanism for this.
;; * Archiving?  Archived motes would not be visible by default,
;;   unless explicitly asked for.
;; * Additional metadata?  Probably RFC822-style headers under the
;;   subject line.
;; * show-context-mode style display of the current mote's title at
;;   the top of the screen if it's not visible.
;; * Highlight the title of the current mote?
;; * Make {x} link to the referenced mote (or general search result?)

(defvar mote-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c n")   #'mote-new)
    (define-key m (kbd "C-c C-n") #'mote-new)
    (define-key m (kbd "C-<")     #'mote-previous)
    (define-key m (kbd "C->")     #'mote-next)
    m))

(defun mote-new (title)
  (interactive "MTitle: ")

  (push-mark)
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (delete-region (point) (point-max))
  (newline 3)
  (insert (format "[[%s%s]]"
                  (format-time-string "%Y-%m-%0e")
                  (if (> (length title) 0)
                      (concat " " title)
                    "")))
  (newline 2))

(defun mote-previous ()
  (interactive)

  (unless (re-search-backward "^\\[\\[" nil t)
    (goto-char (point-min))
    (error "No more motes"))
  (recenter 5))

(defun mote-next ()
  (interactive)

  (unless (eobp)
    (forward-char 1))
  (if (re-search-forward "^\\[\\[" nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))
    (error "No more motes"))
  (recenter 5))

;; C-< and C-> (not taken)?  C-M-a and C-M-e (*-of-defun)?

;; (defun mote-refresh (&optional start end)
;;   (setq start (or start (point-min))
;;         end   (or end   (point-max)))
  
(defconst mote-mode-title-face 'mote-mode-title-face)
(defface mote-mode-title-face
  '((((class color))
     (:foreground "green" :underline t :height 1.4))
    (t (:underline t :height 1.4)))
  "Face for mote titles.")

(defvar mote-mode-keywords
  '(("^\\[\\[.*\\]\\]$" . mote-mode-title-face)))

(define-minor-mode mote-mode
  "Minor mode for editing and navigating files composed of
\"motes\", each of which has a date, title, and body text."
  nil " Mote" nil

  (if mote-mode
      (font-lock-add-keywords nil mote-mode-keywords)
    (font-lock-remove-keywords nil mote-mode-keywords)))

(provide 'mote-mode)
