(defun csv-align-mode-parse-line ()
  (save-excursion
    (beginning-of-line)
    (let ((boundaries (list (point))))
      (while (not (eolp))
        (skip-chars-forward "^,\"\n")
        (case (char-after)
          ((?,) (setq boundaries (cons (1+ (point)) boundaries)))
          ((?\")
           (forward-char)
           (skip-chars-forward "^\"\n")))
        (unless (eolp)
          (forward-char)))
      (reverse (cons (point) boundaries)))))

(defun csv-align-mode-align-line (widths)
  (let* ((bol (point-at-bol))
         (eol (point-at-eol)))
    (remove-overlays bol (1+ eol) 'csv-align t))
  (let ((boundaries (csv-align-mode-parse-line))
        (pos 0))
    (while (cddr boundaries)
      (setq pos (+ pos (car widths))
            widths (cdr widths))
      (let ((ov (make-overlay (car boundaries) (cadr boundaries))))
        (overlay-put ov 'csv-align t)
        (overlay-put ov 'after-string
                     (propertize " " 'display `(space :align-to ,pos))))
      (setq boundaries (cdr boundaries)))))

(defun csv-align-mode-widths (boundaries)
  (let (ret)
    (while (cdr boundaries)
      (setq ret (cons (1+ (- (cadr boundaries) (car boundaries))) ret)
            boundaries (cdr boundaries)))
    (reverse ret)))

(defun csv-align-mode-widths-region (beg end)
  (let (widths maxs)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (setq widths (cons (csv-align-mode-widths
                            (csv-align-mode-parse-line))
                           widths))
        (forward-line)))
    (while widths
      (setq maxs (cons (apply #'max (mapcar #'car widths))
                       maxs)
            widths (delq nil (mapcar #'cdr widths))))
    (reverse maxs)))

(defun csv-align-mode-align-region (beg end)
  (interactive "r")
  (let ((widths (csv-align-mode-widths-region beg end)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end)
        (csv-align-mode-align-line widths)
        (forward-line)))))

(defun csv-align-mode-align-buffer ()
  (interactive)
  (csv-align-mode-align-region (point-min) (point-max)))

(define-minor-mode csv-align-mode
  "Toggle CSV align mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
     
When CSV align mode is enabled, columns in a CSV file will be
automatically aligned.  This affects only the display of the
file, not the actual contents."
  nil " CSV-Align" nil

  (if csv-align-mode
      (progn
        (add-hook 'post-command-hook #'csv-align-mode-align-buffer
                  nil t)
        (csv-align-mode-align-buffer))
    (save-restriction
      (widen)
      (remove-overlays nil nil 'csv-align t))
    (remove-hook 'post-command-hook #'csv-align-mode-align-buffer t)))
