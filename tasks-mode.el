;;; tasks-mode.el --- Simple tasks file editing

;; Copyright (C) 2007 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    19-Sep-2007
;; Version:    0.1

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Customization:

(defgroup tasks-mode ()
  "Major mode for editing tasks files.")

(defconst tasks-incomplete-face 'tasks-incomplete-face)
(defface tasks-incomplete-face
  '((((min-colors 9)))
    (t
     (:bold t)))
  "Face used to indicate incomplete tasks.")

(defconst tasks-completed-face 'tasks-completed-face)
(defface tasks-completed-face
  '((((background dark) (min-colors 9))
     (:foreground "grey60"))
    (((background light) (min-colors 9))
     (:foreground "grey40"))
    (t
     (:foreground "grey")))
  "Face used to indicate completed tasks."
  :group 'tasks-mode)

(defconst tasks-irrelevant-face 'tasks-irrelevant-face)
(defface tasks-irrelevant-face
  '((((background dark) (min-colors 9))
     (:foreground "grey40"))
    (((background light) (min-colors 9))
     (:foreground "grey60"))
    (t
     (:foreground "grey")))
  "Face used to indicate irrelevant tasks."
  :group 'tasks-mode)

;;; Code:

(require 'cl)
(require 'calendar)

(defconst tasks-weekdays
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defconst tasks-months
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defconst tasks-date-regex
  (concat "^\\(?:"
          (regexp-opt tasks-weekdays t)
          ",[ \t]*\\)?"
          (regexp-opt tasks-months t)
          "[ \t]+"
          "\\([0-9][0-9]?\\)"
          ",[ \t]*"
          "\\([0-9][0-9][0-9][0-9]\\)$"))

(defconst tasks-font-lock-keywords
  `((,tasks-date-regex . font-lock-keyword-face)
    ("^ +\\- .*" . tasks-incomplete-face)
    ("^ +\\+ .*" . tasks-completed-face)
    ("^ +\\~ .*" . tasks-irrelevant-face)))

(defun tasks-parse-date (&optional str)
  (save-match-data
    (cond ((if (null str)
               (looking-at tasks-date-regex)
             (string-match tasks-date-regex str))
           (let ((month
                  (+ 1 (position (match-string-no-properties 2 str)
                                 tasks-months :test #'string=)))
                 (day (string-to-number (match-string 3 str)))
                 (year (string-to-number (match-string 4 str))))
             (list month day year)))
          (t
           (if (null str)
               (error "Invalid date at point")
             (error "Invalid date: %s" str))))))

(defun tasks-unparse-date (date)
  (let* ((day (cadr date))  (month (car date))  (year (caddr date))
         (encoded (encode-time 0 0 0 day month year))
         (weekday (nth 6 (decode-time encoded))))
    (concat (nth weekday tasks-weekdays)
            ", "
            (nth (- month 1) tasks-months)
            " "
            (number-to-string day)
            ", "
            (number-to-string year))))

(defun tasks-compare-lists (a b)
  (cond ((null a)            (if (null b) '= '<))
        ((null b)            '>)
        ((< (car a) (car b)) '<)
        ((> (car a) (car b)) '>)
        (t (tasks-compare-lists (cdr a) (cdr b)))))

(defun tasks-compare-dates (a b)
  (unless (and (= (length a) 3) (= (length b) 3))
    (error "Cannot compare dates %s and %s" a b))
  (tasks-compare-lists (list (caddr a) (car a) (cadr a))
                       (list (caddr b) (car b) (cadr b))))

(defun tasks-read-date ()
  (save-window-excursion
    ;; Start the calendar at the date preceding point
    (let ((date
           (save-excursion
             (if (re-search-backward tasks-date-regex nil t)
                 (tasks-parse-date)))))
      (calendar)
      (when date
        (calendar-goto-date date)))
    (let* ((cal-buf (current-buffer))
           (orig-map (current-local-map))
           (new-map (copy-keymap orig-map))
           (old-exit-calendar (symbol-function 'exit-calendar))
           tasks-date-selected)
      ;; Bind RET in our augmented calendar keymap to set
      ;; tasks-date-selected and exit the recursive edit
      (define-key new-map (kbd "RET")
        (lambda ()
          (interactive)
          (setq tasks-date-selected (calendar-cursor-to-date))
          (throw 'exit nil)))
      (define-key new-map (kbd "C-g") #'exit-calendar)
      ;; Enter a recursive edit inside the calendar, using our
      ;; augmented local keymap and a new definition of
      ;; exit-calendar.
      (unwind-protect
          (progn
            (fset 'exit-calendar
                  (lambda () (interactive) (throw 'exit nil)))
            (use-local-map new-map)
            (recursive-edit))
        ;; Restore the map and exit-calendar
        (use-local-map orig-map)
        (fset 'exit-calendar old-exit-calendar))
      (with-current-buffer cal-buf
        (exit-calendar))
      (or tasks-date-selected
          (error "No date selected")))))

(defun tasks-jump-to-date (date)
  (interactive (list (tasks-read-date)))
  (save-match-data
    (let ((start (point)))
      ;; Find a date near point to start
      (if (not (or (re-search-forward  tasks-date-regex nil t)
                   (re-search-backward tasks-date-regex nil t)))
          ;; No luck, just stay here
          (progn
            (goto-char start)
            nil)
        ;; Found a date
        (let* ((last (save-excursion
                       (goto-char (match-beginning 0))
                       (tasks-parse-date)))
               (cmp (tasks-compare-dates last date)))
          (if (eq cmp '=)
              ;; We got lucky
              (progn
                (goto-char (match-beginning 0))
                t)
            (let* ((dir (if (eq cmp '<) 'forward 'backward))
                   (re-search (if (eq dir 'forward)
                                  #'re-search-forward
                                #'re-search-backward)))
              ;; Search
              (while (and last (eq (tasks-compare-dates last date) cmp))
                (if (funcall re-search tasks-date-regex nil t)
                    (save-excursion
                      (goto-char (match-beginning 0))
                      (setq last (tasks-parse-date)))
                  ;; Ran off the edge
                  (setq last nil)))
              (if (not last)
                  ;; Find the first blank line after/before the last valid
                  ;; date, or the end of the buffer
                  (unless (funcall re-search "^$" nil t)
                    (goto-char (if (eq dir 'forward)
                                   (point-max)
                                 (point-min))))
                ;; If I found the date, put point at its beginning
                (if (equal last date)
                    (goto-char (match-beginning 0))
                  ;; Otherwise, move to the preceding line if forward
                  (if (eq dir 'forward)
                      (forward-line -1)
                    ;; .. or the next blank line if backward
                    (unless (re-search-forward "^$" nil t)
                      (goto-char (point-max))))))
              ;; Return
              (equal last date))))))))

(defun tasks-jump-to-today ()
  (interactive)
  (let* ((time (decode-time))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (tasks-jump-to-date (list month day year))))

(defun tasks-jump-or-insert (date)
  (interactive (list (tasks-read-date)))
  (if (tasks-jump-to-date date)
      (forward-line 1)
    (unless (save-excursion
              (forward-line -1)
              (eolp))
      (newline))
    (insert (tasks-unparse-date date))
    (newline)
    (insert " - ")
    (save-excursion
      (newline))))

(defun tasks-toggle-checkmark ()
  (interactive)
  (save-excursion
    (save-match-data
      (let ((bound (save-excursion
                     (if (re-search-backward tasks-date-regex nil t)
                         (match-end 0)
                       (point-min))))
            (regex "^ +\\([-+~]\\) "))
        (if (and (not (looking-at regex))
                 (not (re-search-backward regex bound t)))
            (error "Not in a task")
          (goto-char (match-beginning 1))
          (let ((new (case (char-after (point))
                       ((?-) ?+)
                       ((?+) ?-)
                       ((?~) ?~))))
            (delete-char 1)
            (insert new)))))))

(defvar tasks-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm (kbd "C-c i")   #'tasks-jump-or-insert)
    (define-key mm (kbd "C-c g")   #'tasks-jump-to-date)
    (define-key mm (kbd "C-c c")   #'tasks-toggle-checkmark)
    (define-key mm (kbd "C-c C-c") #'tasks-toggle-checkmark)
    (define-key mm (kbd "C-c .")   #'tasks-jump-to-today)
    (define-key mm (kbd "C-c C-.") #'tasks-jump-to-today)
    mm))

(define-derived-mode tasks-mode text-mode "Tasks"
  "Major mode for editing tasks files."

  ;; Set up font lock
  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (setq font-lock-defaults
        '(tasks-font-lock-keywords nil t)))

(provide 'tasks-mode)
