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

;;; Notes:

;; * Support for narrowing to labels
;; * Better automatic indentation
;; ** Wrapping of titles
;; ** Wrapping of fields
;; ** Wrap in tasks-insert-from-file
;; * Check malformed or out-of-order dates
;; * Finish repeat support
;; * Figure out how to better repeat events
;; * Highlight events according to whether or not the date has passed
;; ** In general, events should act like tasks that get checked off
;;    when their end time passes
;; *** Generate text properties during font-locking for event
;;     highlighting and overdue task highlighting?
;; * Support for more repeat types.  For example, Mother's day is on
;;   the second Sunday of May
;; * Support for hiding events after they have been checked off
;; ** Probably use a field that specifies a relative time until it
;;    gets checked off, then change it to an absolute time

;;; Customization:

(defgroup tasks-mode ()
  "Major mode for editing tasks files.")

(defconst tasks-past-date-face 'tasks-past-date-face)
(defface tasks-past-date-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Plum1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan4"))
    ;; XXX 16 colors?
    (((class color) (min-colors 8)) (:foreground "cyan"))
    (t (:inherit font-lock-keyword-face)))
  "Face used to indicate a past date")

(defconst tasks-future-date-face 'tasks-future-date-face)
(defface tasks-future-date-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used to indicate a future date")

(defconst tasks-present-date-face 'tasks-present-date-face)
(defface tasks-present-date-face
  '((t (:inherit tasks-future-date-face :underline t)))
  "Face used to indicate today's date")

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
     (:foreground "black" :weight bold)))
  "Face used to indicate irrelevant tasks."
  :group 'tasks-mode)

;;; Code:

(require 'cl)
(require 'calendar)

(defconst tasks-weekdays
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "The weekdays recognized in dates, in order from Sunday.")

(defconst tasks-months
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December")
  "The months recognized in dates, in order from January.")

(defconst tasks-date-canonical-regex
  (concat "^"
          (regexp-opt tasks-weekdays t)
          ",[ \t]*"
          (regexp-opt tasks-months t)
          "[ \t]+"
          "\\([0-9][0-9]?\\)"
          ",[ \t]*"
          "\\([0-9][0-9][0-9][0-9]\\)$")
  "A regular expression matching only the 'canonical' form of
dates.  A canonical date looks like:

  Thursday, September 20, 2007")

(defconst tasks-date-regexes
  `((,tasks-date-canonical-regex 2 t 3 4)
    (,(concat "^"
              (regexp-opt tasks-months t)
              "[ \t]+"
              "\\([0-9][0-9]?\\)"
              ",[ \t]*"
              "\\([0-9][0-9][0-9][0-9]\\)$")
     1 t 2 3)
    ("^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]?\\)-\\([0-9][0-9]?\\)$"
     2 nil 3 1))
  "A collection of regexps used to parse different formats of
dates.  This is a list of tuples of the form:

\(REGEX MONTH-SUBEXP MONTH-NAMED DAY-SUBEXP YEAR-SUBEXP)

Where MONTH-SUBEXP, DAY-SUBEXP, and YEAR-SUBEXP indicate the
indexes of the subexpressions to use for the month, day, and
year.  If MONTH-NAMED is non-nil, then MONTH-SUBEXP refers to a
subexpression that matches a month name as found in
`tasks-months'.  Otherwise, it must match a number.")

(defconst tasks-date-regex
  (let ((c (caar tasks-date-regexes)))
    (dolist (r (cdr tasks-date-regexes))
      (setq c (concat c "\\)\\|\\(?:" (car r))))
    (concat "\\(?:" c "\\)"))
  "A regular expression matching any of the forms of dates
recognized by tasks-parse-date.")

(defconst tasks-font-lock-keywords
  `(;; tasks-font-lock-date will return the match bounds as either
    ;; subexpression 1, 2, or 3 depending on whether it's in the past,
    ;; present, or future.
    (tasks-font-lock-date (1 tasks-past-date-face nil t)
                          (2 tasks-present-date-face nil t)
                          (3 tasks-future-date-face nil t))
    ;; Incomplete tasks need to be handled first so that incomplete
    ;; sub-tasks can be overridden by complete or irrelevant parent
    ;; tasks.
    (tasks-font-lock-incomplete . tasks-incomplete-face)
    ;; Complete and irrelevant tasks must override font-locking
    ;; because they may encompass already highlighted sub-tasks.
    (tasks-font-lock-complete 0 tasks-completed-face t)
    ;; Irrelevant must come last so that irrelevant sub-tasks of
    ;; complete parent tasks will still be highlighted as irrelevant
    ;; and irrelevant parent tasks will mark all children as
    ;; irrelevant regardless of if they are complete or not.
    ;;
    ;; Alternatively, we could make it so a complete or irrelevant
    ;; parent task overrides all children, but this cannot be
    ;; expressed in terms of an ordering over states alone; it would
    ;; require different highlighters for different levels of tasks,
    ;; or the highlighter would have to examine the parent task.  We
    ;; take the approach we do because it is actually perfectly
    ;; reasonable, if not a little unexpected.
    (tasks-font-lock-irrelevant 0 tasks-irrelevant-face t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date parsing
;;

;; XXX Make a single state var
(defvar tasks-date-match-state nil)

(defun tasks-parse-date (&optional str)
  "Parse the date beginning at point, or in STR if non-nil.
Return a date object and set the date match state."

  (save-match-data
    (let ((regexes tasks-date-regexes)
          (result nil))
      (while (and (consp regexes) (not result))
        (let* ((this (car regexes))
               (regex     (first this))
               (monthpos  (second this))
               (monthname (third this))
               (daypos    (fourth this))
               (yearpos   (fifth this)))
          (when (if (null str)
                    (looking-at regex)
                  (string-match regex str))
            ;; Got a match, parse it
            (setq tasks-date-match-state
                  (list (match-beginning 0) (match-end 0)))
            (let ((month
                   (if monthname
                       (+ 1 (position (match-string-no-properties monthpos str)
                                      tasks-months :test #'string=))
                     (string-to-number (match-string monthpos str))))
                  (day (string-to-number (match-string daypos str)))
                  (year (string-to-number (match-string yearpos str))))
              (setq result (tasks-make-date-ymd year month day)))))
        (setq regexes (cdr regexes)))
      (unless result
        (if (null str)
            (error "Invalid date at point")
          (error "Invalid date: %s" str)))
      result)))

(defun tasks-date-beginning ()
  (if (null tasks-date-match-state)
      (error "No date has been parsed")
    (first tasks-date-match-state)))

(defun tasks-date-end ()
  (if (null tasks-date-match-state)
      (error "No date has been parsed")
    (second tasks-date-match-state)))

(defun tasks-make-date-ymd (year month day &optional canonicalize)
  "Make a date from the given year, month, and day.  If
canonicalize is non-nil, then the given arguments may not
actually represent a valid date (in particular, DAY may be
greater than the number of days in MONTH or MONTH may be greater
than the number of months in a year), and should be
canonicalized."

  (if (not canonicalize)
      (list year month day)
    (let ((dtime (decode-time (encode-time 0 0 0 day month year))))
      (list (sixth dtime) (fifth dtime) (fourth dtime)))))

(defmacro tasks-let*-ymd (bindings &rest body)
  (let ((lb (mapcan (lambda (b)
                      (let ((temp (gensym)))
                        `((,temp ,@(nthcdr 3 b))
                          (,(first b) (car ,temp))
                          (,(second b) (cadr ,temp))
                          (,(third b) (caddr ,temp)))))
                    bindings)))
    `(let* ,lb ,@body)))

(defun tasks-date-dow (date)
  (tasks-let*-ymd ((year month day date))
    (seventh (decode-time (encode-time 0 0 0 day month year)))))

(defun tasks-unparse-date (date)
  (tasks-let*-ymd ((year month day date))
    (let ((weekday (tasks-date-dow date)))
      (concat (nth weekday tasks-weekdays)
              ", "
              (nth (- month 1) tasks-months)
              " "
              (number-to-string day)
              ", "
              (number-to-string year)))))

(defun tasks-date-today ()
  (let* ((time (decode-time))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (tasks-make-date-ymd year month day)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day parsing
;;

(defun tasks-day-bounds ()
  (let ((min-bound (save-excursion
                     (end-of-line)
                     (if (re-search-backward tasks-date-regex nil t)
                         (match-end 0)
                       (point-min))))
        (max-bound (save-excursion
                     (if (re-search-forward tasks-date-regex nil t)
                         (match-beginning 0)
                       (point-max)))))
    (list min-bound max-bound)))

(defun tasks-day-tasks ()
  (save-excursion
    (let ((min-bound (first (tasks-day-bounds)))
          (tasks nil) task)
      (goto-char min-bound)
      (forward-line)
      (while (setq task (tasks-parse-task t))
        (setq tasks (cons task tasks))
        (goto-char (cdr (tasks-task-string-bounds task))))
      (reverse tasks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task parsing
;;

(defconst tasks-marker-meanings
  '(("-" incomplete)
    ("+" complete)
    ("~" irrelevant)
    (">" event)))

(defconst tasks-meaning-markers
  (mapcar #'reverse tasks-marker-meanings))

(defalias 'tasks-task-string-bounds #'first)

(defalias 'tasks-task-string #'second)

(defalias 'tasks-task-marker-bounds #'third)

(defun tasks-task-marker (task)
  (second (assoc (fourth task) tasks-marker-meanings)))

(defalias 'tasks-task-title-bounds #'fifth)

(defalias 'tasks-task-title #'sixth)

(defalias 'tasks-task-body #'eighth)

(defun tasks-task-field (task field)
  (let ((body (tasks-task-body task))
        (regexp (concat "^ *" field ": \\(.*\\)")))
    (when (and body (string-match regexp body))
      (match-string 1 body))))

(defun tasks-parse-task (&optional top-level)
  "Parse the task or event containing point.  If TOP-LEVEL is
non-nil, first find the enclosing top-level task and parse the
entire task, including sub-tasks in the body.  Otherwise, parse
the inner-most task."

  (let* ((min-bound (first (tasks-day-bounds)))
         (markers (mapcar #'car tasks-marker-meanings))
         (marker-re (regexp-opt markers t))
         (marker-or-space-re (regexp-opt (cons " " markers) nil))
         (start-re (concat "^\\(" (if top-level
                                      " " " +") "\\)" marker-re " "))
         (task-re (concat start-re ".*\n\\(?:\\1  .*\n\\)*"))
         (initial-point (point)))
    (save-excursion
      (end-of-line)
      ;; Find the entire task
      (when (and (re-search-backward start-re min-bound t)
                 (looking-at task-re)
                 (> (match-end 0) initial-point))
        (let* ((start (match-beginning 0)) (end (match-end 0))
               (indent (match-string 1))
               (marker-start (match-beginning 2))
               (marker-end (match-end 2))
               (marker (match-string 2))
               (title-start (+ marker-end 1))
               ;; Find the first line of the body
               (body-re (concat "^" indent "  " marker-or-space-re))
               (body-start (when (re-search-forward body-re end t)
                             (match-beginning 0)))
               ;; Find the title
               (title-end (or body-start end)))
          (list
           ;; Task bounds
           (cons start end)
           ;; Task string
           (buffer-substring start end)
           ;; Marker bounds
           (cons marker-start marker-end)
           ;; Marker string
           marker
           ;; Title bounds
           (cons title-start title-end)
           ;; Title string
           (buffer-substring title-start title-end)
           ;; Body bounds
           (when body-start (cons body-start end))
           ;; Body string
           (when body-start (buffer-substring body-start end))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeat parsing and manipulation
;;

(defun tasks-parse-repeat (str)
  "Parse the repeat specification given in STR.  The result is
this is merely a machine-readable form of the English
specification.  It is semantically interpreted when combined with
a date using `tasks-reify-repeat' to form a repeat object.

The grammar of repeat specifications is

REPEAT ::= COUNT? SPEC | SPECLY

COUNT ::= every NUM? | every other

SPEC ::= year | month MONTH-QUAL | week WEEK-QUAL | day

SPECLY ::= yearly | monthly MONTH-QUAL | weekly WEEK-QUAL | daily

MONTH-QUAL ::= by the? day of the? (week | month)

WEEK-QUAL ::= on WEEKDAY ((,? and | ,) WEEKDAY)*"

  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; Simplify whitespace
    (save-excursion
      (delete-region (point-min)
                     (progn (skip-chars-forward "[:space:]")
                            (point)))
      (while (re-search-forward "[[:space:]]+" nil t)
        (replace-match " "))
      (goto-char (point-max))
      (delete-region (progn (skip-chars-backward "[:space:]")
                            (point))
                     (point-max))
      (insert " "))
    (setq case-fold-search t)
    (let* (;; Parse count
           (count*
            (progn
              ;; Canonicalize -ly words
              (save-excursion
                (cond ((looking-at "\\(year\\|month\\|week\\)ly")
                       (replace-match (concat "Every " (match-string 1))))
                      ((looking-at "daily")
                       (replace-match "Every day"))))
              ;; Parse
              (cond ((looking-at "Every \\([0-9]+\\) ")
                     (string-to-number (match-string 1)))
                    ((looking-at "Every other ") 2)
                    ((looking-at "Every ") 1)
                    (t nil))))
           (count (if (not count*)
                      1
                    (goto-char (match-end 0))
                    count*))
           ;; Parse specifier
           (spec*
            (cond ((looking-at "years? ") 'year)
                  ((looking-at "months? by \\(the \\)?day of \\(the \\)?week ")
                   'month-by-dow)
                  ;; XXX Should this be the default if just given "month"?
                  ((looking-at "months? by \\(the \\)?day of \\(the \\)?month ")
                   'month-by-dom)
                  ((looking-at "months? ")
                   (error "Expecting \"by day of week\" or \"by day of month\""))
                  ((looking-at "weeks? ") 'week)
                  ((looking-at "days? ") 'day)
                  (t (error "Expecting \"year\", \"month\", \"week\" or \"day\""))))
           (spec (progn (goto-char (match-end 0)) spec*))
           ;; Parse qualifier
           (qual
            (when (and (eq spec 'week) (looking-at "on "))
              ;; XXX Support other things like 'weekdays' and
              ;; 'weekends'
              (let* ((dow-regexp (regexp-opt tasks-weekdays t))
                     (first-regexp (concat "on " dow-regexp " ?"))
                     (rest-regexp (concat "\\(?:\\(?:, \\)?and\\|,\\) "
                                          dow-regexp " ?"))
                     (regexp first-regexp)
                     (qlist nil))
                (while (looking-at regexp)
                  ;; XXX Use index instead of name
                  (let ((dow (position (match-string 1) tasks-weekdays
                                       :test #'string=)))
                    (setq qlist (cons dow qlist)))
                  (goto-char (match-end 0))
                  (setq regexp rest-regexp))
                (reverse qlist))))
           ;; XXX Parse "until"
           )
      (unless (looking-at "$")
        (error "Unexpected \"%s\"" (buffer-substring (point)
                                                     (- (point-max) 1))))
      (list spec count qual))))

(defun tasks-reify-repeat (repeat date)
  "Combine a parsed repeat with a specific date to give it
context, producing a reified repeat."

  (tasks-let*-ymd ((year month day date))
    (let ((dow (tasks-date-dow date))
          (spec (first repeat)) (count (second repeat)) (qual (third repeat)))
      (case spec
        ((year) (list 'additive count 0 year month day))
        ((month-by-dom) (list 'additive count 1 year month day))
        ((day) (list 'additive count 2 year month day))
        ((week)
         (cond ((null qual)
                (list 'additive (* count 7) 2 year month day))
               ((not (memq dow qual))
                (error "Repeat does not include the weekday of this event"))
               (t
                (error "Complex week repeat not implemented"))))
        ((month-by-dow)
         ;; See calendar-nth-named-day
         (error "Month-by-DOW not implemented"))
        (t (error "Illegal parsed repeat %S" repeat))))))

(defun tasks-repeat-after (rrepeat date)
  "Compute the earliest date this is strictly later than DATE
and that satisfies the given reified repeat."

  (tasks-let*-ymd ((year month day date))
    (let ((spec (first rrepeat)))
      (case spec
        ((additive)
         ;; ax+b on day, month, or year.  Subtract the base date from
         ;; the target date, round up the n'th field to the next
         ;; multiple of count, and then shift it back by the base
         ;; date.
         (let* ((count (second rrepeat)) (field-idx (third rrepeat))
                (bdate (cdddr rrepeat))
                (tdate (list year month day))
                (bfield (nth field-idx bdate))
                (tfield (nth field-idx tdate))
                (delta (- tfield bfield))
                (next-delta (* (+ 1 (/ delta count)) count)))
           ;; Shift it back
           (setcar (nthcdr field-idx tdate)
                   (+ bfield next-delta))
           ;; Canonicalize the date
           (tasks-make-date-ymd (first tdate) (second tdate) (third tdate) t)))
        (t
         (error "Illegal repeat %S" rrepeat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date manipulation
;;

(defun tasks-compare-lists (a b)
  (cond ((null a)            (if (null b) '= '<))
        ((null b)            '>)
        ((< (car a) (car b)) '<)
        ((> (car a) (car b)) '>)
        (t (tasks-compare-lists (cdr a) (cdr b)))))

(defun tasks-compare-dates (a b)
  (unless (and (= (length a) 3) (= (length b) 3))
    (error "Cannot compare dates %s and %s" a b))
  (tasks-let*-ymd ((ay am ad a)
                   (by bm bd b))
    (tasks-compare-lists (list ay am ad) (list by bm bd))))

(defun tasks-read-date ()
  (save-window-excursion
    ;; Start the calendar at the date preceding point
    (let ((date
           (save-excursion
             (end-of-line)
             (if (re-search-backward tasks-date-regex nil t)
                 (tasks-parse-date)))))
      (calendar)
      (when date
        (tasks-let*-ymd ((year month day date))
           (calendar-goto-date (list month day year)))))
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
          (let ((cdate (calendar-cursor-to-date)))
            (setq tasks-date-selected (tasks-make-date-ymd
                                       (third cdate)
                                       (first cdate)
                                       (second cdate))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font locking
;;

(defun tasks-font-lock-type (type just-title bound)
  ;; Search forward for tasks of the given type
  (let* ((marker (second (assq type tasks-meaning-markers)))
         (start-re (concat "^ +" (regexp-quote marker) " ")))
    (when (re-search-forward start-re bound t)
      (goto-char (match-beginning 0))
      ;; XXX This is a terribly inefficient way to do this
      (let* ((task (tasks-parse-task))
             (task-bounds (tasks-task-string-bounds task))
             (start (car task-bounds))
             (end (cdr (if just-title
                           (tasks-task-title-bounds task)
                         task-bounds))))
        (goto-char end)
        (set-match-data (list start (- end 1)))
        ;; XXX Only if it's actually multi-line?
        (put-text-property start (- end 1) 'font-lock-multiline t)
        t))))

(defun tasks-font-lock-incomplete (bound)
  (tasks-font-lock-type 'incomplete t bound))

(defun tasks-font-lock-complete (bound)
  (tasks-font-lock-type 'complete nil bound))

(defun tasks-font-lock-irrelevant (bound)
  (tasks-font-lock-type 'irrelevant nil bound))

(defun tasks-font-lock-date (bound)
  (when (re-search-forward tasks-date-regex bound t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (goto-char start)
      (let* ((date (tasks-parse-date))
             (today (tasks-date-today))
             (cmp (tasks-compare-dates date today))
             (bounds (list start end))
             (md (append bounds
                         (if (eq cmp '<) bounds '(nil nil))
                         (if (eq cmp '=) bounds '(nil nil))
                         (if (eq cmp '>) bounds '(nil nil)))))
        (goto-char end)
        (set-match-data md)
        t))))

(defun tasks-font-lock-extend-region (beg end old-len)
  ;; Extend beginning backwards by finding a line that is indented by
  ;; no more than one space
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((new-beg (if (re-search-backward "^ ?[^ ]" nil t)
                         (point)
                       (point-min))))
        ;; Extend the end forwards in the same way
        (goto-char end)
        (let ((new-end (if (re-search-forward "^ ?[^ ]" nil t)
                           (match-beginning 0)
                         (point-max))))
          (cons new-beg new-end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;

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
        (push-mark start t)
        (message "Mark saved at previous point")
        ;; Compare this date against the target
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
  (tasks-jump-to-date (tasks-date-today)))

(defun tasks-jump-or-insert (date &optional no-task)
  (interactive (list (tasks-read-date)))
  (if (tasks-jump-to-date date)
      (forward-line 1)
    (unless (save-excursion
              (forward-line -1)
              (eolp))
      (newline))
    (insert (tasks-unparse-date date))
    (newline)
    (unless no-task
      ;; XXX Generate this from the markers map
      (insert " - ")
      (save-excursion
        (newline)))))

(defun tasks-insert-from-file (file)
  (interactive "f")

  (let ((data
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (let ((date (tasks-parse-date)))
             (goto-char (tasks-date-end))
             (unless (looking-at "\n")
               (error "Garbage found after date"))
             (forward-char)
             (let ((start (point)))
               (goto-char (point-max))
               (skip-chars-backward " \n\t")
               (list date (buffer-substring start (point))))))))
    (tasks-jump-or-insert (first data) t)
    (insert (second data))
    (newline)))

(defun tasks-toggle-checkmark (&optional mark-irrelevant no-error)
  "Toggle the checkmark of the task at point.  If this item has a
repeat field and we're not transitioning from complete to
incomplete, then copy the task to its next repetition.  With
prefix arg, this instead marks the task as irrelevant."

  (interactive "P")
  (catch 'done
    (let ((task (tasks-parse-task nil)))
      (unless task
        (if no-error
            (throw 'done nil)
          (error "There is no task here")))
      ;; Handle any repeats
      ;; XXX Only for top-level tasks
      ;; XXX Should we indicate that we've copied this item so it
      ;; doesn't get copied again if the user deletes the next copy?
      (let ((rep-string (tasks-task-field task "Repeat")))
        (when (and rep-string
                   (not (eq (tasks-task-marker task) 'complete)))
          (let* ((date (save-excursion
                         (if (re-search-backward tasks-date-regex nil t)
                             (tasks-parse-date)
                           (error "Unable to find the date of this task"))))
                 (rep (tasks-reify-repeat
                       (tasks-parse-repeat rep-string)
                       date))
                 (next (tasks-repeat-after rep date)))
            (save-excursion
              (tasks-jump-or-insert next t)
              ;; Check for duplicates
              (let ((existing (mapcar #'tasks-task-title
                                      (tasks-day-tasks)))
                    (next-str (tasks-unparse-date next)))
                (if (member (tasks-task-title task) existing)
                    (message "Already repeated on %s" next-str)
                  (message "Repeating on %s" next-str)
                  (insert (tasks-task-string task))))))))
      ;; Toggle the marker
      (let* ((marker-bounds (tasks-task-marker-bounds task))
             (marker (tasks-task-marker task))
             (new-map (if mark-irrelevant
                          '((incomplete irrelevant)
                            (complete   irrelevant)
                            (irrelevant irrelevant))
                        '((incomplete complete)
                          (complete   incomplete)
                          (irrelevant incomplete))))
             (new-marker (or (second (assq marker new-map)) marker))
             (new-string (second (assq new-marker tasks-meaning-markers))))
        (save-excursion
          (goto-char (car marker-bounds))
          (delete-region (car marker-bounds) (cdr marker-bounds))
          (insert new-string))))
    t))

(defun tasks-complete-date ()
  (interactive)
  (save-excursion
    (forward-line 0)
    (let ((date (tasks-parse-date)))
      (delete-region (tasks-date-beginning) (tasks-date-end))
      (insert (tasks-unparse-date date)))))

(defun tasks-dwim (prefix-arg)
  "Invoke `tasks-complete-date' if point is on a date, otherwise,
invoke `tasks-toggle-checkmark'."

  (interactive "P")
  (or (ignore-errors
        (tasks-complete-date)
        t)
      ;; XXX This is a little unfortunate, but we need to get the
      ;; error flag in.  That, or we could use more specific error
      ;; codes.
      (tasks-toggle-checkmark prefix-arg t)
      (error "No date or task at point")))

(defvar tasks-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm (kbd "C-c i")   #'tasks-jump-or-insert)
    (define-key mm (kbd "C-c g")   #'tasks-jump-to-date)
    (define-key mm (kbd "C-c C-c") #'tasks-dwim)
    (define-key mm (kbd "C-c c")   #'tasks-dwim)
    (define-key mm (kbd "C-c .")   #'tasks-jump-to-today)
    (define-key mm (kbd "C-c C-.") #'tasks-jump-to-today)
    mm))

(define-derived-mode tasks-mode text-mode "Tasks"
  "Major mode for editing tasks files.

== Format ==

A task file is simply a structured list of tasks and events,
sorted and grouped by date.  For example,

Wednesday, March 12, 2008
 > 3:00p-4:00p Event
     Location: Elsewhere
 + Completed task
     Repeat: Every day
   + Subtask

Thursday, March 13, 2008
 - Incomplete task
 ~ Irrelevant task

tasks-mode places few restrictions on the structure of the file,
but does support a few conventions.  Each day is introduced by a
date on a line by itself.  Dates can be in a number of
formats (see `tasks-date-regexes'), but there is one canonical
format (see `tasks-date-canonical-regex'), which defaults to the
one shown above.  tasks-mode assumes that the dates are in sorted
order.

Under each date is a list of tasks and events for that date.  A
task must begin with at least one space, followed by a state
indicator, followed by a space and then a description.  A task
can be in one of three states:

 + Indicates a completed task
 - Indicates an incomplete task
 ~ Indicates an \"irrelevant\" task such as one that wasn't
   actually completed, but that you don't want to be reminded
   about

Additionally, a > indicator means the entry is an event and that
the user need not complete it.

Indented two spaces under a task may be fields of that task as
well as sub-tasks.  tasks-mode current understands one field,
\"Repeat\", which contains an English description of when the
task should repeat.  The task will be copied to its next
repetition when checked off.  For the exact grammar, see
`tasks-parse-repeat'.  A few examples are, \"Every other day\",
\"Monthly by day of the month\", \"Every 3 weeks on Sunday,
Monday, and Tuesday\".

== Editing ==\\<tasks-mode-map>

Most editing can be done with the DWIM command \"\\[tasks-dwim]\".
When point is on a task, this toggles the task between complete
and incomplete.  When point is on a date, this expands anything
that can be recognized as a date into the canonical date form,
which allows for quick entry of dates in a terse form (such as
\"2008-03-12\").

New entries can be inserted with \"\\[tasks-jump-or-insert]\", which
brings up a calendar to prompt for a date.

== Navigation ==

To jump to the current date (or as close as possible, if there
are no entries for the current date), use \"\\[tasks-jump-to-today]\".

To jump to an arbitrary date using the calendar prompt, use \
\"\\[tasks-jump-to-date]\"."

  ;; Set up font lock
  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (setq font-lock-defaults
        '(tasks-font-lock-keywords nil t))
  (setq font-lock-extend-after-change-region-function
        #'tasks-font-lock-extend-region))

(provide 'tasks-mode)
