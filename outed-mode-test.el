(require 'cl)
(require 'longlines)

(defun outed-test-all ()
  (interactive)
  (outed-test-forward-soft-line)
  (outed-test-continuation-p)
  (outed-test-cycle-indent)
  (outed-test-fill-paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing system
;;;

(defmacro outed-test (name &rest body)
  `(progn
     (let* ((bufname (format "*test-%s*" ,name))
            (buf (get-buffer bufname)))
       (when buf
         (kill-buffer buf))
       (switch-to-buffer bufname))
     (let ((text-mode-hook '()))
       ,@body)
     (message "Passed")))

(defun compare (expect actual)
  (if (equal expect actual)
      'passed
    (goto-char (point-max))
    (newline)
    (insert "Error")
    (newline)
    (insert (format "Expected: %s" expect))
    (newline)
    (insert (format "Got:      %s" actual))
    (newline)
    (error "FAILED")))

(defun compare-lists (expect actual)
  (if (equal expect actual)
      'passed
    (goto-char (point-max))
    (newline)
    (insert "Errors (expected, actual)")
    (newline)
    (let ((i 0))
      (while (or expect actual)
        (let* ((strx (format "%d. %-30s%-30s" i
                             (if expect (car expect) "<none>")
                             (if actual (car actual) "<none>")))
               (str (if (equal (car expect) (car actual))
                        (propertize strx 'face 'shadow)
                      (concat strx "  <==="))))
          (insert str)
          (newline))
        (setq expect (cdr expect)
              actual (cdr actual)
              i (+ 1 i))))
    (error "FAILED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

(defmacro forall-points (var &rest body)
  `(progn
     (goto-char (point-min))
     (while (not (eobp))
       (setq ,var (point))
       ,@body
       (goto-char (+ ,var 1)))
     (setq ,var (point))
     ,@body))

(defun fill-buffer (&rest lines)
  (let ((goto nil))
    (dolist (l lines)
      (if (eq l 'point)
          (setq goto (point))
        (insert l)
        (newline)))
    (when goto
      (goto-char goto))
    (when (boundp 'moved-point)
      (setq moved-point (not (null goto))))))

(defun check-buffer (&rest lines)
  (let* ((moved-point nil)
         (expect2 (with-temp-buffer
                    (apply 'fill-buffer lines)
                    (list (get-buffer-lines) (point))))
         (expect (car expect2))
         (actual (get-buffer-lines)))
    (when moved-point
      (compare `(point at ,(cadr expect2)) `(point at ,(point))))
    (compare-lists expect actual)))

(defun line-following-point ()
  (save-excursion
    (buffer-substring (point)
                      (progn (end-of-line) (point)))))

(defun get-buffer-lines ()
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (line-following-point) lines)
        (forward-line)))
    (reverse lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;

(defun outed-test-forward-soft-line ()
  (interactive)
  (outed-test
   'forward-soft-line

   ;; Set up the buffer
   (newline 3)
   (dotimes (i 2)
     (dotimes (j 3)
       (insert (make-string 10 ?a))
       (newline))
     (dotimes (j 3)
       (dotimes (k 10)
         (insert (make-string 10 ?a))
         (insert " "))
       (newline)))
   (newline 3)

   (outed-mode)

   ;; Try soft and hard line wraps, moving by -2, -1, 0, 1, 2 from each
   ;; point and gather lists of results up.  Check the property that
   ;; outed-forward-soft-line moves point in the same way regardless
   ;; of whether the lines are unwrapped or soft wrapped.
   (let* ((results
           (mapcar
            (lambda (soft)
              (longlines-mode soft)
              (let ((res '()))
                (forall-points p
                  (dolist (amt '(-2 -1 0 1 2))
                    (save-excursion
                      (outed-forward-soft-line amt)
                      (push `(from ,p to ,(point) by ,amt) res))))
                (reverse res)))
            '(0 1)))
          (with-hard (car results))
          (with-soft (cadr results)))
     (compare-lists with-hard with-soft))))

(defun outed-test-movement ()
  (interactive)
  (let ((test-lines '("" "" "A" "B"
                      "* C" "  D" "  E" "" "  F" "  G"
                      "** H" "   I"
                      "* J"
                      "K" "L"
                      ""
                      "* M"
                      ""
                      "N" "O"
                      "" "")))
    (dolist (func '(outed-beginning-of-node outed-end-of-node))
      (outed-test
       (list func 'idempotent)

       (apply 'fill-buffer test-lines)
       (let ((expect '()) (actual '()))
         (forall-points p
           (funcall func)
           (push `(from ,p to ,(point)) expect)
           (funcall func)
           (push `(from ,p to ,(point)) actual))
         (compare-lists (reverse expect) (reverse actual)))))

    (dolist (func '((outed-beginning-of-node <= >   <= >)
                    (outed-end-of-node       >= <   >= <)
                    (outed-next-node         >  <=  =  /=)))
      (outed-test
       (list (car func) 'direction)

       (apply 'fill-buffer test-lines)
       (let ((expect '()) (actual '()))
         (forall-points p
           (let ((cmp (if (eobp) (fourth func) (second func)))
                 (opp (if (eobp) (fifth func)  (third func))))
             (funcall (car func))
             (push `(moved ,cmp from ,p) expect)
             (push `(moved ,(if (funcall cmp (point) p) cmp opp)
                           from ,p) actual)))
         (compare-lists (reverse expect) (reverse actual)))))

    ;; Actually check the positions moved to from each point (break
    ;; down starting points into regions)
    ))

(defun outed-test-continuation-p ()
  (interactive)
  (dolist (breaks '(hard soft))
    (outed-test
     (list 'continuation-p breaks)

     (let* ((spec '(("" nil)
                    ("" nil)
                    ("A" nil)
                    ("A" t)
                    ("" nil)
                    ("A" nil)
                    ("* A" nil)
                    ("* A" nil)
                    ("  A" t)
                    ("" t)
                    ("  A" t)
                    ("" t)
                    ("" t)
                    ("  A" t)
                    ("" t)
                    ("A" nil)
                    ("" nil)
                    ("A" nil)
                    ("A" t)
                    ("* A" nil)
                    ("A" nil)
                    ("A" t)))
            (expect '()))

       (dolist (s spec)
         (insert (car s))
         (push (cadr s) expect)
         (when (and (eq breaks 'soft) (> (length (car s)) 0))
           (push (cadr s) expect)
           (dotimes (_ 50)
             (insert " A")))
         (newline))
       (outed-mode)
       (when (eq breaks 'soft)
         (longlines-mode))

       (goto-char (point-min))
       (let ((res '()))
         (while (not (eobp))
           (push (outed-continuation-p) res)
           (forward-line 1))
         (compare-lists (reverse expect) (reverse res)))))))

(defun outed-test-cycle-indent ()
  (interactive)
  (outed-test
   '(cycle-indent basic)
   (fill-buffer "* A"
                'point "  Bob"
                "  C"
                "* D")
   (let* ((expect1 '(("* Bob"  "  C"  "* D")
                     ("** Bob" "   C" "* D")
                     ("* Bob"  "  C"  "* D")
                     ("Bob"    "C"    "* D")
                     ("  Bob"  "  C"  "* D")))
          (expect (append expect1 expect1)))
     (setq last-command nil)
     (dolist (e expect)
       (outed-cycle-indent)
       (setq last-command 'outed-cycle-indent)
       (apply 'check-buffer (cons "* A" e))
       ;; Check point
       (compare "Bob" (line-following-point)))))

  (outed-test
   '(cycle-indent in-then-out)
   (fill-buffer "*** A"
                'point "* B"
                "* C")
   (let* ((expect1 '(("** B"   "* C")
                     ("*** B"  "* C")
                     ("**** B" "* C")
                     ("*** B"  "* C")
                     ("** B"   "* C")
                     ("* B"    "* C")
                     ("B"      "* C")
                     ("    B"  "* C")
                     ("*** B"  "* C")))
          (expect (append expect1 (cddr expect1))))
     (setq last-command nil)
     (dolist (e expect)
       (outed-cycle-indent)
       (setq last-command 'outed-cycle-indent)
       (apply 'check-buffer (cons "*** A" e)))))

  (outed-test
   '(cycle-indent level-0-blank)
   (fill-buffer "A" 'point "")

   (setq last-command nil)
   (dolist (e '("* " "" "* "))
     (outed-cycle-indent)
     (setq last-command 'outed-cycle-indent)
     (check-buffer "A" e)
     (compare "" (line-following-point))))

  (outed-test
   '(cycle-indent level-0-nonblank)
   (fill-buffer "A" 'point "B")

   (setq last-command nil)
   (dolist (e '("* B" "B" "* B"))
     (outed-cycle-indent)
     (setq last-command 'outed-cycle-indent)
     (check-buffer "A" e)
     (compare "B" (line-following-point))))
  )

(defun outed-test-fill-paragraph ()
  (interactive)
  (outed-test
   '(fill-paragraph level-1-bullet)
   (fill-buffer "A" "* B" 'point "  B" "  B" "" "  C" "  C")
   (outed-mode)
   (fill-paragraph nil)
   (check-buffer "A" "* B B B" "" "  C" "  C"))

  (outed-test
   '(fill-paragraph level-1-paragraph)
   (fill-buffer  "A" "* B" "  B" "" "  C" 'point "  C" "  C" "" "  D" "  D")
   (outed-mode)
   (fill-paragraph nil)
   (check-buffer "A" "* B" "  B" "" "  C C C" "" "  D" "  D"))

  (outed-test
   '(fill-paragraph level-1-bullet-reformat)
   (fill-buffer "A" "* B" 'point " B" "     B" "" "  C")
   (outed-mode)
   (fill-paragraph nil)
   (check-buffer "A" "* B B B" "" "  C"))

  (outed-test
   '(fill-paragraph level-1-paragraph-reformat)
   (fill-buffer "A" "* B" "  B" "" " C" 'point "  C" "    C" "" "  D")
   (outed-mode)
   (fill-paragraph nil)
   (check-buffer "A" "* B" "  B" "" "  C C C" "" "  D"))

  (outed-test
   '(fill-paragraph level-0)
   (fill-buffer "A" "A" "" "B" 'point "B" "B" "" "C" "C")
   (outed-mode)
   (fill-paragraph nil)
   (check-buffer "A" "A" "" "B B B" "" "C" "C"))
  )
