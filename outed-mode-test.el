(require 'cl)
(require 'longlines)

(defun outed-test-all ()
  (interactive)
  (outed-test-forward-soft-line)
  (outed-test-continuation-p)
  (outed-test-cycle-indent))

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

(defun fill-buffer (&rest lines)
  (let ((goto nil))
    (dolist (l lines)
      (if (eq l 'point)
          (setq goto (point))
        (insert l)
        (newline)))
    (when goto
      (goto-char goto))))

(defun line-following-point ()
  (save-excursion
    (buffer-substring (point)
                      (progn (end-of-line) (point)))))

(defun check-buffer (&rest lines)
  (let ((actual '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (line-following-point) actual)
        (forward-line)))
    (compare-lists lines (reverse actual))))

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
   ;; point and gather lists of results up
   (let* ((results
           (mapcar
            (lambda (soft)
              (longlines-mode soft)
              (let ((res '()))
                (dotimes (p (point-max))
                  (goto-char p)
                  (dolist (amt '(-2 -1 0 1 2))
                    (save-excursion
                      (outed-forward-soft-line amt)
                      (push `(from ,p to ,(point) by ,amt) res))))
                (reverse res)))
            '(0 1)))
          (with-hard (car results))
          (with-soft (cadr results)))
     (compare-lists with-hard with-soft))))

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
