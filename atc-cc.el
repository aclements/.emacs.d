;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C support
;;;
;;; This supports modes derived from cc-mode

(defun c-backward-defun (&optional arg)
  "Move backwards to the beginning of this function declaration.  This
differs from `c-beginning-of-defun' in that it actually takes the
point to the beginning of the function definition instead of just its
block."
  (interactive "p")
  (unless arg (setq arg 1))
  (while (> arg 0)
    (c-beginning-of-defun)
    (c-beginning-of-statement)
    (setq arg (1- arg))))

(defvar c-show-func-delay 0.1
  "Seconds of idle time before c-show-func updates the header line.")

(defvar c-show-func-function-prefix ""
  "String to prefix the header line with when displaying a function.")

(defvar c-show-func-top-level-prefix nil
  "String to prefix the header line with when outside of a function,
or nil to hide the header line.")

(defvar c-show-func-top-line-format nil
  "Header line format to display when already at the top of the
buffer (such as %b to display the buffer name), or nil to hide the
header line.")

(defun c-show-func-get-line ()
  "Helper routine to get the buffer contents at the line containing
point.  This will probably go away in the wash when c-show-func
implements more interesting parsing."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defun c-show-func-get-func ()
  "Get the string to display as the current function containing
point.  If no function contains point, return nil."
  (let ((state (c-parse-state))
        outermost prefix)
    ;; Find the outermost brace by finding the last non-pair entry in
    ;; state.  This should result in nil iff point is at the top
    ;; level.
    (dolist (brace state)
      (if (not (consp brace))
          (setq outermost brace)))
    (when outermost
      ;; Inside defun.  Figure out what it is.
      (save-excursion
        (goto-char outermost)
        (c-beginning-of-statement)
        (c-show-func-get-line)))))

(require 'cl)
(defun c-show-func-set-header-line (value)
  (flet ((slide-window (lines)
                       (set-window-start nil
                                         (save-excursion
                                           (goto-char (window-start))
                                           (forward-line lines)
                                           (point)))))
        (cond ((and value header-line-format)
               ;; Just change the header line
               (setq header-line-format value))

              ((and (not value) header-line-format)
               ;; Hide the header line
               (setq header-line-format nil)
               (slide-window -1))

              ((and value (not header-line-format))
               ;; Show the header line
               (slide-window 1)
               (setq header-line-format value))))
  ;; XXX Emacs has a bug with header-line updates, so do lots of
  ;; magic to hopefully work around it
  (force-mode-line-update)
  (let ((window-min-height 1))
    (shrink-window 1)
    (enlarge-window 1)))

(defun c-show-func ()
  "Set the header line to something that gives meaningful context
about whatever structure is currently going off the top of the screen.
If there's no such structure, this emulates the header line not being
present by displaying the line that would be there anyways."
  (interactive)
  (let ((func-line
         (save-excursion
           (goto-char (window-start))
           (if (null header-line-format)
               ;; Avoid instabilities by ignoring the line where the
               ;; header line would be if it were present
               (forward-line))
           (or (when (= (point) (point-min))
                 c-show-func-top-line-format)
               (let ((func (c-show-func-get-func)))
                 (when func
                   (concat c-show-func-function-prefix
                           func)))
               (progn
                 (forward-line -1)
                 (when c-show-func-top-level-prefix
                   (concat c-show-func-top-level-prefix
                           (c-show-func-get-line))))))))
    (c-show-func-set-header-line func-line)))

;;; Define C-specific features

(require 'atc-features)

(defmodefeature c-defun-jump
  (local-set-key "\M-p" (function c-backward-defun))
  (local-set-key "\M-n" (function c-end-of-defun)))

(defmodefeature c-auto-hungry
  (c-toggle-auto-hungry-state 1))

(defmodefeature c-filladapt
  (function c-setup-filladapt))

(defmodefeature c-auto-brace-space
  (defadvice c-electric-brace (around c-auto-brace-space activate)
    (if (or (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if I'm in a literal or if the
        ;; user typed anything other than an open brace
        ad-do-it
      ;; Go ahead and put a space here
      (just-one-space)
      (let ((here (point)))
        ad-do-it
        ;; Delete any extra space this may have inserted
        (save-excursion
          (goto-char here)
          (if (looking-at "[ \t]*\n")
              (delete-horizontal-space)))))))

(defmodefeature c-auto-close-brace
  (defadvice c-electric-brace (around c-auto-close-brace activate)
    (if (or (not c-auto-newline)
            (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if not in auto mode, if the user
        ;; typed anything other than an open brace, or if the point is
        ;; in a literal
        ad-do-it
      (let ((syntax-type
             (or (save-excursion
                   (c-backward-syntactic-ws)
                   (let ((cb (char-before)))
                     (cond ((= cb ?=) 'assignment)
                           ((= cb ?,) 'list)
                           ((or (= cb ?\)) (= cb ?\;)) 'block)
                           (t nil))))
                 (save-excursion
                   ;; Class, struct, or enum?
                   (c-beginning-of-statement-1)
                   (if (looking-at "typedef\\>")
                       (c-forward-token-1))
                   (if (looking-at "\\(class\\|struct\\|enum\\)\\>")
                       'definition
                     nil))
                 (progn
                   (message "c-auto-close-brace is confused")
                   'block))))
        ad-do-it
        (save-excursion
          (newline)
          ;; Type the close brace
          (let ((last-command-char ?})
                ;; Inhibit cleanup of empty defuns
                (c-cleanup-list
                 (remq 'empty-defun-braces c-cleanup-list)))
            (c-electric-brace arg)
            ;; Insert any additional characters dictated by the
            ;; syntactic context
            (cond ((or (eq syntax-type 'assignment)
                       (eq syntax-type 'definition))
                   (let ((last-command-char ?\;)
                         (c-cleanup-list (cons 'defun-close-semi
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))
                  ((eq syntax-type 'list)
                   (let ((last-command-char ?,)
                         (c-cleanup-list (cons 'list-close-comma
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))))
          ;; Clean up extra whitespace that may have been inserted
          ;; after the close characters
          (let ((end (point))
                (begin (save-excursion
                         (skip-chars-backward " \t\n")
                         (point))))
            (delete-region begin end)))))))

(defmodefeature c-show-func
  (if (boundp 'header-line-format)
      (run-with-idle-timer c-show-func-delay t
                           (function c-show-func))))

;;; Set up the mode itself

;; A lot of .h files are actually C++
(setcdr (assoc "\\.h\\'" auto-mode-alist) (function c++-mode))

;; Set C's features
(atc:put-mode-features 'c-mode-common-hook
                       '(autofill filladapt flyspell highlight-unhappy
                                  final-newline-always
                                  c-defun-jump c-auto-hungry
                                  c-filladapt c-auto-brace-space
                                  c-auto-close-brace c-show-func))
