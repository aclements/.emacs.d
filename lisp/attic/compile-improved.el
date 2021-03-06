;; XXX If there are any errors, leave point on the first one (and if
;; there are warnings before that, show an informative message).  If
;; not, and there are warnings, leave point on the first one.  If not,
;; then close the compile window and show a useful message.


(defun compilation-finish-hide-or-jump (buf msg)
  (with-current-buffer buf
    (message "Point %s" (point))
    (goto-char (point-min))
    (message "Point2 %s" (point))
    (let ((success
           (and (eq process-status 'exit)
                (= exit-status 0)))
          (found-error
           (ignore-errors
             ;; XXX This isn't changing the point.  In face, goto-char
             ;; in this function doesn't do anything.  It's probably
             ;; something obvious.
             (compilation-next-error 1 nil (point-min))))
          (win (get-buffer-window buf)))
      (when (and success (not found-error) win)
        (message "%s %s" mode-name msg)
        ;; Hide the window
        (delete-window win)))))
(when (boundp 'compilation-finish-functions)
  (add-hook 'compilation-finish-functions
            (function compilation-finish-hide-or-jump)
            t))


(defun compilation-handle-exit (process-status exit-status msg)
  "Write MSG in the current buffer and hack its mode-line-process."
  (let ((inhibit-read-only t)
	(status (if compilation-exit-message-function
		    (funcall compilation-exit-message-function
			     process-status exit-status msg)
		  (cons msg exit-status)))
	(omax (point-max))
	(opoint (point))
	(cur-buffer (current-buffer)))
    ;; Record where we put the message, so we can ignore it later on.
    (goto-char omax)
    (insert ?\n mode-name " " (car status))
    (if (and (numberp compilation-window-height)
	     (zerop compilation-window-height))
	(message "%s" (cdr status)))
    (if (bolp)
	(forward-char -1))
    (insert " at " (substring (current-time-string) 0 19))
    (goto-char (point-max))
    ;; Prevent that message from being recognized as a compilation error.
    (add-text-properties omax (point)
			 (append '(compilation-handle-exit t) nil))
    (setq mode-line-process (format ":%s [%s]" process-status (cdr status)))
    ;; Force mode line redisplay soon.
    (force-mode-line-update)
    ;; (if (and opoint (< opoint omax))
    ;;     (goto-char opoint))
    (with-no-warnings
      (if compilation-finish-function
	  (funcall compilation-finish-function cur-buffer msg)))
    (run-hook-with-args 'compilation-finish-functions cur-buffer msg)))

(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(exit signal))
      (let ((buffer (process-buffer proc)))
	(if (null (buffer-name buffer))
	    ;; buffer killed
	    (set-process-buffer proc nil)
	  (with-current-buffer buffer
            (goto-char (point-min))
	    ;; Write something in the compilation buffer
	    ;; and hack its mode line.
	    (compilation-handle-exit (process-status proc)
				     (process-exit-status proc)
				     msg)
	    ;; Since the buffer and mode line will show that the
	    ;; process is dead, we can delete it now.  Otherwise it
	    ;; will stay around until M-x list-processes.
	    (delete-process proc)))
	(setq compilation-in-progress (delq proc compilation-in-progress)))))
