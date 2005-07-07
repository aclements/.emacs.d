;;; $Id: compilation-colorization.el,v 1.1 2003/05/30 22:24:45 gjb Exp $
;;; By Greg J. Badros, August 2003; Revised September 2003
;;;
;;; Newer versions of grep (such as that shipped with RH9)
;;; apply colorization;  this code makes that work inside
;;; grep/igrep
;;;
;;; This may also be useful with colorgcc
;;;

(defvar compilation-ansi-color-filter-last-output-start 0)

(make-variable-buffer-local 'compilation-ansi-color-filter-last-output-start)

(defun compilation-ansi-color-filter ()
  (let ((start (or compilation-ansi-color-filter-last-output-start 0))
	(end (let ((proc (get-buffer-process (current-buffer))))
	       (if proc
		   (marker-position (process-mark proc))
		 (setq compilation-ansi-color-filter-last-output-start 0)
		 (point-max)))))
    ;;; because ansi-color-apply-on-region rewrites the buffer fragment to
    ;;; a possibly smaller number of characters (because ansi escape sequences
    ;;; are erase), we need to use a marker to track the end position through
    ;;; the call to ansi-color-apply-on-region
    (let ((marker (make-marker)))
      (set-marker marker end)
      ;; start from 4 less than where we think we need to start, in
      ;; case of partial escape sequences
      (ansi-color-apply-on-region (max 0 (- start 4)) end)
      ;; now end may be too far along in the buffer, but our marker is
      ;; at the right spot, so copy its offset back into end
      (setq end (marker-position marker)))
    (setq compilation-ansi-color-filter-last-output-start end)))

;; (remove-hook 'compilation-filter-hook 'compilation-ansi-color-filter)
(add-hook 'compilation-filter-hook 'compilation-ansi-color-filter)

(require 'ansi-color)
(provide 'compilation-colorization)
