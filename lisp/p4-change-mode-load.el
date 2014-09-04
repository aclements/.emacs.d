;;; p4-change-mode-load.el --- major mode for Perforce change specifications

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    29-Aug-2009
;; Version:    0.1

;;; Commentary:

;; To install this mode, add the following lines to your .emacs file:
;;   (add-to-list 'load-path "PATH CONTAINING p4-change-mode-load.el" t)
;;   (require 'p4-change-mode-load)
;; After this, p4-change-mode will be used for Perforce change and
;; client specifications.

;;; Code:

;; To update this file, evaluate the following form
;;   (let ((generated-autoload-file buffer-file-name)) (update-file-autoloads "p4-change-mode.el"))


;;;### (autoloads (p4-change-mode) "p4-change-mode" "p4-change-mode.el"
;;;;;;  (19098 2420))
;;; Generated autoloads from p4-change-mode.el

(autoload (quote p4-change-mode) "p4-change-mode" "\
Major mode for editing Perforce change specifications.

This mode indents all text by one tab, sets up paragraph filling
to account for section headings, and hyperlinks file
specifications to run p4 diff.  If `p4-change-move-point' is
non-nil and point is at the beginning of the buffer, entering
this mode moves point to the View or Description section.

\(fn)" t nil)

(setq magic-mode-alist (nconc magic-mode-alist (quote (("^# A Perforce \\(Change\\|Client\\) Specification" . p4-change-mode)))))

;;;***

(provide 'p4-change-mode-load)
