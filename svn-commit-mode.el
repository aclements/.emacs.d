;;; svn-commit-mode.el --- subversion commit log major mode

;; Copyright (C) 2005 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    09-Jul-2005
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

;;; Commentary:

;; Features
;; * Gratuitous color
;; * Disables Emacs features (saveplace and backups) that just get in
;;   the way with transient non-uniquely-named files
;; * Fixes paragraph filling so it ignores the ignore block
;; * Removes extraneous trailing whitespace when saving

;; The recommended auto-mode regexp is:
;; "svn-commit\\.tmp\\(\\.[0-9]+\\)?"

;;; Code:

(defconst svn-commit-ignore-regexp
  "^--This line, and those below, will be ignored--\n"
  "The regexp to match at the beginning of the svn commit message's
ignore block.")

(defvar svn-commit-font-lock-keywords
  `((,(concat svn-commit-ignore-regexp "\\(.\\|\n\\)*") .
     font-lock-comment-face)))

(defvar svn-commit-mode nil)
(make-variable-buffer-local 'svn-commit-mode)

(define-derived-mode svn-commit-mode text-mode "SVN-Commit"
  "Major mode for editing svn commit log messages"

  ;; Colorize the ignored part of the log message
  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (setq font-lock-defaults
        '(svn-commit-font-lock-keywords nil nil nil nil))

  ;; Disable saveplace, since it's pretty close to meaningless and
  ;; very annoying on log messages
  (when (featurep 'saveplace)
    (setq save-place nil)
    ;; saveplace may already have a position for this filename.  Since
    ;; saveplace will ignore being disabled if it knows the filename,
    ;; take precautions against this.
    (or save-place-loaded (load-save-place-alist-from-file))
    (let ((cell (assoc buffer-file-name save-place-alist)))
      (if cell
          (setq save-place-alist (remq cell save-place-alist)))))

  ;; And disable backups, since these files are meant to be transient
  ;; and the backup files just clutter up the workspace
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)

  ;; Fix paragraph filling
  (ad-activate 'fill-paragraph)

  ;; Cleanup extraneous whitespace on save (really, svn should do
  ;; this, but judging by the flamewars around this issue, it's not
  ;; going to happen)
  (add-hook 'local-write-file-hooks (function svn-cleanup-whitespace))

  (setq svn-commit-mode t))

(defadvice fill-paragraph (around svn-ignore-lines (arg))
  "If in svn-commit-mode, cause paragraph filling to not extend to the
ignore block.  If the point is in the ignore block, completely ignores
the fill request."
  (if (not svn-commit-mode)
      ad-do-it
    (let ((ignore-begin
           ;; Look forward for the ignore block
           (save-excursion
             (when (re-search-forward svn-commit-ignore-regexp nil t)
               (match-beginning 0)))))
      (if (null ignore-begin)
          ;; If point is in the ignore block, don't do anything
          (unless
              (save-excursion
                (goto-char (point-min))
                (re-search-forward svn-commit-ignore-regexp nil t))
            ;; There is no ignore block
            ad-do-it)
        ;; Narrow to exclude the ignore block and then fill
        (save-restriction
          (narrow-to-region (point-min) ignore-begin)
          ad-do-it)))))

(defun svn-cleanup-whitespace ()
  "Remove any extra whitespace between the user text and the ignore
block."
  (save-excursion
    (goto-char (point-min))
    ;; Nuke all of the whitespace leading up to the ignore block
    (when (re-search-forward svn-commit-ignore-regexp nil t)
      (goto-char (match-beginning 0))
      (skip-chars-backward " \n\t")
      (delete-region (point) (match-beginning 0))
      (if (= (point) (point-min))
          ;; There is no user text; leave an extra blank line
          (newline))
      ;; Put a newline back in there
      (newline)))
  ;; Report that this hook did not save the buffer
  nil)
