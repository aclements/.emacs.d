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
;; * Can offer to restore old commit messages left over by commit
;;   failures and delete the old message.

;; The recommended auto-mode regexp is:
;; "svn-commit\\(\\.[0-9]+\\)?\\.tmp"

;;; Code:

(defconst svn-commit-file-face 'svn-commit-file-face)
(defface svn-commit-file-face
  '((((class color) (background dark))
     (:weight bold))
    (t (:inherit default)))
  "Face used for files."
  :group 'svn-commit-mode)

(defconst svn-commit-added-face 'svn-commit-added-face)
(defface svn-commit-added-face
  '((((class color) (background light))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "green"))
    (t (:inherit svn-commit-file-face)))
  "Face used for added files."
  :group 'svn-commit-mode)

(defconst svn-commit-modified-face 'svn-commit-modified-face)
(defface svn-commit-modified-face
  '((t (:foreground "yellow") (:inherit svn-commit-file-face)))
  "Face used for modified files."
  :group 'svn-commit-mode)

(defconst svn-commit-deleted-face 'svn-commit-deleted-face)
(defface svn-commit-deleted-face
  '((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:inherit svn-commit-file-face)))
  "Face used for removed files."
  :group 'svn-commit-mode)

(defconst svn-commit-ignore-regexp
  "^--This line, and those below, will be ignored--\n"
  "The regexp to match at the beginning of the svn commit message's
ignore block.")

(defvar svn-commit-font-lock-keywords
  `((,svn-commit-ignore-regexp . font-lock-comment-face)
    ("^A[ M] [ +] .*" . svn-commit-added-face)
    ("^M[ M] [ +] .*" . svn-commit-modified-face)
    ("^D[ M] [ +] .*" . svn-commit-deleted-face)
    ("^[ AMD]M [ +] .*" . svn-commit-modified-face)))

;; Customizable variables
(defvar svn-commit-offer-to-restore t
  "When a commit message is loaded, check if an old commit message was
left over by a earlier failed commit and offer to restore its contents
in this commit message.")
(defvar svn-commit-show-old-message t
  "While offering to restore an old commit message, split the window
and display the old commit message.")
(defvar svn-commit-delete-old-message t
  "If an old commit message was restored, offer to delete the old
commit message file when this commit message is saved.")
(defcustom svn-commit-mode-hook nil
  "Normal hook run when entering svn commit mode."
  :type 'hook
  :group 'svn-commit-mode)


(defvar svn-commit-restored-filename nil
  "If this commit message was restored from an old commit message,
this contains the filename of the commit message it was restored
from.  This is used to offer to delete the old message when this
message is saved.  nil if there was no restoration (or if the old
message has been deleted).")
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

  ;; Prompt to load an old commit message, if there is one.  This is
  ;; run off an idle timer so Emacs gets a chance to actually display
  ;; this buffer.  Unfortunately, this means it will pop up a dialog,
  ;; so we explicitly inhibit this.
  (if svn-commit-offer-to-restore
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (let ((last-nonmenu-event t))
                               (call-interactively
                                (function
                                 svn-load-old-commit-message))))))

  ;; Delete the old commit message when this one is saved
  (if svn-commit-delete-old-message
      (add-hook 'after-save-hook
                (function svn-delete-old-commit-message))))

(defadvice fill-paragraph (around svn-ignore-lines)
  "If in svn-commit-mode, cause paragraph filling to not extend to the
ignore block.  If the point is in the ignore block, completely ignores
the fill request."
  (if (not (eq major-mode 'svn-commit-mode))
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
      ;; Leave the ignore line at the beginning of the line
      (if (not (bobp))
          (forward-char -1))
      ;; Scan over extraneous whitespace and delete it
      (let ((end (point)))
        (skip-chars-backward " \n\t")
        ;; Allow trailing whitespace (not doing this is annoying with,
        ;; for example, templates that have unfilled-in labels at the
        ;; bottom)
        (end-of-line)
        (delete-region (point) end))))
  ;; Report that this hook did not save the buffer
  nil)

(defun svn-find-old-commit-message ()
  "Search for an old commit message that was left over by a failed
previous commit.  Returns the file name of the old commit message if
found, or nil otherwise."
  (let ((n 0)
        old-message)
    ;; Iterate from 0 to 9 as long as we haven't found an old message
    (while (and (not old-message) (< n 10))
      ;; Compose the old message name.  Message 0 has no number.
      (let ((filename (format "svn-commit%s.tmp"
                              (if (= n 0) "" (format ".%d" n)))))
        ;; Is it there and not the one that's open?
        (if (and (file-readable-p filename)
                 (not (eq (get-file-buffer filename)
                          (current-buffer))))
            ;; Found it
            (setq old-message filename)
          ;; Try the next number
          (setq n (+ n 1)))))
    old-message))

(defun svn-load-old-commit-message (filename)
  "Load the contents of an old commit message into this commit
message.  When called interactively, looks for old commit messages
and, if found, prompts the user to restore the old message.  The name
of the old message is remembered so it can be deleted later by
`svn-delete-old-commit-message'."
  ;; If there is an old commit message, prompt to restore it
  (interactive
   (let ((old (svn-find-old-commit-message)))
     (if (and old
              (progn
                (save-window-excursion
                  ;; Show the old message in a split window
                  (if svn-commit-show-old-message
                      ;; Make sure we don't get into a restore loop
                      ;; when the other window goes into
                      ;; svn-commit-mode
                      (let ((svn-commit-offer-to-restore nil))
                        (find-file-other-window old)))
                  ;; Restore?
                  (y-or-n-p
                   (format "Old commit message found in %s.  Restore? "
                           old)))))
         ;; If so, pass the old file name
         (list old)
       ;; If not, or there is no old file, pass nil
       (list nil))))
  (when filename
    (let* ((buf (find-file-noselect filename))
           (contents
            (with-current-buffer buf
              (goto-char (point-min))
              (buffer-substring
               (point)
               (if (re-search-forward svn-commit-ignore-regexp nil t)
                   (match-beginning 0)
                 (point-max))))))
      (insert contents)
      (setq svn-commit-restored-filename filename))))

(defun svn-delete-old-commit-message (&optional force)
  "If this buffer was restored from an old commit message by
`svn-load-old-commit-message', then prompt the user to delete the old
commit message and delete it.  If force is non-nil, don't prompt the
user."
  (interactive)
  ;; If we're in a commit mode buffer that was restored from an old
  ;; message, prompt the user about deleting the old message
  (when (and (eq major-mode 'svn-commit-mode)
             svn-commit-restored-filename
             (or force
                 (yes-or-no-p (format "Delete old commit message %s? "
                                      svn-commit-restored-filename))))
    ;; Delete it
    (delete-file svn-commit-restored-filename)
    (setq svn-commit-restored-filename nil)))
