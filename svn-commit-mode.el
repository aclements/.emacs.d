;;; svn-commit-mode.el --- subversion commit log major mode

;; Copyright (C) 2005-2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    09-Jul-2005
;; Version:    0.2

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

;; This major mode provides support for editing Subversion commit
;; messages.  It enhances editing by customizing paragraph filling,
;; automatically cleaning up whitespace, and providing font locking.
;; To improve log message quality, file status lines are automatically
;; hyperlinked to provide easy access to diff's that show the changes
;; being checked in.  Finally, the major mode understands Subversion's
;; behavior when commits fail, offering to restore the failed commit
;; message and clean up the stale file.

;; Features
;; * Modifies paragraph filling to not wrap the information block
;;   Subversion appends to commit messages
;; * Automatically removes extraneous trailing whitespace when saving
;;   so that logs don't contain extra vertical space
;; * Gratuitous color
;; * Previews and offers to restore and delete old commit messages
;;   left by failed commits
;; * File status lines are hyperlinked to provide easy access to
;;   diff's between the working copy and the base revision
;; * Disables backup files and the saveplace package when editing
;;   commit messages

;; To install this mode, add the following lines to your .emacs file:
;;   (autoload 'svn-commit-mode "svn-commit-mode" nil t)
;;   (add-to-list 'auto-mode-alist
;;                (cons "svn-commit\\(\\.[0-9]+\\)?\\.tmp"
;;                      'svn-commit-mode)

;;; Code:

(defgroup svn-commit-mode nil
  "Mode for editing Subversion commit messages.")

(defconst svn-commit-file-face 'svn-commit-file-face)
(defface svn-commit-file-face
  '((((class color) (background dark))
     (:weight bold))
    (t (:inherit default)))
  "Face used for files."
  :group 'svn-commit-mode)

(defconst svn-commit-added-face 'svn-commit-added-face)
(defface svn-commit-added-face
  '((((class color))
     (:foreground "green"))
    (t (:inherit svn-commit-file-face)))
  "Face used for added files."
  :group 'svn-commit-mode)

(defconst svn-commit-modified-face 'svn-commit-modified-face)
(defface svn-commit-modified-face
  '((((class color))
     (:foreground "yellow"))
    (t (:inherit svn-commit-file-face)))
  "Face used for modified files."
  :group 'svn-commit-mode)

(defconst svn-commit-deleted-face 'svn-commit-deleted-face)
(defface svn-commit-deleted-face
  '((((class color))
     (:foreground "red"))
    (t (:inherit svn-commit-file-face)))
  "Face used for removed files."
  :group 'svn-commit-mode)

(defconst svn-commit-ignore-face 'svn-commit-ignore-face)
(defface svn-commit-ignore-face
  '((((class color) (min-colors 9))
     (:inherit font-lock-comment-face))
    ;; font-lock-comment-face is completely unhelpful in low color
    ;; situations
    (((class color) (min-colors 8))
     (:foreground "red"))
    (t (:inherit font-lock-comment-face)))
  "Face used for the ignored line in the commit message."
  :group 'svn-commit-mode)

(defconst svn-path-face 'svn-path-face)
(defface svn-path-face
  '((((supports :underline t)) :underline t)
    (t (:foreground "lightblue")))
  "Face used to in addition to the added/modified/etc face to
highlight paths in the commit message."
  :group 'svn-commit-mode)

(defconst svn-commit-ignore-regexp
  "^--This line, and those below, will be ignored--\n"
  "The regexp to match at the beginning of the svn commit message's
ignore block.")

(defvar svn-commit-stat-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'svn-commit-mouse-visit-diff)
    (define-key map "\r" 'svn-commit-visit-diff)
    map)
  "Keymap applied to status lines in the commit message's ignore
block.  By default, these bindings visit diffs when status lines
are selected.")

(defvar svn-commit-stat-line nil
  "A variable whose properties are used as the default properties
of status lines in the commit message's ignore block.")
(put 'svn-commit-stat-line 'mouse-face 'highlight)
(put 'svn-commit-stat-line
     'help-echo "mouse-2, RET: visit diff in other window")
(put 'svn-commit-stat-line
     'point-entered (lambda (o n) (message "RET: visit diff in other window")))
(put 'svn-commit-stat-line 'follow-link t)
(put 'svn-commit-stat-line 'pointer x-sensitive-text-pointer-shape)
(put 'svn-commit-stat-line 'keymap svn-commit-stat-line-map)

(defvar svn-commit-font-lock-keywords
  `((,svn-commit-ignore-regexp . svn-commit-ignore-face)
    ("^A[ M] [ +] \\(.*\\)"
     (0 '(face svn-commit-added-face
          category svn-commit-stat-line))
     (1 '(face svn-path-face
          svn-commit-path t) prepend))
    ("^M[ M] [ +] \\(.*\\)"
     (0 '(face svn-commit-modified-face
          category svn-commit-stat-line))
     (1 '(face svn-path-face
          svn-commit-path t) prepend))
    ("^D[ M] [ +] \\(.*\\)"
     (0 '(face svn-commit-deleted-face
          category svn-commit-stat-line))
     (1 '(face svn-path-face
          svn-commit-path t) prepend))
    ("^[ AMD]M [ +] .*" . svn-commit-modified-face)))

;; Customizable variables
(defcustom svn-commit-mode-hook nil
  "Normal hook run when entering svn commit mode."
  :type 'hook
  :group 'svn-commit-mode)

(defcustom svn-commit-offer-to-restore t
  "Offer to restore the contents of failed commit messages.

When a commit message is loaded, check if an old commit message was
left over by a earlier failed commit and offer to restore its contents
into this commit message."
  :type 'boolean
  :group 'svn-commit-mode)

(defcustom svn-commit-show-old-message t
  "Display the old commit message when offering to restore it.

While offering to restore an old commit message, split the window
and display the old commit message."
  :type 'boolean
  :group 'svn-commit-mode)

(defcustom svn-commit-delete-old-message t
  "Delete the old commit message on save.

If an old commit message was restored, offer to delete the old
commit message file when this commit message is saved.  If 'auto,
then don't bother prompting."
  :type '(choice
          (const :tag "Don't delete" nil)
          (const :tag "Offer to delete" t)
          (const :tag "Automatically delete" auto))
  :group 'svn-commit-mode)

(defcustom svn-commit-diff-args ()
  "Additional arguments to pass to svn diff."
  :type '(repeat string)
  :group 'svn-commit-mode)

(defcustom svn-commit-diff-mode 'diff-mode
  "Major mode to use when viewing the results of svn diff."
  :type 'function
  :group 'svn-commit-mode)

(defcustom svn-commit-diff-height 3
  "Height of the commit message window when viewing a diff."
  :type 'integer
  :group 'svn-commit-mode)


(defvar svn-commit-restored-filename nil
  "If this commit message was restored from an old commit message,
this contains the filename of the commit message it was restored
from.  This is used to delete the old message when this message
is saved.  nil if there was no restoration (or if the old message
has been deleted).")
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

  ;; Set text properties on the ignore block
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward svn-commit-ignore-regexp nil t)
      ;; Changing text properties updates the undo list and modifies
      ;; the buffer; inhibit this
      (let ((buffer-undo-list t)
            (modified (buffer-modified-p)))
        ;; Make the ignore block immutable.  We also grab the newline
        ;; before the ignore block to make it impossible to insert
        ;; text at the beginning of the ignore line.  We could use a
        ;; front-sticky property, but then users could delete the
        ;; initial blank line and wind up with a completely read-only
        ;; buffer.
        (add-text-properties (- (match-beginning 0) 1) (point-max)
                             '(read-only t))
        (set-buffer-modified-p modified))))

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
                                 svn-load-old-commit-message)))))))

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
message.  When called interactively, looks for old commit
messages and, if found, prompts the user to restore the old
message.  The user is also prompted for whether or not they want
to delete the old message.  If so, then the name of the old
message is remembered so it can be deleted later by the save hook
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
               (progn
                 (goto-char
                  (if (re-search-forward svn-commit-ignore-regexp nil t)
                      (match-beginning 0)
                    (point-max)))
                 (skip-chars-backward " \n\t")
                 (point))))))
      (insert contents)
      (when (and svn-commit-delete-old-message
                 (or (eq svn-commit-delete-old-message 'auto)
                     (y-or-n-p
                      (format "Delete old commit message %s on save? "
                              filename))))
        (setq svn-commit-restored-filename filename)
        (add-hook 'after-save-hook
                  (function svn-delete-old-commit-message))))))

(defun svn-delete-old-commit-message ()
  "If this buffer was restored from an old commit message by
`svn-load-old-commit-message', then delete the old commit
message."
  (interactive)
  ;; Are we in a commit mode buffer that was restored from an old
  ;; message?
  (when (and (eq major-mode 'svn-commit-mode)
             svn-commit-restored-filename)
    ;; Delete it
    (delete-file svn-commit-restored-filename)
    (setq svn-commit-restored-filename nil)))

(defun svn-commit-mouse-visit-diff (event)
  "Visit the SVN diff of the file selected by the mouse."
  (interactive "e")
  (let ((posn (event-start event)))
    (select-window (posn-window posn) t)
    (svn-commit-visit-diff (posn-point posn))))

(defun svn-commit-visit-diff (pos)
  "Visit the SVN diff of the file selected at point."
  (interactive "d")
  ;; Check that we're on a commit line by checking its text category
  (let ((category (get-text-property pos 'category)))
    (when (eq category 'svn-commit-stat-line)
      ;; Extract the path from the line
      (let* ((beg1 (previous-single-property-change (1+ pos) 'category))
             (beg  (next-single-property-change beg1 'svn-commit-path))
             (end  (next-single-property-change beg  'svn-commit-path))
             (path (buffer-substring-no-properties beg end))
             (name (concat "diff " path)))
        ;; Split the message window and create a buffer for the diff
        (delete-other-windows)
        (select-window (split-window nil (max svn-commit-diff-height
                                              window-min-height)))
        (switch-to-buffer (generate-new-buffer name))
        ;; Put this buffer in to diff mode
        (funcall svn-commit-diff-mode)
        ;; Make the buffer read-only
        (setq buffer-read-only t)
        ;; Define 'q' to destroy the window and return to the message
        (let ((ro-map (assq 'buffer-read-only minor-mode-overriding-map-alist)))
          (when ro-map
            (define-key (cdr ro-map) "q" 'delete-window)))
        ;; Retrieve the diff, asynchronously
        (let ((proc (apply #'start-process name (current-buffer)
                           "svn" "diff" (append svn-commit-diff-args
                                                (list path)))))
          ;; Filter the process in the normal way, but don't move
          ;; point
          (set-process-filter proc
                              (lambda (p s)
                                (with-current-buffer (process-buffer p)
                                  (save-excursion
                                    (goto-char (process-mark p))
                                    (let ((inhibit-read-only t))
                                      (insert s))
                                    (set-marker (process-mark p) (point))))))
          ;; Put the process status in the echo area instead of the
          ;; buffer
          (set-process-sentinel proc
                                (lambda (p e)
                                  (message "svn diff %s" e))))))))
