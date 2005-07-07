;;; ebackup.el --- Enhanced backup operation for Emacs

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: backup
;; Version: 1.3.1

;; $Id: ebackup.el,v 1.19 2003/01/10 05:57:59 burton Exp $

;; This file is [not yet] part of GNU Emacs.

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
;;
;; Some lisp magic to add enhanced backup functions basically it mirrors the
;; filesystem that I am editing and copies all new backups into backup directory
;; that you specify.  Default is ~/.emacs-backup
;;
;; The ebackup provides an "enhanced" backup mechanism for Emacs.  Instead of
;; the conventional backup alternatives in the standard Emacs, ebackup provides
;; the following.
;;
;; - keeps all backups in ~/.emacs-backups
;;
;; - stores files with directory structure.  So for example if you edited the file
;;   /home/burton/test.txt the backup file would be saved as
;;   ~/.emacs-backups/home/burton/test.txt.1
;;
;; - support rotation of files.  Each time a file is backed up we store it as a
;;   number and then rotate these backups.  Example:
;;
;;     /home/burton/test.txt.1
;;     /home/burton/test.txt.2
;;     /home/burton/test.txt.3
;;
;; Should work out of the box.  All you really need to do is add this to your
;; lisp load path and do a (require 'ebackup).  You should also read the backups
;; section of the GNU Emacs manual to determine how to turn this on.

;; Note: Emacs21 also includes numbered backup files but it isn't as advanced as
;; ebackup (it doesnt include directory structure).

;;; Design:
;;
;; This just disables the native Emacs file backup system and hooks itself into
;; the file save system.  Everytime you save a file, ebackup determines if it
;; should take a backup.

;;; Install:
;;
;; Need to do a (require 'ebackup)
;;
;; Tell emacs to turn on backups:

;;; TODO:

;;; History
;;
;; - Sat Feb 09 2002 03:11 PM (burton@openprivacy.org): fixes a bug WRT buffers
;; that don't have filenames associated with them.
;;
;; - Thu Nov 15 2001 01:02 PM (burton@openprivacy.org): don't backup tramp file
;; names?
;;
;; - Tue Oct 16 2001 05:52 PM (burton@relativity.yi.org): Just labeled this
;;   version 1.3.0.  Seems to work fine.
;;
;; - Tue Oct 16 2001 05:52 PM (burton@relativity.yi.org): keep a log of what is
;;   going on.  This can also be used for debugging.
;;
;; - Wed Dec 20 00:09:00 2000 (burton): (version 1.1) added support for multiple
;;   revisions.
;;
;; - Wed Dec 2 00:10:04 2000 (burton): (version 1.0) init

;;; Code:
(defvar ebackup-max-copies 10 "The maximum number of backup copies to keep around.")

(defvar ebackup-destination-dir "~/.emacs-backup" "The destination director to store backups.")
(if (not (file-exists-p ebackup-destination-dir))
    (make-directory ebackup-destination-dir))

(defvar ebackup-log nil "If true, we will log all backup operations.")

(defvar ebackup-current-file-backed-up nil "True if this file has been backed up at least once.")
(make-variable-buffer-local 'ebackup-current-file-backed-up)

(defvar ebackup-log-message "*ebackup-log-message*" "The buffer name to use for logging operations.")

(defun ebackup-make-backup()
  "Make a backup from the current buffer."

  (ebackup-make-backup-file-name (buffer-file-name (current-buffer))))

;;need to replace the system 'make-backup-file-name' lisp with a new function
;;to determine a better filename.
(defun ebackup-make-backup-file-name(file)
  "Create the file name for `file'."

  (if (and (file-exists-p file)
           (not ebackup-current-file-backed-up)
           file)
      (let(backup-filename)

        (setq ebackup-current-file-backed-up t)
        
        (ebackup-log (format "Backing up file: %s \n" file))
        
        ;;first... use the absolute file-name so that we don't have duplicate entries
        ;;which are caused by symlinks.
        
        (setq backup-filename (subst-char-in-string ?: ?_ (file-truename file)))
        
        ;;determine the filename to use...
        (setq backup-filename (concat ebackup-destination-dir file))

        ;;make the parent directory if it doesn't exist
        (let(parent)
          (setq parent (file-name-directory backup-filename))
          ;;now prune the name from the path and make its parent directory
          (make-directory parent t))

        (ebackup-rotate backup-filename)
        (setq backup-filename (concat backup-filename ".1"))

        (ebackup-log (format "Backing up file %s as %s \n" file backup-filename))

        (copy-file file backup-filename t t))))

(defun ebackup-rotate(backup-file)
  "Given a specific file name ... rotate through all possible entries and move
them down one number.  This is a fairly fast backup system because we rename the
files."
  ;;variable backup-file should be the full path to the file without any version number.

  ;;remove the last possible backup file.
  (let(last-file)

    (setq last-file (concat backup-file "." (number-to-string ebackup-max-copies)))

    (if (file-exists-p last-file)
        (delete-file last-file)))

  ;;cycle from ebackup-max-copies-1 until 1 renaming files as we go..
  ;;Example: 9 -> 10
  ;;         8 -> 9
  ;;         7 -> 8
  (let(i)
    (setq i (1- ebackup-max-copies))
    (while (>= i 1)
      
      (let(last-file next-file)

        (setq last-file (concat backup-file "." (number-to-string i)))
        (setq next-file (concat backup-file "." (number-to-string (1+ i))))
        (if (file-exists-p last-file)
            (rename-file last-file next-file)))
      
      (setq i (1- i)))))

(defun ebackup-log(message)
  "Log the given message if logging is enabled."

  (if ebackup-log
      (save-excursion

        (set-buffer (get-buffer-create ebackup-log-message))

        (goto-char (point-max))

        (insert message))))

(defun toggle-ebackup-log()
  "Toggle use of the ebackup log."
  (interactive)
  
  (if ebackup-log
      (progn
        (message "ebackup log is now disabled.")
        (setq ebackup-log nil))
    (progn
      (message "ebackup log is now enabled.")
      (setq ebackup-log t))))

(defun toggle-ebackup-current-file-backed-up()
  "Toggle use of ebackups for the current buffer."
  (interactive)
  
  (if ebackup-current-file-backed-up
      (progn
        (message "file now marked as NOT backed up..")
        (setq ebackup-current-file-backed-up nil))
    (progn
      (message "file now marked as backed up.")
      (setq ebackup-current-file-backed-up t))))

;disable the normal emacs backup system
(setq make-backup-file-name-function nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

;;but make sure files are precious.
(setq file-precious-flag t)

(setq backup-inhibited t)

(setq version-control 'never)

;;don't use numbered backups...
(setq keep-new-versions 0)
(setq keep-old-versions 0)

(add-hook 'after-save-hook 'ebackup-make-backup)

(provide 'ebackup)

;;; ebackup.el ends here
