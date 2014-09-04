;;; h-auto-mode.el --- automatically select major mode of .h files

;; Copyright (C) 2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    06-June-2008
;; Version:    0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Code:

(eval-when-compile (require 'cl))

(defgroup h-auto-mode nil
  "Automatically select major mode of C header files"
  :group 'c)

(defcustom h-auto-mode-exts
  '(("c" c-mode)
    ("cc" c++-mode) ("C" c++-mode) ("c++" c++-mode) ("cpp" c++-mode))
  "Association list mapping extensions to C major modes.

h-auto-mode searches for related files in the same directory as a
header file in order to determine the likely major mode of the
header.  It uses this map to determine the eligible modes and
related extensions."
  :group 'h-auto-mode
  :type '(alist :key-type string :value-type function))

;;;###autoload
(defun h-auto-mode ()
  "Automatically select a major mode for the current header file.

This works by examining related files in the same directory as
the header file.  First, it sees if another file exists with the
same base name and a different extension and selects the major
mode based on that.  If no such file exists, it takes a vote from
all of the recognized files in the same directory as the header."
  (interactive)

  (let ((mode (h-auto-mode-guess (buffer-file-name))))
    (if mode
        (funcall mode)
      (c-mode)
      (message "Unable to guess header mode"))))

(defun h-auto-mode-guess (fname)
  "Guess the major mode of a given file name.

FNAME should be either an absolute path or relative to the
current directory.  This function implements the heuristic
described in `h-auto-mode'."

  (catch 'done
    ;; First see if there is a corresponding source file
    (let ((base-name (file-name-sans-extension fname)))
      (dolist (ext h-auto-mode-exts)
        (when (file-exists-p (concat base-name "." (first ext)))
          (throw 'done (second ext)))))
    ;; No corresponding file.  Find the most common source file type
    ;; in this directory.
    (let (counts)
      (dolist (ext h-auto-mode-exts)
        (add-to-list 'counts (cons (second ext) 0)))
      (dolist (dirfile (directory-files (file-name-directory fname)))
        (let* ((ext (file-name-extension dirfile))
               (mode (assoc ext h-auto-mode-exts)))
          (when mode
            (let ((counter (assoc (second mode) counts)))
              (setcdr counter (1+ (cdr counter)))))))
      (let ((max (first counts)))
        (dolist (count (cdr counts))
          (when (> (cdr count) (cdr max))
            (setq max count)))
        (when (> (cdr max) 0)
          (throw 'done (car max)))))
    nil))

(provide 'h-auto-mode)
