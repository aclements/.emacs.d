;;; color-completion.el --- highlight file completion buffers

;; Copyright (C) 2010 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    22-Oct-2010
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

;;; Commentary:

;; After loading this file, use M-x global-color-completion to enable
;; (or disable) highlighting.  To permanently enable, add something
;; like (global-color-completion t) to your ~/.elisp file.

;;; Customization:

(defgroup color-completion nil
  "Highlighting file completion buffers.")

(defconst color-completion-symlink-face
  'color-completion-symlink-face)
(defface color-completion-symlink-face
  '((((class color) (min-colors 88))
     (:foreground "#55FFFF"))
    (((class color) (min-colors 8))
     (:foreground "cyan")))
  "Face used for symlinks."
  :group 'color-completion)

(defconst color-completion-directory-face
  'color-completion-directory-face)
(defface color-completion-directory-face
  '((((class color) (min-colors 88))
     (:foreground "#5555FF"))
    (((class color) (min-colors 8))
     (:foreground "blue"))
    (t (:inverse-video t)))
  "Face used for directories."
  :group 'color-completion)

(defconst color-completion-executable-face
  'color-completion-executable-face)
(defface color-completion-executable-face
  '((((class color) (min-colors 88))
     (:foreground "#55FF55"))
    (((class color) (min-colors 8))
     (:foreground "green")))
  "Face used for executable files."
  :group 'color-completion)

;;; Code:

(defadvice file-name-all-completions (after color-completions)
  "Add a 'color-completion-full-path property to each file name
in the completion list that records the file's full path, for
later use by color-completion-highlight."

  ;; We can't simply add face properties to the file name strings at
  ;; this point because that breaks the common substring highlighting
  ;; done by completion-hilit-commonality, which sets font-lock-face
  ;; on the common prefix and the first difference.  Thus, if we set
  ;; 'face, we'll override it completely, but if we set
  ;; 'font-lock-face, we'll get overridden for the common substring.
  ;;
  ;; Thus, we mark each string with the information we'll need to
  ;; highlight it later, but hold off on changing faces until the
  ;; completion-setup-hook.
  ;;
  ;; It may be possible to do all of this from completion-setup-hook,
  ;; since completion-setup-function sets default-directory of the
  ;; completion buffer for file name completions, but I believe that
  ;; would require parsing the completion buffer to get file names.
  (let ((dir (ad-get-arg 1)))
    (setq ad-return-value
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file dir)))
               (propertize file 'color-completion-full-path path)))
           ad-return-value))))

;; We color the buffer in completion-setup-hook, which is run at the
;; end of display-completion-list, after common substring highlighting
;; has been performed.  You could imagine using
;; completion-annotate-function to get a more palatable completions
;; list, but that runs before common substring highlighting.

(defun color-completion-highlight ()
  "A completion-setup-hook that highlights file name completion
buffers using the information left behind by the
color-completions advice on file-name-all-completions."

  (with-current-buffer standard-output
    ;; Find each bit of text we marked in our
    ;; file-name-all-completions advice and highlight it.
    (let ((prop 'color-completion-full-path)
          (pos (point-min)))
      (while pos
        (setq pos (next-single-property-change pos prop))
        (when pos
          (let ((end (or (next-single-property-change pos prop)
                         (point-max)))
                (path (get-text-property pos prop)))

            ;; We use overlays so Emacs will combine faces
            (let ((ov (make-overlay pos end)))
              (color-completion-set-props ov path))

            (setq pos end)))))))

(defun color-completion-set-props (ov path)
  "Highlight a single path in a file name completion buffer by
setting the properties of overlay OV."

  (let* ((attrs (file-attributes path 'integer))
         (typ   (car attrs))
         (face  (cond
                 ;; Symlink
                 ((stringp typ) color-completion-symlink-face)
                 ;; Directory
                 (typ color-completion-directory-face)
                 ;; Executable (ls --color accepts any x bit)
                 ((string-match-p "x" (nth 8 attrs))
                  color-completion-executable-face))))
    (when face
      (overlay-put ov 'face face))))

(defvar color-completion-enabled nil)

(defun global-color-completion (&optional arg)
  "Toggle highlighting of file names in minibuffer completion.
With universal prefix ARG, turn on highlighting.
With zero or negative ARG, turn highlighting off."

  ;; Follow define-minor-mode conventions
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq color-completion-enabled
        (cond ((eq arg 'toggle) (not color-completion-enabled))
              (arg (> (prefix-numeric-value arg) 0))
              (t nil)))

  (if color-completion-enabled
      (progn
        (ad-enable-advice 'file-name-all-completions 'after 'color-completions)
        (add-hook 'completion-setup-hook #'color-completion-highlight))
    (ad-disable-advice 'file-name-all-completions 'after 'color-completions)
    (remove-hook 'completion-setup-hook #'color-completion-highlight))

  ;; Effect advice changes
  (ad-activate 'file-name-all-completions))

(provide 'color-completion)
