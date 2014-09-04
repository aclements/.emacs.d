;;; svn-commit-mode.el --- subversion commit log major mode

;; Copyright (C) 2006 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    11-Aug-2006
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

;; Other svn helpers:
;; * svn-blame -- Display a blame of the current file
;;   * Have the ability to show the log message for the revision under
;;     the cursor
;;   * Perhaps attempt to font-lock the file contents
;;     * Colors might clash with blame highlighting, so it might be
;;       better to do blame highlighting only on the blame columns
;;     * Can I draw boxes around things?
;;   * At least alternate between background colors with each change
;;     block
;;     * It would be interesting to assign each revision a color so
;;       that different parts that are from the same revision have the
;;       same color, but this might be infeasible for large files
;;     * Could just highlight all occurrences of the revision the
;;       cursor is currently over, or let the user say to highlight a
;;       revision
;;       * For other revisions, indicate older/newer than selected
;;         revision?
;;   * Perhaps this could be inlined, too?  This makes less sense,
;;     since the blame is always of base.  Perhaps I could interweave
;;     them, though this probably requires diff logic, again.

;; To do
;; * Make some useful indication when you've edited the buffer, as the
;;   diff is somewhat out of sync.  It would be hard to actually keep
;;   this correct, but I could so something like highlighting things
;;   differently that aren't part of the diff
;;   * Perhaps implement the diff myself and just ask svn for a copy
;;     of base.  Perhaps steal something from ediff or call upon
;;     regular diff?  diff logic is hard
;;     * And little changes can cause big changes in diffs.  For
;;       example, take the first line of a remove chunk and rewrite it
;;       right after the end of the add chunk and the entire remove
;;       chunk has to move.
;;   * One problem with this is that I won't know when base has
;;     changed unless I ask svn each time.  This isn't a problem for
;;     revision numbers or dates.  It is for BASE, HEAD, COMMITTED,
;;     and PREV.
;;   * As an optimization, I could diff directly against
;;     .svn/text-base/filename.svn-base.  This would also deal with
;;     base changing
;; * Do something about diffing against the saved file.  Either refuse
;;   to diff if it hasn't been saved, or somehow use a temporary file
;;   to do the diff
;;   * Using regular diff to compute the diff would help with this
;; * Make it easy to refresh the diff
;;   * Perhaps refresh it automatically when the file is saved?
;; * Don't hard-code faces
;; * Better error checking (compare line contents in diff against
;;   file)
;; * Make it easy to jump between diff chunks
;;   * Currently this is somewhat awkward with removals
;; * Better control of the svn process, such as not hard coding the
;;   command name, and dealing with the possibility of prompts like
;;   login prompts
;; * Be able to diff against revisions other than base
;; * Additional diff options, like ignoring whitespace
;; * There are certainly diff syntax things I've missed
;;   * Empty diff
;;   * Newline at end of file?
;;   * Property changes
;; * Do something better with removal overlays.  It would be nice if
;;   you could copy from within them.  Also, Emacs won't display just
;;   part of a before-string, making scrolling behave oddly,
;;   especially for really big removals.  But, they shouldn't confuse
;;   things like font-locking of the actual buffer contents

;;; Code:

(require 'cl)

(defvar svn-inline-diff-revision nil)
(make-variable-buffer-local 'svn-inline-diff-revision)

(defvar svn-inline-diff-overlays nil)
(make-variable-buffer-local 'svn-inline-diff-overlays)

(defun svn-inline-diff-clear-overlays ()
  (mapcar
   (lambda (overlay)
     (delete-overlay overlay))
   svn-inline-diff-overlays)
  (setq svn-inline-diff-overlays nil))

(defun svn-inline-diff-create-overlays (dest)
  (goto-char (point-min))

  ;; Consume the header of the diff
  (unless (eobp)                        ; Empty diff
    (dolist (expect '("Index: " "=====" "--- " "+++ "))
      (unless (looking-at expect)
        (error "Expected '%s' in diff header" expect))
      (forward-line)))

  (let ((removals ())
        (additions ())
        (src-line 1))
    ;; Parse each line of the diff, recording removals and additions
    (while (not (eobp))
      (let ((type (char-after)))
        (case type
          ((?@)
           ;; New chunk
           (unless (looking-at "@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@$")
             (error "Unable to parse chunk header"))
           (setq src-line (string-to-int (match-string 1)))
           (forward-line))
          ((? )
           ;; Unchanged line
           (forward-line)
           (incf src-line))
          ((?+)
           ;; Addition
           (push (list src-line (1+ src-line)) additions)
           (forward-line)
           (incf src-line))
          ((?-)
           ;; Removal
           (let ((line
                  (save-excursion
                    (buffer-substring (1+ (point))
                                      (progn
                                        (forward-line)
                                        (point))))))
             (push (list src-line line) removals))
           (forward-line))
          (otherwise
           (error "Unknown diff line type: %c" type)))))

    ;; Simplify the additions list so that multiple lines are
    ;; collapsed into one chunk
    (let ((radd ()))
      (dolist (add additions)
        (if (and (not (null radd)) (= (second add) (first (car radd))))
            (setcar radd (list (first add) (second (car radd))))
          (push add radd)))
      (setq additions radd))

    ;; Likewise, simplify the removals list so that multiple lines are
    ;; collapsed into one chunk
    (let ((rremove ()))
      (dolist (remove removals)
        (if (and (not (null rremove))
                 (= (first remove) (first (car rremove))))
            (setcar rremove
                    (list (first remove)
                          (concat (second remove)
                                  (second (car rremove)))))
          (push remove rremove)))
      (setq removals rremove))

    ;; Construct the overlays in the destination buffer
    (with-current-buffer dest
      (save-excursion
        ;; Additions
        (dolist (add additions)
          (let ((start (first add))
                (end (second add)))
            (goto-line start)
            (let ((ov (make-overlay (point)
                                    (progn
                                      (goto-line end)
                                      (point))
                                    nil t nil)))
              (overlay-put ov 'face
                           '((:background "midnight blue")))
              (push ov svn-inline-diff-overlays))))
        ;; Removals
        (dolist (remove removals)
          (let ((start (first remove))
                (content (second remove)))
            (goto-line start)
            (let ((ov (make-overlay (point) (point) nil nil nil)))
              (overlay-put ov 'before-string
                           (propertize content
                                       'face '((:background "red4"))))
              (push ov svn-inline-diff-overlays))))))))

(defvar svn-inline-diff-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm "\C-c\C-d\C-n" 'svn-inline-diff-next-chunk)
    (define-key mm "\C-c\C-d\C-p" 'svn-inline-diff-prev-chunk)
    mm))

(defun svn-inline-diff-next-chunk ()
  (interactive)
  (let (best-ov (best-start 0))
    (dolist (ov svn-inline-diff-overlays)
      (let ((start (overlay-start ov)))
        (if (and (> start (point))
                 (or (null best-ov)
                     (< start best-start)))
            (setq best-ov ov
                  best-start start))))
    (when best-ov
      (goto-char best-start))))

(defun svn-inline-diff-prev-chunk ()
  (interactive)
  (let (best-ov (best-end 0))
    (dolist (ov svn-inline-diff-overlays)
      (let ((end (overlay-end ov)))
        (if (and (< end (point))
                 (or (null best-ov)
                     (> end best-end)))
            (setq best-ov ov
                  best-end end))))
    (when best-ov
      (goto-char (overlay-start best-ov)))))

(define-minor-mode svn-inline-diff-mode
  "A minor mode for annotating the current buffer with its differences
from another revision of the file in svn (typically base)."

  nil
  (" diff:" svn-inline-diff-revision " ")
  'svn-inline-diff-mode-map

  (if (not svn-inline-diff-mode)
      (svn-inline-diff-clear-overlays)
    (let ((rev "head")
          (file-name (buffer-file-name))
          (mode-buffer (current-buffer)))
      (setq svn-inline-diff-revision rev)
      (with-temp-buffer
        (message "Diffing against %s..." rev)
        (let ((ret (call-process "svn" nil t nil
                                 "diff" "-x" "-u" "-r" rev file-name)))
          (if (/= ret 0)
              (error "svn call failed: %S" ret)))
        (message "Diffing against %s...done" rev)
        (svn-inline-diff-clear-overlays)
        (svn-inline-diff-create-overlays mode-buffer)))))
