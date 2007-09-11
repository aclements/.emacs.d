;;; outed-mode.el --- Simple outline editing

;; Copyright (C) 2007 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    10-Sep-2007
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

;; This is yet another outline mode, inspired by outline-mode and
;; org-mode.
;;
;; Each node is colored according to its indentation level.
;; outed-mode supports multi-line nodes with both hard and soft
;; newlines.  Each line of a node following the first line should
;; either be completely empty or should start with a space (generally,
;; it should be indented to align with the first line of the node).
;; In buffers that use soft newlines (such as with longlines-mode),
;; only lines following a hard newline need to be indented.
;;
;; outed-mode also supports simple outline editing.
;; * M-RET creates a new sibling immediately below the current node.
;; * RET starts a new paragraph within the current node.
;; * M-left and M-right change the indentation level of the current
;;   node.

(defgroup outed nil
  "Simple outline fontification and editing."
  :group 'outlines)

(defface outed-level-1
  '((((class color) (background dark))
     (:foreground "yellow")))
  "Outed level 1 face."
  :group 'outed)

(defface outed-level-2
  '((((class color) (background dark))
     (:foreground "green")))
  "Outed level 2 face."
  :group 'outed)

(defface outed-level-3
  '((((class color) (background dark))
     (:foreground "DeepSkyBlue")))
  "Outed level 3 face."
  :group 'outed)

(defface outed-level-4
  '((((class color) (background dark))
     (:foreground "yellow3")))
  "Outed level 4 face."
  :group 'outed)

(defface outed-level-5
  '((((class color) (background dark))
     (:foreground "green3")))
  "Outed level 5 face."
  :group 'outed)

(defface outed-level-6
  '((((class color) (background dark))
     (:foreground "DeepSkyBlue3")))
  "Outed level 6 face."
  :group 'outed)

(defface outed-hide
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black")))
  "Face used to hide leading stars if `outed-hide-leading-stars' is true."
  :group 'outed)

(defcustom outed-hide-leading-stars t
  "Decrease visual clutter by hiding leading stars.

If this is non-nil, a node heading like
*** Foo
will appear only as
  * Foo
in order to decrease visual clutter.

Note that this does not affect the actual buffer contents, it
simply displays the leading stars in the `outed-hide' face."
  :group 'outed
  :type 'boolean)

(defcustom outed-fontify-level-zero nil
  "Non-nil to consider unindented text for fontification.

If this is nil, then text that is not preceded by any stars (that
is \"level 0\" text), will be displayed in the standard buffer
face.  Otherwise, such text will be displayed using the
`outed-level-1' face."
  :group 'outed
  :type '(choice
          (const :tag "fontify starting with top-level text" nil)
          (const :tag "fontify starting with level 1 text" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation
;;;

(defun outed-beginning-of-node ()
  "Move point to the first character in the heading of the
current node."
  (interactive)
  (forward-line 0)
  (catch 'done
    (while t
      ;; Find the beginning of the soft-wrapped line under point
      (while (not (or (not use-hard-newlines)
                      (bobp)
                      (get-text-property (- (point) 1) 'hard)))
        (forward-line -1))
      ;; An empty line or a line that begins with a space is a
      ;; continuation, otherwise, it either begins with stars, so its
      ;; a subnode, or text, in which case its a root node
      (unless (or (eolp)
                  (= (char-after (point)) ?\ ))
          (throw 'done t))
      (forward-line -1))))

(defun outed-forward-soft-line ()
  ;; Find the end of the soft-wrapped line under point
  (forward-line 1)
  (while (not (or (not use-hard-newlines)
                  (eobp)
                  (get-text-property (- (point) 1) 'hard)))
    (forward-line 1)))

(defun outed-next-node ()
  "Move point to the first character of the next node."
  (interactive)
  ;; Find the beginning of the next node
  (catch 'done
    (while t
      ;; Go to the beginning of the next interesting line
      (outed-forward-soft-line)
      (when (eobp)
        (throw 'done t))
      ;; Is this a continuation line?
      (unless (or (eolp)
                  (= (char-after (point)) ?\ ))
          (throw 'done t)))))

(defun outed-end-of-node ()
  "Move point to the last character of the current node."
  (interactive)
  (outed-next-node)
  (when (not (eobp))
    (backward-char))
  ;; Move backwards over blank lines
  (while (and (eolp) (bolp))
    (backward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing
;;;

(defun outed-level ()
  (save-excursion
    (outed-beginning-of-node)
    (if (= (char-after (point)) ?\*)
        (let ((begin (point))
              (end (progn (skip-chars-forward "*")
                          (point))))
          (- end begin))
      0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun outed-new-sibling ()
  "Create a new sibling immediately below the current node.  If
this is done in the middle of a node, then the node's text will
be split at point."
  (interactive)
  (let ((level (outed-level)))
    (newline)
    (when (> level 0)
      (insert (make-string level ?\*))
      (insert " "))))

(defun outed-new-paragraph ()
  "Create a new paragraph in the current node.  If point in the
heading of a node, then a newline is inserted before the
heading."
  (interactive)
  ;; Am I in the stars of a heading?
  (let ((start (point)))
    (skip-chars-backward "*")
    (if (and (bolp) (eql (char-after (point)) ?\*))
        (progn
          ;; Yes.  Just insert the newline here
          (newline)
          (goto-char (+ start 1)))
      ;; No.  Insert a newline and indent
      (goto-char start)
      (let ((level (outed-level)))
        ;; If I'm in the whitespace before a line, clear the
        ;; whitespace so the indentation gets updated correctly
        (save-excursion
          (skip-chars-backward " ")
          (when (bolp)
            (delete-horizontal-space)))
        
        ;; Finally, insert the appropriate amount of indentation
        (newline)
        (when (> level 0)
          (insert (make-string (+ 1 level) ?\ )))))))

(defun outed-increase-level (&optional by)
  "Increase (or decrease) the indentation level of the current
node.  If a numeric argument is not provided, then indent by one
additional level, otherwise, indent (or outdent) by the value of
the numeric argument."
  (interactive "p")
  (setq by (or by 1))
  (save-excursion
    ;; Find the whole node
    (let* ((start (progn (outed-beginning-of-node)
                         (point)))
           (level (outed-level))
           (end (progn (outed-end-of-node)
                       (point-marker))))
      (goto-char start)
      ;; Don't outdent more than I can
      (if (> (- by) level)
          (setq by (- level)))
      (cond ((> by 0)
             ;; Indent
             (let* ((insert1 (concat (make-string by ?\*)
                                     (if (= level 0)
                                         " "
                                       "")))
                    (insert2 (make-string (length insert1) ?\ )))
               (insert insert1)
               (outed-forward-soft-line)
               (while (< (point) end)
                 (unless (eolp)
                   (insert insert2))
                 (outed-forward-soft-line))))
            ((< by 0)
             ;; Outdent
             (let ((deln (+ (- by) (if (= (- by) level)
                                       1
                                     0))))
               (delete-char deln)
               (outed-forward-soft-line)
               (while (< (point) end)
                 (let ((deln deln))
                   (while (and (> deln 0)
                               (= (char-after (point)) ?\ ))
                     (delete-char 1)
                     (setq deln (- deln 1))))
                 (outed-forward-soft-line))))))))

(defun outed-decrease-level (&optional by)
  "Like `outed-increase-level', but decrease the indentation by
the numeric argument."
  (interactive "p")
  (setq by (or by 1))
  (outed-increase-level (- by)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification
;;;

(defvar outed-faces
  '(outed-level-1 outed-level-2 outed-level-3
    outed-level-4 outed-level-5 outed-level-6))

(defun outed-jit-fontify (start end)
  (goto-char start)
  (outed-beginning-of-node)
  (let ((real-start (point))
        (overlays '())
        (outed-faces (if outed-fontify-level-zero
                         outed-faces
                       (cons nil outed-faces))))

    ;; Create fontification overlays for all nodes that overlap with
    ;; the fontification region
    (while (< (point) end)
      (let* ((level (min (outed-level) (- (length outed-faces) 1)))
             (n-start (point))
             (body-start (if (and outed-hide-leading-stars
                                  (eql (char-after (point)) ?\*))
                             (progn
                               (skip-chars-forward "*")
                               (backward-char)
                               (point))
                           n-start))
             (n-end (progn (outed-end-of-node)
                           (point)))
             (face (nth level outed-faces)))
        (unless (= n-start body-start)
          (setq overlays (cons (list n-start body-start 'outed-hide)
                               overlays)))
        (if face
            (setq overlays (cons (list body-start n-end face) overlays)))
        (outed-next-node)))

    ;; Bring the actual overlays up to speed
    (outed-create-overlays real-start (point) (nreverse overlays))))

(defun outed-unfontify (start end)
  (remove-overlays start end 'outed t))

(defun outed-create-overlays (start end overlays)
  (overlay-recenter end)
  (let ((news-box (cons nil overlays)))

    ;; Iterate through the existing overlays.  Remove those that don't
    ;; correspond to a specification in `overlays' and remove elements
    ;; in `overlays' that doesn't correspond to an existing overlay.
    (dolist (existing (overlays-in start end))
      (when (overlay-get existing 'outed)
        (let ((ostart (overlay-start existing))
              (oend   (overlay-end existing))
              (oface  (overlay-get existing 'face))
              (news   news-box)
              (keep   nil))
          (while (consp (cdr news))
            (let ((new (cadr news)))
              (if (and (=  ostart (car new))
                       (=  oend   (cadr new))
                       (eq oface  (caddr new)))
                  (progn
                    (setq keep t)
                    ;; Remove this overlay from the list
                    (setcdr news (cddr news))
                    ;; Terminate the loop
                    (setq news (cons nil '())))
                (setq news (cdr news)))))
          ;; Remove this overlay if it didn't correspond to one in the
          ;; list
          (unless keep
            (if (< (overlay-start existing) start)
                (if (> (overlay-end existing) end)
                    (progn
                      (move-overlay (copy-overlay existing)
                                    (overlay-start existing) start)
                      (move-overlay existing end (overlay-end existing)))
                  (move-overlay existing (overlay-start existing) start))
            (if (> (overlay-end existing) end)
                (move-overlay existing end (overlay-end existing))
              (delete-overlay existing)))))))

    ;; Create the overlays that weren't found
    (dolist (new (cdr news-box))
      (let ((ovl (make-overlay (car new) (cadr new) nil nil t)))
        (overlay-put ovl 'face (caddr new))
        (overlay-put ovl 'outed t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor mode
;;;

(define-minor-mode outed-mode
  "Toggle outed mode."
  nil
  " Outed"
  '(("\M-\r"    . outed-new-sibling)
    ("\r"       . outed-new-paragraph)
    ([\M-right] . outed-increase-level)
    ([\M-left]  . outed-decrease-level))

  (cond (outed-mode
         (outed-unfontify (point-min) (point-max))
         (jit-lock-register (function outed-jit-fontify) t))
        (t
         (jit-lock-unregister (function outed-jit-lock))
         (outed-unfontify (point-min) (point-max)))))

(provide 'outed-mode)
