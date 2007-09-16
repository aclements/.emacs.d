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
;; org-mode.  See the docstring of `outed-mode' (at the bottom of this
;; file) for a full description.

;; TODO

;; * Indentation cycling for multiparagraph nodes is broken
;; * Backspace at the beginning of a continuation line should be
;;   hungry.  Backspace in other places should probably have meaning,
;;   too.
;; * Fix binding of RET
;; * Bind tab properly
;; * outed-increase-subtree-level
;; * Make S-TAB the inverse of TAB

(defgroup outed nil
  "Simple outline highlighting and editing."
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

(defcustom outed-highlight-unindented nil
  "Non-nil to highlight unindented text using `outed-level-1'.

If this is nil, then text that is not preceded by any
stars (\"level 0\" text), will be displayed in the standard
buffer face.  Otherwise, such text will be displayed using the
`outed-level-1' face, the first level of indented text in
`outed-level-2' and so forth."
  :group 'outed
  :type '(choice
          (const :tag "Highlight top-level text" nil)
          (const :tag "Highlight only indented text" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation
;;;

(require 'cl)
(require 'derived)

(defun outed-beginning-of-node ()
  "Move point to the first character in the heading of the
current node."
  (interactive)
  (forward-line 0)
  (catch 'done
    (while t
      (outed-forward-soft-line 0)
      (when (bobp)
        (throw 'done t))
      (unless (outed-continuation-p)
        (throw 'done t))
      (forward-line -1))))

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
      (unless (or (outed-continuation-p)
                  (eolp))
        (throw 'done t)))))

(defun outed-end-of-node ()
  "Move point to the end of the current node."
  (interactive)
  (let ((start (point)))
    (outed-next-node)
    (when (and (not (eobp)) (not (bobp)) (> (point) start))
      (backward-char))
    ;; Move backwards over blank lines
    (while (and (eolp) (bolp) (not (bobp)) (> (point) start))
      (backward-char))))

(defun outed-forward-soft-line (&optional amt)
  (or amt (setq amt 1))
  (cond ((not use-hard-newlines)
         ;; If there are no soft lines, just use `forward-line'
         (forward-line amt))
        ((<= amt 0)
         ;; Move backwards
         (forward-line 0)
         (while (<= amt 0)
           ;; Find the beginning of the soft-wrapped line under point
           (while (not (or (bobp)
                           (get-text-property (- (point) 1) 'hard)))
             (forward-line -1))
           ;; Move to the previous line unless we're done
           (if (< amt 0)
               (forward-line -1))
           (setq amt (+ 1 amt))))
        (t
         ;; Move forwards
         (while (> amt 0)
           ;; Start at the beginning of the next line
           (forward-line 1)
           ;; Find the beginning of the next soft-wrapped line
           (while (not (or (eobp)
                           (get-text-property (- (point) 1) 'hard)))
             (forward-line 1))
           (setq amt (- amt 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing
;;;

(defun outed-continuation-p ()
  "Returns t if and only if the soft line under point is a
continuation line."
  (save-excursion
    (outed-forward-soft-line 0)
    (cond ((eql (char-after (point)) ?\*)
           ;; Headings are never continuation lines
           nil)
          ((eql (char-after (point)) ?\ )
           ;; Indented lines are always continuation lines
           t)
          ((eolp)
           ;; Blank lines are continuation lines unless the non-blank
           ;; preceded line is level 0
           (while (and (eolp) (not (bobp)))
             (outed-forward-soft-line -1))
           (let ((ch (char-after (point))))
             (or (eql ch ?\*) (eql ch ?\ ))))
          (t
           ;; Level 0 lines are continuation lines unless immediately
           ;; preceded by a blank line, header, or non-level 0
           ;; continuation
           (if (bobp)
               nil
             (outed-forward-soft-line -1)
             (let ((ch (char-after (point))))
               (not (or (eolp)
                        (eql ch ?\*)
                        (eql ch ?\ )))))))))

(defun outed-level ()
  (save-excursion
    (outed-beginning-of-node)
    (if (eql (char-after (point)) ?\*)
        (let ((begin (point))
              (end (progn (skip-chars-forward "*")
                          (point))))
          (- end begin))
      0)))

(defun outed-make-heading (level)
  (if (= level 0)
      ""
    (concat (make-string level ?\*) " ")))

(defun outed-make-continuation (level)
  (if (= level 0)
      ""
    (concat (make-string (+ level 1) ?\ ))))

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
               (insert-before-markers insert1)
               (outed-forward-soft-line)
               (while (< (point) end)
                 (unless (eolp)
                   (insert-before-markers insert2))
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
                               (eql (char-after (point)) ?\ ))
                     (delete-char 1)
                     (setq deln (- deln 1))))
                 (outed-forward-soft-line))))))))

(defun outed-decrease-level (&optional by)
  "Like `outed-increase-level', but decrease the indentation by
the numeric argument."
  (interactive "p")
  (setq by (or by 1))
  (outed-increase-level (- by)))

(defvar outed-cycle-direction 'in)
(make-variable-buffer-local 'outed-cycle-direction)

(defun outed-cycle-indent ()
  "Cycle the indentation level of the current node.  When used
repeatedly (with no intervening commands), this first indents the
current node until it is a sibling of the preceding node, then it
reverses direction and outdents the node until it is a root node.
A root node will be indented to be continuation line under the
preceding node.  Continuation lines will be made into siblings of
the preceding node."

  ;; XXX This has one problem: If a multiline node gets cycled to the
  ;; top level, then the connection between the paragraphs gets lost.
  ;; This is acceptable if the command isn't being repeated, but
  ;; shouldn't happen for a repeated command.
  (interactive)
  (save-excursion
    (save-restriction
      (outed-forward-soft-line 0)
      (let* ((repeat (eq last-command 'outed-cycle-indent))
             (contp (outed-continuation-p))
             (blank (eolp))
             (level (if blank 0 (outed-level)))
             (prev-level (cond (blank (outed-level))
                               (contp level)
                               (t
                                (save-excursion
                                  (outed-beginning-of-node)
                                  (if (bobp)
                                      0
                                    (backward-char)
                                    (outed-level))))))
             (end (save-excursion
                    (outed-end-of-node)
                    (point))))
        ;; Ensure that we can't get confused by earlier lines (for
        ;; example, without this, increase-level may try to move
        ;; point up)
        (narrow-to-region (point) end)
        ;; Always start by indenting
        (when (not repeat)
          (setq outed-cycle-direction 'in))
        ;; Don't indent more than one level beyond the previous node
        (when (and (eq outed-cycle-direction 'in)
                   (>= level (+ 1 prev-level)))
          (setq outed-cycle-direction 'out))
        ;; If we're trying to outdent, but we've hit the top level
        ;; (either we have text at the top level, or this is a blank
        ;; continuation line [in which case level is useless]), then
        ;; switch back to indenting.
        (when (and (eq outed-cycle-direction 'out)
                   (or (= level 0)
                       (and contp (eolp))))
          (setq outed-cycle-direction 'in))

        (cond ((and contp (/= level 0))
               ;; Make this a sibling to the node it is currently in
               (delete-horizontal-space)
               (insert-before-markers (outed-make-heading level)))
              ((and blank (/= prev-level 0))
               ;; Indent to the level of a continuation
               (insert-before-markers (outed-make-continuation prev-level)))
              ((and (= level 0) (/= prev-level 0))
               ;; Indent the rest of this node to make it a
               ;; continuation of the previous node
               (let ((indent (outed-make-continuation prev-level)))
                 (while (not (eobp))
                   ;; Leave blank lines blank
                   (unless (eolp)
                     (insert-before-markers indent))
                   (outed-forward-soft-line))))
              ((eq outed-cycle-direction 'in)
               ;; Indent
               (outed-increase-level))
              ((eq outed-cycle-direction 'out)
               ;; Outdent
               (outed-decrease-level)))))))

(defun outed-fill-paragraph (&optional justify)
  (interactive)
  (save-excursion
    (save-restriction
      ;; Find the beginning and end of the paragraph
      (let* ((start (progn
                      (forward-line 0)
                      (while (and (outed-continuation-p)
                                  (not (eolp))
                                  (not (bobp)))
                        (forward-line -1))
                      (point)))
             (level (outed-level))
             (end (progn
                    (forward-line)
                    (while (and (outed-continuation-p)
                                (not (eolp))
                                (not (eobp)))
                      (forward-line))
                    (point)))
             ;; Is this a heading or a subparagraph?
             (has-heading (progn
                            (goto-char start)
                            (not (outed-continuation-p)))))
        (narrow-to-region start end)
        ;; Compute the indentation for the first and remaining lines
        ;; accordingly.
        (let* ((indent2 (outed-make-continuation level))
               (indent1 (if has-heading
                            (outed-make-heading level)
                          indent2)))
          ;; Nuke the stars
          (when has-heading
            (delete-region start
                           (progn
                             (skip-chars-forward "*")
                             (point))))
          ;; Nuke all leading whitespace
          (while (not (eobp))
            (delete-horizontal-space)
            (forward-line))
          ;; Actually fill the region, using the appropriate margin
          (let ((fill-paragraph-function nil)
                (left-margin (length indent1)))
            (fill-region-as-paragraph (point-min) (point-max) justify))
          ;; If the region used to start with a heading, put the stars
          ;; back.
          (when has-heading
            (goto-char (point-min))
            (delete-horizontal-space)
            (insert indent1))
          ;; Inhibit further filling this paragraph
          t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlighting
;;;

(defvar outed-faces
  '(outed-level-1 outed-level-2 outed-level-3
    outed-level-4 outed-level-5 outed-level-6))

(defun outed-jit-fontify (start end)
  (goto-char start)
  (outed-beginning-of-node)
  (let ((real-start (point))
        (overlays '())
        (outed-faces (if outed-highlight-unindented
                         outed-faces
                       (cons nil outed-faces))))

    ;; Create face overlays for all nodes that overlap with the
    ;; font-lock region
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
;;; Modes
;;;

(defvar outed-mode nil)
(make-variable-buffer-local 'outed-mode)

(defvar outed-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm "\M-\r"    (function outed-new-sibling))
    (define-key mm "\r"       (function outed-new-paragraph))
    (define-key mm [\M-right] (function outed-increase-level))
    (define-key mm [\M-left]  (function outed-decrease-level))
    (define-key mm [tab]      (function outed-cycle-indent))
    mm))

(define-minor-mode outed-minor-mode
  "Toggle outed minor mode.  This provides the editing commands
and highlighting facilities of `outed-mode'.  See the mode's
docstring for a full description of outed-mode.  Unlike the major
mode, this does not redefine paragraph boundaries or filling
rules.

Interactively, with no prefix argument, toggle outed-minor-mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.

\\{outed-mode-map}"

  :init-value nil
  :lighter    " Outed"
  :keymap     outed-mode-map

  (cond (outed-minor-mode
         (setq outed-mode t)
         (outed-unfontify (point-min) (point-max))
         (jit-lock-register (function outed-jit-fontify) t))
        (t
         (setq outed-mode nil)
         (jit-lock-unregister (function outed-jit-lock))
         (outed-unfontify (point-min) (point-max)))))

(define-derived-mode outed-mode text-mode
  "Outed"
  "Outed major mode for editing outlines.

outed-mode provides simple outline editing commands and
highlights each node according to its indentation level.

Multiline nodes with both hard and soft newlines are supported.
Any line following the first line of a node counts as a
continuation line either if it beings with a space (generally, it
should be indented to the align with the first line of the node),
or if it a blank and there is a non-blank continuation line
eventually following it (that is, blank lines after a node aren't
considered part of the node).  In buffers that use soft
newlines (such as with longlines-mode), a \"line\" is understood
to be a soft-wrapped line that may span multiple visible lines.
Only hard newlines are considered line separators.

outed-mode defines paragraph boundaries so as not to cross node
boundaries.  Paragraph filling is node-aware and will
appropriately indent the paragraph to the level of the node.

\\{outed-mode-map}"

  (jit-lock-register (function outed-jit-fontify) t)

  ;; Each node counts as a paragraph.  This also has the effect of
  ;; making sentences not cross node boundaries.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|\\(?:\\*+\\)"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "[ \t\f]*$")

  ;; Fill correctly
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function (function outed-fill-paragraph))

  ;; Disable filladapt.  It breaks everything.
  (setq fill-prefix ""))

(provide 'outed-mode)
