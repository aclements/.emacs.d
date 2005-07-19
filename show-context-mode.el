;;; show-context-mode.el --- display context in the header line

;; Copyright (C) 2005 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    18-Jul-2005
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

;; Computer screens are small.  Even big ones are small.  In
;; particular, even very tall computer screens are shorter than an
;; infinitely long strip of paper.  Unfortunately, in many languages,
;; the meaning of the current line of code depends heavily on its
;; context, which may be influenced by lines that occur an unbounded
;; distance above the top of the window.  Show-context mode is meant
;; to help with this by scrunching together pieces of the buffer that
;; appear above the top of the window into one header line that
;; summarizes everything you need to know, but otherwise can't see, to
;; understand the context of the visible buffer.

;; Current show-context mode vaguely understands C/C++ code.  It's
;; designed to be extensible to be able to compute context for any
;; editing mode.

;; The concept for show-context-mode is based on Semantic's stickyfunc
;; mode.  Originally, this was just an attempt to do the same thing
;; without the five minute startup time, but has since grown into
;; something more conceptually general.

;;; To do:

;; C++ mode
;; * If the line gets too long, start removing types from function
;;   signatures
;; * Be smarter about what is chosen to scrunch.  Instead of just
;;   going to the top-level, look for interesting intermediate
;;   levels.  For instance, a method in a class should display both
;;   levels (or something fancier, like put class:: in the function
;;   name and just display the function level)
;; * If there's room, might as well scrunch more levels in there

;; LaTeX mode
;; * Scrunch section names

;;; Customization:

(defgroup show-context nil
  "Minor mode for displaying context in the header line")

(defcustom show-context-mode-context-prefix ""
  "String to prefix the header line with when displaying context."
  :group 'show-context)

(defcustom show-context-mode-top-level-prefix "  "
  "String to prefix the header line with when displaying the regular
line (ie, if there is no context)."
  :group 'show-context)

(defcustom show-context-mode-top-line-format "%b"
  "Header line format to display when already at the top of the
buffer (such as %b to display the buffer name)."
  :group 'show-context)

;;; Code:

(defvar show-context-mode-old-hlf nil)
(make-variable-buffer-local 'show-context-mode-old-hlf)

(defvar show-context-mode-getter nil)
(make-variable-buffer-local 'show-context-mode-getter)

(defun show-context-mode-get-line ()
  "Helper routine to get the buffer contents at the line containing
point.  This will probably go away in the wash when show-context-mode
implements more interesting parsing."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defun show-context-mode-compute-headerline ()
  "Compute the value of the header line."
  (save-excursion
    (goto-char (window-start))
    (if (= (point) (point-min))
        ;; The top is already at the top
        show-context-mode-top-line-format
      ;; Figure out what my context is
      (vertical-motion -1)
      (or (when show-context-mode-getter
            (let ((context (funcall show-context-mode-getter)))
              (when context
                (list show-context-mode-context-prefix
                      context))))
          (progn
            ;; XXX This doesn't deal with continuation lines correctly,
            ;; since it just grabs the whole line
            (list show-context-mode-top-level-prefix
                  (show-context-mode-get-line)))))))

(require 'easy-mmode)
(define-minor-mode show-context-mode
  "Minor mode that sets the header line to something that gives
meaningful context about whatever structure is currently going off the
top of the screen.  If there's no such structure, this emulates the
header line not being present by displaying the line that would be
there anyways."
  nil "" nil

  (if show-context-mode
      (progn
        ;; Figure out what getter to use (find the first parent of
        ;; major-mode with a show-context-mode-getter)
        (let ((mode major-mode)
              getter)
          (while (and (null getter) (not (null mode)))
            (setq getter (get mode 'show-context-mode-getter))
            (setq mode (get mode 'derived-mode-parent)))
          (setq show-context-mode-getter getter))
        ;; Setup the header line
        (kill-local-variable 'show-context-mode-old-hlf)
        (when (local-variable-p 'header-line-format)
          (setq show-context-mode-old-hlf header-line-format))
        (if show-context-mode-getter
            (setq header-line-format
                  '(:eval (show-context-mode-compute-headerline)))
          (setq header-line-format nil)
          (error "Show-context mode doesn't know about %s"
                 major-mode)))
    ;; Restore the header line
    (kill-local-variable 'header-line-format)
    (when (local-variable-p 'show-context-mode-old-hlf)
      (setq header-line-format show-context-mode-old-hlf))))

;;; Getters

(defun show-context-mode-c-scrunch (start end)
  "Scrunch together the code between start and end into a single,
succinct, newline-less line."
  ;; Copy the region into a temporary buffer for acrobatics.
  (let ((source (buffer-substring start end)))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (catch 'done-scrunching
        (while t
          (end-of-line)
          ;; Am I done scrunching?
          (unless (looking-at "\n")
            (throw 'done-scrunching nil))
          ;; Nuke any newlines and scrunch any whitespace down to one
          ;; space.
          (delete-region (point)
                         (progn (skip-chars-forward "\n")
                                (point)))
          (just-one-space)))
      (buffer-substring (point-min) (point-max)))))

(defun show-context-mode-c-get-context ()
  "In C code, return a string that represents the current context for
point.  Specifically, this returns the first line of the statement
that begins the top-level block containing point, though this might
change.  If no top-level block contains point, returns nil."

  (let ((parse-from-point
         ;; Point might be in the middle of the statement that
         ;; introduces the current block, in which case tracing braces
         ;; backwards from point is going to miss it.  Move point
         ;; forward a bit if this is the case.
         (or (save-excursion
               (c-end-of-statement)
               (c-forward-syntactic-ws)
               (if (looking-at "{")
                   (+ (point) 1)))
             (point)))
        (max-end-point
         ;; Since we don't want it displaying stuff that we can
         ;; actually see, compute the maximum end point for the
         ;; scrunch.
         (save-excursion
           (vertical-motion 1)
           (point))))
    (save-excursion
      (goto-char parse-from-point)
      (let ((state (c-parse-state))
            outermost prefix)
        ;; Find the outermost brace by finding the last non-pair entry
        ;; in state.  This should result in nil iff point is at the
        ;; top level.
        (dolist (brace state)
          (if (and (not (consp brace))
                   (save-excursion
                     (goto-char brace)
                     (looking-at "{")))
              (setq outermost brace)))
        (when outermost
          ;; parse-from-point is inside a top-level block.  Figure out
          ;; where the introductory statement begins.
          (goto-char outermost)
          (c-beginning-of-statement)
          (let ((start-point (point))
                (end-point
                 ;; The end point would be the brace (inclusive), but
                 ;; that might actually be visible, so bound it
                 (min (+ outermost 1) max-end-point)))
            ;; If I still have anything left, gather it up
            (when (< start-point end-point)
              (show-context-mode-c-scrunch start-point end-point))))))))

(put 'c++-mode 'show-context-mode-getter
     (function show-context-mode-c-get-context))

(provide 'show-context-mode)
