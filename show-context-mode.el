;;; show-context-mode.el --- display context in the header line

;; Copyright (C) 2005 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    18-Jul-2005
;; Version:    0.3

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

;; Currently show-context mode vaguely understands C/C++ code and
;; understands Python code quite well (both are about in proportion to
;; how screwed-up their syntax is).  It's designed to be easily
;; extensible to be able to compute context for any editing mode.

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
;; * Do some work to never traverse backwards into CPP stuff

;; LaTeX mode
;; * Scrunch section names
;; * Perhaps scrunch environments

;;; Customization:

(defgroup show-context nil
  "Minor mode for displaying context in the header line")

(defcustom show-context-mode-context-prefix nil
  "Header line prefix when displaying context

When there is context to be displayed in the header line, this will be
prepended to it.  This should be in the same format as
`header-line-format', except that it assigns special meaning to a
value of t.  In its simplest form, this can be just a prefix string.
The default value of nil left-aligns the context line.  A value of t
is equivalent to 'show-context-mode-left-margin, which will
automatically align the header line contents with the buffer
contents."
  :group 'show-context)

(defcustom show-context-mode-top-level-prefix t
  "Header line prefix when displaying a regular line

When there is no context to be displayed (ie, the line at the top of
the window is at the top-level of the file), this will be prepended to
the buffer text from the line at the top of the window and displayed
in the header line.  This should be in the same format as
`header-line-format', except that it assigns special meaning to a
value of t.  In its simplest form, this can just be a prefix string.
A value of t (the default value) is equivalent to
'show-context-mode-left-margin, which will automatically align the
header line contents with the buffer contents."
  :group 'show-context)

(defcustom show-context-mode-top-line-format "%b"
  "Header line format when at the top of the buffer

When the window is already at the top of the buffer, there's nothing
to reasonably display in the header line.  This string specifies the
header line format in this circumstance (see `header-line-format').
The default value of \"%b\" displays the buffer name."
  :type 'string
  :group 'show-context)

;;; Code:

(defvar show-context-mode-old-hlf nil
  "The old value of `header-line-format' to return to if
show-context-mode is disabled")
(make-variable-buffer-local 'show-context-mode-old-hlf)

(defvar show-context-mode-getter nil
  "The context getter function for the buffer")
(make-variable-buffer-local 'show-context-mode-getter)

(defvar show-context-mode-cache nil
  "The context cache.  nil if nothing is cached, otherwise a list of
the value of (window-start) and the context string for that window
start position.  When the window start position doesn't change,
there's no need to recompute the context.")
(make-variable-buffer-local 'show-context-mode-cache)

(defvar show-context-mode-left-margin nil
  "A string suitable for placement at the beginning of
header-line-format that should align it with the left column of the
current buffer.  This is computed by
`show-context-mode-get-left-edge-space'.")
(make-variable-buffer-local 'show-context-mode-left-margin)

(defun show-context-mode-get-visible-line ()
  "Helper routine to get the buffer contents of the visible line
containing point."

  (let ((here (point))
        (eol (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (catch 'done-whilling
        (while t
          (let ((last-beginning (point)))
            (vertical-motion 1)
            (when (or (> (point) here) (eobp))
              (throw 'done-whilling
                     (buffer-substring last-beginning
                                       (min (point) eol)))))
          (if (> (point) eol)
              (error
               "Bug in show-context-mode-get-visible-line")))))))

(defun show-context-mode-compute-headerline ()
  "Compute the value of the header line."

  (if (and show-context-mode-cache
           (= (car show-context-mode-cache) (window-start)))
      ;; The header line is cached
      (cadr show-context-mode-cache)
    (let* (;; Canonicalize the prefixes (t has special meaning)
           (context-prefix
            (if (eq show-context-mode-context-prefix t)
                'show-context-mode-left-margin
              show-context-mode-context-prefix))
           (top-level-prefix
            (if (eq show-context-mode-top-level-prefix t)
                'show-context-mode-left-margin
              show-context-mode-top-level-prefix))
           ;; Compute the header line
           (header-line
            (save-excursion
              (goto-char (window-start))
              (if (= (point) (point-min))
                  ;; The top is already at the top
                  show-context-mode-top-line-format
                ;; Figure out what my context is
                (or (when show-context-mode-getter
                      (let ((context
                             (condition-case err
                                 (save-excursion
                                   (funcall show-context-mode-getter))
                               (error
                                (format "Context error (%s)"
                                        (error-message-string err))))))
                        (when context
                          `((t ,context-prefix) ,context))))
                    (progn
                      ;; No context.  Fake the line that should be
                      ;; under the header-line
                      (vertical-motion -1)
                      (let ((line
                             (show-context-mode-get-visible-line)))
                        `((t ,top-level-prefix) ,line))))))))
      ;; Cache the result (XXX This can thrash with multiple windows
      ;; displaying the same buffer, but probably isn't noticeable
      ;; anyways)
      (setq show-context-mode-cache
            (list (window-start) header-line))
      header-line)))

(defun show-context-mode-get-left-edge-width ()
  "Computes the width of the left edge of the buffer as a factor of
the character width.  This includes, for example, the left fringe and
left vertical scroll bar.  On windowed displays, this can return
fractional numbers.  This is surprisingly hard to do, so if the
computation fails, this will return nil."

  ;; This is wonky
  (let ((x 0)
        (window (frame-selected-window))
        ;; Disable the header line so it doesn't get in the way
        header-line-format width)
    (catch 'done-scanning
      (while t
        ;; Convert frame coordinates to window region/coordinates
        (let ((coord (coordinates-in-window-p (cons x 0) window)))
          (cond ((null coord)
                 ;; Must have gone off the window.  Give up
                 (throw 'done-scanning nil))
                ((consp coord)
                 ;; Found the offset of a point in the buffer
                 (throw 'done-scanning (- x (car coord))))
                (t
                 ;; In a special region.  Move over another
                 (setq x (1+ x)))))))))

(defun show-context-mode-get-left-edge-space ()
  "Returns a string suitable for prefixing in the header line that
should align the rest of the header with the left column of the
current buffer.  This attempts to deal even with the bizarre
characteristics of windowed displays that can cause this string to be
of non-integral character width.  If this function can't figure out
what's going on, it returns nil."

  (let ((left-edge-width (show-context-mode-get-left-edge-width)))
    (if left-edge-width
        (if (= (truncate left-edge-width) left-edge-width)
            ;; Whole number of spaces.  This special case is
            ;; particularly useful for tty displays, where space
            ;; display properties are ignored.
            (make-string (truncate left-edge-width) ? )
          ;; Fractional spaces
          (propertize " " 'display `(space :width ,left-edge-width)))
      nil)))

(require 'easy-mmode)
(define-minor-mode show-context-mode
  "Minor mode that sets the header line to give meaningful context
about whatever structure is currently going off the top of the screen.
If there's no such structure, this emulates the header line not being
present by displaying the line that would be there anyways."
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
        ;; Initialize the getter
        (when show-context-mode-getter
          (funcall (get show-context-mode-getter
                        'show-context-mode-getter-init)))
        ;; Get the left margin space
        (setq show-context-mode-left-margin
              (show-context-mode-get-left-edge-space))
        ;; Setup the header line
        (kill-local-variable 'show-context-mode-old-hlf)
        (when (local-variable-p 'header-line-format)
          (setq show-context-mode-old-hlf header-line-format))
        (if show-context-mode-getter
            (setq header-line-format
                  '(:eval (show-context-mode-compute-headerline)))
          (setq header-line-format nil)
          (message "Show-context mode doesn't know about %s"
                   major-mode)))
    ;; Restore the header line
    (kill-local-variable 'header-line-format)
    (when (local-variable-p 'show-context-mode-old-hlf)
      (setq header-line-format show-context-mode-old-hlf))))

(defun show-context-mode-scrunch (start end &optional nuke-comments)
  "Scrunch together the code between start and end into a single,
succinct, newline-less string.  If nuke-comments is true, this will
attempt to use the buffer's comment syntax to also strip comments."

  ;; Copy the region into a temporary buffer for acrobatics
  (let ((source (buffer-substring start end))
        (syntax-table (syntax-table)))
    (with-temp-buffer
      (with-syntax-table syntax-table
        (insert source)
        (goto-char (point-min))
        ;; Nuke beginning whitespace
        (delete-horizontal-space)
        ;; Scrunch as long as there are newlines
        (catch 'done-scrunching
          (let ((parse-point (point-min))
                (parse-state nil)
                (parse-sexp-ignore-comments nil)
                (iterations 0))
            (while t
              (when (> iterations 50)
                ;; Bail, just in case
                (error "show-context-mode-scrunch is probably stuck"))
              (setq iterations (1+ iterations))
              ;; Scrunch one line
              (end-of-line)
              ;; Nuke comments
              (let ((nuked-comment
                     (when nuke-comments
                       (save-excursion
                         (let ((s (parse-partial-sexp parse-point
                                                      (point)
                                                      nil
                                                      nil
                                                      parse-state)))
                           ;; Am I in a comment?
                           (if (not (nth 4 s))
                               (progn
                                 ;; Not in a comment.  Record the
                                 ;; parse state so I don't have to
                                 ;; start over next time (don't do
                                 ;; this if we remove a comment or the
                                 ;; change will confuse the parser)
                                 (setq parse-point (point))
                                 (setq parse-state s)
                                 ;; No, we did not nuke a comment
                                 nil)
                             ;; Go to the beginning of the comment
                             (goto-char (nth 8 s))
                             ;; Nuke until close comment syntax
                             (delete-region (point)
                                            (progn
                                              (forward-comment 1)
                                              (point)))
                             ;; Yes, we did nuke a comment
                             t))))))
                ;; Am I done scrunching?
                (when (and (not (and nuke-comments nuked-comment))
                           (not (looking-at "\n")))
                  (throw 'done-scrunching nil))
                ;; Nuke any newlines and scrunch any whitespace down
                ;; to one space.
                (delete-region (point)
                               (progn (skip-chars-forward "\n")
                                      (point)))
                (just-one-space)))))
        ;; Nuke trailing whitespace
        (delete-trailing-whitespace)
        ;; Nuke beginning whitespace
        (goto-char (point-min))
        (delete-horizontal-space)
        ;; Gather up result
        (buffer-substring (point-min) (point-max))))))

;;; Parser:

(defvar show-context-mode-parser nil
  "The state of the show-context-mode parser for a given buffer.
This is a tuple of the form:

  (VALID CACHE OPEN-CHAR CLOSE-CHAR)

OPEN-CHAR and CLOSE-CHAR are the characters used to increase and
decrease the nesting level, respectively.

CACHE is a cached list of the locations of nesting markers and
the state of the Emacs parser at these locations.  Each entry is
of the form:

  (POS INCREASE-P DEPTH PARSER-STATE)

POS indicates the buffer position of the nesting marker.
INCREASE-P is t if this marker increases the nesting, nil
otherwise.  DEPTH is the depth of this marker, starting at 0 (the
open and close markers have the same depth).  DEPTH may be
negative if nesting is unbalanced.  PARSER-STATE is the state of
the Emacs syntax table parser at this position.

The cache must be sorted in decreasing order of position.

VALID indicates the largest buffer position to which the cache
can be considered valid.  Any entries with a position greater
than or equal to this should be considered stale.")
(make-variable-buffer-local 'show-context-mode-parser)

(defun show-context-mode-init-parser (open-char close-char)
  "Initialize the show-context-mode parser for this buffer, using
open-char and close-char as the characters that increase and
decrease the nesting level, respectively.

The show-context-mode parser is capable of efficiently
determining the nesting level of any point in the buffer, as well
as traversing backwards by nesting.  It also provides functions
to efficiently search the buffer while ignoring text in
comments."

  (setq show-context-mode-parser
        (list 0 '() open-char close-char))
  (add-to-list 'after-change-functions #'show-context-mode-parser-update))

(defun show-context-mode-parser-update (beg end len)
  "The after-change-function used to keep the show-context-mode
parser's cache up to date."

  (when (and show-context-mode-parser
             (< beg (car show-context-mode-parser)))
    (setcar show-context-mode-parser beg)))

(defun show-context-mode-parser-parse (to)
  "Parse up to the given point.  Returns a list of tuples in the
format described for the cache in `show-context-mode-parser',
which are guaranteed to be only leading up to the given point."

  (unless show-context-mode-parser
    (error "show-context-mode: No active parser"))
  (let ((valid      (first show-context-mode-parser))
        (cache      (second show-context-mode-parser))
        (open-char  (third show-context-mode-parser))
        (close-char (fourth show-context-mode-parser)))
    ;; Drop invalid entries from the cache
    (while (and cache (>= (first (car cache)) valid))
      (setq cache (cdr cache)))
    ;; Parse from the last valid cache entry
    (save-excursion
      (save-restriction
        (widen)
        (let ((pos    (if cache (first (car cache)) (point-min)))
              (depth  (if cache
                          (- (third (car cache))
                             ;; If the last entry was a close,
                             ;; subtract one from the current depth
                             (if (second (car cache)) 0 1))
                        -1))
              (pstate (if cache (fourth (car cache)) nil))
              (skip   (string ?^ open-char close-char)))
          ;; Go to the open marker.  We have to go to one past because
          ;; the loop expects this (otherwise skip-chars-forward stays
          ;; put)
          (goto-char (+ pos 1))
          ;; Parse forward to the destination
          (while (and (not (eobp))
                      (< (point) to))
            ;; Find the nest nesting marker
            (skip-chars-forward skip)
            (let ((increase-p (eql (char-after (point)) open-char))
                  (decrease-p (eql (char-after (point)) close-char)))
              (when (or increase-p decrease-p)
                ;; Bring the syntax table parser up to date
                (setq pstate (parse-partial-sexp pos (point)
                                                 nil nil pstate)
                      pos (point))
                ;; Am I not inside a comment or string?
                (when (not (or (fourth pstate)
                               (fifth pstate)))
                  ;; Found a nesting marking
                  (when increase-p
                    (setq depth (+ depth 1)))
                  (setq cache (cons (list (point) increase-p depth pstate) cache))
                  (when decrease-p
                    (setq depth (- depth 1))))))
            ;; Move to the next character and continue
            (unless (eobp)
              (forward-char))))
        ;; Update the stored parser state
        (setcar show-context-mode-parser (point))
        (setcar (cdr show-context-mode-parser) cache)))
    ;; Find and return the tail of the cache that applies to `to'
    (while (and cache (> (first (car cache)) to))
      (setq cache (cdr cache)))
    cache))

(defun show-context-mode-parser-current-level ()
  "Get the nesting level at point.  Returns -1 if point is not
within any nesting markers, 0 if point is within one pair, etc."

  (let ((cache (show-context-mode-parser-parse (point))))
    (cond ((null cache)
           ;; We're not inside any markers
           -1)
          ((second (car cache))
           ;; We're at the indentation level of the preceding open
           ;; marker
           (third (car cache)))
          (t
           ;; We're one level above the preceding close marker
           (- (third (car cache)) 1)))))

(defun show-context-mode-parser-up (level)
  "Move point to the enclosing open marker for `level'.  If there
is no marker pair enclosing point at that level, does nothing and
returns nil."

  (let ((cache (show-context-mode-parser-parse (point))))
    ;; Are we at least at the goal level currently?
    (if (>= (show-context-mode-parser-current-level) level)
        (progn
          ;; Traverse the cache until we find the open of the
          ;; requested level.  Thanks to proper nesting, we don't have
          ;; to be more careful.  We also know that we won't find a
          ;; close of this level before the open.
          (while (and cache (> (third (car cache)) level))
            (setq cache (cdr cache)))
          ;; Go!
          (unless cache
            (error "show-context-mode-parser-up: BUG Couldn't find open"))
          (goto-char (first (car cache)))
          t)
      nil)))

(defun show-context-mode-skip-chars-forward (skip)
  "Work-a-like for `skip-chars-forward', but comments will be
skipped over."

  (let* ((cache (show-context-mode-parser-parse (point)))
         (start (point))
         (pos (if cache (first (car cache)) (point-min)))
         (pstate (if cache (fourth (car cache)) nil)))
    (catch 'done
      (skip-chars-forward skip)
      (while (not (eobp))
        ;; Update the parser
        (setq pstate (parse-partial-sexp pos (point) nil nil pstate)
              pos (point))
        ;; Are we in a comment?
        (if (fifth pstate)
            (progn
              ;; Skip over the comment
              (goto-char (ninth pstate))
              (forward-comment 1)
              (when (< (point) pos)
                (error "BUG: Failed to skip over comment"))
              ;; Update the parser
              (setq pstate (parse-partial-sexp pos (point) nil nil pstate)
                    pos (point)))
          ;; Found it
          (throw 'done nil))
        ;; Try again
        (skip-chars-forward skip)))
    ;; Return
    (- (point) start)))

(defun show-context-mode-skip-chars-backward (skip)
  "Work-a-like for `skip-chars-backward', but comments will be
skipped over."

  ;; Work forward from each parse state captured in the cache
  (let ((cache (show-context-mode-parser-parse (point)))
        (start (point))
        (limit (point))
        (point-min (point-min))
        ;; Record the furthest satisfactory point
        (furthest nil))
    (save-restriction
      (while (null furthest)
        (let ((pos (if cache (first (car cache)) point-min))
              (pstate (if cache (fourth (car cache)) nil)))
          ;; Eat this element from the cache.  When the cache is
          ;; empty, we perform one final iteration from the very
          ;; beginning of the buffer.
          (if cache
              (setq cache (cdr cache))
            (setq furthest 'failed))
          (when (fifth pstate)
            (error "BUG: Nesting marker parse state %s is in a comment"
                   (car cache)))
          ;; Only consider the region covered by this cache entry
          (narrow-to-region pos limit)
          (setq limit pos)
          ;; Start at the beginning of this entry and work forward
          (goto-char pos)
          (skip-chars-forward skip)
          (while (not (eobp))
            ;; Update the parser
            (setq pstate (parse-partial-sexp pos (point) nil nil pstate)
                  pos (point))
            ;; Are we in a comment?
            (if (fifth pstate)
                (progn
                  ;; Skip over the comment
                  (goto-char (ninth pstate))
                  (forward-comment 1)
                  (when (< (point) pos)
                    (error "BUG: Failed to skip over comment"))
                  ;; Update the parser
                  (setq pstate
                        (parse-partial-sexp pos (point) nil nil pstate)
                        pos (point)))
              ;; We found a satisfactory point
              (setq furthest (point))
              (forward-char))
            ;; Push the frontier
            (skip-chars-forward skip)))))
    ;; Go to what we found
    (if (eq furthest 'failed)
        (goto-char (point-min))
      (goto-char (+ furthest 1)))
    ;; Return
    (- (point) start)))

;;; Getters:

;; cc-mode

(defgroup show-context-c nil
  "Options affecting the computation of context in cc-mode buffers"
  :group 'show-context)

(defcustom show-context-mode-c-finagle-level 'strip-comments
  "How hard to finagle C context into a more compact form.

If none, show-context-mode will simply gather up enclosing context
lines.  If strip-comments, show-context-mode will gather enclosing
context but remove comments."
  :type '(radio (const none)
                (const strip-comments))
  :group 'show-context-c)

(defun show-context-mode-c-get-context ()
  "In C code, return a string that represents the current context for
point.  Specifically, this returns the first line of the statement
that begins the top-level block containing point, though this might
change.  If no top-level block contains point, returns nil."

  (catch 'done
    (let ((limit (point))
          (strip-comments (eq show-context-mode-c-finagle-level
                              'strip-comments)))
      ;; Find the outermost brace
      (unless (show-context-mode-parser-up 0)
        ;; Figure out if I'm in the beginning of a function
        (show-context-mode-skip-chars-forward "^{;")
        (unless (eql (char-after (point)) ?{)
          ;; Nope, return nil
          (throw 'done nil)))
      ;; Include the curly brace
      (when (eql (char-after (point)) ?{)
        (forward-char))
      ;; Scootch the limit back
      (setq limit (min (point) limit))
      ;; Find the beginning of this expression
      (show-context-mode-skip-chars-backward "^};")
      ;; Gather up
      (when (< (point) limit)
        (let ((text (show-context-mode-scrunch (point) limit
                                               strip-comments)))
          (if (string= text "")
              nil
            text))))))

(put 'c-mode 'show-context-mode-getter
     #'show-context-mode-c-get-context)
;; c++-mode is not a derived mode
(put 'c++-mode 'show-context-mode-getter
     #'show-context-mode-c-get-context)

(defun show-context-mode-c-init ()
  (show-context-mode-init-parser ?{ ?}))

(put 'show-context-mode-c-get-context 'show-context-mode-getter-init
     #'show-context-mode-c-init)

;; java-mode

;; This has some odd behavior in certain circumstances.  For example,
;; only up until the syntactic whitespace preceding the first method
;; declaration, it will return the class declaration.

(defun show-context-mode-java-get-context ()
  (catch 'done
    (let ((limit (point))
          (strip-comments (eq show-context-mode-c-finagle-level
                              'strip-comments)))
      ;; Find the beginning of the method or class
      (unless (show-context-mode-parser-up 1)
        ;; Is point in a method declaration?
        (show-context-mode-skip-chars-forward "^{;")
        (unless (eql (char-after (point)) ?{)
          ;; Nope.  Get the class declaration
          (unless (show-context-mode-parser-up 0)
            ;; Is point in the class declaration?
            (show-context-mode-skip-chars-forward "^{;")
            (unless (eql (char-after (point)) ?{)
              ;; Nope, return nil
              (throw 'done nil)))))
      ;; Scootch the limit back
      (setq limit (min (point) limit))
      ;; Include the curly brace
      (when (eql (char-after limit) ?{)
        (setq limit (+ 1 limit)))
      ;; Find the beginning of this expression
      (show-context-mode-skip-chars-backward "^{};")
      ;; Gather up
      (when (< (point) limit)
        (let ((text (show-context-mode-scrunch (point) limit
                                               strip-comments)))
          (if (string= text "")
              nil
            text))))))

(put 'java-mode 'show-context-mode-getter
     #'show-context-mode-java-get-context)

(put 'show-context-mode-java-get-context 'show-context-mode-getter-init
     #'show-context-mode-c-init)

;; python-mode

(defgroup show-context-python nil
  "Options affecting the computation of context in python-mode buffers"
  :group 'show-context)

(defcustom show-context-mode-python-finagle-level 'names-and-args
  "How hard to finagle pythonic context into a more compact form.

If none, show-context-mode will simply find enclosing def and class
lines and scrunch these into the context line.  If strip-comments,
show-context-mode will scrunch together def and class lines, but
remove comments.  If names-and-args, show-context-mode will only
display the names of defs and classes and the argument lists of defs."
  :type '(radio (const none)
                (const strip-comments)
                (const names-and-args))
  :group 'show-context-python)

(defun show-context-mode-python-get-context ()
  "In Python code, returns a string that represents the current
context for point.  What's included in this string depends on the
value of `show-context-mode-python-finagle-level', but it will be
somehow related to the set of enclosing classes and defs.  If there
are no enclosing classes or defs, returns nil."

  (let ((strip-comments (memq show-context-mode-python-finagle-level
                              '(strip-comments names-and-args)))
        (max-end-point
         ;; Don't display anything the user can already see
         (point)))
    ;; The following sequence is a bit counter-intuitive.  You could
    ;; just py-goto-statement-at-or-above, but if point is past the
    ;; last statement in a block, you're not really "in that block"
    ;; any more.  So, really, you want to start at the indentation
    ;; level of the statement following the previous line, but,
    ;; obviously, not that statement itself.
    (forward-line -1)
    (if (py-goto-statement-below)
        (if (not (zerop (current-indentation)))
            (py-goto-block-up t))
      ;; We're already past the last statement in the buffer.  Skip
      ;; forward again to simulate going to the next statement.
      ;; Ultimately, this will lead to returning nil
      (forward-line 1))
    ;; Make sure the above dance actually moved me up.  If it moved me
    ;; down, then there's no point in showing the user context they
    ;; already have, so don't display any context.
    (when (<= (point) max-end-point)
      ;; Accumulate def's and class's
      (let ((context-re "^[ \t]*\\(class\\|def\\)\\>")
            accum)
        (catch 'done-accumulating
          (while t
            (beginning-of-line)
            (when (looking-at context-re)
              ;; Suck this statement into accum
              (let ((context (show-context-mode-scrunch
                              (point)
                              (save-excursion
                                (py-goto-beyond-final-line)
                                (min max-end-point (point)))
                              strip-comments)))
                ;; context may be empty if we hit max-end-point
                (if (not (zerop (length context)))
                    (setq accum
                          (cons (cons (point) context)
                                accum)))))
            ;; Have I hit top-level?
            (if (zerop (current-indentation))
                (throw 'done-accumulating accum)
              (py-goto-block-up t))))
        ;; Turn accum into a real, live context line
        (cond ((null accum)
               ;; This can happen when we started out entirely nested in
               ;; blocks that weren't of interest
               nil)
              ((and (= (length accum) 1)
                    (>= (caar accum)
                        (save-excursion
                          (goto-char max-end-point)
                          (vertical-motion -1)
                          (point))))
               ;; Special case when the only context line is where the
               ;; header line is
               nil)
              ((eq show-context-mode-python-finagle-level
                   'names-and-args)
               ;; Finagle just names and arguments out of accum
               (let* ((id-re "[a-zA-Z_]+[a-zA-Z0-9_]*")
                      (ws-re "[ \t]+")
                      (class-re (concat "^class" ws-re
                                        ;; Get the class name
                                        "\\(" id-re "\\)"))
                      (def-re (concat "^def" ws-re
                                      ;; Get the function name and
                                      ;; argument list
                                      "\\(" id-re "([^)]*)?\\)"))
                      (context-line
                       (mapconcat
                        (lambda (elt)
                          (cond ((string-match class-re elt)
                                 (concat (match-string 1 elt) "."))
                                ((string-match def-re elt)
                                 (concat (match-string 1 elt) " "))
                                (t
                                 ;; Syntax error?
                                 (concat elt " "))))
                        (mapcar #'cdr accum)
                        "")))
                 ;; This is a little bogus.  Strip off the last
                 ;; character, since it's supposed to be a separator
                 (substring context-line 0 -1)))
              (t
               ;; Just join it with spaces and call it good
               (mapconcat #'cdr accum " ")))))))

(put 'python-mode 'show-context-mode-getter
     #'show-context-mode-python-get-context)

(provide 'show-context-mode)
