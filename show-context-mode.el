;;; show-context-mode.el --- Display context in the header line for
;;; functions, classes, and methods in C/C++, Java, and Python

;; Copyright (C) 2005-2008 Austin Clements

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

;; Computer screens are small.  Even big ones are too small.
;; Unfortunately, in many languages, the meaning of the current line
;; of code depends heavily on its context, which may be influenced by
;; lines that occur an unbounded distance above the top of the window.
;; Show-context mode is meant to help with this by scrunching together
;; pieces of the buffer that appear above the top of the window into
;; one header line that summarizes everything you need to know, but
;; otherwise can't see, to understand the context of the visible
;; buffer.

;; Currently show-context mode vaguely understands C/C++ code and
;; understands Java and Python code quite well.  It's designed to be
;; easily extensible to be able to compute context for any editing
;; mode.

;; The concept for show-context-mode is based on Semantic's stickyfunc
;; mode.  Originally, this was just an attempt to do the same thing
;; without the five minute startup time, but has since grown into
;; something more conceptually general.

;; To install, add the following to your .emacs:
;;   (autoload 'show-context-mode "show-context-mode" nil t)
;;   (add-hook 'c-mode-hook 'show-context-mode)
;;   (add-hook 'c++-mode-hook 'show-context-mode)
;;   (add-hook 'java-mode-hook 'show-context-mode)
;;   (add-hook 'python-mode-hook 'show-context-mode)

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
                          `((t ,context-prefix)
                            ,(replace-regexp-in-string "%" "%%"
                                                       context t t)))))
                    (progn
                      ;; No context.  Fake the line that should be
                      ;; under the header-line
                      (vertical-motion -1)
                      (let ((line
                             (show-context-mode-get-visible-line)))
                        `((t ,top-level-prefix)
                          ,(replace-regexp-in-string "%" "%%" line t t)))))))))
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

;; XXX Really this ought to work with the comments in the buffer and
;; then pull it out to scrunch whitespace.  Then we can use it for
;; stripping comments off CPP directives and anything else that isn't
;; guaranteed to be well-balanced with respect to comments.  Probably
;; introduce a show-context-mode-buffer-substring-no-comments.  Use the
;; parser cache and deal with comments that do not include eol.
(defun show-context-mode-scrunch (start end &optional nuke-comments)
  "Scrunch together the code between start and end into a single,
succinct, newline-less string using
`show-context-mode-scrunch-string'."

  (show-context-mode-scrunch-string
   (buffer-substring start end)
   nuke-comments))

(defun show-context-mode-scrunch-string (source &optional nuke-comments)
  "Scrunch together the code in source into a single, succinct,
newline-less string.  If nuke-comments is true, this will attempt
to use the buffer's comment syntax to also strip comments."

  ;; Copy the region into a temporary buffer for acrobatics
  (let ((syntax-table (syntax-table)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;

(defvar show-context-mode-parser nil
  "The state of the show-context-mode parser for a given buffer.
This is a tuple of the form:

  (VALID CACHE TOKEN-RE)

TOKEN-RE is a regular expression where group 1 must match text
that increases the nesting level, group 2 must match text that
decreases the nesting level, and group 3 must match the entirety
of any additional tokens of interest.  Group 3 may contain
subgroups.

CACHE is a cached list of the locations of nesting markers and
the state of the Emacs parser at these locations.  Each entry is
of the form:

  (POS DELTA DEPTH PARSER-STATE MATCH-DATA)

POS indicates the buffer position of the beginning of the matched
token.  DELTA is 1 if this marker increases the nesting, -1 if it
decreases the nesting, and 0 otherwise.  DEPTH is the depth of
this marker, starting at 0 (the open and close markers have the
same depth).  DEPTH may be negative if nesting is unbalanced.
PARSER-STATE is the state of the Emacs syntax table parser at
POS.  MATCH-DATA is the match data of groups 3 and above from
matching this text.

The cache must be sorted in decreasing order of position.

VALID indicates the largest buffer position to which the cache
can be considered valid.  Any entries with a position greater
than or equal to this should be considered stale.")
(make-variable-buffer-local 'show-context-mode-parser)

(defun show-context-mode-init-parser (open-str close-str &optional other-re)
  "Initialize the show-context-mode parser for this buffer, using
open-str and close-str as the text that increases or decreases
the nesting level, respectively.  other-re is an optional regular
expression to match other tokens of interest.

The show-context-mode parser is capable of efficiently
determining the nesting level of any point in the buffer, as well
as traversing backwards by nesting.  It also provides functions
to efficiently search the buffer while ignoring text in
comments."

  (let ((token-re (concat "\\(" (regexp-quote open-str)
                          "\\)\\|\\(" (regexp-quote close-str)
                          "\\)" (when other-re
                                  (concat "\\|\\(" other-re "\\)")))))
    (setq show-context-mode-parser
          (list 0 '() token-re)))
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
        (token-re   (third show-context-mode-parser)))
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
                             (if (= (second (car cache)) -1) 1 0))
                        -1))
              (pstate (if cache (fourth (car cache)) nil)))
          ;; Go to the cached position.  We have to start one past so
          ;; our search doesn't just stay in place.
          (goto-char (+ pos 1))
          ;; Parse forward to the destination
          (while (and (not (eobp))
                      (< (point) to))
            ;; Find the next token
            (when (re-search-forward token-re to 'limit)
              (goto-char (match-beginning 0))
              ;; Bring the syntax table parser up to date
              (setq pstate (parse-partial-sexp pos (point)
                                               nil nil pstate)
                    pos (point))
              ;; Am I not inside a comment or string?
              (when (not (or (fourth pstate)
                             (fifth pstate)))
                ;; What did I find?
                (let ((delta (cond
                              ((match-string 1) 1)
                              ((match-string 2) -1)
                              (t                0)))
                      (match-data
                       (let ((partial (nthcdr 6 (match-data 1))))
                         (if (null (cdr partial))
                             ;; We removed all of the groups, just not
                             ;; the buffer they came from.
                             nil
                           partial))))
                  (when (= delta 1)
                    (setq depth (+ depth 1)))
                  (setq cache (cons (list pos delta depth pstate match-data)
                                    cache))
                  (when (= delta -1)
                    (setq depth (- depth 1)))))
              ;; Move forward over this token
              (goto-char (match-end 0))))
          ;; Update the stored parser state.  Note that we can only
          ;; report validity as far as `pos' because `to' might have
          ;; been in the middle of a token, so we have to resume
          ;; parsing at `pos'.
          ;;
          ;; XXX Is this actually true?  It would be better for
          ;; performance if we can claim all the way to `to' is valid.
          ;; We'll restart our state from the last entry anyways.
          ;;
          ;; XXX Short-circuit everything if (<= to valid) or at least
          ;; don't set valid backwards to the highest cache entry if
          ;; we didn't do anything.
          (setcar show-context-mode-parser pos)
          (setcar (cdr show-context-mode-parser) cache))))
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
          ((= (second (car cache)) -1)
           ;; We're one level above the preceding close marker
           (- (third (car cache)) 1))
          (t
           ;; We're at the indentation level of the preceding open
           ;; marker
           (third (car cache))))))

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
          (while (and cache (or (> (third (car cache)) level)
                                (/= (second (car cache)) 1)))
            (setq cache (cdr cache)))
          ;; Go!
          (unless cache
            (error "show-context-mode-parser-up: BUG Couldn't find open"))
          (goto-char (first (car cache)))
          t)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser-based utilities
;;;

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
        ;; Are we in a string or comment?
        (if (or (fourth pstate) (fifth pstate))
            (progn
              ;; Skip over the comment
              (goto-char (ninth pstate))
              (if (fourth pstate)
                  ;; Skip string
                  (forward-sexp)
                ;; Skip comment
                (forward-comment 1))
              (when (< (point) pos)
                (error "BUG: Failed to skip over string/comment"))
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
            ;; Are we in a string or comment?
            (if (or (fourth pstate) (fifth pstate))
                (progn
                  ;; Skip over the comment
                  (goto-char (ninth pstate))
                  (if (fourth pstate)
                      ;; Skip string
                      (forward-sexp)
                    ;; Skip comment
                    (forward-comment 1))
                  (when (< (point) pos)
                    (error "BUG: Failed to skip over string/comment"))
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

(defun show-context-mode-forward-balanced (open close &optional limit)
  "Skip over a balanced expression of open and close characters.
The character under point must be the open character.  If
successful, place point on the character after the closing
delimiter and return t.  Otherwise, return nil."

  (unless (eql (char-after (point)) open)
    (error "Character at point is not open character"))
  (save-restriction
    (when limit
        (narrow-to-region (point) limit))
    (catch 'done
      (let ((level 0)
            (skip-spec (string ?^ open close)))
        (while (and (>= level 0) (not (eobp)))
          (forward-char 1)
          (show-context-mode-skip-chars-forward skip-spec)
          (let ((chr (char-after (point))))
            (cond ((eql chr open)
                   (setq level (+ level 1)))
                  ((eql chr close)
                   (setq level (- level 1)))
                  (t
                   (throw 'done nil)))))
        (forward-char 1)
        (= level -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer-attached substrings
;;;

;; XXX These could be implemented much more efficiently

(defun show-context-mode-buffer-substring (start end)
  (let* ((str (buffer-substring start end))
         (pos 0) (len (length str)))
    (while (< pos len)
      (put-text-property pos (+ 1 pos)
                         's-c-m-source (+ start pos)
                         str)
      (setq pos (+ 1 pos)))
    str))

(defun show-context-mode-string-up-to (string limit)
  (let* ((pos 0) (len (length string)) buf-pos (end len))
    (while (and (< pos len) (= end len))
      (let ((source (get-text-property pos 's-c-m-source string)))
        (if source
            (setq buf-pos source)
          (if buf-pos
              (setq buf-pos (+ 1 buf-pos)))))
      (when (and buf-pos (>= buf-pos limit))
        ;; Found the limit.  Trim
        (setq end pos))
      (setq pos (+ 1 pos)))
    (substring string 0 end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getters
;;;

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

(defcustom show-context-mode-c-include-cpp-directives 'always
  "When to include #if directives in the context.

If always, show-context-mode will always summarize and display
enclosing #if directives.  If functions, show-context-mode will
only display enclosing #if directives if it is also summarizing a
function definition that is off the screen.  If nil,
show-context-mode will never display #if directives."
  :type '(radio (const always)
                (const functions)
                (const nil))
  :group 'show-context-c)

(defcustom show-context-mode-cpp-finagle-level 'reduce-elses
  "How hard to finagle CPP context into a more compact form.

If nil, show-context-mode will include all of the enclosing #if
directives, unmodified.  If reduce-elses, then a #if, #ifdef, or
#ifndef followed by a #else will be collapsed into just the #if,
but with the condition inverted."
  :type '(radio (const reduce-elses)
                (const nil))
  :group 'show-context-c)

(defun show-context-mode-c-get-context ()
  "In C code, return a string summarizing the function enclosing
point and, optionally, any CPP directives enclosing point."

  (let* ((orig-point (point))
         (function-context
          (show-context-mode-c-get-function-context))
         (include show-context-mode-c-include-cpp-directives))
    (cond
     ((not include) function-context)
     ((or (and (eq include 'functions) function-context)
          (not (eq include 'functions)))
      (when (not function-context)
        (goto-char orig-point))
      ;; XXX What about directives between the end of the function
      ;; declaration and orig-point?  Or, for that matter, directives
      ;; embedded in the function declaration (that's probably not
      ;; worth worrying about)
      (let ((cpp-context (show-context-mode-c-get-cpp-context)))
        (when (or cpp-context function-context)
          (concat cpp-context (when cpp-context " ")
                  function-context)))))))

(defun show-context-mode-c-get-function-context ()
  "In C code, return a string that represents the current context for
point.  Specifically, this summarizes the text leading up to the
top-level block containing point from the last declaration or CPP
directive preceding that and leaves point at the beginning of
that text.  If no top-level block contains point, returns nil."

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
      (show-context-mode-skip-chars-backward "^};#")
      ;; If we hit CPP goop, find the end of it.  XXX We probably
      ;; don't want to ignore all CPP goop, but if we don't, this has
      ;; to interact carefully with the CPP context.
      (when (eql (char-before (point)) ?#)
        (end-of-line)
        (while (eql (char-before (point)) ?\\)
          (forward-char 1)
          (end-of-line)))
      ;; Gather up
      (when (< (point) limit)
        (let ((text (show-context-mode-scrunch (point) limit
                                               strip-comments)))
          (if (string= text "")
              nil
            text))))))

(defun show-context-mode-c-get-cpp-context ()
  "Return a string summarizing the CPP directives enclosing
point.  If there are no enclosing CPP directives, return nil."

  (let ((parse (show-context-mode-parser-parse (point)))
        ;; (string symbol this-depth-offset depth-delta watermark-delta)
        (syms '(("if"     'if     -1 -2 -3)
                ("ifdef"  'ifdef  -1 -2 -3)
                ("ifndef" 'ifndef -1 -2 -3)
                ("else"   'else   -1  0  0)
                ("elif"   'elif   -1  0  0)
                ("endif"  nil      1  2  nil)))
        (enclosing nil))
    ;; Find just the directives, compute their depths, and keep just
    ;; the enclosing ones
    (let ((depth 0)
          (watermark 0))
      (dolist (entry parse)
        (when (fifth entry)
          (set-match-data (fifth entry))
          (let* ((op (match-string 2))
                 (sym (assoc op syms)))
            (when sym
              (when (and (second sym)
                         (<= (+ depth (third sym)) watermark))
                (push (list (match-string 2) (match-string 1) (match-string 3))
                      enclosing)
                (setq watermark (+ depth (fifth sym))))
              (setq depth (+ depth (fourth sym))))))))
    (when enclosing
      (when (eq show-context-mode-cpp-finagle-level 'reduce-elses)
        ;; Simplify #else's.  XXX Can we do anything about #elif's?
        (let ((ifs (nreverse enclosing)))
          (setq enclosing nil)
          (flet ((match2 (a b)
                  (and (equal (first (car ifs))  a)
                       (equal (first (cadr ifs)) b)))
                 (push-replaced (entry op expr)
                  (let* ((op-props (text-properties-at 0 (first entry)))
                         (pretty-op (if op
                                        (apply #'propertize op op-props)
                                      (first entry)))
                         (hash (second entry))
                         (expr-props (text-properties-at 0 (third entry)))
                         (pretty-expr (if expr
                                          (apply #'propertize expr expr-props)
                                        (third entry))))
                    (push (list pretty-op hash pretty-expr) enclosing))))
            (while ifs
              (cond ((match2 "else" "ifdef")
                     (push-replaced (cadr ifs) "ifndef" nil)
                     (setq ifs (cddr ifs)))
                    ((match2 "else" "ifndef")
                     (push-replaced (cadr ifs) "ifdef" nil)
                     (setq ifs (cddr ifs)))
                    ((match2 "else" "if")
                     (let ((new-expr (concat "!(" (third (cadr ifs)) ")")))
                       (push-replaced (cadr ifs) nil new-expr))
                     (setq ifs (cddr ifs)))
                    (t
                     (push (car ifs) enclosing)
                     (setq ifs (cdr ifs))))))))
      ;; Put all the strings together
      (mapconcat
       (lambda (entry)
         (concat (second entry) (first entry)
                 (when (> (length (third entry)) 0) " ")
                 (third entry)))
       enclosing " "))))

(put 'c-mode 'show-context-mode-getter
     #'show-context-mode-c-get-context)
;; c++-mode is not a derived mode
(put 'c++-mode 'show-context-mode-getter
     #'show-context-mode-c-get-context)

(defun show-context-mode-c-init ()
  (show-context-mode-init-parser
   "{" "}"
   ;; Record #if directives
   ;; XXX Doesn't deal with comments after CPP directives, or
   ;; continuation lines
   "^[ \t]*\\(#\\)[ \t]*\\(if\\(?:n?def\\)?\\|else\\|elif\\|endif\\)\\b[ \t]*\\(.*\\)$"))

(put 'show-context-mode-c-get-context 'show-context-mode-getter-init
     #'show-context-mode-c-init)

;; java-mode

(defgroup show-context-c nil
  "Options affecting the computation of context in cc-mode buffers"
  :group 'show-context)

(defcustom show-context-mode-java-finagle-level 'simplify-all
  "How hard to finagle Java context into a more compact form.

If none, simply show the enclosing method or class.  If
strip-comments, show the enclosing method or class, but remove
comments.  If simplify-once, show a simplified form of just the
immediately enclosing method or class (with comments stripped).
If simplify-all, show a simplified form of the enclosing method
or class that also indicates all of the further enclosing
classes."
  :type '(radio (const none)
                (const strip-comments)
                (const simplify-once)
                (const simplify-all))
  :group 'show-context-c)

(defconst show-context-mode-java-modifiers
  (regexp-opt '(;; Classes and methods
                "public" "protected" "private"
                "abstract" "static" "final" "strictfp"
                ;; Methods only
                "synchronized" "native") 'words)
  "The set of keywords that can appear before the return type of
a method declaration.")

(defconst show-context-mode-java-block-keywords
  (regexp-opt '("if" "else" "switch" "while" "do" "for"
                "try" "catch" "finally"))
  "The set of keywords that introduce a statement that may be
followed by a block.  This is used to disambiguate package
private methods from such statements, since the only obvious
syntactic difference is that the \"name\" of the method will be a
Java keyword.

Note that this should _not_ include the synchronized keyword, as
that can appear in a method signature.  This particular case is
handled specially.")

(defun show-context-mode-java-get-id ()
  ;; XXX Unicode letters and digits
  (when (looking-at "[A-Za-z_$][A-Za-z_$0-9]*")
    (prog1
        (show-context-mode-buffer-substring (match-beginning 0)
                                            (match-end 0))
      (goto-char (match-end 0)))))

(defun show-context-mode-java-read-method-or-class ()
  (save-restriction
    (narrow-to-region (point-min) (point))
    ;; Figure out if this is a method, class, or something else
    (let ((found 'working) first-char)
      ;; Find the beginning
      (show-context-mode-skip-chars-backward "^{};")
      ;; Skip over the fluff that comes at the beginning of a class or
      ;; method declaration.  If it's a class declaration, it will
      ;; become obvious in this process.  If it's a method
      ;; declaration, we'll get as far as the return type and set
      ;; found to 'maybe-method.
      (while (eq found 'working)
        (while (forward-comment 1))
        (unless first-char
          (setq first-char (point))
          ;; Check if it's simply a statement that introduces a block
          (if (looking-at show-context-mode-java-block-keywords)
              (setq found nil)))
        (cond ((looking-at "class\\>")
               ;; Found a class
               (let ((cl (show-context-mode-buffer-substring
                          (match-beginning 0) (match-end 0))))
                 (goto-char (match-end 0))
                 (while (forward-comment 1))
                 ;; Get the class name
                 (let ((id (show-context-mode-java-get-id)))
                   (if id
                       (setq found
                             (list 'class first-char (point-max)
                                   (concat cl " ") id
                                   ;; extends, etc
                                   (show-context-mode-buffer-substring
                                    (point) (point-max))))
                     (setq found nil)))))
              ((looking-at show-context-mode-java-modifiers)
               ;; Modifier (public, private, etc)
               (goto-char (match-end 0)))
              ((eql (char-after (point)) ?@)
               ;; Annotation
               (forward-char 1)
               (if (not (show-context-mode-java-get-id))
                   (setq found nil)
                 (while (forward-comment 1))
                 (if (looking-at "(")
                     (forward-sexp))))
              ((eql (char-after (point)) ?<)
               ;; Type parameters
               (unless (show-context-mode-forward-balanced ?< ?>)
                 (setq found nil)))
              ((eql (char-after (point)) ?\[)
               ;; Array type
               (unless (show-context-mode-forward-balanced ?\[ ?\])
                 (setq found nil)))
              ((eql (char-after (point)) ?\()
               ;; We never found a method or class name.  This
               ;; happens, for example, when we're looking at a
               ;; synchronized block.  We can't ignore those
               ;; out-right, because it could be a synchronized
               ;; modifier on a method, but we'll skip over it and
               ;; then immediately hit the paren on the condition.
               (setq found nil))
              (t
               (if (eq found 'working)
                   (setq found 'maybe-method)))))
      ;; Did we get to what might be the return type of a method (or a
      ;; constructor name)?
      (when (eq found 'maybe-method)
        ;; Eat up identifiers and type parameters until we hit either
        ;; a paren or something else
        (let ((start (point)) (last-point (point)) (last-piece ""))
          (while (eq found 'maybe-method)
            ;; Get the next piece
            (while (forward-comment 1))
            (cond ((eobp)
                   (setq found nil))
                  ((eql (char-after (point)) ?<)
                   ;; Type parameter
                   (unless (show-context-mode-forward-balanced ?< ?>)
                     (setq found nil)))
                  ((eql (char-after (point)) ?\[)
                   ;; Array type
                   (unless (show-context-mode-forward-balanced ?\[ ?\])
                     (setq found nil)))
                  ((eql (char-after (point)) ?\()
                   ;; Method or constructor
                   (let (;; Get return type
                         (ret-type (show-context-mode-buffer-substring
                                    start last-point))
                         ;; Get argument list and throws
                         (args (show-context-mode-buffer-substring
                                (point) (point-max))))
                     (setq found (list 'method first-char (point-max)
                                       ret-type last-piece args))))
                  (t
                   ;; Return type or method name
                   (setq last-point (point))
                   (setq last-piece (show-context-mode-java-get-id))
                   (unless last-piece
                     (setq found nil)))))))
      found)))

(defun show-context-mode-java-get-context ()
  (let ((limit (point))
        (strip-comments (memq show-context-mode-java-finagle-level
                              '(strip-comments simplify-once
                                               simplify-all)))
        (to-the-top (eq show-context-mode-java-finagle-level
                        'simplify-all))
        (enclosing nil))
    ;; If I'm in the middle of a declaration, move to the open brace
    ;; of the declaration.  However, also be careful not to walk out
    ;; of the current block.
    (show-context-mode-skip-chars-forward "^{};")
    ;; The close curly brace is one level lower than the previous
    ;; character, so get back into the level if we were in a block.
    (when (eql (char-after (point)) ?})
      (backward-char 1))
    ;; Walk up the levels, looking for methods or classes
    (let ((level (show-context-mode-parser-current-level)))
      (while (and (>= level 0) (or to-the-top (null enclosing)))
        ;; Find this level
        (show-context-mode-parser-up level)
        ;; Find what's at this level
        (let ((thing (show-context-mode-java-read-method-or-class)))
          ;; If I found something that starts before limit, accept it
          (if (and thing
                   (< (second thing) limit))
              (setq enclosing (cons thing enclosing))))
        ;; Try the next level up
        (setq level (- level 1))))
    (when (consp enclosing)
      ;; Scrunch!
      (let ((text
             (case show-context-mode-java-finagle-level
               ((none strip-comments)
                ;; Include the open curly brace
                (goto-char (third (car enclosing)))
                (show-context-mode-skip-chars-forward "^{")
                (unless (eobp)
                  (forward-char 1))
                ;; Scrunch text
                (show-context-mode-scrunch (second (car enclosing))
                                           (min (point)
                                                limit)
                                           strip-comments))
               ((simplify-once simplify-all)
                (let (pieces)
                  (if (eq show-context-mode-java-finagle-level
                          'simplify-once)
                      (setq pieces (cdddr (car enclosing)))
                    ;; Build a dot-separated list of all enclosing
                    ;; names
                    (dolist (enc enclosing)
                      (when pieces
                        (setq pieces (cons "." pieces)))
                      (setq pieces (cons (fifth enc) pieces)))
                    ;; Make the inner-most be first in the list
                    (setq enclosing (nreverse enclosing))
                    ;; Add the arguments/extends of the inner-most
                    (setq pieces (cons (sixth (car enclosing))
                                       pieces))
                    ;; Reverse everything and prepend the return value
                    ;; or "class "
                    (setq pieces (cons (fourth (car enclosing))
                                       (nreverse pieces))))
                  (let ((str (apply #'concat pieces)))
                    ;; Include the open curly brace
                    (goto-char (third (car enclosing)))
                    (show-context-mode-skip-chars-forward "^{")
                    (unless (eobp)
                      (setq str (concat str " "
                                        (show-context-mode-buffer-substring
                                         (point) (+ (point) 1)))))
                    ;; Limit the string
                    (setq str (show-context-mode-string-up-to str limit))
                    (show-context-mode-scrunch-string str t))))
               (t
                (error "Unknown finagle level %s"
                       show-context-mode-java-finagle-level)))))
        (unless (string= text "")
          text)))))

(put 'java-mode 'show-context-mode-getter
     #'show-context-mode-java-get-context)

(defun show-context-mode-java-init ()
  (show-context-mode-init-parser "{" "}"))

(put 'show-context-mode-java-get-context 'show-context-mode-getter-init
     #'show-context-mode-java-init)

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
