;;; c-magic-punctuation.el --- minor mode for automatically inserting
;;; punctuation in c-mode

;; Copyright (C) 2005 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    28-Jul-2005
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

;; C programs have very low information content (in the information
;; theoretic sense), requiring C programmers to infuse their code with
;; punctuation that adds nothing to the meaning of the program, but
;; exists to keeps the compiler happy.  `c-magic-punctuation-mode' is
;; an attempt to bring life and creativity back to C coding by
;; reducing the amount of mechanical work the programmer needs to put
;; into satisfying the tedious constraints of C's low bit-rate syntax.

;; Specifically, `c-magic-punctuation-mode' automatically inserts
;; spaces, braces, parentheses, and more.  `c-magic-punctuation-mode'
;; goes hand-in-hand with `cc-mode's auto-mode (this must be enabled
;; for certain features to work), which can automatically insert many
;; forms of whitespace.  `c-magic-punctuation-mode' adds the
;; following features:

;; * It can automatically insert brace pairs.  This is triggered by
;;   hitting '{'.  This can automatically insert the close brace,
;;   followed by 'danglers' appropriate to the syntax (everything from
;;   semicolons to 'while ();'), place your cursor in the right place,
;;   and cleanup whitespace near the braces (such as inserting a space
;;   to separate the opening brace from a preceding 'if' condition).
;;   If you have `transient-mark-mode' enabled or hit '\C-u' before
;;   the open brace, it can put the braces around the region,
;;   automatically re-indenting and placing your cursor at the
;;   beginning of the first line of the block.

;; * It can automatically insert close parens.  This is triggered by
;;   either '{' or ';' (under the right circumstances).  Either of
;;   these will automatically balance remaining unbalanced open
;;   parens, including matching up the spacing between the parens.  It
;;   can analyze the syntax to figure out when it is appropriate to do
;;   this (try typing 'for (' followed by three semicolons).

;; * Most of the features have fine-grained customization controls.
;;   Try M-x customize-group<RET>c-magic-punctuation<RET>.

;; Free yourself from the constraints of low bit-rate syntax!

;;; Todo:

;; * Spaces around 'if' conditions: "if ( x )".  Automatically detect
;;   if this is the style.  Insert on either open or close paren.
;;   This should automatically work with space balancing for
;;   auto-balanced parens (above).  Similarly for 'for', 'while', and
;;   'do'.
;; * Various other automatic spaces could be inserted.  For example,
;;   around some operators (=, ==, && ||), before open paren in if,
;;   for, and while, and after/around semicolon in for
;; * If the user types a space immediately after one has been
;;   automatically inserted, eat it (how does one detect if other
;;   things have happened in the mean time?)
;; * Option for close brace binding to skip forward over the next
;;   close brace (include any syntactic whitespace).  Re-indent the
;;   close brace as we pass over it.

;;; Customization:

(defgroup c-magic-punctuation nil
  "Minor mode for automatically inserting punctuation in C code")

(defcustom c-magic-punctuation-space-before-open-brace t
  "Automatic whitespace space before open brace

When the user types a brace that opens a block, this ensures there's a
space before the brace where appropriate.  For example, if the user
types an if statement with a block and the current syntax uses
non-hanging braces, this will ensure there's exactly one space between
the closing paren and the open brace."
  :type 'boolean
  :group 'c-magic-punctuation)

(defcustom c-magic-punctuation-auto-close-brace t
  "Automatic close brace insertion (requires auto mode)

When the user types an open brace, this automatically inserts the
corresponding close brace.  Spacing and newlines are taken care of
according to the current style and the point is placed inside the
braces."
  :type 'boolean
  :group 'c-magic-punctuation)

(defconst c-magic-punctuation-danglers
  '(assignment array class struct-or-enum do-while))
(defcustom c-magic-punctuation-close-brace-danglers
  c-magic-punctuation-danglers
  "Automatic close brace also inserts danglers for these syntaxes

When `c-magic-punctuation-auto-close-brace' is enabled, this allows it
to insert further \"danglers\" following the automatically inserted
close brace for this set of special syntaxes.

* assignment - Dangle a semicolon after the close brace for array or
  struct assignments.
* array - Dangle a comma after the close brace for assignments of
  nested arrays or structs.
* class - Dangle a semicolon after the close brace for class
  definitions.
* struct-or-enum - Dangle a semicolon after the close brace for struct
  or enum definitions.
* do-while - Dangle \"while ();\" after the close brace for do-while
  blocks."
  :type `(set ,@(mapcar (lambda (d) `(const ,d))
                        c-magic-punctuation-danglers))
  :group 'c-magic-punctuation)

(defcustom c-magic-punctuation-point-in-do-while 'condition
  "Where to put point when automatically inserting \"while ();\".

When do-while is present in
`c-magic-punctuation-close-brace-danglers', this affects where the
point will be placed after insertion of the dangler.  It can be
condition, indicating that point should be placed between the
condition parens, or it can be body, indicating that point should be
placed between the braces."
  :type '(radio (const condition) (const body))
  :group 'c-magic-punctuation)

(defcustom c-magic-punctuation-allow-embrace t
  "Prefix arg or transient mark puts braces around the region.

If this is enabled, auto-close-brace will put braces around the region
if the '{' is preceded by a universal argument or transient-mark-mode
is enabled and the mark is active.  It will automatically re-indent
the region and place point at the beginning of the first line."
  :type 'boolean
  :group 'c-magic-punctuation)

(defcustom c-magic-punctuation-auto-close-parens t
  "Automatically insert closing parens.

When the user types an open brace or a semicolon (except in some
situations), this will automatically insert closing parens and
inter-paren whitespace to balance any unbalanced open parens in the
current statement."
  :type 'boolean
  :group 'c-magic-punctuation)

(defcustom c-magic-punctuation-enable-funky-close-paren-logic t
  "Enables the use of complex logic when inserting close parens.

C has nasty syntax.  If this is t, auto-close-parens will attempt to
be intelligent about determining whether or not it should insert close
parens.  This logic may be wrong, though.  Set this to nil if you have
problems and auto-close-parens will fall back on simple logic."
  :type 'boolean
  :group 'c-magic-punctuation)

;;; Code:

(require 'easy-mmode)
(define-minor-mode c-magic-punctuation-mode
  "Minor mode that automatically inserts various forms of punctuation
in C code."
  nil "{.}" '(("{" . c-magic-open-brace)
              (";" . c-magic-semicolon))

  ;; Make sure cc-mode is loaded, just in case this is called outside
  ;; a C buffer for some reason
  (require 'cc-mode))

(defun c-magic-punctuation-compute-brace-syntax ()
  "Computes the syntactic context in which an open brace is being
inserted.  This is used to determine which danglers to insert.  This
returns one of assignment, array, block, class, do-while, or
struct-or-enum."

  (or (save-excursion
        (c-backward-syntactic-ws)
        (let ((cb (char-before)))
          (cond ((null cb) nil)
                ((= cb ?=) 'assignment)
                ((= cb ?,) 'array)
                ((or (= cb ?\)) (= cb ?\;) (= cb ?\{)) 'block)
                (t nil))))
      (save-excursion
        ;; Class, struct, or enum?  This is really hacky,
        ;; but covers the common cases.
        (c-beginning-of-statement-1)
        (if (looking-at "typedef\\>")
            (c-forward-token-1))
        (if (looking-at "class\\>")
            'class
          (if (looking-at "\\(struct\\|enum\\)\\>")
              'struct-or-enum)))
      (save-excursion
        ;; Do-while?
        (c-beginning-of-statement-1)
        (if (looking-at "do\\>")
            'do-while))
      (progn
        (message "c-auto-close-brace is confused")
        'block)))

(defun c-magic-punctuation-insert-danglers (syntax-type)
  "Inserts the appropriate danglers for `syntax-type' at point,
filtered by `c-magic-punctuation-close-brace-danglers'."

  (let ((real-syntax-type
         (if (memq syntax-type
                   c-magic-punctuation-close-brace-danglers)
             syntax-type
           'block))
        return-to-point)
    (when (eq real-syntax-type 'do-while)
      ;; Insert while
      (insert " while (")
      (if (eq c-magic-punctuation-point-in-do-while 'condition)
          (setq return-to-point (point-marker)))
      (insert ")"))
    (cond ((memq real-syntax-type
                 '(assignment class struct-or-enum do-while))
           ;; Insert semicolon
           (let ((last-command-char ?\;)
                 (c-cleanup-list (cons 'defun-close-semi
                                       c-cleanup-list)))
             (c-electric-semi&comma arg)))
          ((eq real-syntax-type 'array)
           ;; Insert comma
           (let ((last-command-char ?,)
                 (c-cleanup-list (cons 'list-close-comma
                                       c-cleanup-list)))
             (c-electric-semi&comma arg))))
    ;; Clean up extra whitespace that may have been inserted after the
    ;; close characters
    (let ((end (point))
          (begin (save-excursion
                   (skip-chars-backward " \t\n")
                   (point))))
      (delete-region begin end))
    ;; Return the return-to point
    return-to-point))

(defun c-magic-up-parens (&optional level state)
  "Move point out to the level'th level of unbalanced open paren that
doesn't involve passing over any block boundaries.  This is useful in
conjunction with `c-beginning-of-statement' in order to get around
confusions that arise from things like semicolons in for conditions.
If level is omitted, it defaults to 0 (meaning the outer-most
unbalanced paren).  If the result of `c-parse-state' is handy, pass it
in as the option parameter state, otherwise omit it and it will be
computed."

  (interactive)
  (let ((state (if (null state)
                   (c-parse-state)
                 state))
        points)
    (catch 'done
      (dolist (paren-point state)
        (let ((open-point (if (consp paren-point)
                              (cadr paren-point)
                            paren-point)))
          (if (/= (char-after open-point) ?\()
              (throw 'done nil))
          (setq points (cons open-point points)))))
    (let ((paren-point (nth (or level 0) points)))
      (if paren-point
          (goto-char paren-point)))))

(unless (fboundp 'string-reverse)
  (defun string-reverse (s)
    "Reverse s."
    (let ((l (string-to-list s)))
      (if (null l)
          ""
        (apply 'string (reverse l))))))

(defun c-magic-close-parens ()
  "Automatically insert close parens matching unbalanced open parens
in the current statement.  This also automatically inserts balancing
whitespace around the closing parens to match the whitespace around
the matching open parens.  This is a no-op when point is in a literal
or comment."

  (interactive)
  (unless (c-in-literal)
    (let* ((state (c-parse-state))
           (bos (save-excursion
                  (c-magic-up-parens 0 state)
                  (c-beginning-of-statement-1)
                  (point)))
           (cur-point (point)))
      (dolist (paren-point state)
        ;; Check if this is an unbalanced open paren that's in this
        ;; statement
        (if (and (not (consp paren-point))
                 (> paren-point bos)
                 (= (char-after paren-point) ?\())
            ;; Accumulate the whitespace that should be inserted
            ;; before this paren (the reverse of the whitespace after
            ;; this paren)
            (let ((whitespace
                   ;; I'd like to accumulate syntactic ws, but that's
                   ;; not going to happen
                   (string-reverse
                    (buffer-substring
                     (+ paren-point 1)
                     (save-excursion
                       (goto-char (+ paren-point 1))
                       (skip-chars-forward " \t\n" cur-point)
                       (point))))))
              (insert whitespace ")")))))))

(defun c-magic-open-brace (arg)
  "Insert a magic open brace, applying
`c-magic-punctuation-auto-close-parens',
`c-magic-punctuation-space-before-open-brace', and
`c-magic-punctuation-auto-close-brace'.  With auto close brace
insertion, if the mark is transient and active or a universal prefix
argument is supplied, this puts braces around the region and
re-indents the region."

  (interactive "*P")
  (let* ((auto-close-parens c-magic-punctuation-auto-close-parens)
         (auto-space (and c-magic-punctuation-space-before-open-brace
                          (not (c-in-literal))))
         (auto-close-brace (and c-magic-punctuation-auto-close-brace
                                c-auto-newline
                                (not (c-in-literal))))
         (embrace (and c-magic-punctuation-allow-embrace
                       (or (consp arg)
                           (and transient-mark-mode mark-active))
                       auto-close-brace))
         (close-brace-point (if embrace
                                (set-marker (make-marker)
                                            (region-end))))
         return-to-point)
    (when embrace
      (goto-char (region-beginning)))
    (when auto-close-parens
      (c-magic-close-parens))
    (when auto-space
      ;; Go ahead and ensure there's space here; we'll clean up later
      (just-one-space))
    ;; Figure out what's going on around me before I fiddle with it
    (let ((brace-insert-point (point))
          (syntax-type
           (when auto-close-brace
             (if (null c-magic-punctuation-close-brace-danglers)
                 ;; Skip the syntax check altogether
                 'block
               (c-magic-punctuation-compute-brace-syntax)))))
      ;; Insert the open brace
      (let ((last-command-char ?{)
            (current-prefix-arg
             (if auto-close-brace
                 ;; The prefix arg doesn't interact well with the
                 ;; close brace insertion
                 nil
               ;; Don't screw with brace behavior if asked not to
               current-prefix-arg)))
        (call-interactively (function c-electric-brace)))
      (when auto-close-brace
        ;; Insert the closing stuff
        (let ((begin-body-point (point-marker)))
          (save-excursion
            (if (not embrace)
                ;; Just put a blank line here
                (newline)
              ;; First, nuke the extra space inserted after the open
              ;; brace such that point winds up at the beginning of the
              ;; line following the open brace
              (let ((begin (point))
                    (end (save-excursion
                           (skip-chars-forward " \t\n")
                           (point))))
                (delete-region begin end))
              ;; Now go to the close brace insertion point
              (goto-char close-brace-point))
            ;; Insert the close brace
            (let ((last-command-char ?})
                  (current-prefix-arg nil)
                  ;; Inhibit cleanup of empty defuns
                  (c-cleanup-list
                   (remq 'empty-defun-braces c-cleanup-list)))
              (call-interactively (function c-electric-brace))
              ;; Re-indent the body if appropriate
              (when embrace
                (indent-region begin-body-point (point) nil))
              ;; Insert any additional characters dictated by the
              ;; syntactic context and the user-selected danglers
              (let ((rtp (c-magic-punctuation-insert-danglers
                          syntax-type)))
                (setq return-to-point (or return-to-point rtp)))))))
      (when auto-space
        ;; Delete any extra space that may have been inserted
        (save-excursion
          (goto-char brace-insert-point)
          (if (looking-at "[ \t]*\n")
              (delete-horizontal-space)))))
    ;; Move to the return-to-point if one is set
    (when return-to-point
      (goto-char return-to-point))))

(defun c-magic-semicolon ()
  "If `c-magic-punctuation-auto-close-parens', automatically close
parens before inserting the semicolon if appropriate."

  (interactive)
  (when c-magic-punctuation-auto-close-parens
    ;; Check if I should put close parens.  Technically this ambiguous
    ;; for most flow control statements, since semi-colons are allowed
    ;; in the condition, but only crazy people do that.  This is
    ;; designed for common cases.
    (if (save-excursion
          (let ((cond-end (point))
                (cond-begin (progn (c-magic-up-parens)
                                   (point))))
            (c-beginning-of-statement-1)
            ;; Am I in a for statement?
            (if (looking-at "for\\>")
                (when c-magic-punctuation-enable-funky-close-paren-logic
                  ;; Check that point is preceded by two significant
                  ;; semicolons.  Yes, this is screwy.  No, I'm not
                  ;; sure it's right.
                  (goto-char cond-end)
                  ;; c-beginning-of-statement-1 won't cross unbalanced
                  ;; parens (but don't back out of the for)
                  (c-magic-up-parens 1)
                  ;; Get to the beginning of the increment clause,
                  ;; being weary of a null statement (which would
                  ;; cause c-beginning-of-statement-1 to skip to the
                  ;; beginning of the loop condition clause)
                  (c-backward-syntactic-ws)
                  (if (/= (char-before) ?\;)
                      (c-beginning-of-statement-1))
                  ;; Make sure there's a semicolon here (this is the
                  ;; first significant semicolon)
                  (c-backward-syntactic-ws)
                  (when (= (char-before) ?\;)
                    ;; Skip backwards, first backing up over the
                    ;; semicolon because c-beginning-of-statement-1
                    ;; won't move if there's a preceding null
                    ;; statement with an open paren before it (ie, if
                    ;; the initial condition clause is null)
                    (backward-char)
                    ;; Of course, now that we've backed up over the
                    ;; semicolon, if we're in a null statement,
                    ;; c-beginning-of-statement-1 will move us back
                    ;; too far
                    (c-backward-syntactic-ws)
                    (if (/= (char-before) ?\;)
                        (c-beginning-of-statement-1))
                    ;; And make sure there's another semicolon here
                    ;; (this is the second significant semicolon)
                    (c-backward-syntactic-ws)
                    (eq (char-before) ?\;)))
              t)))
        (c-magic-close-parens)))
  (call-interactively (function c-electric-semi&comma)))

(provide 'c-magic-punctuation)
