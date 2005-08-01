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

;; To do
;; * On open brace or semicolon not in a literal, automatically
;;   balance parens (including spacing)
;; * Spaces around 'if' conditions: "if ( x )".  Automatically detect
;;   if this is the style.  Insert on either open or close paren.
;;   This should automatically work with space balancing for
;;   auto-balanced parens (above)
;; * Various other automatic spaces could be inserted.  For example,
;;   around some operators (=, ==, && ||), before open paren in if,
;;   for, and while, and after/around semicolon in for
;; * If the user types a space immediately after one has been
;;   automatically inserted, eat it
;; * Option for close brace binding to skip forward over the next
;;   close brace (include any syntactic whitespace)

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

;;; Code:

(require 'easy-mmode)
(define-minor-mode c-magic-punctuation-mode
  "Minor mode that automatically inserts various forms of punctuation
in C code."
  nil "{.}" '(("{" . c-magic-open-brace))

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

(unless (fboundp 'string-reverse)
  (defun string-reverse (s)
    (let ((l (string-to-list s)))
      (if (null l)
          ""
        (apply 'string (reverse l))))))

(defun c-magic-close-parens ()
  ;; XXX Don't do this on semicolon in a for
  ;; XXX This is a work-in-progress
  (interactive)
  (unless (c-in-literal)
    (let ((state (c-parse-state))
          (bos (save-excursion
                 (c-beginning-of-statement)
                 (point)))
          (cur-point (point)))
      (dolist (paren-point state)
        (message "%s" paren-point)
        (let ((whitespace
               (and (not (consp paren-point))
                    (> paren-point bos)
                    (save-excursion
                      (goto-char paren-point)
                      (when (looking-at "(")
                        ;; I'd like to accumulate syntactic ws, but
                        ;; that's not going to happen
                        (forward-char)
                        (string-reverse
                         (buffer-substring
                          (point)
                          (save-excursion
                            (skip-chars-forward " \t\n" cur-point)
                            (point)))))))))
          (when whitespace
            (insert whitespace ")")))))))

(defun c-magic-open-brace (arg)
  "Insert a magic open brace, applying
  `c-magic-punctuation-space-before-open-brace' and
  `c-magic-punctuation-auto-close-brace'.  With auto close brace
  insertion, if the mark is transient and active or a universal prefix
  argument is supplied, this puts braces around the region and
  re-indents the region."

  (interactive "*P")
  (let* ((auto-space (and c-magic-punctuation-space-before-open-brace
                          (not (c-in-literal))))
         (auto-close-brace (and c-magic-punctuation-auto-close-brace
                                c-auto-newline
                                (not (c-in-literal))))
         (embrace (or (consp arg)
                      (and transient-mark-mode mark-active)))
         (close-brace-point (if embrace
                                (set-marker (make-marker)
                                            (region-end))))
         return-to-point)
    (when embrace
      (goto-char (region-beginning)))
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
            (current-prefix-arg nil))
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

(provide 'c-magic-punctuation)
