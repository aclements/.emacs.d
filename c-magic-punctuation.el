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
;; * Document better
;; * On open brace or semicolon not in a literal, automatically
;;   balance parens (including spacing)
;; * Spaces around 'if' conditions: "if ( x )".  Automatically detect
;;   if this is the style.  Insert on either open or close paren.
;;   This should automatically work with space balancing for
;;   auto-balanced parens (above)
;; * If the user types a space immediately after one has been
;;   automatically inserted, eat it
;; * Have the option to put the point in the condition for automatic
;;   dangling while

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

;;; Code:

(require 'easy-mmode)
(define-minor-mode c-magic-punctuation-mode
  "Minor mode that automatically inserts various forms of punctuation
in C code."
  nil "{.}" nil

  ;; Nothing actually needs to be done.  The advice is already in
  ;; place and keeping an eye on the mode variable.
  )

(defadvice c-electric-brace (around c-space-before-open-brace activate)
  (if (or (not c-magic-punctuation-mode)
          (not c-magic-punctuation-space-before-open-brace))
      ad-do-it
    (if (or (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if I'm in a literal or if the user
        ;; typed anything other than an open brace
        ad-do-it
      ;; Go ahead and put a space here
      (just-one-space)
      (let ((here (point)))
        ad-do-it
        ;; Delete any extra space this may have inserted
        (save-excursion
          (goto-char here)
          (if (looking-at "[ \t]*\n")
              (delete-horizontal-space)))))))

(defun c-magic-punctuation-compute-close-brace-syntax ()
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
  (if (eq syntax-type 'do-while)
      (insert " while ()"))
  (cond ((memq syntax-type
               '(assignment class struct-or-enum do-while))
         ;; Insert semicolon
         (let ((last-command-char ?\;)
               (c-cleanup-list (cons 'defun-close-semi
                                     c-cleanup-list)))
           (c-electric-semi&comma arg)))
        ((eq syntax-type 'array)
         ;; Insert comma
         (let ((last-command-char ?,)
               (c-cleanup-list (cons 'list-close-comma
                                     c-cleanup-list)))
           (c-electric-semi&comma arg)))))

(defadvice c-electric-brace (around c-auto-close-brace activate)
  (if (or (not c-magic-punctuation-mode)
          (not c-magic-punctuation-auto-close-brace))
      ad-do-it
    (if (or (not c-auto-newline)
            (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if not in auto mode, if the user
        ;; typed anything other than an open brace, or if the point is
        ;; in a literal
        ad-do-it
      (let ((syntax-type
             (if (null c-magic-punctuation-close-brace-danglers)
                 'block
               (c-magic-punctuation-compute-close-brace-syntax))))
        (if (not (memq syntax-type
                       c-magic-punctuation-close-brace-danglers))
            (setq syntax-type 'block))
        ad-do-it
        (save-excursion
          (newline)
          ;; Type the close brace
          (let ((last-command-char ?})
                ;; Inhibit cleanup of empty defuns
                (c-cleanup-list
                 (remq 'empty-defun-braces c-cleanup-list)))
            (c-electric-brace arg)
            ;; Insert any additional characters dictated by the
            ;; syntactic context
            (c-magic-punctuation-insert-danglers syntax-type))
          ;; Clean up extra whitespace that may have been inserted
          ;; after the close characters
          (let ((end (point))
                (begin (save-excursion
                         (skip-chars-backward " \t\n")
                         (point))))
            (delete-region begin end)))))))

(provide 'c-magic-punctuation)
