;;; vmstyle.el --- VMware style customizations

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Created:    16-Jun-2008
;; Version:    0.1

;; See vmstyle-load.el for information on how to load this.

;;; Code:

(eval-when-compile (require 'cl))

;;;###autoload
(defgroup vmstyle nil
  "Emacs style customizations for editing VMware code.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C style
;;

;;;###autoload
(defcustom vmstyle-enable-c-style "/bora/"
  "Whether or not to set the VMware C style.

If this is t, then the VMware C style will be set automatically
for every c-mode buffer.  If it is nil, then it will never be set
automatically.  If it is a string, then it will be set
automatically for paths that match the regexp given by its
value."
  :type '(radio (const :tag "Always enable" t)
                (const :tag "Never enable" nil)
                (const :tag "Enable if file path contains \"/bora/\""
                       "/bora/")
                (regexp :tag "Match regexp against file path"))
  :group 'vmstyle)

;;;###autoload
(defun vmstyle-set-c-style (&optional just-add)
  "Add and set a C style called \"vmware\" that matches the engineering guide.

If optional argument JUST-ADD is non-nil, skips activating the
style and just adds it to the set of available styles.  A typical
use of this function is:

  (add-hook 'c-mode-hook #'vmstyle-set-c-style)

It can also be called interactively to set the style for the
current buffer."

  (interactive)

  (c-add-style
   "vmware"
   '("k&r"
     ;; While the style guide says the limit is 89 characters, many
     ;; groups internally restrict themselves to 79, so we use 79
     ;; here.
     (fill-column . 79)
     ;; 3-space indentation
     (c-basic-offset . 3)
     ;; C99-style comments
     (comment-start . "// ")
     (comment-end . ""))
   (not just-add)))

;;;###autoload
(defun vmstyle-maybe-enable-c-style ()
  "Call `vmstyle-set-c-style' according to `vmstyle-enable-c-style'.

This is meant to be used in the `c-mode-common-hook'."

  (cond
   ((not vmstyle-enable-c-style) nil)
   ((eq vmstyle-enable-c-style t) (vmstyle-set-c-style))
   ((stringp vmstyle-enable-c-style)
    (when (and (buffer-file-name)
               (string-match (buffer-file-name) vmstyle-enable-c-style))
      (vmstyle-set-c-style)))
   (t (message "Invalid vmstyle-enable-c-style value"))))

;;;###autoload
(add-hook 'c-mode-common-hook #'vmstyle-maybe-enable-c-style)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Long line highlighting
;;

;;;###autoload
(defcustom vmstyle-enable-highlight-long-lines '(c-mode)
  "Modes in which to enable long line highlighting."
  :type '(repeat symbol)
  :group 'vmstyle)

;;;###autoload
(defface vmstyle-long-line-face
  '((((class color)) (:background "red4")))
  "Face for highlighting long lines"
  :group 'vmstyle)

;;;###autoload
(defun vmstyle-highlight-long-lines (&optional column)
  "Highlight the end of lines that are too long in this buffer.

The optional argument COLUMN specifies the column at which to
start highlighting.  It defaults to 80."

  (interactive)

  (unless column
    (setq column 80))

  (font-lock-add-keywords nil
   `((,(concat "^[^\n]\\{" (number-to-string column) "\\}\\(.*\\)$")
      1 'vmstyle-long-line-face append))))

(defun vmstyle-maybe-enable-highlight-long-lines ()
  "Call `vmstyle-highlight-long-lines' according to
`vmstyle-enable-highlight-long-lines'.

This is meant to be used in the `after-change-major-mode-hook'."

  ;; XXX The after-change-major-mode-hook trick doesn't work with
  ;; cc-mode in Emacs 21 for some reason.
  (when (memq major-mode vmstyle-enable-highlight-long-lines)
    (vmstyle-highlight-long-lines)))

;; If we simply autoload the above function, then there's basically no
;; point to autoload'ing at all because it will be called via a hook
;; right away.  Thus, we copy it into the load file directly.

;;;###autoload (defun vmstyle-maybe-enable-highlight-long-lines () (when (memq major-mode vmstyle-enable-highlight-long-lines) (vmstyle-highlight-long-lines)))

;;;###autoload
(add-hook 'after-change-major-mode-hook
          #'vmstyle-maybe-enable-highlight-long-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment template system
;;

;;;###autoload
(defgroup vmstyle-comment-templates ()
  "Templates to use for inserted comments.

Each template is a list of strings, lisp expressions, and
symbols.  Strings will be inserted into the buffer verbatim.
Lisp expressions will be evaluated and their result inserted.
Each template has a particular set of symbols that will be bound
to meaningful values.  The special symbol 'point can be used to
indicate where the point should be left after template
expansion."
  :group 'vmstyle)

(defun vmstyle-comment-template-type (&rest others)
  `(repeat (choice (string :tag "Literal string")
                   (const :tag "Place point here" point)
                   ,@others
                   (sexp :tag "Lisp expression"))))

(defun vmstyle-insert-template (template)
  "Insert TEMPLATE at point, returning a point possibly within
template.

TEMPLATE is simply a list of forms to evaluate whose results will
be inserted in the buffer, one after the other.  The one
exception is the symbol 'point, which indicates where point
should be left after expanding the template.  Note that templates
usually have access to variables explicitly bound in the
enclosing environment.  See each template's documentation for
details."

  (let (point)
    (dolist (piece template)
      (cond ((stringp piece)
             (insert piece))
            ((eq piece 'point)
             (setq point (point)))
            (t
             (insert (eval piece)))))
    (or point (point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prologue comments
;;

;;;###autoload
(defcustom vmstyle-function-prologue
  '("/*
 *-----------------------------------------------------------------------------
 *
 * " name " --
 *
 *      " point "XXX
 *
 * Results:
 *      XXX
 *
 * Side effects:
 *      XXX
 *
 *-----------------------------------------------------------------------------
 */

")
  "The prologue to insert at the beginning of functions.  The
symbol `name' will be bound to the name of the function at
expansion time."
  :type (vmstyle-comment-template-type
         '(const :tag "Name of the function" name))
  :group 'vmstyle-comment-templates)

;;;###autoload
(defcustom vmstyle-file-prologue
  '("/* **********************************************************
 * Copyright "
    (number-to-string (nth 5 (decode-time)))
    " VMware, Inc.  All rights reserved.
 * VMware Confidential
 * **********************************************************/

/*
 * " (file-name-nondirectory (buffer-file-name)) " --
 *
 *      " point "XXX
 */

")
  "The prologue to insert at the beginning of a file."
  :type (vmstyle-comment-template-type)
  :group 'vmstyle-comment-templates)

;;;###autoload
(defcustom vmstyle-header-prologue
  '("#ifndef " protection "
#define " protection "

#define INCLUDE_ALLOW_XXX
#include \"includeCheck.h\"

")
  "The prologue to insert at the beginning of header files.  The
symbol `protection' will be bound to the pre-processor symbol to
use to protect this header from multiple inclusion."
  :type (vmstyle-comment-template-type
         '(choice :tag "Multiple-inclusion protection token" protection))
  :group 'vmstyle-comment-templates)

;;;###autoload
(defcustom vmstyle-header-epilogue
  '("
#endif // ifndef " protection "
")
  "The prologue to insert at the end of header files.  As for the
variable `vmstyle-header-prologue', the symbol `protection' will
be bound."
  :type (vmstyle-comment-template-type
         '(choice :tag "Multiple-inclusion protection symbol" protection))
  :group 'vmstyle-comment-templates)

(defun vmstyle-function-prologue ()
  "Insert a procedure header at the top of the current function.

This uses some rudimentary parsing to find the beginning of the
enclosing function, where it expands `vmstyle-function-prologue'."

  (interactive)

  (push-mark)
  (c-beginning-of-defun)
  (when (looking-at "{")
    ;; Emacs 21 cc-mode behavior is to move to the opening brace, not
    ;; the beginning of the declaration
    (c-beginning-of-statement))
  (unless (bolp)
    (error "Declaration does not start at the beginning of a line"))
  (let ((beginning (point)))
    (forward-line)
    ;; This way oversimplifies things, but should work 99% of the
    ;; time and should generally bail when it would be wrong.
    (unless (looking-at "\\([a-zA-Z_][a-zA-Z0-9_$]*\\)(")
      (error "This doesn't look like a function name"))
    (let ((name (match-string 1)))
      (goto-char beginning)
      (goto-char (vmstyle-insert-template vmstyle-function-prologue)))))

(defun vmstyle-source-header ()
  "Insert a source file prologue.

This inserts the standard file prologue.  See
`vmstyle-file-prologue'."

  (interactive)

  (push-mark)
  (goto-char (save-excursion (vmstyle-file-prologue))))

(defun vmstyle-header-prologue ()
  "Insert a header file prologue and epilogue.

In addition to inserting the standard file prologue (see
`vmstyle-file-prologue'), this expands `vmstyle-header-prologue'
after the file header and `vmstyle-header-epilogue' at the bottom
of the current buffer."

  (interactive)

  (push-mark)
  (goto-char
   (save-excursion
     (let ((middle (vmstyle-file-prologue))
           (protection (concat
                        "_"
                        (upcase
                         (replace-regexp-in-string
                          "\\." "_"
                          (file-name-nondirectory (buffer-file-name))))
                        "_")))
       (vmstyle-insert-template vmstyle-header-prologue)
       (goto-char (point-max))
       (unless (bolp)
         (insert "\n"))
       (vmstyle-insert-template vmstyle-header-epilogue)

       middle))))

(defun vmstyle-file-prologue ()
  "Insert the common file prologue at the beginning of this buffer.

This does a rudimentary check that this buffer does not already
contain a file prologue and then expands `vmstyle-file-prologue' at
the beginning of the buffer."
  
  (goto-char (point-min))
  (let ((comment-end (save-excursion
                       (while (forward-comment 1) t)
                       (point))))
    (when (re-search-forward "Copyright [0-9]+ VMware" comment-end t)
      (error "This file appears to have a header comment already")))

  (vmstyle-insert-template vmstyle-file-prologue))

;;;###autoload
(defun vmstyle-prologue-dwim ()
  "Guess which type of prologue comment to insert.

This calls `vmstyle-function-prologue', `vmstyle-source-header',
or `vmstyle-header-prologue' depending on the context at point
and the file name of the current buffer.  If the appropriate
action can't be determined, this prompts the user via the
mini-buffer."

  (interactive)

  (cond ((save-excursion
           (unless (eobp)
             (forward-char))
           (c-beginning-of-defun)
           (not (bobp)))
         (vmstyle-function-prologue))
        ((and (buffer-file-name)
              (string-match "\\.c$" (buffer-file-name)))
         (vmstyle-source-header))
        ((and (buffer-file-name)
              (string-match "\\.h$" (buffer-file-name)))
         (vmstyle-header-prologue))
        (t
         (message "Prologue comment type?  [fch?]")
         (let (done)
           (while (not done)
             (let ((key (read-event)))
               (setq done t)
               (case key
                 ((?f ?F) (vmstyle-function-prologue))
                 ((?c ?C) (vmstyle-source-header))
                 ((?h ?H) (vmstyle-header-prologue))
                 ((??)
                  (message "(f) Function, (c) Source file, (h) Header file")
                  (setq done nil)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment editing
;;

;;;###autoload
(defgroup vmstyle-prologue-comment-style ()
  "How to recognize and format prologue comments."
  :group 'vmstyle)

;;;###autoload
(defconst vmstyle-prologue-comment-function-or-file-re
  "^/\\*\n\\([- *]*\n\\)* \\* [^ ]+ --"
  "Value for `vmstyle-prologue-comment-begin-re' that recognizes
both function and file prologue comments.")
;;;###autoload
(defconst vmstyle-prologue-comment-function-re "^/\\*\n \\*--"
  "Value for `vmstyle-prologue-comment-begin-re' that recognizes
function prologue comments.")

;;;###autoload
(defcustom vmstyle-prologue-comment-begin-re
  vmstyle-prologue-comment-function-or-file-re
  "Regexp for a comment that should be indented like a prologue comment."
  :type `(radio (const :tag "Standard function prologue comments"
                       ,vmstyle-prologue-comment-function-re)
                (const :tag "Function or file prologue comments"
                       ,vmstyle-prologue-comment-function-or-file-re)
                (regexp :tag "Other regexp"))
  :group 'vmstyle-prologue-comment-style)
  ;; This version only matches function prologue comments
  ;; "^/\\*\n \\*--"
  ;; This version also applies to file prologue comments
  ;; "^/\\*\n\\([- *]*\n\\)* \\* [^ ]+ --")

;;;###autoload
(defcustom vmstyle-prologue-comment-continuation-re "^[ \t]*\\*?[ \t]*$"
  "Regexp for a blank comment line."
  :type 'regexp
  :group 'vmstyle-prologue-comment-style)

;;;###autoload
(defcustom vmstyle-prologue-comment-section-re
  "^[ \t]*\\*\\(-\\| [^ ].*\\(--\\|:\\)[ \t]*$\\)"
  "Regexp for \"section headers\" in a prologue comment.

Section headers are only indented by one space, unlike the rest
of a prologue comment.  This is used to distinguish header
lines."
  :type 'regexp
  :group 'vmstyle-prologue-comment-style)

;;;###autoload
(defcustom vmstyle-prologue-comment-default-indent
  " *      "
  "Default indentation string to use for non-header lines.

Usually, prologue comment indentation tries to find an example
non-header line in the current comment to copy the indentation
from, but if that fails, it falls back to this."
  :type 'string
  :group 'vmstyle-prologue-comment-style)

;;;###autoload
(defcustom vmstyle-enable-prologue-comment-indentation t
  "Whether or not to specialize indentation behavior in prologue comments.

If non-nil, the C indentation function will be tweaked so that
lines of a prologue comment are indented according to the style
guide.  This is most effective when combined with something like
filladapt so that both newly indented comment lines and wrapped
lines will be indented appropriately.

Note that this requires Emacs 22 or greater."
  :type 'boolean
  :group 'vmstyle)

;;;###autoload
(defun vmstyle-compute-prologue-comment-indent ()
  (let (range)
    ;; Are we looking at a function comment?
    (when (c-save-buffer-state
              ()
            (setq range (c-literal-limits))
            (when range
              (save-excursion
                (goto-char (car range))
                (looking-at vmstyle-prologue-comment-begin-re))))
      ;; We are.  Go backwards until we find another line of text
      (save-excursion
        (beginning-of-line)
        (forward-line -1)
        (while (looking-at vmstyle-prologue-comment-continuation-re)
          (forward-line -1))
        ;; What did we hit?
        (when (or (looking-at "^/\\*")
                  (looking-at vmstyle-prologue-comment-section-re))
          ;; We hit the beginning of the comment or a section
          ;; within the comment.  Find the first real line of text.
          (goto-char (car range))
          (forward-line)
          (beginning-of-line)
          (while (or (looking-at vmstyle-prologue-comment-continuation-re)
                     (looking-at vmstyle-prologue-comment-section-re))
            (forward-line)))
        ;; Take our indentation from this line
        (if (looking-at "^[ \t]*\\*?[ \t]*")
            (match-string 0)
          vmstyle-prologue-comment-default-indent)))))

;;;###autoload
(defadvice c-indent-line (around indent-prologue-comment activate)
  (or (when vmstyle-enable-prologue-comment-indentation
        (let ((indent (vmstyle-compute-prologue-comment-indent)))
          (when indent
            (save-excursion
              (beginning-of-line)
              ;; Clean up existing comment markers
              (let ((deleted 0))
                (when (looking-at "[ \t]*\\*[ \t]*")
                  (delete-region (match-beginning 0) (match-end 0))
                  (setq deleted (- (match-end 0) (match-beginning 0))))
                ;; Insert the new comment marker.  If point was at the
                ;; beginning of line, it should move forward.
                (insert-before-markers indent)
                (- (length indent) deleted))))))
      ad-do-it))

;; Our indentation computation depends heavily on the comment parser
;; from cc-mode in Emacs 22, so disable the advice if we don't have
;; that.
;;;###autoload
(unless (fboundp 'c-save-buffer-state)
  (ad-disable-advice 'c-indent-line 'around 'indent-prologue-comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc utilities
;;

;;;###autoload
(defun vmstyle-line-to-block-comment (beg end)
  "Convert a block of line comments to a block comment.

If the transient mark is active, then this converts the comment
in the region.  Otherwise, point must be in a comment and this
will find the beginning and end of the comment block."

  (interactive
   (if (and transient-mark-mode mark-active)
       (list (region-beginning) (region-end))
     (save-excursion
       (beginning-of-line)
       (let ((start (point))
             (beg (point)))
         ;; Find the beginning of this comment
         (while (and (looking-at "[ \t]*//") (not (bobp)))
           (setq beg (match-beginning 0))
           (forward-line -1))
         ;; Find the end
         (goto-char start)
         (while (and (looking-at "[ \t]*//") (not (eobp)))
           (forward-line))
         (list beg (point))))))

  (unless (markerp end)
    (setq end (set-marker (make-marker) end)))
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (unless (looking-at "\\([ \t]*\\)//")
      (error "Not in a line comment %s %s" beg end))
    (let ((leader (match-string 1)))
      (insert (concat leader "/*\n"))
      (while (re-search-forward "^[ \t]*\\(//\\)" end t)
        (replace-match " *" nil t nil 1))
      (goto-char (match-end 0))
      (end-of-line)
      (insert (concat "\n" leader " */")))))

;;;###autoload
(defun vmstyle-convert-line-comments ()
  "Convert all block line comments following point into block comments.

This will search for all two-or-more line blocks of line comments
after point.  For each comment, it will prompt for whether or not
to convert it into a block comment."

  (interactive)

  ;; XXX This can get tricked by side comments
  (push-mark)
  (beginning-of-line)
  ;; If we're in a comment, get to the beginning
  (while (and (looking-at "[ \t]*//") (not (bobp)))
    (forward-line -1))
  ;; For each 2 or more line comment...
  (while (re-search-forward "^\\([ \t]*//.*\n\\)\\{2,\\}" nil t)
    ;; Should we convert it?
    (when (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (unwind-protect
                (progn
                  (overlay-put ov 'face 'highlight)
                  (y-or-n-p "Convert to block comment? "))
              (delete-overlay ov)))
      ;; Convert it
      (vmstyle-line-to-block-comment (match-beginning 0) (match-end 0)))))

(provide 'vmstyle)
