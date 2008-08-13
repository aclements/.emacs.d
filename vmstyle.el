(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C style
;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Long line highlighting
;;

(defface vmstyle-long-line-face
  '((((class color)) (:background "red4")))
  "Face for highlighting overly long lines")

(defun vmstyle-highlight-long-lines (&optional column mode)
  "Modify font-lock to highlight the end of lines that are too long.

The optional argument COLUMN specifies the column at which to
start highlighting.  It defaults to 80.  The optional argument
MODE specifies which mode to add font-lock keywords for.  It
defaults to 'c-mode."

  (interactive)

  (unless column
    (setq column 80))
  (unless mode
    (setq mode 'c-mode))

  (font-lock-add-keywords 
   mode
   `((,(concat "^[^\n]\\{" (number-to-string column) "\\}\\(.*\\)$")
      1 'vmstyle-long-line-face append))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header comments
;;

(defvar vmstyle-function-prologue
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
expansion time.")

(defvar vmstyle-file-prologue
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
  "The prologue to insert at the beginning of a file.")

(defvar vmstyle-header-prologue
  '("#ifndef " protection "
#define " protection "

#define INCLUDE_ALLOW_XXX
#include \"includeCheck.h\"

")
  "The prologue to insert at the beginning of header files.  The
symbol `protection' will be bound to the pre-processor symbol to
use to protect this header from multiple inclusion.")

(defvar vmstyle-header-epilogue
  '("
#endif // ifndef " protection "
")
  "The prologue to insert at the end of header files.  As for the
variable `vmstyle-header-prologue', the symbol `protection' will
be bound.")


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

(defun vmstyle-function-prologue ()
  "Insert a procedure header at the top of the current function.

This uses some rudimentary parsing to find the beginning of the
enclosing function, where it expands `vmstyle-function-prologue'."

  (interactive)

  (push-mark)
  (c-beginning-of-defun)
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

(defun vmstyle-prologue-dwim ()
  "Guess which type of prologue to insert.

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
        ((string-match "\\.c$" (buffer-file-name))
         (vmstyle-source-header))
        ((string-match "\\.h$" (buffer-file-name))
         (vmstyle-header-prologue))
        (t
         (message "Couldn't determine header type.  Insert header?  [fch?]")
         (let (done)
           (while (not done)
             (let ((key (read-event)))
               (setq done t)
               (case key
                 ((?f ?F) (vmstyle-function-prologue))
                 ((?c ?C) (vmstyle-source-header))
                 ((?h ?H) (vmstyle-header-prologue))
                 ((??)
                  (message "Header type: (f) Function, (c) Source file, (h) Header file")
                  (setq done nil)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment editing
;;

(defvar vmstyle-function-prologue-begin-re "/\\*\n \\*--")
(defvar vmstyle-function-prologue-continuation-re "^[ \t]*\\*?[ \t]*$")
(defvar vmstyle-function-prologue-section-re
  "^[ \t]*\\*\\(-\\| [^ ].*\\(--\\|:\\)[ \t]*$\\)")

(defvar vmstyle-function-prologue-default-indent " *      ")

(defvar vmstyle-specialize-comment-indent t)

;; XXX This isn't always correct.  For example, hitting tab on an
;; existing comment line screws it up instead of re-indenting it.
(defun vmstyle-compute-comment-indent ()
  (let (range)
    (when (c-save-buffer-state
              ()
            (setq range (c-literal-limits))
            (when range
              (save-excursion
                (goto-char (car range))
                (looking-at vmstyle-function-prologue-begin-re))))
      ;; We are.  Go backwards until we find another line of text
      (save-excursion
        (beginning-of-line)
        (forward-line -1)
        (while (looking-at vmstyle-function-prologue-continuation-re)
          (forward-line -1))
        ;; What did we hit?
        (when (or (looking-at "^/\\*")
                  (looking-at vmstyle-function-prologue-section-re))
          ;; We hit the beginning of the comment or a section
          ;; within the comment.  Find the first real line of text.
          (goto-char (car range))
          (forward-line)
          (beginning-of-line)
          (while (or (looking-at vmstyle-function-prologue-continuation-re)
                     (looking-at vmstyle-function-prologue-section-re))
            (forward-line)))
        ;; Take our indentation from this line
        (if (looking-at "^[ \t]*\\*?[ \t]*")
            (match-string 0)
          vmstyle-function-prologue-default-indent)))))

(defadvice c-indent-line (around indent-in-function-header activate)
  (or (when vmstyle-specialize-comment-indent
        (let ((indent (vmstyle-compute-comment-indent)))
          (when indent
            (insert indent)
            (length indent))))
      ad-do-it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc utilities
;;

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

(defun vmstyle-convert-line-comments ()
  "Convert all block line comments following point into block comments.

This will search for all two-or-more line blocks of line comments
after point.  For each comment, it will prompt for whether or not
to convert it into a block comment."

  (interactive)

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
