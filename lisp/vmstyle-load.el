;;; vmstyle-load.el --- Autoload wrapper for vmstyle.el

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Created:    16-Jun-2008
;; Version:    0.1

;;; Commentary:

;; To install vmstyle, add the following lines to your .emacs file:
;;   (add-to-list 'load-path "PATH CONTAINING vmstyle.el" t)
;;   (require 'vmstyle-load)
;; Alternatively, you can (require 'vmstyle) to load vmstyle eagerly.
;; This will increase your Emacs start-up time, but decrease the delay
;; when opening the first file that triggers vmstyle.

;; It is recommended that you bind `vmstyle-prologue-dwim'.  For example
;;   (eval-after-load 'cc-mode
;;     '(define-key c-mode-map "\C-ci" #'vmstyle-prologue-dwim))
;; You may also want to bind `vmstyle-line-to-block-comment' and/or
;; `vmstyle-convert-line-comments'.

;;; Code:

;; To update this file, evaluate the following form
;;   (let ((generated-autoload-file buffer-file-name)) (update-file-autoloads "vmstyle.el"))

(unless (fboundp 'custom-autoload)
  ;; Emacs 21 doesn't have custom-autoload
  (defun custom-autoload (symbol load &optional noset)
    "Compatibility with Emacs 21."
    (put symbol 'custom-autoload (if noset 'noset t))
    (custom-add-load symbol load)))


;;;### (autoloads (vmstyle-convert-line-comments vmstyle-line-to-block-comment
;;;;;;  vmstyle-compute-prologue-comment-indent vmstyle-enable-prologue-comment-indentation
;;;;;;  vmstyle-prologue-comment-default-indent vmstyle-prologue-comment-section-re
;;;;;;  vmstyle-prologue-comment-continuation-re vmstyle-prologue-comment-begin-re
;;;;;;  vmstyle-prologue-comment-style vmstyle-prologue-dwim vmstyle-header-epilogue
;;;;;;  vmstyle-header-prologue vmstyle-file-prologue vmstyle-function-prologue
;;;;;;  vmstyle-comment-templates vmstyle-highlight-long-lines vmstyle-enable-highlight-long-lines
;;;;;;  vmstyle-maybe-enable-c-style vmstyle-set-c-style vmstyle-enable-c-style
;;;;;;  vmstyle) "vmstyle" "vmstyle.el" (18596 29967))
;;; Generated autoloads from vmstyle.el

(let ((loads (get (quote vmstyle) (quote custom-loads)))) (if (member (quote "vmstyle") loads) nil (put (quote vmstyle) (quote custom-loads) (cons (quote "vmstyle") loads))))

(defvar vmstyle-enable-c-style "/bora/" "\
Whether or not to set the VMware C style.

If this is t, then the VMware C style will be set automatically
for every c-mode buffer.  If it is nil, then it will never be set
automatically.  If it is a string, then it will be set
automatically for paths that match the regexp given by its
value.")

(custom-autoload (quote vmstyle-enable-c-style) "vmstyle" t)

(autoload (quote vmstyle-set-c-style) "vmstyle" "\
Add and set a C style called \"vmware\" that matches the engineering guide.

If optional argument JUST-ADD is non-nil, skips activating the
style and just adds it to the set of available styles.  A typical
use of this function is:

  (add-hook 'c-mode-hook #'vmstyle-set-c-style)

It can also be called interactively to set the style for the
current buffer.

\(fn &optional JUST-ADD)" t nil)

(autoload (quote vmstyle-maybe-enable-c-style) "vmstyle" "\
Call `vmstyle-set-c-style' according to `vmstyle-enable-c-style'.

This is meant to be used in the `c-mode-common-hook'.

\(fn)" nil nil)

(add-hook (quote c-mode-common-hook) (function vmstyle-maybe-enable-c-style))

(defvar vmstyle-enable-highlight-long-lines (quote (c-mode)) "\
Modes in which to enable long line highlighting.")

(custom-autoload (quote vmstyle-enable-highlight-long-lines) "vmstyle" t)

(defface vmstyle-long-line-face (quote ((((class color)) (:background "red4")))) "\
Face for highlighting long lines" :group (quote vmstyle))

(autoload (quote vmstyle-highlight-long-lines) "vmstyle" "\
Highlight the end of lines that are too long in this buffer.

The optional argument COLUMN specifies the column at which to
start highlighting.  It defaults to 80.

\(fn &optional COLUMN)" t nil)
 (defun vmstyle-maybe-enable-highlight-long-lines () (when (memq major-mode vmstyle-enable-highlight-long-lines) (vmstyle-highlight-long-lines)))

(add-hook (quote after-change-major-mode-hook) (function vmstyle-maybe-enable-highlight-long-lines))

(let ((loads (get (quote vmstyle-comment-templates) (quote custom-loads)))) (if (member (quote "vmstyle") loads) nil (put (quote vmstyle-comment-templates) (quote custom-loads) (cons (quote "vmstyle") loads))))

(defvar vmstyle-function-prologue (quote ("/*\n *-----------------------------------------------------------------------------\n *\n * " name " --\n *\n *      " point "XXX\n *\n * Results:\n *      XXX\n *\n * Side effects:\n *      XXX\n *\n *-----------------------------------------------------------------------------\n */\n\n")) "\
The prologue to insert at the beginning of functions.  The
symbol `name' will be bound to the name of the function at
expansion time.")

(custom-autoload (quote vmstyle-function-prologue) "vmstyle" t)

(defvar vmstyle-file-prologue (quote ("/* **********************************************************\n * Copyright " (number-to-string (nth 5 (decode-time))) " VMware, Inc.  All rights reserved.\n * VMware Confidential\n * **********************************************************/\n\n/*\n * " (file-name-nondirectory (buffer-file-name)) " --\n *\n *      " point "XXX\n */\n\n")) "\
The prologue to insert at the beginning of a file.")

(custom-autoload (quote vmstyle-file-prologue) "vmstyle" t)

(defvar vmstyle-header-prologue (quote ("#ifndef " protection "\n#define " protection "\n\n#define INCLUDE_ALLOW_XXX\n#include \"includeCheck.h\"\n\n")) "\
The prologue to insert at the beginning of header files.  The
symbol `protection' will be bound to the pre-processor symbol to
use to protect this header from multiple inclusion.")

(custom-autoload (quote vmstyle-header-prologue) "vmstyle" t)

(defvar vmstyle-header-epilogue (quote ("\n#endif // ifndef " protection "\n")) "\
The prologue to insert at the end of header files.  As for the
variable `vmstyle-header-prologue', the symbol `protection' will
be bound.")

(custom-autoload (quote vmstyle-header-epilogue) "vmstyle" t)

(autoload (quote vmstyle-prologue-dwim) "vmstyle" "\
Guess which type of prologue comment to insert.

This calls `vmstyle-function-prologue', `vmstyle-source-header',
or `vmstyle-header-prologue' depending on the context at point
and the file name of the current buffer.  If the appropriate
action can't be determined, this prompts the user via the
mini-buffer.

\(fn)" t nil)

(let ((loads (get (quote vmstyle-prologue-comment-style) (quote custom-loads)))) (if (member (quote "vmstyle") loads) nil (put (quote vmstyle-prologue-comment-style) (quote custom-loads) (cons (quote "vmstyle") loads))))

(defconst vmstyle-prologue-comment-function-or-file-re "^/\\*\n\\([- *]*\n\\)* \\* [^ ]+ --" "\
Value for `vmstyle-prologue-comment-begin-re' that recognizes
both function and file prologue comments.")

(defconst vmstyle-prologue-comment-function-re "^/\\*\n \\*--" "\
Value for `vmstyle-prologue-comment-begin-re' that recognizes
function prologue comments.")

(defvar vmstyle-prologue-comment-begin-re vmstyle-prologue-comment-function-or-file-re "\
Regexp for a comment that should be indented like a prologue comment.")

(custom-autoload (quote vmstyle-prologue-comment-begin-re) "vmstyle" t)

(defvar vmstyle-prologue-comment-continuation-re "^[ 	]*\\*?[ 	]*$" "\
Regexp for a blank comment line.")

(custom-autoload (quote vmstyle-prologue-comment-continuation-re) "vmstyle" t)

(defvar vmstyle-prologue-comment-section-re "^[ 	]*\\*\\(-\\| [^ ].*\\(--\\|:\\)[ 	]*$\\)" "\
Regexp for \"section headers\" in a prologue comment.

Section headers are only indented by one space, unlike the rest
of a prologue comment.  This is used to distinguish header
lines.")

(custom-autoload (quote vmstyle-prologue-comment-section-re) "vmstyle" t)

(defvar vmstyle-prologue-comment-default-indent " *      " "\
Default indentation string to use for non-header lines.

Usually, prologue comment indentation tries to find an example
non-header line in the current comment to copy the indentation
from, but if that fails, it falls back to this.")

(custom-autoload (quote vmstyle-prologue-comment-default-indent) "vmstyle" t)

(defvar vmstyle-enable-prologue-comment-indentation t "\
Whether or not to specialize indentation behavior in prologue comments.

If non-nil, the C indentation function will be tweaked so that
lines of a prologue comment are indented according to the style
guide.  This is most effective when combined with something like
filladapt so that both newly indented comment lines and wrapped
lines will be indented appropriately.")

(custom-autoload (quote vmstyle-enable-prologue-comment-indentation) "vmstyle" t)

(autoload (quote vmstyle-compute-prologue-comment-indent) "vmstyle" "\
Not documented

\(fn)" nil nil)

(defadvice c-indent-line (around indent-prologue-comment activate) (or (when vmstyle-enable-prologue-comment-indentation (let ((indent (vmstyle-compute-prologue-comment-indent))) (when indent (save-excursion (beginning-of-line) (let ((deleted 0)) (when (looking-at "[ 	]*\\*[ 	]*") (delete-region (match-beginning 0) (match-end 0)) (setq deleted (- (match-end 0) (match-beginning 0)))) (insert-before-markers indent) (- (length indent) deleted)))))) ad-do-it))

(unless (fboundp (quote c-save-buffer-state)) (ad-disable-advice (quote c-indent-line) (quote around) (quote indent-prologue-comment)))

(autoload (quote vmstyle-line-to-block-comment) "vmstyle" "\
Convert a block of line comments to a block comment.

If the transient mark is active, then this converts the comment
in the region.  Otherwise, point must be in a comment and this
will find the beginning and end of the comment block.

\(fn BEG END)" t nil)

(autoload (quote vmstyle-convert-line-comments) "vmstyle" "\
Convert all block line comments following point into block comments.

This will search for all two-or-more line blocks of line comments
after point.  For each comment, it will prompt for whether or not
to convert it into a block comment.

\(fn)" t nil)

;;;***

(provide 'vmstyle-load)
