;;; svnci.el --- interactive Subversion commit and status/diff/add

;; Copyright (C) 2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    02-May-2008
;; Version:    0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; svnci provides an interactive Emacs interface to Subversion commit
;; that integrates status, diff, and add; unifying the entire commit
;; process into one integrated interface.  This interface resembles
;; the standard svn commit message editing, but provides support for
;; interactively choosing which files to include or exclude from the
;; commit while editing the commit message, in addition to the
;; features provided by `svn-commit-mode' such as diff integration and
;; font locking.

;; The `svnci' command starts the commit process and initially
;; includes the same set of files that an 'svn commit' command would.
;; `svnci-this' also starts the commit process, but initially includes
;; only the file in the current buffer.  Either way, the user is free
;; to edit the list of included or excluded files before committing.
;; See the documentation for these two commands for more information.

;; With 'svn commit', svn drives the commit process and calls on an
;; editor simply to retrieve the log message.  svnci works the
;; opposite way.  In svnci, Emacs drives the commit process and only
;; calls on svn to perform the final commit operation.

;; To do
;; * Provide a shell script to make it possible to invoke svnci from
;;   the terminal in a way similar to 'svn commit'.
;; * Provide the option to revert checked-in files that don't have
;;   modifications after a check in, in order to update Id tags and
;;   the like.

;;; Code:

(eval-when-compile (require 'cl))
(require 'svn-commit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;

(defgroup svnci nil
  "Interactive Subversion status/diff/add/commit."
  :link '(custom-group-link svn-commit-mode))

(defconst svnci-omit-face 'svnci-omit-face)
(defface svnci-omit-face
  '((((supports :strike-through t)) :strike-through t))
  "Face for files that will be omitted from the check-in."
  :group 'svnci)

(defcustom svnci-commit-prompt t
  "Whether or not to prompt for confirmation before committing.

Set to nil to never prompt for confirmation.  Set to 'empty to
prompt only if the commit message is empty.  Set to t to always
prompt for confirmation before committing."
  :type '(choice
          (const :tag "Never prompt" nil)
          (const :tag "Prompt if commit message empty" empty)
          (const :tag "Always prompt" t))
  :group 'svnci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status lines and font lock
;;

(defconst svnci-select-info "--Select files to commit below--\n")
(defconst svnci-select-info-regexp (concat "^" svnci-select-info))

(defvar svnci-stat-line-regexp "\\([ ACDIMRX?!~]\\)[ CM][ L][ +][ S][ K] \\(.*\\)"
  "A regular expression that should match a status line in the
output of 'svn status'.  It must contain two groups.  The first
must match the status character and the second must match the
path.")

(defvar svnci-stat-line-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map svn-commit-stat-line-map)
    (define-key map " " #'svnci-toggle-commit)
    map)
  "Keymap used on svnci status lines.

This builds on the svn-commit-mode status line keymap to add
support for toggling the include/exclude status of files.")

(defvar svnci-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'svnci-commit)
    map)
  "Major mode keymap for svnci.")

(defvar svnci-font-lock-keywords
  `((,svnci-select-info-regexp . svn-commit-info-face))
  "Font lock keywords for `svnci-mode'.

Note that this will be made buffer-local and appended to when
entering svnci-mode in order to enable hyperlinks and status line
highlighting according to `svn-commit-status-faces'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message construction
;;

(defun svnci (&optional paths non-recursive)
  "Start an svn commit process.

This command constructs and visits a new svn commit message.
This commit message will contain status lines for all of the
files under the current directory, including unversioned files.
Initially, only the files that 'svn commit' would include will be
marked for inclusion in the commit.  If invoked with a prefix
argument (\\[universal-argument]), then this will prompt for which file or path to
include initially.

\\<svnci-stat-line-map>\
Files can be marked for inclusion or exclusion from the commit by
moving point to a file status line and pressing \\[svnci-toggle-commit].

\\<svnci-mode-map>\
Once ready to commit, use \\[svnci-commit] to commit the selected
files with the given commit message.

If `svn-commit-offer-to-restore' is non-nil, this first checks
for any existing svnci messages (for example, from failed
commits) and offers to restore them instead of constructing a new
message."
  (interactive
   (if (consp current-prefix-arg)
       (list (list (read-file-name "Default file/path: " nil "." t)))
     '()))

  ;; Check for modified buffers
  (let ((cnt 0))
    (dolist (b (buffer-list))
      (when (and (buffer-modified-p b)
                 (buffer-file-name b))
        (setq cnt (1+ cnt))))
    (when (> cnt 0)
      (unless (y-or-n-p (format "%d modified buffer%s.  Continue? "
                                cnt (if (= cnt 1) " exists" "s exist")))
        (error "svnci aborted"))))

  ;; Normalize the paths and enter the common ancestor directory
  (let* ((normalized (svnci-normalize-paths paths))
         (default-directory (car normalized))
         (paths (cdr normalized)))

    ;; Try to restore an old commit message
    (unless (svnci-maybe-load-old-commit-message)
      ;; Get the status before we construct the buffer so that the
      ;; user gets a nice error if something fails.
      (let ((status (svnci-compute-status paths non-recursive)))
        ;; Visit an unused message file, like svn commit
        (let ((num 0)
              (path "svnci-commit.tmp"))
          (while (file-exists-p path)
            (setq num (1+ num))
            (setq path (format "svnci-commit.%d.tmp" num)))
          ;; Visit that file, but don't enter any mode until we've set up
          ;; the message.  Note that the default directory of this file
          ;; will be the common ancestor of the paths.
          (let ((auto-mode-alist '()))
            (find-file path)))

        ;; Construct the message contents
        (save-excursion
          (svn-commit-without-modifying
           (setq status (sort status #'svnci-compare-statuses))
           (insert "\n" svnci-select-info "\n")
           (dolist (s status)
             (insert (if (first s) "*" " ") " " (second s) "\n"))))

        ;; Now actually enter the mode
        (let ((svn-commit-offer-to-restore nil))
          (svnci-mode))))))

(defun svnci-this ()
  "Like `svnci', but include only the currently visited file.

As an added bonus, if the current file is not under version
control, this first prompts to add the file."
  (interactive)

  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))

  ;; Find out if this file is unversioned or ignored
  (let ((status (svnci-get-status (list buffer-file-name) t nil)))
    (unless (= (length status) 1)
      (error "Failed to get status for %s" buffer-file-name))

    (let ((state (second (car status))))
      (case state
        ((??)
         (when (y-or-n-p "File is not under version control.  Add? ")
           (let ((file-name (file-name-nondirectory buffer-file-name)))
             (let ((svn-commit-offer-to-restore nil))
               (svnci))
             ;; Include this file and exclude everything else
             (save-excursion
               (svn-commit-without-modifying
                (re-search-forward svnci-select-info-regexp)
                (skip-chars-forward "\n")
                (while (progn
                         (beginning-of-line)
                         (looking-at (concat "^[* ] " svnci-stat-line-regexp)))
                  (let ((inhibit-read-only t))
                    (if (string= (match-string 2) file-name)
                        (insert "*")
                      (insert " "))
                    (delete-char 1))
                  (forward-line)))))))
        ((?I)
         (error "File is ignored."))
        (t
         (let ((svn-commit-offer-to-restore nil))
           (svnci (list buffer-file-name))))))))

(defun svnci-normalize-paths (paths)
  "Normalizes a set of paths into the lowest common ancestor
directory and a new set of paths relative to that ancestor that
are guaranteed to be below the ancestor.  If no paths are given,
assumes the current directory.  This is meant to simulate the
normalization operation performed by 'svn commit' and to
normalize the paths seen in the output of 'svn status'."
  
  (unless paths
    (setq paths '(".")))

  (let ((paths (save-match-data
                 (mapcar (lambda (p)
                           (let ((p (if (file-directory-p p)
                                        (file-name-as-directory p)
                                      p)))
                             (split-string p "/" nil)))
                         (mapcar #'expand-file-name paths))))
        (leading nil)
        (same t))

    ;; Strip off leading components that are the same between all
    ;; paths, up to the last component, which will be either a file
    ;; name for a file or "" for a directory.
    (while same
      (dolist (path paths)
        (when (or (null (cdr path))
                  (not (string= (car path) (caar paths))))
          (setq same nil)))
      (when same
        (setq leading (cons (caar paths) leading))
        (setq paths (mapcar #'cdr paths))))

    ;; Reconstruct the paths
    (let ((res-leading
           ;; Put slashes after every element.  This will create an
           ;; absolute directory name because the first element will
           ;; always be "" and because this will place a slash on the
           ;; end.
           (with-output-to-string
             (dolist (e (nreverse leading))
               (princ e) (princ "/"))))
          (res-paths
           (mapcar
            ;; Put slashes between every element.  For directories,
            ;; this will create a directory name because the last
            ;; element will be "" and otherwise this will create a
            ;; file name.  The result will always be a relative path
            ;; because these cannot begin with "", except in the one
            ;; case where "" is the _only_ path element (indicating a
            ;; directory), which we handle specially.
            (lambda (path)
              (if (equal path '(""))
                  "."
                (with-output-to-string
                  (princ (car path))
                  (dolist (e (cdr path))
                    (princ "/") (princ e)))))
            paths)))
      (cons res-leading res-paths))))

(defun svnci-maybe-load-old-commit-message ()
  "Possibly restore an old commit message, returning t if and
only if a restore was performed.  The restored buffer will be
put in svnci-mode."

  (when svn-commit-offer-to-restore
    ;; Find an old message
    (let ((old-msg (svn-find-old-commit-message "svnci-commit")))
      (when old-msg
        ;; Prompt to restore
        (if (save-window-excursion
              (when svn-commit-show-old-message
                (let ((auto-mode-alist '()))
                  (find-file old-msg))
                (let ((svn-commit-offer-to-restore nil))
                  (svnci-mode)))
              (y-or-n-p (format "Old commit message found in %s.  Restore? "
                                old-msg)))
            ;; Find the old message for real and return t
            (progn (find-file old-msg) t)
          ;; Kill the old message if we previewed it and return nil
          (let ((buf (get-file-buffer old-msg))
                (svnci-inhibit-kill-query t))
            (kill-buffer buf))
          nil)))))

(defun svnci-compute-status (paths non-recursive)
  "Compute the status information that will be displayed in the
commit message, including the default include/omit state.  PATHS
and NON-RECURSIVE are passed directly to 'svn status' (see
`svnci-get-status').  Returns a list of status items, each of
which has the following form

  (INCLUDE STATUS-LINE STATUS-FLAG PATH)

where INCLUDE is non-nil if the path should be included in the
set of commit files by default.  See `svnci-get-status' for the
meaning of the remaining fields."

  (prog2
    ;; Let the user know what we're up to
    (message "Retrieving svn status...")
    (let* ((commitable (svnci-get-status paths non-recursive t))
           (all (svnci-get-status nil (and (null paths) non-recursive) nil))
           (rest (copy-sequence all)))
      ;; Missing items are ignored by commit, even if explicitly
      ;; passed.  Obstructed paths are ignored by commit by default
      ;; and just behave really, really strangely if explicitly
      ;; passed.  Nuke both from the commitable list.
      (let (new-commitable)
        (dolist (c commitable)
          (unless (or (= (second c) ?!) (= (second c) ?~))
            (setq new-commitable (cons c new-commitable))))
        (setq commitable (nreverse new-commitable)))

      ;; Find specified paths that aren't commitable.  This tries to
      ;; emulate the behavior of 'svn commit', but isn't quite right.
      ;; For example, svn commit complains if explicitly passed an
      ;; ignored file, but with svn status, it is very difficult to
      ;; distinguish ignored files from files that simply have no
      ;; changes.
      (dolist (c commitable)
        (delete (third c) paths))
      (let ((rest-paths (mapcar #'third all)))
        (dolist (c commitable)
          (delete (third c) rest-paths))
        (dolist (p paths)
          ;; Paths that don't satisfy this predicate are either
          ;; unchanged or ignored and we can't tell the difference
          ;; without asking more questions.
          (when (or (member p rest-paths)
                    (not (file-exists-p p)))
            (error "svn: '%s' is not under version control" p))))

      ;; Add the include state to each status entry in all
      (mapcar (lambda (s) (cons (member s commitable) s)) all))
    ;; All done
    (message "Retrieving svn status... Done")))

(defun svnci-get-status (paths non-recursive quiet)
  "Execute 'svn status' and parse its output.  NON-RECURSIVE and
QUIET are passed as arguments to status and externals are always
ignored.  Returns a list of status items, each of which has the
form

 (STATUS-LINE STATUS-FLAG PATH)

where STATUS-LINE is the text of the original line, STATUS-FLAG
is the character indicating the line's status, and PATH is the
file path."

 (with-temp-buffer
    ;; We ignore externals because 'svn commit' also does.
    (let* ((args (list* "--non-interactive" "--ignore-externals" paths))
           (args (if non-recursive (cons "-N" args) args))
           (args (if quiet (cons "-q" args) args))
           (res  (apply #'call-process "svn" nil t nil "status" args)))
      ;; XXX It probably printed an error message
      (cond ((stringp res)
             (error "svn status exited with %s" res))
            ((/= res 0)
             (error "svn status exited with code %d" res))))

    ;; Parse the output
    (let (lines)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring (point) (line-end-position))))
          (cond ((looking-at "svn: ")
                 (error "%s" line))
                ((looking-at svnci-stat-line-regexp)
                 (setq lines (cons (list (match-string 0)
                                         (aref (match-string 1) 0)
                                         (match-string 2))
                                   lines)))
                (t
                 (error "Unknown status line: %s" line))))
        (forward-line))
      (nreverse lines))))

(defun svnci-compare-statuses (a b)
  "Compare two statuses, as returned by `svnci-compute-status',
for sorting.  The sorting order is somewhat complicated, but
matches intuition.  The statuses are primarily sorted according
to the path hierarchy.  Within each directory, the entries are
sorted first by status, where externals and unversioned files
come after other files, then alphabetically."

  (flet ((rank (status)
           (case status
             ((?X) 1)
             ((??) 2)
             (t    0))))
    (let ((a-rank (rank (third a)))
          (a-path (split-string (fourth a) "/"))
          (b-rank (rank (third b)))
          (b-path (split-string (fourth b) "/")))
      (flet ((compare (p1 p2)
               (cond
                ;; Parent directories come before sub-directories
                ((null p1) t)
                ((null p2) nil)
                ;; Group by directory hierarchy
                ((string= (car p1) (car p2))
                 (compare (cdr p1) (cdr p2)))
                ;; Sort within a directory by rank
                ((/= a-rank b-rank)
                 (< a-rank b-rank))
                ;; And alphabetically within a rank
                (t (string< (car p1) (car p2))))))
        (compare a-path b-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interaction
;;

(defun svnci-toggle-commit ()
  "Toggle whether the status line containing point should be
included or omitted.  If point is not on a status line, does
nothing.  If the status line is for a directory, includes/omits
the children as well."
  (interactive)

  (let ((stat-line (concat "[ *] " svnci-stat-line-regexp)))
    (save-match-data
      ;; Are we on a status line?
      (when (save-excursion (beginning-of-line) (looking-at stat-line))
        (let ((adding (= (char-after (line-beginning-position)) ? ))
              (status (aref (match-string 1) 0))
              (path (match-string 2)))

          (flet ((set (v)
                   ;; Select or deselect the current line and move to
                   ;; the next line
                   (save-excursion
                     (beginning-of-line)
                     (let ((inhibit-read-only t))
                       (if v (insert "*") (insert " "))
                       (delete-char 1)))
                   (forward-line))
                 (map-children (f)
                   ;; Apply f on each line with a path that is a child
                   ;; of `path', starting with the current line.
                   ;; Stops as soon as it encounters a line that is
                   ;; not a child of `path'.
                   (let* ((prefix (concat path "/"))
                          (prefix-len (length prefix)))
                     (while (and (save-excursion
                                   (beginning-of-line)
                                   (looking-at stat-line))
                                 (eq (compare-strings prefix 0 nil
                                                      (match-string 2)
                                                      0 prefix-len)
                                     t))
                       (save-excursion
                         (funcall f))
                       (forward-line)))))

            (if (file-directory-p path)
                ;; Deal with children of this directory
                (case status
                  ((?A)
                   (if adding
                       (let (prompted)
                         (set t)
                         ;; Possibly include the children, too
                         (catch 'cancelled
                           (map-children
                            (lambda ()
                              (unless prompted
                                (if (y-or-n-p "Also include sub-files? ")
                                    (setq prompted t)
                                  (throw 'cancelled nil)))
                              (set t)))))
                     ;; Always omit the children, since they depend on
                     ;; adding the parent
                     (set nil)
                     (map-children (lambda () (set nil)))))
                  ((?D)
                   (if adding
                       ;; Always include the children, since deleting
                       ;; the parent depends on deleting them
                       (progn
                         (set t)
                         (map-children (lambda () (set t))))
                     ;; We don't technically have to omit the
                     ;; sub-files
                     (set nil)
                     (map-children (lambda () (set nil)))))
                  ((?X)
                   ;; XXX This could be worked around.  The real
                   ;; problem is that we told status to ignore
                   ;; externals, so we don't even have the files from
                   ;; this in the list.  We could, for example,
                   ;; dynamically add them at this point.
                   (message "Cannot include external directory"))
                  ((??)
                   ;; XXX Same thing
                   (message "Cannot include unversioned directory"))
                  (t (set adding)))

              ;; Include/omit file
              ;; XXX Technically, we should make sure any parent
              ;; directories of this file that appear in the list are
              ;; included, but that gets really messy.
              (case status
                ((??)
                 (if adding
                     (when (y-or-n-p "Add file? ")
                       (set t))
                   (set nil)))
                (t
                 (set adding))))))))))

(defvar svnci-inhibit-kill-query nil
  "If non-nil, allow svnci message buffers to be killed without
prompting.  This is intended to be set temporarily when such a
buffer needs to be killed programmatically.")

(defun svnci-kill-buffer-query ()
  "Prompt before killing an uncommitted svnci message buffer."

  (or svnci-inhibit-kill-query
      (not svnci-mode)
      (y-or-n-p "Kill SVN message buffer without committing? ")))

(defun svnci-kill-emacs-query ()
  "Prompt before killing Emacs when there are uncommitted svnci
message buffers."

  (let (exist)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when svnci-mode
          (setq exist t))))
    (if exist
        (yes-or-no-p "Uncommitted SVN messages exist; exit anyway? ")
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit
;;

(defun svnci-commit-preview ()
  "Preview what would be done in an `svnci-commit'.  Does
everything except actually executing the svn commands.  Meant
primarily for debugging and paranoia."
  (interactive)

  (svnci-commit svnci-commit-prompt t))

(defun svnci-commit (&optional prompt preview)
  "Perform an svn commit, based on the current svnci message buffer.

This parses the current buffer to find the commit message, the
set of files that should be added, and the set of files that
should be committed and makes the appropriate 'svn add' and 'svn
commit' calls.

PROMPT controls under which conditions confirmation is required
before the commit is actually performed.  If nil, confirmation is
never required.  If 'empty, confirmation is only required if the
commit message consists only of whitespace.  If any other value,
confirmation is always required.  When called interactively, the
value of PROMPT defaults to `svnci-commit-prompt'.

PREVIEW controls whether or not this is actually a dry-run.  See
`svnci-commit-preview' for more details."
  (interactive (list svnci-commit-prompt))

  ;; Save the message in order to perform automatic clean up and so we
  ;; can retrieve it if the commit fails
  (save-buffer)
  ;; Parse the file
  (let (message-empty message-end message-file to-add to-commit
                      need-recursive need-non-recursive)
    (save-excursion
      ;; Get the commit message
      (goto-char (point-min))
      (skip-chars-forward " \n\t")
      (when (looking-at svnci-select-info-regexp)
        (setq message-empty t))
      (unless (re-search-forward svnci-select-info-regexp nil t)
        (error "Unable to find list of files to commit"))
      (setq message-end (match-beginning 0))

      ;; Get the files to add and to commit
      (skip-chars-forward "\n")
      (while (looking-at (concat "[ *] " svnci-stat-line-regexp))
        (when (= (char-after) ?*)
          (let ((status (aref (match-string 1) 0))
                (path (match-string 2)))
            (setq to-commit (cons path to-commit))
            (when (= (aref (match-string 1) 0) ??)
              (setq to-add (cons path to-add)))
            (cond ((and (= status ?D) (file-directory-p path))
                   (setq need-recursive t)
                   ;; XXX Check that all children are being removed,
                   ;; too, and don't including them in to-commit.
                   )
                  ((and (= status ?A) (file-directory-p path))
                   ;; XXX This is conservative.  If all of the
                   ;; children are also selected for addition, then
                   ;; we don't need non-recursive.
                   (setq need-non-recursive t)))))
        (end-of-line)
        (skip-chars-forward "\n"))
      (unless (eobp)
        (error "Unable to parse list of files to commit"))
      (when (and need-recursive need-non-recursive)
        ;; XXX Update this message if I make the add check less
        ;; conservative.
        (error "Cannot add a directory and delete a directory in one commit")))

    ;; Final check with the user
    (when prompt
      (if message-empty
          (unless (y-or-n-p "Empty commit message.  Really commit? ")
            (error "Commit aborted"))
        (when (not (eq prompt 'empty))
          (unless (y-or-n-p "Really commit? ")
            (error "Commit aborted")))))

    ;; Save the message to a temporary file
    (setq message-file (make-temp-file "svnci"))
    (write-region (point-min) message-end message-file nil 0)

    ;; Create commit buffer
    (let ((original-file buffer-file-name))
      (pop-to-buffer "*svn commit*")
      (erase-buffer)

      ;; Construct a plist of information that the commit process will
      ;; need.  We carry this information carefully in process plists
      ;; so that, even if the commit buffer goes away, we can still
      ;; access it.
      (let ((plist `(svnci-to-add ,to-add
                     svnci-to-commit ,to-commit
                     ;; Be paranoid and try to exert as much control
                     ;; over the commit as possible
                     svnci-non-recursive ,(not need-recursive)
                     svnci-message-file ,message-file
                     svnci-original-file ,original-file
                     svnci-output ,(current-buffer)
                     svnci-window ,(selected-window)
                     svnci-preview ,preview)))
        ;; Start the commit process by adding files
        (svnci-commit-add-files plist)))))

(defun svnci-commit-add-files (plist)
  "Helper for `svnci-commit' that starts the 'svn add' process.

If the set of files to add is empty, simply calls
`svnci-commit-commit'."

  (let ((svnci-to-add (plist-get plist 'svnci-to-add)))
    (if (null svnci-to-add)
        (svnci-commit-commit plist)
      (svnci-commit-execute "svn add"
                            (list* "svn" "add" "-N" "--" svnci-to-add)
                            #'svnci-commit-add-sentinel
                            plist))))

(defun svnci-commit-add-sentinel (process event)
  "'svn add' process sentinel.

If the add process succeeds, this starts the commit process."

  (let ((exit-status (process-exit-status process)))
    (if (and (eq (process-status process) 'exit) (= exit-status 0))
        (with-current-buffer (process-buffer process)
          (svnci-commit-commit (process-plist process)))
      (message "svn add %s" event)
      (when noninteractive
        (kill-emacs (if (= exit-status 0) 1 exit-status))))))

(defun svnci-commit-commit (plist)
  "Helper for `svnci-commit' that starts the 'svn commit' process."

  (let ((svnci-to-commit (plist-get plist 'svnci-to-commit))
        (svnci-non-recursive (plist-get plist 'svnci-non-recursive))
        (svnci-message-file (plist-get plist 'svnci-message-file)))
    (unless (null svnci-to-commit)
      (let* ((cmd (list* "-F" svnci-message-file "--non-interactive"
                         "--" svnci-to-commit))
             (cmd (if svnci-non-recursive (cons "-N" cmd) cmd))
             (cmd (list* "svn" "commit" cmd)))
        (svnci-commit-execute "svn commit" cmd
                              #'svnci-commit-commit-sentinel
                              plist)))))

(defun svnci-commit-commit-sentinel (process event)
  "'svn commit' process sentinel.

If the commit process succeeds, this deletes the commit message
buffer and the message file, cleans up the window configuration,
and buries the process output buffer."

  (let* ((exit-status (process-exit-status process))
         (plist (process-plist process))
         (svnci-preview (plist-get plist 'svnci-preview)))
    (if (and (eq (process-status process) 'exit) (= exit-status 0)
             (not svnci-preview))
        (progn
          ;; Commit succeeded
          (let ((svnci-message-file (plist-get plist 'svnci-message-file))
                (svnci-original-file (plist-get plist 'svnci-original-file))
                (svnci-window (plist-get plist 'svnci-window)))
            ;; Kill the buffer and delete the file containing the
            ;; original commit message as well as the trimmed commit
            ;; message
            (delete-file svnci-message-file)
            (let ((msg-buf (find-buffer-visiting svnci-original-file)))
              (when msg-buf
                ;; Now that we've committed, don't complain that we're
                ;; killing an uncommitted message
                (let ((svnci-inhibit-kill-query t))
                  (kill-buffer msg-buf))))
            (delete-file svnci-original-file)
            ;; If we're in batch mode, then we're done
            (when noninteractive
              (kill-emacs))
            ;; Hide the commit window
            (when (and svnci-window
                       (window-live-p svnci-window))
              (delete-window svnci-window))
            ;; Bury the commit buffer (we keep it around in case the
            ;; user wants to look at it)
            (let ((buf (process-buffer process)))
              (when (buffer-live-p buf) (bury-buffer buf)))
            ;; And tout our success.
            (message "Commit succeeded")))
      ;; Admit to our failure
      (message "svn commit %s" event)
      (when noninteractive
        (kill-emacs (if (= exit-status 0) 1 exit-status))))))

(defun svnci-commit-execute (name cmd sentinel plist)
  "`svnci-commit' helper to start a new process."

  (let* ((svnci-output (plist-get plist 'svnci-output))
         (svnci-preview (plist-get plist 'svnci-preview))
         (standard-output svnci-output))
    (if svnci-preview
        (princ "Would execute")
      (princ "Executing"))
    (dolist (e cmd) (princ " ") (princ e))
    (terpri)
    (when svnci-preview
      (setq cmd (cons "true" cmd)))
    (let ((process (apply #'start-process name (current-buffer) cmd)))
      (set-process-plist process plist)
      (set-process-filter process #'svnci-commit-filter)
      (set-process-sentinel process sentinel))))

(defun svnci-commit-filter (process output)
  "`svnci-commit' process filter.

This is very similar to the standard process filter, but will
redirect process output to the terminal if run in batch mode."

  (with-current-buffer (process-buffer process)
    (let* ((svnci-output (plist-get (process-plist process) 'svnci-output))
           (standard-output svnci-output)
           (moving (= (point) (process-mark process))))
      (save-excursion
        (goto-char (process-mark process))
        (princ output)
        (set-marker (process-mark process) (point)))
      (if moving (goto-char (process-mark process))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode
;;

(defvar svnci-mode nil
  "Non-nil if this buffer is in svnci-mode.")
(make-variable-buffer-local 'svnci-mode)

(define-derived-mode svnci-mode svn-commit-mode "svnci"
  "Major mode for editing svnci commit messages.

In general, you should not use this directly.  Instead, use
`svnci' or `svnci-this' to construct or restore a commit
message."

  (setq svnci-mode t)

  ;; Update the font lock keywords with the status faces and the
  ;; necessary text properties to make hyperlinks and omit toggling
  ;; work
  (let ((new-keywords
         (mapcar (lambda (re-face)
                   `(,(concat "^\\([ *] \\)" (first re-face)
                              "[ L][ +][ S][ K] \\(.*\\)")
                     (0 '(face ,(second re-face)
                          category svn-commit-stat-line))
                     (2 '(face svn-commit-path-face
                          svn-commit-path t) prepend)
                     ,@(when (face-differs-from-default-p svnci-omit-face)
                         ;; If the omit face actually distinguishes
                         ;; omitted files, then hide the stars and
                         ;; just use the face.
                         `((1 '(face default invisible t))
                           ("^ .*" (goto-char (match-beginning 0)) nil
                            (0 svnci-omit-face prepend))))))
                 svn-commit-status-faces)))
    (set (make-local-variable 'svnci-font-lock-keywords)
         (append svnci-font-lock-keywords new-keywords)))

  ;; Enable font lock
  (set (make-local-variable 'font-lock-defaults)
       '(svnci-font-lock-keywords nil nil nil nil))

  ;; Make svn-commit-mode's diff work
  (set (make-local-variable 'svn-commit-stat-line-regexp)
       (concat "[ *] " svnci-stat-line-regexp))

  ;; Activate the hyperlinks
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward svnci-select-info-regexp nil t)
      (let ((ov (make-overlay (point) (point-max) nil nil t)))
        (overlay-put ov 'keymap svnci-stat-line-map)
        (overlay-put ov 'svn-commit-msg (lambda (obj pos)
                       (substitute-command-keys "\\<svnci-stat-line-map>\
\\[svn-commit-mouse-visit-diff], \\[svn-commit-visit-diff]: visit file/diff; \
\\[svnci-toggle-commit]: include/omit file"))))))

  ;; Fix paragraph filling
  (make-variable-buffer-local 'paragraph-separate)
  (setq paragraph-separate
        (concat svnci-select-info-regexp "\\|" paragraph-separate))

  ;; Make the info block immutable
  (svn-commit-immutate svnci-select-info-regexp)

  ;; Warn the user before killing the buffer
  (set (make-local-variable 'kill-buffer-query-functions)
       (cons #'svnci-kill-buffer-query kill-buffer-query-functions))

  ;; Warn the user before exiting if there are messages
  (add-to-list 'kill-emacs-query-functions
               #'svnci-kill-emacs-query t))

;; Disable flyspell in the information block
(put 'svnci-mode 'flyspell-mode-predicate 'svnci-flyspell-verify)
(defun svnci-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in
svnci-mode.  Causes flyspell to ignore everything in the info
block."
  (save-excursion
    (save-match-data
      (re-search-forward svnci-select-info-regexp nil t))))

(provide 'svnci)
