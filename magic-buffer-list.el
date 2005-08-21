;;; magic-buffer-list.el --- highly interactive buffer list

;; Copyright (C) 2005 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    02-Aug-2005
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

;; * Have a summary, either at the bottom line of the buffer or in the
;;   mode line (custom mode line format), of the current
;;   grouping/limiting
;;
;; * Have a bunch of functions (including user-defined ones), that,
;;   given a list of buffers, return an alist associating each buffer
;;   with a group value and a priority.  Buffers with equal group
;;   values are grouped together and visibly displayed under that
;;   label, sorted by their priority.
;;
;; * Perhaps the grouping info mechanism should be the same as the
;;   mechanism that gets information for columns?  The semantics are
;;   very similar, except that grouping should have way for
;;   prioritizing the groups and the buffers within each group and
;;   columns should always return printable information, but groups
;;   can be over any comparable object.
;; ** Let information functions specify whether they can be used for
;;    either or both?  Give reasonable defaults for grouping
;;    priorities if they are not returned?  Though, if reasonable
;;    defaults are given, then there's no reason to differentiate the
;;    interfaces at all.  Perhaps, then, this is just a user
;;    convenience of "you probably don't want to group by this" (ie,
;;    you probably don't want to group by buffer size)
;; ** This brings up another good point.  What about continuous
;;    things?  It may be useful to group things like size for easier
;;    navigation, and then sort within the groups by value.  The
;;    groupings wouldn't be known until all of the values were known
;;    (though if the info getters are handed all of the buffers at
;;    once, not just one at a time, the size info getter could do
;;    this)
;;
;; * Ultimately, produce something that looks like
;;   ((<buffer> (directory :group "/foo"
;;                         :group-priority 1
;;                         :value "/foo"
;;                         :priority 1) ...)
;;    (...))
;;   where :group specifies the value to group by, :group-priority
;;   specifies the priority of the group (the actual priority will be
;;   the min of the group priorities over the entries in a group),
;;   :value specifies the value to display in the buffer list (either
;;   in the column, or at the group header), and :priority specifies
;;   the priority of this buffer within the buffers with eq :group.
;;   If :group is omitted, the list cannot be grouped by this
;;   property.  If :group is not omitted but :group-priority is,
;;   :group-priority defaults to :priority.  If :value is omitted,
;;   columns cannot use this property (and group headings will be the
;;   value of :group).  If :priority is omitted, it defaults to the
;;   index of this buffer in the buffer ring.
;;
;; * Have a function that, given an abstract spec of what the buffer
;;   list should contain, produces the buffer list buffer.
;;   Interaction with the buffer list should use the information in
;;   this specification.
;;
;; * Probably transform this into another spec that's just for how to
;;   render a buffer list.
;;
;; * And, of course, allow isearch (perhaps implicitly and put the
;;   control keys on the control key?)
;;
;; Interesting fields
;; * Buffer major mode
;; * Directory
;; * File name sans extension
;; * Directory and file name sans extension
;; * Extension
;; * How recently viewed
;; * When viewed (ie, group by day)
;;
;; * Instead of having this code drive low-level user code, what about
;;   moving the user code injection point higher?  Provide a
;;   convenient way for user code to find out information about
;;   buffers, an easy way to render the buffer list, and a mechanism
;;   for triggering on certain events.  Thus, it's more like the user
;;   code is driving the magic buffer list, and deferring to the magic
;;   buffer list's event loop.
;; ** Pass the user code either just a list of buffers it should work
;;    with and provide another function that takes a buffer and the
;;    name of a property and returns the value of that property (less
;;    sprawling than having lots of little getters, and easier for the
;;    user code to support data-drivenness)
;; ** Pass the user code a list of buffers that already have all sorts
;;    of available information associated with them.  There's probably
;;    no advantage to this over the previous approach
;; ** The user code returns a render spec that includes callback
;;    information for the event system
;; *** For related buffer highlighting, this information could either
;;     be precomputed and placed in the render spec, or the ability
;;     could be provided as one of the callbacks.
;; **** But shouldn't related buffer highlighting be orthogonal to the
;;      method that produces the current rendering?  It's more like a
;;      way of giving a preview of what another (filtering) view would
;;      contain
;; **** Perhaps parameterized filters are the proper way of
;;      implementing related buffer searching?  The related buffer
;;      view still needs some view driver to group things together
;; ** For example, it should be easy to implement something that
;;    recursively groups buffers by directory components, including
;;    things like collapsing elements that contain only a single
;;    subelement into one element, but there's no clear way to do this
;;    with the purely data-driven model
;; *** Plus, this is where what magic-buffer-list should be diverges
;;     from what scary things like ee represent
;;
;; * Compute adaptive buffer relatedness by watching how the user
;;   switches between buffers.  For every ordered pair (A,B) of
;;   buffers, track the frequency with which the user switches from A
;;   to B given the chance to switch to B instead of switching from A
;;   to some buffer other than A or B.  Possibly weight frequency
;;   affects by recency of switches.
;; ** The frequency information should be gathered by either a timer
;;    or a buffer switching hook if there's one built-in.
;; *** Hmm, does this have odd effects if the user switches
;;     windows/frames?  Perhaps only in-window switches should be
;;     considered (watch for current-buffer change without a
;;     selected-window change)
;; ** Compute second-order entropy over the buffer sequence (ie,
;;    first-order over buffer switches) with a time decay, accounting
;;    for the changing event space (as buffers are created and
;;    killed).  Use a genetic algorithm to evolve a good one.  Sort
;;    the buffers by the number of bits of information associated with
;;    the event of switching from the current buffer to the new
;;    buffer.
;;
;; * Preview buffer option.  Show the buffer that would be switched to
;;   when the selection is changed.  Revert to the original buffer if
;;   canceled
;; * Autoselect next buffer option.  When popping up, automatically
;;   select the next buffer instead of the current buffer.

;;; Code:

(defun magic-buffer-list-reload ()
  (interactive)
  (if (featurep 'magic-buffer-list)
      (unload-feature 'magic-buffer-list t))
  (load-file "~/sys/elisp/magic-buffer-list.el"))

(require 'cl)

(defvar magic-buffer-list-ignore-re "^ ")

(defconst magic-buffer-list-buffer-name " *Magic Buffer List*")

(defun magic-buffer-list ()
  (interactive)
  (magic-buffer-list-show-view
   nil
   (current-buffer)
   (car magic-buffer-list-view-sequence))
  ;; XXX Do this better
  (message
   "RET selects buffer, q buries list, p pivots, TAB jumps to next group"))

;;
;; Views
;;

(defvar magic-buffer-list-view-basic-flags
  '(concat
    (if modified "*" " ")
    (if read-only "%" " ")
    (if visible "V" " ")))

(defvar magic-buffer-list-view-basic-info
  '(concat
    (25 (concat (repeat indent-level "  ") name) :trim "..")
    " "
    (4 size-string :align right)
    " "
    (reeval magic-buffer-list-view-basic-flags)))

(defvar magic-buffer-list-view-basic-group
  '(concat
    (repeat indent-level "  ")
    title))

(defvar magic-buffer-list-views
  `((t
     :sorter ,#'magic-buffer-list-view-sort-mru
     :grouper ,#'magic-buffer-list-view-group-by-none
     :format (" " (reeval magic-buffer-list-view-basic-info))
     :group-format (" " (reeval magic-buffer-list-view-basic-group)))
    (group-by-major-mode
     :grouper ,#'magic-buffer-list-view-group-by-major-mode
     :format (" "
              (reeval magic-buffer-list-view-basic-info)
              " "
              (-39 filename-dimmed :align left :trim ".."
                   :trim-align right)))
    (group-by-directory
     :grouper ,#'magic-buffer-list-view-group-by-directory
     :format (" "
              (25 (concat (repeat indent-level "  ")
                          (if filename
                              filename-sans-directory
                            name)))
              " "
              (4 size-string :align right)
              " "
              (reeval magic-buffer-list-view-basic-flags)
              " "
              (15 major-mode :trim "..")
              " "
              filename-sans-directory))))

(defvar magic-buffer-list-view-sequence
  '(group-by-major-mode group-by-directory))

(defun magic-buffer-list-view-sort-mru (buffers)
  buffers)

(defun magic-buffer-list-view-sort-alphabetical (buffers)
  (sort buffers
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun magic-buffer-list-view-group-by-none (buffers)
  (mapcar (lambda (buffer) `(buffer ,buffer)) buffers))

(defun magic-buffer-list-view-group-by-major-mode (buffers)
  (magic-buffer-list-coalesce-groups
   (mapcar
    (lambda (buffer)
      `(group (:title ,(magic-buffer-list-get 'major-mode buffer))
              (buffer ,buffer)))
    buffers)))

(defun magic-buffer-list-view-group-by-directory (buffers)
  (let (directoried nondirectoried)
    (dolist (buffer buffers)
      (let ((directory (magic-buffer-list-get 'directory buffer)))
        (if (null directory)
            (push buffer nondirectoried)
          (let* ((dirlist-almost (split-string directory "/"))
                 (dirlist
                  (cons (if (equal (substring directory 0 1) "/")
                            (concat "/" (car dirlist-almost))
                          (car dirlist-almost))
                        (cdr dirlist-almost)))
                 (group `(buffer ,buffer)))
            (dolist (dir (reverse dirlist))
              (setq group `(group (:title ,(concat dir "/")) ,group)))
            (push group directoried)))))
    (append
     (magic-buffer-list-swivel-groups
      (magic-buffer-list-collapse-singleton-groups
       (magic-buffer-list-coalesce-groups (reverse directoried))
       "")
      t)
     `((group (:title "(no directory)")
              ,@(mapcar (lambda (buffer) `(buffer ,buffer))
                        (reverse nondirectoried)))))))

(defun magic-buffer-list-get-buffers ()
  (remove-if (lambda (buffer)
               (let ((name (buffer-name buffer)))
                 (or (string-match magic-buffer-list-ignore-re name)
                     (string= name magic-buffer-list-buffer-name))))
             (buffer-list)))

(defun magic-buffer-list-show-view (view-name &optional
                                              select-buffer
                                              default-view-name)
  (let ((buffer (get-buffer-create magic-buffer-list-buffer-name)))
    (save-excursion
      (set-buffer buffer)
      (let* ((view-name (or view-name
                            (if (boundp
                                 'magic-buffer-list-current-view)
                                magic-buffer-list-current-view)
                            default-view-name))
             (view
              (cdr-safe (assq view-name magic-buffer-list-views)))
             (default-view
              (cdr-safe (assq t magic-buffer-list-views))))
        (if (null view)
            (error "Unknown view: %s" view-name))
        (flet ((view-get
                (prop)
                (if (plist-member view prop)
                    (plist-get view prop)
                  (plist-get default-view prop))))
          (let ((sorter (view-get :sorter))
                (grouper (view-get :grouper))
                (buffer-format (view-get :format))
                (group-format (view-get :group-format)))
            (magic-buffer-list-mode)
            (let* ((buffers (magic-buffer-list-get-buffers))
                   (sorted-buffers (if sorter
                                       (funcall sorter buffers)
                                     buffers))
                   (spec (funcall grouper sorted-buffers)))
              (magic-buffer-list-render spec
                                        buffer-format
                                        group-format))))
        (make-local-variable 'magic-buffer-list-current-view)
        (setq magic-buffer-list-current-view view-name)))
    ;; Now that we're done building the view and have unexcursed, pop
    ;; up the buffer
    (magic-buffer-list-pop-up buffer)
    ;; and select the right buffer in the list
    (magic-buffer-list-point-to-buffer select-buffer)))

;;
;; Getters
;;

(defun magic-buffer-list-get (property &optional buffer)
  "Used for column values, and recommended for use by view builders
for consistency.  In the future, this may employ optimizations such as
caching."
  (let ((getter (intern
                 (concat (symbol-name magic-buffer-list-getter-prefix)
                         "-"
                         (symbol-name property)))))
    (if (functionp getter)
        (with-current-buffer (or buffer (current-buffer))
          (funcall getter))
      (error "No such property: %s" property))))

(defvar magic-buffer-list-getter-prefix 'magic-buffer-list-getter)

(defun magic-buffer-list-getter-name ()
  (buffer-name))
(defun magic-buffer-list-getter-filename ()
  (let ((filename (buffer-file-name buffer)))
    (when filename (abbreviate-file-name filename))))
(defun magic-buffer-list-getter-filename-dimmed ()
  (let ((directory (magic-buffer-list-get 'directory))
        (filename (magic-buffer-list-get 'filename-sans-directory)))
    (when (and directory filename)
      (concat (propertize directory 'face '((:foreground "gray")))
              filename))))
(defun magic-buffer-list-getter-directory ()
  (let ((filename (magic-buffer-list-get 'filename buffer)))
    (when filename (file-name-directory filename))))
(defun magic-buffer-list-getter-filename-sans-directory ()
  (let ((filename (magic-buffer-list-get 'filename buffer)))
    (when filename (file-name-nondirectory filename))))
(defun magic-buffer-list-getter-major-mode ()
  mode-name)
(defun magic-buffer-list-getter-size ()
  (buffer-size))
(defun magic-buffer-list-sizify (size)
  ;; XXX Yeah, I know Emacs can't represent sizes bigger than 134M
  (if (and (<= size 9999) nil)
      (number-to-string size)
    (let* ((postfixes (list "B" "K" "M" "G" "T" "P"))
           (last-postfix (car (last postfixes)))
           (divider 1000.0)
           (power 0))
      (catch 'done
        (dolist (postfix postfixes)
          (let ((new-size (/ size (expt divider power))))
            (cond ((and (< new-size 10) (= power 0))
                   (throw 'done
                          (concat (number-to-string (floor new-size))
                                  postfix)))
                  ((< new-size 10)
                   (throw 'done
                          (concat (number-to-string
                                   (/ (ffloor (* new-size
                                                        10))
                                             10))
                                  postfix)))
                  ((or (< new-size 1000)
                       (equal postfix last-postfix))
                   (throw 'done (concat (number-to-string
                                         (floor new-size))
                                        postfix))))
            (incf power)))))))
(defun magic-buffer-list-getter-size-string ()
  (magic-buffer-list-sizify (magic-buffer-list-get 'size)))
(defun magic-buffer-list-getter-display-time ()
  (if buffer-display-time
      (float buffer-display-time)
    0))
(defun magic-buffer-list-getter-modified ()
  (buffer-modified-p))
(defun magic-buffer-list-getter-read-only ()
  buffer-read-only)
(defun magic-buffer-list-getter-visible ()
  (not (null (get-buffer-window (current-buffer) t))))

;;
;; Line specs
;;

(defun magic-buffer-list-format-buffer-row (format-spec buffer
                                                        indent-level)
  (flet ((magic-buffer-list-getter-indent-level () indent-level)
         (magic-buffer-list-getter-indent ()
            (make-string indent-level ? )))
    (with-current-buffer buffer
      (magic-buffer-list-format-internal
       (cons 'concat format-spec) t))))

(defun magic-buffer-list-format-group-row (format-spec group-plist
                                                       indent-level)
  (flet ((magic-buffer-list-group-getter-indent-level () indent-level)
         (magic-buffer-list-group-getter-indent ()
            (make-string indent-level ? ))
         (magic-buffer-list-group-getter-title ()
            (plist-get group-plist :title)))
    (let ((magic-buffer-list-getter-prefix
           'magic-buffer-list-group-getter))
      (magic-buffer-list-format-internal
       (cons 'concat format-spec) t))))

(defun magic-buffer-list-string-repeat (string repeat)
  (mapconcat #'identity (make-list repeat string) ""))

(defun magic-buffer-list-string-pack (string width
                                             align trim trim-align
                                             padding)
  (when (not (memq align '(left right)))
    (error "Unknown alignment %s" align))
  (when (not (memq trim-align '(left right)))
    (error "Unknown trim alignment %s" trim-align))
  (let ((length (length string)))
    (cond ((= length width)
           str)
          ((< length width)
           (let ((pad-str (magic-buffer-list-string-repeat
                           padding
                           (1+ (/ (- width length)
                                  (length padding))))))
             (case align
               ;; XXX What's the proper way to do this?  The padding
               ;; should probably align regardless of length (ie, the
               ;; exact opposite of this)
               (left (substring (concat str pad-str) 0 width))
               (right (substring (concat pad-str str) (- width))))))
          ((> length width)
           (let ((trim-width (- width (length trim))))
             (case trim-align
               (left (concat (substring str 0 trim-width) trim))
               (right (concat trim
                              (substring str (- trim-width))))))))))

(defun magic-buffer-list-format-internal (elt &optional to-string)
  (let ((data (magic-buffer-list-format-internal-eval elt)))
    (if to-string
        (mapconcat
         (lambda (x) (if x (format "%s" x)))
         (magic-buffer-list-format-internal-flatten (list data))
         "")
      data)))

(defun magic-buffer-list-format-internal-eval (elt)
  ;; XXX Note that current window matters for some alignment
  ;; operations
  (cond ((stringp elt)
         elt)
        ((null elt)
         nil)
        ((symbolp elt)
         (magic-buffer-list-get elt))
        ((numberp (car-safe elt))
         (let* ((width-spec (car elt))
                (width (if (minusp width-spec)
                           (+ (window-width) width-spec)
                         width-spec))
                (str (magic-buffer-list-format-internal (cadr elt) t))
                (plist (cddr elt))
                (align (or (plist-get plist :align) 'left))
                (trim (or (plist-get plist :trim) ""))
                (trim-align (or (plist-get plist :trim-align) align))
                (padding (or (plist-get plist :padding) " ")))
           (magic-buffer-list-string-pack str width
                                          align trim trim-align
                                          padding)))
        ((eq (car-safe elt) 'concat)
         (mapcar #'magic-buffer-list-format-internal (cdr elt)))
        ((eq (car-safe elt) 'repeat)
         (let ((times
                (magic-buffer-list-format-internal (cadr elt)))
               (value
                (magic-buffer-list-format-internal (caddr elt))))
           (make-list times value)))
        ((eq (car-safe elt) 'if)
         (let ((condition (cadr elt))
               (consequent (caddr elt))
               (alternate (car-safe (cdddr elt))))
           (if (magic-buffer-list-format-internal-eval condition)
               (magic-buffer-list-format-internal-eval consequent)
             (magic-buffer-list-format-internal-eval alternate))))
        ((eq (car-safe elt) 'eval)
         (eval (cadr elt)))
        ((eq (car-safe elt) 'reeval)
         (let ((value (eval (cadr elt))))
           (magic-buffer-list-format-internal value)))
        (t (error "Unknown format spec %s" elt))))

(defun magic-buffer-list-format-internal-flatten (data)
  (reduce (lambda (l r)
            (if (consp l)
                (append (magic-buffer-list-format-internal-flatten l)
                        r)
              (cons l r)))
          data :from-end t :initial-value ()))

;;
;; Render specs
;;

(defun magic-buffer-list-render (spec buffer-format group-format)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((indent-level 0))
    (flet ((indent
            ()
            (insert (make-string indent-level ? )))
           (render-component
            (component)
            (let ((type (car component)))
              (cond ((eq type 'group)
                     (render-group component))
                    ((eq type 'buffer)
                     (render-buffer component))
                    (t (error "Unknown render component %s"
                              component)))))
           (render-group
            (group-spec)
            (let ((plist (cadr group-spec))
                  (body (cddr group-spec))
                  (here (point)))
              (insert (magic-buffer-list-format-group-row
                       group-format
                       plist
                       indent-level))
              (newline)
              (magic-buffer-list-put-prop here (point)
                                          'group plist)
              (let ((indent-level (1+ indent-level)))
                (render-spec body))))
           (render-buffer
            (buffer-spec)
            (let ((buffer (cadr buffer-spec))
                  (here (point)))
              (insert (magic-buffer-list-format-buffer-row
                       buffer-format
                       buffer
                       indent-level))
              (newline)
              (magic-buffer-list-put-prop here (point)
                                          'buffer buffer)))
           (render-spec
            (spec)
            (dolist (component spec)
              (render-component component))))
      (render-spec spec)))
  ;; Clean up blank lines at the end
  (delete-region (save-excursion (goto-char (point-max))
                                 (skip-chars-backward " \n\t")
                                 (point))
                 (point-max))
  (magic-buffer-list-colorize)
  (goto-char (point-min))
  (setq buffer-read-only t))

(defun magic-buffer-list-coalesce-groups (spec &optional non-recursive)
  (let (result groups)
    (dolist (elt spec)
      (cond ((eq (car elt) 'buffer)
             (push elt result))
            ((eq (car elt) 'group)
             (let* ((plist (cadr elt))
                    (body (cddr elt))
                    (title (plist-get plist :title))
                    (group (assoc title groups)))
               (if group
                   (nconc (cdr group) body)
                 (let ((new-group `(group ,plist
                                          ,@body)))
                   (push (cons title new-group) groups)
                   (push new-group result)))))
            (t (error "Unknown element %s" elt))))
    (unless non-recursive
      (dolist (group-pair groups)
        (let ((group (cdr group-pair)))
          (setf (cddr group)
                (magic-buffer-list-coalesce-groups (cddr group)
                                                   nil)))))
    (reverse result)))

(defun magic-buffer-list-collapse-singleton-groups (spec separator)
  (remove-if
   #'null
   (mapcar
    (lambda (elt)
      (cond ((eq (car elt) 'buffer) elt)
            ((eq (car elt) 'group)
             (let* ((plist (cadr elt))
                    (body (cddr elt))
                    (new-body
                     (magic-buffer-list-collapse-singleton-groups
                      body separator))
                    (len (length new-body)))
               (cond ((= len 0) nil)
                     ((and (= len 1)
                           (eq (caar new-body) 'group))
                      (let ((subplist (cadar new-body))
                            (subbody (cddar new-body)))
                        `(group (:title ,(concat
                                          (plist-get plist
                                                     :title)
                                          separator
                                          (plist-get subplist
                                                     :title)))
                                ,@subbody)))
                     (t `(group ,plist ,@new-body)))))
            (t (error "Unknown element: %S" elt))))
    spec)))

(defun magic-buffer-list-swivel-groups (spec buffers-before-groups)
  (let* ((buffers (remove-if-not (lambda (elt)
                                   (eq (car elt) 'buffer))
                                 spec))
         (groups (remove-if-not (lambda (elt)
                                  (eq (car elt) 'group))
                                spec))
         (new-groups
          (mapcar (lambda (elt)
                    `(group ,(cadr elt)
                            ,@(magic-buffer-list-swivel-groups
                               (cddr elt) buffers-before-groups)))
                  groups)))
    (if buffers-before-groups
        (append buffers new-groups)
      (append new-groups buffers))))

;;
;; Frame manipulations
;;

(defvar magic-buffer-list-pop-up-min-context-height 5)

(defun magic-buffer-list-pop-up (buffer)
  (let ((in-buffer (eq (current-buffer) buffer))
        (prev-window (magic-buffer-list-un-pop-up buffer)))
    (when (and in-buffer prev-window)
      (select-window prev-window)))
  (let* ((orig-window (selected-window))
         (orig-height (window-height))
         (orig-buffer (current-buffer))
         (window-split nil)
         (buffer-point (with-current-buffer buffer (point)))
         (buffer-lines (max (save-excursion
                              (set-buffer buffer)
                              (1+ (count-lines (point-min)
                                               (point-max))))
                            window-min-height))
         (split-point (max magic-buffer-list-pop-up-min-context-height
                           window-min-height))
         (goal-lines
          (save-window-excursion
            (split-window nil split-point)
            (window-height (next-window)))))
    (if (>= goal-lines buffer-lines)
        (progn
          ;; The buffer fits in the split window
          (split-window nil (+ split-point
                               (- goal-lines buffer-lines)))
          (select-window (next-window))
          (setq window-split t))
      ;; Resize this window to make it fit
      (enlarge-window (- buffer-lines (window-height))))
    (switch-to-buffer buffer)
    (set-window-start (selected-window) 0)
    (goto-char buffer-point)
    ;; Save restore state
    (when (not (boundp 'magic-buffer-list-pop-up-buffer-state))
      (make-local-variable 'magic-buffer-list-pop-up-buffer-state)
      (put 'magic-buffer-list-pop-up-buffer-state 'permanent-local t))
    (setq magic-buffer-list-pop-up-buffer-state
          (list (selected-window) orig-window orig-height orig-buffer
                window-split))))

(defun magic-buffer-list-un-pop-up (&optional buffer)
  (interactive)
  (let ((state (with-current-buffer (or buffer (current-buffer))
                 (when (boundp 'magic-buffer-list-pop-up-buffer-state)
                   (prog1
                       magic-buffer-list-pop-up-buffer-state
                     (setq magic-buffer-list-pop-up-buffer-state
                           nil))))))
    (when state
      (let ((buf-window (nth 0 state))
            (orig-window (nth 1 state))
            (orig-height (nth 2 state))
            (orig-buffer (nth 3 state))
            (window-split (nth 4 state)))
        (save-selected-window
          (when (and window-split (window-live-p buf-window))
            (delete-window buf-window))
          (when (window-live-p orig-window)
            (select-window orig-window)
            (enlarge-window (- orig-height (window-height)))
            (when (and (not window-split) (buffer-live-p orig-buffer))
              (switch-to-buffer orig-buffer))
            orig-window)
          (or (and (window-live-p orig-window) orig-window)
              (and (window-live-p buf-window) buf-window)))))))

;;
;; Line properties (overlays)
;;

(defun magic-buffer-list-point-to-buffer (buffer)
  (goto-char (point-min))
  (while (let ((buffer-here (magic-buffer-list-get-prop 'buffer)))
           (and (or (not buffer-here)
                    (not (eq buffer-here buffer)))
                (not (eobp))))
    (forward-line))
  (if (eobp)
      (progn
        (goto-char (point-min))
        (while (and (not (magic-buffer-list-get-prop 'buffer))
                    (not (eobp)))
          (forward-line))
        nil)
    t))

(defun magic-buffer-list-get-prop (prop &optional pt)
  (let ((overlays (overlays-at (or pt (point))))
        (symbol (intern (concat "magic-buffer-list-"
                                (symbol-name prop)))))
    (when overlays
      ;; XXX Deal with multiple overlays
      (overlay-get (car overlays) symbol))))

(defun magic-buffer-list-put-prop (start end prop value)
  ;; XXX Search for existing overlay?
  (let ((overlay (make-overlay start end))
        (symbol (intern (concat "magic-buffer-list-"
                                (symbol-name prop)))))
    (overlay-put overlay symbol value)))

;;
;; Buffer flags
;;

(defun magic-buffer-list-set-buffer-flag (buffer flag)
  (with-current-buffer buffer
    (unless (boundp 'magic-buffer-list-buffer-flags)
      (make-local-variable 'magic-buffer-list-buffer-flags)
      (setq magic-buffer-list-buffer-flags ()))
    (add-to-list 'magic-buffer-list-buffer-flags flag)))

(defun magic-buffer-list-reset-buffer-flag (buffer flag)
  (with-current-buffer buffer
    (when (boundp 'magic-buffer-list-buffer-flags)
      (setq magic-buffer-list-buffer-flags
            (delete flag magic-buffer-list-buffer-flags))
      (if (null magic-buffer-list-buffer-flags)
          (kill-local-variable 'magic-buffer-list-buffer-flags)))))

(defun magic-buffer-list-get-buffer-flag (buffer flag)
  (with-current-buffer buffer
    (when (boundp 'magic-buffer-list-buffer-flags)
      (memq flag magic-buffer-list-buffer-flags))))

(defun magic-buffer-list-get-buffers-with-flag (flag)
  (remove-if-not
   (lambda (buffer)
     (magic-buffer-list-get-buffer-flag buffer flag))
   (buffer-list)))

;;
;; Colorization
;;

;; XXX Use custom faces instead of hard-coding
(defun magic-buffer-list-colorize (&optional buffer)
  (save-excursion
    (goto-char (point-min))
    (beginning-of-line)
    (while (not (eobp))
      (dolist (overlay (overlays-at (point)))
        (when (or (not buffer)
                  (eq (magic-buffer-list-get-prop 'buffer) buffer))
          (let ((group (overlay-get overlay
                                    'magic-buffer-list-group))
                (buffer (overlay-get overlay
                                     'magic-buffer-list-buffer)))
            (cond (group
                   (overlay-put overlay
                                'face '(:background "dim gray")))
                  (buffer
                   (let ((face
                          (append
                           (if (magic-buffer-list-get-buffer-flag
                                buffer 'kill)
                               '(:strike-through "red"))
                           (if (magic-buffer-list-get-buffer-flag
                                buffer 'save)
                               '(:foreground "green")))))
                     (overlay-put overlay 'face face))))))
        (forward-line)))))

;;
;; Major mode and interactive commands
;;

(defvar magic-buffer-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"    #'magic-buffer-list-int-expunge-and-quit)
    (define-key map "\C-g" #'magic-buffer-list-int-expunge-and-quit)
    (define-key map "\C-m" #'magic-buffer-list-int-selected-goto)
    (define-key map "p"    #'magic-buffer-list-int-pivot)
    (define-key map "\C-i" #'magic-buffer-list-int-next-group)
    (define-key map [up]   #'magic-buffer-list-int-prev-buffer)
    (define-key map "\C-p" #'magic-buffer-list-int-prev-buffer)
    (define-key map [down] #'magic-buffer-list-int-next-buffer)
    (define-key map "\C-n" #'magic-buffer-list-int-next-buffer)
    (define-key map "s"    #'magic-buffer-list-int-mark-save)
    (define-key map "k"    #'magic-buffer-list-int-mark-kill)
    (define-key map "x"    #'magic-buffer-list-int-expunge)
    (define-key map "u"    #'magic-buffer-list-int-unmark)
    map))

(define-derived-mode magic-buffer-list-mode
  fundamental-mode "Buffers"
  "Major mode for magic-buffer-list buffer list buffers."
  (setq truncate-lines t))
(put 'magic-buffer-list-mode 'mode-class 'special)

(defun magic-buffer-list-int-expunge-and-quit ()
  (interactive)
  (magic-buffer-list-int-expunge)
  (magic-buffer-list-un-pop-up))

(defun magic-buffer-list-int-selected-goto ()
  (interactive)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (if (null buffer)
        (message "That's not a buffer")
      (let ((window (magic-buffer-list-un-pop-up)))
        (when window
          (select-window window))
        (switch-to-buffer buffer)))))

(defun magic-buffer-list-int-pivot ()
  (interactive)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (if (null buffer)
        ;; XXX Just switch to the next view and reselect the
        ;; originally selected buffer
        (message "You can only pivot around a buffer")
      (let ((next-view
             (car (or (cdr-safe (memq magic-buffer-list-current-view
                                      magic-buffer-list-view-sequence))
                      magic-buffer-list-view-sequence))))
        (magic-buffer-list-show-view next-view buffer)))))

(defun magic-buffer-list-int-next-group ()
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (flet ((skip-groups
            ()
            (while (and (magic-buffer-list-get-prop 'group)
                        (not (eobp)))
              (forward-line)))
           (skip-to-group
            ()
            (while (and (null (magic-buffer-list-get-prop 'group))
                        (not (eobp)))
              (forward-line)))
           (skip
            ()
            (skip-to-group)
            (skip-groups)))
      (skip)
      (when (eobp)
        (goto-char (point-min))
        (skip)
        (if (eobp)
            (goto-char here))))))

(defun magic-buffer-list-int-next-buffer ()
  (interactive)
  (let ((here (point)))
    (forward-line)
    (while (and (null (magic-buffer-list-get-prop 'buffer))
                (not (eobp)))
      (forward-line))
    (if (eobp)
        (progn
          (goto-char here)
          nil)
      t)))

(defun magic-buffer-list-int-prev-buffer ()
  (interactive)
  (let ((here (point)))
    (forward-line -1)
    (while (and (null (magic-buffer-list-get-prop 'buffer))
                (not (bobp)))
      (forward-line -1))
    (if (bobp)
        (progn
          (goto-char here)
          nil)
      t)))

(defun magic-buffer-list-int-mark-save ()
  (interactive)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (if (null buffer)
        (ding)
      (magic-buffer-list-set-buffer-flag buffer 'save)
      (magic-buffer-list-int-next-buffer)
      (magic-buffer-list-colorize buffer))))

(defun magic-buffer-list-int-mark-kill ()
  (interactive)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (if (null buffer)
        (ding)
      (magic-buffer-list-set-buffer-flag buffer 'kill)
      (magic-buffer-list-int-next-buffer)
      (magic-buffer-list-colorize buffer))))

(defun magic-buffer-list-int-unmark ()
  (interactive)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (if (null buffer)
        (ding)
      (magic-buffer-list-reset-buffer-flag buffer 'save)
      (magic-buffer-list-reset-buffer-flag buffer 'kill)
      (magic-buffer-list-int-next-buffer)
      (magic-buffer-list-colorize buffer))))

(defun magic-buffer-list-int-expunge ()
  (interactive)
  (while (and (let ((buffer (magic-buffer-list-get-prop 'buffer)))
                (if buffer
                    (magic-buffer-list-get-buffer-flag buffer 'kill)
                  t))
              (magic-buffer-list-int-next-buffer))
    nil)
  (let ((buffer (magic-buffer-list-get-prop 'buffer)))
    (dolist (save (magic-buffer-list-get-buffers-with-flag 'save))
      (magic-buffer-list-point-to-buffer save)
      (with-current-buffer save
        (save-buffer))
      (magic-buffer-list-reset-buffer-flag buffer 'save))
    (dolist (kill (magic-buffer-list-get-buffers-with-flag 'kill))
      (magic-buffer-list-point-to-buffer kill)
      (kill-buffer kill))
    (magic-buffer-list-show-view magic-buffer-list-current-view
                                 (if (buffer-live-p buffer)
                                     buffer))))

(provide 'magic-buffer-list)
