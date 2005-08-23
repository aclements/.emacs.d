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
;; *** Buffer switch entropy
;; ** Emacs' MRU ordering can be seen as a cheap approximation of
;;    buffer relatedness.
;;
;; * Preview buffer option.  Show the buffer that would be switched to
;;   when the selection is changed.  Revert to the original buffer if
;;   canceled
;; * Autoselect next buffer option.  When popping up, automatically
;;   select the next buffer instead of the current buffer.  It would
;;   be nice if this were more easily customizable.
;;
;; * Add buttons to the customizers the bring up lists of available
;;   filters, sorters, and groupers


;; list-buffers is arcane.  On the other hand, it does precisely what
;; it says: it lists buffers.  However, time and again, its overly
;; simplistic interface has been used as the basis for buffer
;; navigation.

;; Extensibility

;; magic-buffer-list is designed to be highly extensible, but provide
;; reasonable and usable defaults.  Many components go into producing
;; a view of the buffer list, and the easiest way to customize
;; magic-buffer-list is to piece together provided components.
;; However, if the existing components aren't enough and you're
;; willing to dive into Elisp, you can completely customize the look,
;; feel, and organization of magic-buffer-list.

;; Scalability

;; magic-buffer-list is designed to effectively scale up to huge
;; numbers of buffers, without losing its usefulness when used for
;; small numbers of buffers.

;;; Customization:

(defgroup magic-buffer-list nil
  "XXX")

(defcustom magic-buffer-list-view-defaults
  `(:filter
    (,#'magic-buffer-list-view-filter-system-buffers
     ,#'magic-buffer-list-view-filter-most-star-buffers)
    :sorter ,#'magic-buffer-list-view-sort-mru
    :grouper ,#'magic-buffer-list-view-group-by-none
    :format (" " (reeval magic-buffer-list-view-basic-info))
    :group-format (" " (reeval magic-buffer-list-view-basic-group))
    :select-next-buffer t)
  "Property list of view defaults.

This is a property list that specifies default values for view
properties that are not overridden by specific views in
`magic-buffer-list-views'.  See `magic-buffer-list-views' for
information on the meaning of each property."
  :group 'magic-buffer-list
  :type '(restricted-sexp
          :match-alternatives (magic-buffer-list-valid-view-p)))

(defcustom magic-buffer-list-views
  `((group-by-major-mode
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
              filename-sans-directory)))
  "List of available buffer list views.

This is an alist, where each key should be either a symbol naming the
view.  The value of each element should be a property list, specifying
how to build and format each view.  If a view lacks a particular
property, it will inherit the value of that property from
`magic-buffer-list-view-defaults'.  The available properties are:

* :filter - Specifies either a function or a list of functions to use
  as filters for the buffer list before sorting or grouping it.
  Filter functions must take a single argument which will be a list of
  buffers and must return a subset of that list.  If a list of
  functions is specified, those functions are composed to form the
  filter.  If this property is omitted or null, no filtering will be
  performed.  The filter should typically include at least
  `magic-buffer-list-view-filter-system-buffers'.  All provided
  filters have the prefix magic-buffer-list-view-filter-.
* :sorter - Specifies a function to sort the buffer list before
  grouping it.  Most (though not necessary all) groupers use the order
  of the buffers as a hint on the order of their output.  This
  function should take a list of buffers and return some permutation
  of it.  If this property is omitted or null, the buffer list will
  not be sorted.  All provided sorters have the prefix
  magic-buffer-list-view-sort-.
* :grouper - Specifies a function to group a buffer list.  This
  function should take a list of buffers and return a render spec.
  See `magic-buffer-list-render' for information about render
  specs.  If this property is omitted or null, it defaults to
  `magic-buffer-list-view-group-by-none'.  All provided groupers have
  the prefix magic-buffer-list-view-group-by-.
* :format - Specifies the format of buffer lines in the buffer list.
  See `magic-buffer-list-format-buffer-row' for information about
  format specs for buffers.
* :group-format - Specifies the format of group lines in the buffer
  list.  See `magic-buffer-list-format-group-row' for information
  about format specs for groups.
* :select-next-buffer - When the buffer list is first popped up via
  `magic-buffer-list', if this view is selected and this property is
  t, then immediately select the next buffer, instead of the current
  one.

The process for showing a view is to get the unadulterated buffer
list, filter it, sort it, and then group it.  This produces the
structure that will be visible in the buffer list (the render spec).
This structure is then rendered, with buffer lines formatted according
to :format and group lines formatted according to :group-format."
  :group 'magic-buffer-list
;;  :type '(alist :key-type symbol :value-type plist)
;;  :options '(t)
  :type '(restricted-sexp
          :match-alternatives (magic-buffer-list-valid-views-p)))

(defcustom magic-buffer-list-view-sequence
  '(group-by-major-mode group-by-directory)
  "Sequence of views from `magic-buffer-list-views' to cycle through.

This is a list of symbols, where each symbol must be the name of a
view from `magic-buffer-list-views'.  When the buffer list is first
popped it, the first view from this list is selected.  Subsequent
pivoting cycles forward through this list."
  :group 'magic-buffer-list
  :type '(repeat symbol))

(defcustom magic-buffer-list-preview t
  "XXX"
  :type 'boolean)

;;; Code:

(defun magic-buffer-list-reload ()
  "Utility function for reloading magic-buffer-list.  This is meant to
be used while developing magic-buffer-list, and will probably go away
when it's stable enough to not need constant reloading."
  (interactive)
  (let ((buffer (get-buffer magic-buffer-list-buffer-name)))
    (if buffer
        (kill-buffer buffer)))
  (let ((file (if (and (featurep 'magic-buffer-list)
                       (fboundp 'feature-file))
                  (feature-file 'magic-buffer-list)
                (or (locate-library "magic-buffer-list")
                    "~/sys/elisp/magic-buffer-list.el"))))
    (if (featurep 'magic-buffer-list)
        (unload-feature 'magic-buffer-list t))
    (unless (file-exists-p file)
      (error "Couldn't find magic-buffer-list.el.  Use M-x load-file"))
    (load-file file)))

(require 'cl)

(defconst magic-buffer-list-buffer-name " *Magic Buffer List*"
  "Buffer name of the magic-buffer-list list buffer")

(defun magic-buffer-list ()
  "Pop up the magic buffer list in the previously used view.  If this
is the first time the buffer list is being popped up, the first view
from `magic-buffer-list-view-sequence' is used.  Depending on the
value of the view's :select-next-buffer property, either the current
buffer or the next buffer is selected."
  (interactive)
  (magic-buffer-list-show-view nil (current-buffer) t)
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
    (if visible "V" " "))
  "Basic buffer flags.  Use
'(reeval magic-buffer-list-view-basic-flags)' in a buffer format spec
to insert modified, read-only, and visible flags.")

(defvar magic-buffer-list-view-basic-info
  '(concat
    (25 (concat (repeat indent-level "  ") name) :trim "..")
    " "
    (4 size-string :align right)
    " "
    (reeval magic-buffer-list-view-basic-flags))
  "Basic buffer information.  This include the buffer's name, indented
to the current level, the size of the buffer, and the basic flags for
the buffer.  Use '(reeval magic-buffer-list-view-basic-info)' in a
buffer format spec to insert this information.")

(defvar magic-buffer-list-view-basic-group
  '(concat
    (repeat indent-level "  ")
    title)
  "Basic group information.  This is just the group's title, indented
to the current level.  Use
'(reeval magic-buffer-list-view-basic-group)' in a group format spec
to insert this information.")

(defun magic-buffer-list-valid-view-p (view-plist
                                       &optional return-error)
  "Predicate for basic validity checking of a view.  With one
argument, it behaves like a predicate should.  If return-error is t,
then this returns a string describing what's wrong with view-plist, or
nil if nothing is wrong with it."
  (let* ((properties '(:filter :sorter :grouper :format 
                               :group-format :select-next-buffer))
         (result
          (catch 'result
            (unless (listp view-plist)
              (throw 'result "View must be a list"))
            (unless (evenp (length view-plist))
              (throw 'result
                     (concat "View isn't a property list"
                             " (it's length must be even)")))
            (do ((plist view-plist (cddr plist)))
                ((null plist)
                 nil)
              (unless (memq (car plist) properties)
                (throw 'result
                       (format "View contains unknown property %S"
                               (car plist))))))))
    (if return-error
        result
      (if (stringp result)
          nil
        t))))

(defun magic-buffer-list-valid-views-p (views)
  "Predicate for basic validity checking of a views list."
    (and (listp views)
         (every (lambda (v) (and (listp v)
                                 (not (null v))
                                 (symbolp (car v))))
                views)
         (every #'magic-buffer-list-valid-view-p
                (mapcar #'cdr views))))

(defun magic-buffer-list-view-filter-system-buffers (buffers)
  "Buffer list filter that eliminates system buffers.  This filter
should almost always be used, as system buffers are meant to be hidden
from the user."
  (remove-if (lambda (buffer)
               (let ((name (buffer-name buffer)))
                 (or (string= (substring name 0 1) " ")
                     (string= name magic-buffer-list-buffer-name))))
             buffers))

(defun magic-buffer-list-view-filter-most-star-buffers (buffers)
  "Buffer list filter that eliminates all \"starred\" buffers except
the scratch buffer."
  (remove-if (lambda (buffer)
               (let ((name (buffer-name buffer)))
                 (and (string= (substring name 0 1) "*")
                      (string= (substring name -1) "*")
                      (not (string= name "*scratch*")))))
             buffers))

(defun magic-buffer-list-view-sort-mru (buffers)
  "Buffer list sorter that sorts according to MRU.  This is the way
most buffer lists are sorted (such as `list-buffers')."
  ;; In the current implementation, the buffer list should already be
  ;; sorted this way, and it's hard to actually do it.
  buffers)

(defun magic-buffer-list-view-sort-alphabetical (buffers)
  "Buffer list sorter that sorts in alphabetical order by buffer
name.  Comparisons are done using `string<'."
  (sort buffers
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun magic-buffer-list-view-group-by-none (buffers)
  "Buffer list grouper that does no grouping.  This is essentially an
identity grouper and produces a flat buffer list, similar to typical
buffer lists."
  (mapcar (lambda (buffer) `(buffer ,buffer)) buffers))

(defun magic-buffer-list-view-group-by-major-mode (buffers)
  "Buffer list grouper that groups by major mode.  Within each group,
buffers are sorted according to the incoming sort, and the groups are
sorted according to the earliest buffer in each group."
  (magic-buffer-list-coalesce-groups
   (mapcar
    (lambda (buffer)
      `(group (:title ,(magic-buffer-list-get 'major-mode buffer))
              (buffer ,buffer)))
    buffers)))

(defun magic-buffer-list-view-group-by-directory (buffers)
  "Buffer list grouper that groups hierarchically by directory.
Directories that contain only a single subdirectory in the hierarchy
are collapsed to save space.  Within each directory, buffers are
placed above subdirectories, buffers are sorted according to the
incoming sort, and subdirectories are sorted according to the earliest
buffer in the subdirectory.  Directoryless buffers (such as the
scratch buffer) are placed in a special group that appears after
everything else."
  ;; There are two types of buffers, the haves and the have-nots.
  ;; directoried will be a list of group render components for have
  ;; buffers.  nondirectoried is just a list of have-not buffers.
  (let (directoried nondirectoried)
    (dolist (buffer buffers)
      (let ((directory (magic-buffer-list-get 'directory buffer)))
        (if (null directory)
            ;; Have-not
            (push buffer nondirectoried)
          ;; Have.  Split up the directory into components
          (let* ((dirlist-almost (split-string directory "/"))
                 ;; But tack the initial / back on if there is one
                 (dirlist
                  (cons (if (equal (substring directory 0 1) "/")
                            (concat "/" (car dirlist-almost))
                          (car dirlist-almost))
                        (cdr dirlist-almost)))
                 ;; Start out with just a buffer render component
                 (group `(buffer ,buffer)))
            ;; For each directory component, wrap group in the next
            ;; group.
            (dolist (dir (reverse dirlist))
              (setq group `(group (:title ,(concat dir "/")) ,group)))
            (push group directoried)))))
    (append
     ;; Format the haves
     (magic-buffer-list-swivel-groups
      (magic-buffer-list-collapse-singleton-groups
       (magic-buffer-list-coalesce-groups (reverse directoried))
       "")
      ;; Put buffers above directories.  It looks nicer
      t)
     ;; Wrap the have-nots in a special group at the end
     `((group (:title "(no directory)")
              ,@(mapcar (lambda (buffer) `(buffer ,buffer))
                        (reverse nondirectoried)))))))

(defun magic-buffer-list-show-view (view-name &optional
                                              select-buffer
                                              initial-appearance)
  "Show a magic buffer list view.  view-name must either by nil, or
must specify one of the views in `magic-buffer-list-views'.  If
view-name is nil, the previously used view type is reused, or, if
there is no previously used view type, the first type in
`magic-buffer-list-view-sequence' is used.  select-buffer, if
provided, must be the buffer to select once the view is built.  If the
view specifies :select-next-buffer and initial-appearance is true,
then the buffer after select-buffer is selected instead.  Once this is
done building the view, it pops up the list.  If the list is already
visible, this resizes it to the new appropriate size."
  (unless (magic-buffer-list-valid-views-p magic-buffer-list-views)
    (error "magic-buffer-list-views is malformed"))
  (let ((err (magic-buffer-list-valid-view-p
              magic-buffer-list-view-defaults t)))
    (if err
        (error "magic-buffer-list-view-defaults is malformed: %s"
               err)))
  (let ((buffer (get-buffer-create magic-buffer-list-buffer-name)))
    (with-current-buffer buffer
      ;; Maybe switch modes.  This needs to be done early and once so
      ;; it doesn't clobber buffer-local variables.
      (if (not (eq major-mode 'magic-buffer-list-mode))
          (magic-buffer-list-mode))
      ;; Get the view
      (let* ((view-name (or view-name
                            (if (boundp
                                 'magic-buffer-list-current-view)
                                magic-buffer-list-current-view)
                            (car magic-buffer-list-view-sequence)))
             (view
              (cdr-safe (assq view-name magic-buffer-list-views)))
             (default-view
               magic-buffer-list-view-defaults))
        (if (null view)
            (error "Unknown view: %s" view-name))
        ;; Get the view properties
        (flet ((view-get
                (prop)
                (if (plist-member view prop)
                    (plist-get view prop)
                  (plist-get default-view prop))))
          (let ((filter (view-get :filter))
                (sorter (view-get :sorter))
                (grouper (view-get :grouper))
                (buffer-format (view-get :format))
                (group-format (view-get :group-format))
                (select-next-buffer (view-get :select-next-buffer)))
            ;; Get the buffers, filter them, sort them, and group them
            (let* ((buffers (buffer-list))
                   (filtered-buffers
                    (cond ((null filter)
                           buffers)
                          ((functionp filter)
                           (funcall filter buffers))
                          ((listp filter)
                           (dolist (f filter)
                             (setq buffers (funcall f buffers)))
                           buffers)))
                   (sorted-buffers
                    (if sorter
                        (funcall sorter filtered-buffers)
                      buffers))
                   (spec
                    (funcall (or grouper
                                 #'magic-buffer-list-view-group-by-none)
                             sorted-buffers)))
              ;; Render the buffer list
              (magic-buffer-list-render spec
                                        buffer-format
                                        group-format))
            ;; Point to the appropriate buffer
            (magic-buffer-list-point-to-buffer select-buffer)
            (if (and initial-appearance select-next-buffer)
                (magic-buffer-list-int-next-buffer))))
        ;; Record the current view
        (make-local-variable 'magic-buffer-list-current-view)
        (setq magic-buffer-list-current-view view-name)))
    ;; Now that we're done building the view and have unexcursed, pop
    ;; up the buffer
    (magic-buffer-list-pop-up buffer)
    ;; Update the preview, ignoring the cache (which may now be
    ;; invalid)
    (magic-buffer-list-update-preview t)))

;;
;; Getters
;;

(defun magic-buffer-list-get (name &optional buffer)
  "Get the value of the getter specified by name.  If buffer is
specified, then do the getting in buffer, otherwise do it in the
current buffer.

Getters are used by numerous components of magic-buffer-list.  Buffer
and group line formats use getters to retrieve values to display in
the columns of the buffer list.  Some filters, sorters, and groupers
likewise use getters to retrieve information about buffers, since they
provide a uniform and data-driven way to access buffer information.

Getters are functions whose names are prefixed with
`magic-buffer-list-getter-prefix', followed by a hyphen, followed by
the name of the getter.  The function should take no arguments.  It
will be called with the current buffer set the buffer.

In the future, this may employ optimizations such as caching."
  (let ((getter (intern
                 (concat (symbol-name magic-buffer-list-getter-prefix)
                         "-"
                         (symbol-name name)))))
    (if (functionp getter)
        (with-current-buffer (or buffer (current-buffer))
          (funcall getter))
      (error "No such getter: %s" name))))

(defvar magic-buffer-list-getter-prefix 'magic-buffer-list-getter
  "The getter system support multiple, independent namespaces of
getters.  This specifies the prefix of the function names that
comprise the current namespace.  This is used, for example, by the
group formatter to create a getter namespace independent of the one
that only makes sense for buffers.")

(defun magic-buffer-list-getter-name ()
  "Get the buffer name"
  (buffer-name))
(defun magic-buffer-list-getter-filename ()
  "Get the abbreviated file name (including directory), or nil if no
filename"
  (let ((filename (buffer-file-name buffer)))
    (when filename (abbreviate-file-name filename))))
(defun magic-buffer-list-getter-filename-dimmed ()
  "Get the abbreviated file name, and dim the directory component"
  (let ((directory (magic-buffer-list-get 'directory))
        (filename (magic-buffer-list-get 'filename-sans-directory)))
    (when (and directory filename)
      ;; XXX Do the face right
      (concat (propertize directory 'face '((:foreground "gray")))
              filename))))
(defun magic-buffer-list-getter-directory ()
  "Get the directory name, or nil if no directory"
  (let ((filename (magic-buffer-list-get 'filename buffer)))
    (when filename (file-name-directory filename))))
(defun magic-buffer-list-getter-filename-sans-directory ()
  "Get the filename without directory, or nil if no filename"
  (let ((filename (magic-buffer-list-get 'filename buffer)))
    (when filename (file-name-nondirectory filename))))
(defun magic-buffer-list-getter-major-mode ()
  "Get the major mode"
  mode-name)
(defun magic-buffer-list-getter-size ()
  "Get the buffer size, as a number"
  (buffer-size))
(defun magic-buffer-list-getter-size-string ()
  "Get the buffer size, pretty-printed as a string"
  (magic-buffer-list-sizify (magic-buffer-list-get 'size)))
(defun magic-buffer-list-getter-display-time ()
  "Get the buffer's last display time, as a float, or 0 if it has
never been displayed"
  (if buffer-display-time
      (float buffer-display-time)
    0))
(defun magic-buffer-list-getter-modified ()
  "Get whether or not the buffer is modified, as a boolean"
  (buffer-modified-p))
(defun magic-buffer-list-getter-read-only ()
  "Get whether or not the buffer is read-only, as a boolean"
  buffer-read-only)
(defun magic-buffer-list-getter-visible ()
  "Get whether or not the buffer is visible in any window, as a
boolean"
  (not (null (get-buffer-window (current-buffer) t))))

;;
;; Getter utilities
;;

(defun magic-buffer-list-sizify (size)
  "Pretty-print size as a possibly suffixed floating point number.
This tries to make the resulting string length at most 4 characters."
  ;; XXX Yeah, I know Emacs can't represent sizes bigger than 134M
  (let* ((suffixes (list "B" "K" "M" "G" "T" "P"))
         (last-suffix (car (last suffixes)))
         (divisor 1000.0)
         (power 0))
    (catch 'done
      (dolist (suffix suffixes)
        (let ((new-size (/ size (expt divisor power))))
          (cond ((and (< new-size 10) (= power 0))
                 ;; Size is small, but shouldn't be printed as a
                 ;; floating point
                 (throw 'done
                        (concat (number-to-string (floor new-size))
                                suffix)))
                ((< new-size 10)
                 ;; Size the small enough to fit one decimal place
                 (throw 'done
                        (concat (number-to-string
                                 (/ (ffloor (* new-size 10)) 10))
                                suffix)))
                ((or (< new-size 1000)
                     (equal suffix last-suffix))
                 ;; Size is small enough to fit three digits in (or
                 ;; we're out of suffixes)
                 (throw 'done (concat (number-to-string
                                       (floor new-size))
                                      suffix)))
                (t
                 ;; Try the next power
                 (incf power))))))))

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
         (orig-window-start (window-start))
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
          (list (selected-window) orig-window orig-height
                orig-window-start orig-buffer window-split))))

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
            (orig-window-start (nth 3 state))
            (orig-buffer (nth 4 state))
            (window-split (nth 5 state)))
        (save-selected-window
          (when (and window-split (window-live-p buf-window))
            (delete-window buf-window))
          (when (window-live-p orig-window)
            (select-window orig-window)
            (enlarge-window (- orig-height (window-height)))
            (when (and (buffer-live-p orig-buffer))
              (switch-to-buffer orig-buffer))
            (set-window-start orig-window orig-window-start)
            orig-window)
          (or (and (window-live-p orig-window) orig-window)
              (and (window-live-p buf-window) buf-window)))))))

(defun magic-buffer-list-pop-up-preview (buffer)
  (unless (boundp 'magic-buffer-list-pop-up-buffer-state)
    (error "Preview must be called from a popped up buffer"))
  (let* ((state magic-buffer-list-pop-up-buffer-state)
         (orig-window (nth 1 state))
         (window-split (nth 5 state)))
    (if (and window-split
             (window-live-p orig-window))
        (save-selected-window
          (select-window orig-window)
          (switch-to-buffer buffer t)))))

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
;; Major mode and interaction
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
  (setq truncate-lines t)
  (make-local-variable 'magic-buffer-list-last-point)
  (setq magic-buffer-list-last-point -1)
  (add-hook 'post-command-hook #'magic-buffer-list-update-preview
            nil t))
(put 'magic-buffer-list-mode 'mode-class 'special)

(defun magic-buffer-list-update-preview (&optional ignore-cache)
  (when (and magic-buffer-list-preview
             (or ignore-cache
                 (and (boundp 'magic-buffer-list-last-point)
                      (/= magic-buffer-list-last-point
                          (line-beginning-position)))))
    (setq magic-buffer-list-last-point (line-beginning-position))
    (let ((buffer (magic-buffer-list-get-prop 'buffer)))
      (if buffer
          (magic-buffer-list-pop-up-preview buffer)))))

(defun magic-buffer-list-int-quit ()
  (interactive)
  (magic-buffer-list-un-pop-up))

(defun magic-buffer-list-int-expunge-and-quit ()
  (interactive)
  (magic-buffer-list-int-expunge)
  (magic-buffer-list-int-quit))

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
        (setq buffer (magic-buffer-list-int-next-buffer)))
    (if (null buffer)
        ;; Fight mind games with mind games
        (message "You can't do that in horizontal mode"))
    (let ((next-view
           (car (or (cdr-safe (memq magic-buffer-list-current-view
                                    magic-buffer-list-view-sequence))
                    magic-buffer-list-view-sequence))))
      (magic-buffer-list-show-view next-view buffer))))

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
      (magic-buffer-list-get-prop 'buffer))))

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
      (magic-buffer-list-get-prop 'buffer))))

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
