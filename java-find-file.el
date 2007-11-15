;;; java-find-file.el --- find-file enhancements for Java editing

;; Copyright (C) 2007 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    11-15-2007
;; Version:    0.2

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

;; To load the package, simply add
;;   (require 'java-find-file)
;; to your .emacs.  If you're concerned about start up time, you can
;; instead load it lazily with
;;   (add-hook 'java-mode-hook (lambda () (require 'java-find-file)))

;; To do
;; * Be able to specify alternate source directories for other
;;   packages.  I'm not sure if this should just be a sourcepath or if
;;   it should specify which parts of the tree are where.
;; * Completion for java-find-class
;; * Handle static imports
;; * Fix up preamble parsing
;; * Better customization

;;; Code:

(require 'cl)

(defvar jff-highlight-secs 2)

(defconst jff-use-conventions nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing

(defconst jff-identifier-regex "[a-zA-Z_$][a-zA-Z0-9_$]*")

(defconst jff-package-name-regex
  (if jff-use-conventions "[a-z]+" jff-identifier-regex))

(defconst jff-class-name-regex
  (if jff-use-conventions "[A-Z][a-zA-Z0-9_$]*" jff-identifier-regex))

(defconst jff-class-regex
  (concat
   ;; Optional package name
   "\\(?:" jff-package-name-regex "\\.\\)*"
   ;; Class name
   jff-class-name-regex))

(defconst jff-package-regex
  (concat
   ;; package keyword
   "\\bpackage[ \t\n]+"
   ;; Package name
   "\\(\\(?:" jff-package-name-regex "\\.\\)*" jff-package-name-regex "\\)"
   ;; Semicolon
   "[ \t\n]*;"))

(defconst jff-import-regex
  (concat
   ;; import keyword
   "\\bimport[ \t\n]+"
   ;; Package name
   "\\(\\(?:" jff-package-name-regex "\\.\\)+"
   ;; Class name or wildcard
   "\\(" jff-class-name-regex "\\|\\*\\)\\)"
   ;; Semicolon
   "[ \t\n]*;"))

(defun jff-parse-class (str)
  (save-match-data
    (split-string str "\\.")))

(defun jff-get-preamble ()
  "Parse the preamble of the current file.  The return value is a
triple of:

  (PACKAGE SINGLE-TYPE-IMPORTS TYPE-IMPORTS-ON-DEMAND)

PACKAGE is the type name of the package declaration or nil if
there is no package declaration.

SINGLE-TYPE-IMPORTS is a list of type names for single type
imports (as defined by JLS 3, 7.5).

TYPE-IMPORTS-ON-DEMAND is a list of type names (including the
trailing *) for type imports on demand.

XXX This needs to be spruced up, since it's based heavily on
naming conventions and ignores sub-type imports."

  (save-match-data
    (save-excursion
      (let* ((case-fold-search nil)
             (bound (progn
                      (parse-partial-sexp (point-min) (point-max) 1)
                      (point)))
             (ppoint (point-min))
             (pstate nil))
        (goto-char (point-min))
        (let ((package 'unknown) (imports '()) (star-imports '()))
          ;; Get the package name
          (while (eq package 'unknown)
            (if (not (re-search-forward jff-package-regex bound t))
                (setq package '())
              (save-excursion
                (setq pstate (parse-partial-sexp ppoint (match-beginning 0)
                                                 nil nil pstate))
                (setq ppoint (match-beginning 0)))
              (unless (or (fourth pstate) (fifth pstate))
                (setq package (jff-parse-class (match-string 1))))))
          ;; Get the imports
          (while (re-search-forward jff-import-regex bound t)
            (save-excursion
              (setq pstate (parse-partial-sexp ppoint (match-beginning 0)
                                               nil nil pstate))
              (setq ppoint (match-beginning 0)))
            (unless (or (fourth pstate) (fifth pstate))
              (let ((pkg (jff-parse-class (match-string 1))))
                (if (string= (match-string 2) "*")
                    (setq star-imports (cons pkg star-imports))
                  (setq imports (cons pkg imports))))))
          (list package
                (reverse imports)
                (reverse star-imports)))))))

(defun jff-type-name-at-point ()
  "Get the type name at point as a list of strings, one for each
identifier in the type name.  Note that, without further type
information, the returned name is technically an 'ambiguous name'
as defined by JLS 3, 6.5), so all that can be known is that some
prefix of the returned list is actually a type name.  For
example, foo.bar.baz could be package.subpackage.class or
package.class.member or class.member.member or even
member.member.member.

The returned strings will be annotated with jff-source properties
on their first character that specify the buffer position where
the string was found.  This can be used to later refer back to
the type name as it appeared in the buffer."

  (save-match-data
    (save-excursion
      (catch 'done
        ;; Get to the beginning of an identifier in the type name
        ;; enclosing point.  First, as a special case, we allow the
        ;; point to be immediately after the end of an identifier.
        (when (and (not (bobp))
                   (save-excursion
                     (backward-char 1)
                     (looking-at "[a-zA-Z0-9_$]")))
          (backward-char 1))
        ;; Now do the right thing based on the character at point
        (let ((ch (char-after (point))))
          (cond ((eql ch ?.)
                 ;; We could be in a type name, but looking at a dot.
                 ;; Get to where the identifier should begin
                 (when (eobp)
                   (throw 'done '()))
                 (forward-char 1)
                 (while (forward-comment 1)))
                ((memq ch '(?  ?\t ?\n ?\r))
                 ;; The only way point can be in a type name is if the
                 ;; preceding non-whitespace is a dot
                 (save-excursion
                   (while (forward-comment -1))
                   (unless (eql (char-before (point)) ?.)
                     (throw 'done '())))
                 ;; We could be in a type name.  Get to where the next
                 ;; identifier should begin
                 (while (forward-comment 1)))
                ((looking-at "[a-zA-Z0-9_$]")
                 ;; We could be in an identifier.  Get to its
                 ;; beginning
                 (skip-chars-backward "a-zA-Z0-9_$"))
                (t
                 ;; We're looking at something else
                 (throw 'done '()))))

        (let ((idents '())
              (start (point)))
          ;; Forward consume dot-separated identifiers
          (let ((done nil))
            (while (and (not done)
                        (looking-at jff-identifier-regex))
              ;; Add this identifier
              (setq idents (cons (list (match-string 0)
                                       (match-beginning 0))
                                 idents))
              ;; Is it followed by a dot?
              (goto-char (match-end 0))
              (while (forward-comment 1))
              (if (not (eql (char-after (point)) ?.))
                  (setq done t)
                ;; Get to the next potential identifier
                (forward-char 1)
                (while (forward-comment 1)))))
          ;; Did we actually find any identifiers?
          (when (null idents)
            (throw 'done '()))
          (setq idents (nreverse idents))

          ;; Backward consume dot-separated identifiers
          (goto-char start)
          (while (forward-comment -1))
          (let ((done nil))
            (while (and (not done)
                        (eql (char-before (point)) ?.))
              ;; Find the preceding identifier
              (backward-char 1)
              (while (forward-comment -1))
              (skip-chars-backward "a-zA-Z0-9_$")
              (if (not (looking-at jff-identifier-regex))
                  (setq done t)
                ;; Add this identifier
                (setq idents (cons (list (match-string 0)
                                         (match-beginning 0))
                                   idents))
                ;; Get to the preceding potential dot
                (while (forward-comment -1)))))

          ;; We've gathered up the whole type name.  Now turn the list
          ;; of identifiers into a list of propertized strings.
          (mapl (lambda (ids)
                  (let ((ident (caar ids)))
                    (put-text-property 0 1 'jff-source (cadar ids) ident)
                    (setcar ids ident)))
                idents)
          idents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File lookup
;;;

(defun jff-get-search-path (package)
  "Compute the search path for classes.  The result is a list of
pairs of

  (PACKAGE PATH)

PACKAGE is a list of strings and PATH is a string.

The package given as the argument must be the package of the
current source file.  This will be used to figure out the search
path for the current source tree.

Currently, the only element in the returned search path is for
the current source tree, but this may be extended in the future."

  ;; Given this package, walk up the directory structure as long as
  ;; the package and directory names match
  (let ((parts (reverse package))
        (curdir (file-name-directory (buffer-file-name))))
    (while (and parts
                (not (string= curdir
                              (directory-file-name curdir)))
                (string= (car parts)
                         (file-name-nondirectory
                          (directory-file-name curdir))))
      (setq parts (cdr parts)
            curdir (file-name-directory
                    (directory-file-name curdir))))
    (list (list (reverse parts) curdir))))

(defun jff-find-qualified-class (search-path type-name maybe-nested)
  "Search for a qualified type name in the given search path.
search-path must be of the form returned by
`jff-get-search-path'.  type-name must be a list of type name
elements, some non-zero prefix of which must be a package name.
If maybe-nested is non-nil, then the division between the package
name and the type name is unspecified; otherwise, all but the
last element of type-name must be a package.  Returns the
absolute path of the source file containing the requested type,
or nil if one cannot be found."

  (catch 'done
    (dolist (search search-path)
      ;; Check if this package matches
      (let ((spackage (first search))
            (package type-name))
        ;; Test if spackage is a prefix of package
        (while (and spackage package (string= (car package) (car spackage)))
          (setq spackage (cdr spackage)
                package (cdr package)))
        (when (null spackage)
          ;; This element of the search path matches.  Walk down the
          ;; directory tree, looking for packages or source files.
          (let ((dir (second search)))
            (while package
              (let* ((pkg-path (concat dir (car package)))
                     (is-pkg (if (cdr package)
                                 (file-directory-p pkg-path)
                               nil))
                     (src-path (concat pkg-path ".java"))
                     (is-src (if (or maybe-nested
                                     (null (cdr package)))
                                 (file-exists-p src-path)
                               nil)))
                ;; JLS 3, 6.5.2 prefers type names over packages
                (cond (is-src
                       (throw 'done src-path))
                      (is-pkg
                       (setq dir (file-name-as-directory pkg-path)
                             package (cdr package)))
                      (t
                       (setq package nil)))))))))
    ;; Couldn't find it
    nil))

(defun jff-find-class (type-name)
  "Disambiguate type name and find the file it corresponds to.
Returns a pair of the absolute path of the source file containing
the specified type and the prefix of type-name that corresponds
to that file, or nil if the type cannot be found.  This may also
produce an error in situations where it should be possible to
find the source for the type, but the file cannot be
found (though a nil return is much more likely, due to
ambiguities)."

  ;; Following JLS 3, 6.5.2, and assuming that the left-most prefix of
  ;; the ambiguous name is _not_ an expression name (which we can't
  ;; handle), we should disambiguate a name as follows:
  ;; 1. Treat the left-most element as a simple type name.  Look it up
  ;;    according to the priorities derived below.
  ;; 2. Treat each successive prefix as a package name followed by a
  ;;    simple type name and search for this qualified name.

  (catch 'done
    (let* ((preamble (jff-get-preamble))
           (package (first preamble))
           (single-imports (second preamble))
           (on-demand-imports (third preamble))
           (search-path (jff-get-search-path package)))
      ;; Suppose the type name is unqualified

      ;; According to JLS 3, 6.3.1, the priority of unqualified type
      ;; shadowing is as follows:
      ;; 1. Types declared in the local scope (we ignore this)
      ;; 2. Single type import
      (dolist (si single-imports)
        (when (string= (car (last si)) (car type-name))
          ;; Found it.  The class we're actually looking for may be
          ;; buried in si, if we found a nested class, so we still
          ;; have to look it up
          (let ((res (jff-find-qualified-class search-path si t)))
            (unless res
              ;; Since we explicitly imported this, it is an error to
              ;; not find it (unless, of course, the name got shadowed
              ;; by a local name, but that's sufficiently unlikely
              ;; that I think an error is useful)
              (let ((class-name (car si)))
                (dolist (c (cdr si))
                  (setq class-name (concat class-name "." c)))
                (error (concat "Could not find class " class-name))))
            (throw 'done (list res (list (car type-name)))))))
      ;; 3. Types in the same package
      ;; 4. java.lang.* (JLS 3, 7.5.5) and type imports on demand
      ;;    (ambiguity here is considered an error, but we ignore this)
      (let ((packages (cons package
                            (cons '("java" "lang")
                                  ;; XXX Just fix jff-get-preamble
                                  (mapcar #'butlast on-demand-imports)))))
        (dolist (p packages)
          (let* ((qualified (append p (list (car type-name))))
                 (res (jff-find-qualified-class search-path qualified t)))
            (when res
              (throw 'done (list res (list (car type-name))))))))

      ;; Now try assuming that successive prefixes of the name are
      ;; qualified
      (when (cdr type-name)
        (let ((qualified (list (car type-name) (cadr type-name)))
              (rest (cddr type-name)))
          (while rest
            (let ((res (jff-find-qualified-class search-path qualified nil)))
              (when res
                (throw 'done (list res qualified))))

            (nconc qualified (list (car rest)))
            (setq rest (cdr rest)))))

      ;; Couldn't find anything.  This isn't necessarily an error
      ;; because, for example, type name could actually have been an
      ;; expression name.
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive
;;;

(defun jff-highlight-type-name (type-name)
  "Temporarily highlight a type name in the buffer.  type-name
must be a non-null sublist of the list returned by
`jff-type-name-at-point'."

  (let* ((first (car type-name))
         (start (get-text-property 0 'jff-source first))
         (last (car (last type-name)))
         (end (+ (get-text-property 0 'jff-source last)
                 (length last)))
         (ov (make-overlay start end)))
    (overlay-put ov 'face 'highlight)
    (run-at-time jff-highlight-secs nil #'delete-overlay ov)))

(defun java-find-file-or-class-at-point ()
  "Like find-file, but if point is on a class name and the source
of that class can be found, defaults to that source file."

  (interactive)
  (let* ((type-name (ignore-errors (jff-type-name-at-point)))
         (path-and-name (and type-name
                             (condition-case err
                                 (jff-find-class type-name)
                               (error
                                (message "%s" (error-message-string err))
                                (sit-for 2)
                                nil)))))
    (if (not path-and-name)
        ;; Defer to regular interactive find-file
        (call-interactively #'find-file)
      ;; Prompt like find-file, but with our own default
      (jff-highlight-type-name (second path-and-name))
      (let* ((default (first path-and-name))
             (filename
              (read-file-name "Find file: "
                              (file-name-directory default) nil nil
                              (file-name-nondirectory default))))
        (find-file filename)))))

(defun java-find-class (class-name)
  (interactive "sClass: ")
  (find-file (jff-find-class (jff-parse-class class-name))))

(eval-after-load "cc-mode"
  '(progn
     (message "Re-binding C-x C-f for java-find-file")
     (define-key java-mode-map (kbd "\C-x\C-f") #'java-find-file-or-class-at-point)))

(provide 'java-find-file)
