;; This does not currently handle a few annoying ambiguities,
;; particularly if conventions are turned off
;; * foo.Bar.baz() will be always treated as the class foo.Bar.baz
;;   even if it's actually [variable]foo... or
;;   [package]foo.[class]Bar...
;; * Imports of subtypes are not handled.  For example "import
;;   java.util.Map.Entry;"
;; * Static imports are ignored

;; Enhancements
;; * Be able to specify alternate source directories for other
;;   packages.  I'm not sure if this should just be a sourcepath or if
;;   it should specify which parts of the tree are where.
;; * Completion for java-find-class

(require 'cl)

(defvar jff-highlight-secs 2)

(defconst jff-use-conventions t)

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

(defun jff-class-name-at-point (&optional blink)
  (save-match-data
    (save-excursion
      (let ((case-fold-search nil))
        (skip-chars-backward "a-zA-Z0-9_$.")
        (unless (looking-at jff-class-regex)
          (error "No class name at point"))
        (when blink
          (let ((ov (make-overlay (match-beginning 0)
                                  (match-end 0))))
            (overlay-put ov 'face 'highlight)
            (run-at-time jff-highlight-secs nil
                         (lambda (ov)
                           (delete-overlay ov))
                         ov)))
        (jff-parse-class (match-string 0))))))

(defun jff-get-preamble ()
  (save-match-data
    (save-excursion
      (let* ((case-fold-search nil)
             (bound (progn
                      (parse-partial-sexp (point-min) (point-max) 1)
                      (point)))
             (ppoint (point-min))
             (pstate nil))
        (goto-char (point-min))
        (let ((package nil) (imports '()) (star-imports '()))
          ;; Get the package name
          (while (not package)
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

(defun jff-find-class (class-name)
  (let* ((case-fold-search nil)
         (preamble (jff-get-preamble))
         (package (first preamble))
         (imports
          (append (second preamble)
                  (list (append package '("*")))
                  (third preamble)))
         (unq-class-name (car (last class-name)))
         (found nil))
    ;; Is this a qualified class name?
    (when (cdr class-name)
      ;; Replace the imports so we search only for this exact class
      (setq imports (list class-name)))
    ;; Search imports
    (while (and (consp imports) (not found))
      (let* ((import (car imports))
             (unq-import (car (last import))))
        (setq imports (cdr imports))
        ;; Is this import worth considering?
        (if (or (string= unq-import "*")
                (string= unq-import unq-class-name))
            ;; Figure out how to get from this package to that package
            (let ((goal-package (butlast import))
                  (this-package (copy-list package))
                  (path "./"))
              (while (and goal-package this-package
                          (string= (car goal-package)
                                   (car this-package)))
                (setq goal-package (cdr goal-package)
                      this-package (cdr this-package)))
              (dolist (_ this-package)
                (setq path (concat path "../")))
              (dolist (part goal-package)
                (setq path (concat path part "/")))
              (setq path (concat path unq-class-name ".java"))
              ;; Look for the class
              (if (file-exists-p path)
                  (setq found path)
                (when (not (string= unq-import "*"))
                  (error "Could not find source: %s" path)))))))
    (unless found
      (let ((cn (car class-name)))
        (dolist (cnp (cdr class-name))
          (setq cn (concat cn "." cnp)))
        (error "Could not find source for %s" cn)))
    found))

(defun java-find-file-or-class-at-point ()
  (interactive)
  (let* ((class-name (ignore-errors (jff-class-name-at-point t)))
         (path (and class-name
                    (condition-case err
                        (jff-find-class class-name)
                      (error
                       (message "%s" (error-message-string err))
                       (sit-for 2)
                       nil)))))
    (if path
        (let ((filename
               (read-file-name "Find file: "
                               (file-name-directory path) nil nil
                               (file-name-nondirectory path))))
          (find-file filename))
      (call-interactively #'find-file))))

(defun java-find-class (class-name)
  (interactive "sClass: ")
  (find-file (jff-find-class (jff-parse-class class-name))))

(eval-after-load 'cc-mode
  '(progn
     (message "Binding C-x C-f to java-find-file-or-class-at-point")
     (define-key java-mode-map (kbd "\C-x\C-f") #'java-find-file-or-class-at-point)))

(provide 'java-find-file)
