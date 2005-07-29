;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Features support
;;;
;;; The features system is something I defined to separate out general
;;; features that could be applied to modes from the actual choosing
;;; of which modes to apply those features to.  This file defines some
;;; standard features that are likely to be useful across lots of
;;; modes.  Features that are more specific to a given mode can still
;;; use the feature system (it's nice for organization), but should
;;; define those features close to their use.
;;;
;;; This provides atc-features

;; Set up autoloading of a language mode (okay, so this isn't really
;; feature-related, but is general across modes)
(defun atc:autoload-mode (modefunc filename pattern &optional interpreter)
  ;; Autoload this mode when modefunc is invoked
  (autoload modefunc filename nil t)
  ;; Call modefunc when a file matching pattern is loaded
  (add-to-list 'auto-mode-alist (cons pattern modefunc))
  ;; If this mode can be invoked by a #! line, add that interpreter to
  ;; the interpreter mode alist so this mode will be loaded by
  ;; matching #! lines
  (if interpreter
      (add-to-list 'interpreter-mode-alist (cons interpreter modefunc))))

;; Macro to define a mode feature
(defmacro defmodefeature (name &rest forms)
  (let ((func-name (intern (concat "atc:mode-feature-"
                                   (symbol-name name)))))
    `(progn
       (defun ,func-name ()
         (interactive)
         ,@forms)
       (assq-delete-all ',name mode-features-alist)
       (add-to-list 'mode-features-alist
                    (cons ',name (function ,func-name))
                    t))))
(defvar mode-features-alist ())

;; Enabled all of the features from feature-list when the modehook is
;; run.  The features will be enabled in the order in which they were
;; defined, not the order in feature-list (note that I don't like
;; elisp enough to make this true across multiple calls to this
;; function).  modehook-maybe-plural may either be a single mode hook,
;; or a list of mode hooks.  If a feature has already been added to a
;; modehook, this does not re-add it.
(defun atc:add-mode-features (modehook-maybe-plural feature-list)
  (if (listp modehook-maybe-plural)
      ;; Iterate over the mode hooks
      (dolist (hook modehook-maybe-plural)
        (atc:add-one-mode-feature hook feature-list))
    ;; Just one mode hook
    (atc:add-one-mode-feature modehook-maybe-plural feature-list)))

;; Do the real work of atc:add-mode-features for just one modehook
(defun atc:add-one-mode-feature (modehook feature-list)
  ;; Iterate over the available features
  (dolist (name-func mode-features-alist)
    (let ((name (car name-func))
          (func (cdr name-func)))
      (when (memq name feature-list)
        ;; Always append the hook in order to keep the order right (if
        ;; this feature has already been added, this is a no-op)
        (add-hook modehook func t)
        (setq feature-list (delq name feature-list)))))
  ;; Are there any features in the list that weren't registered
  (if (not (null feature-list))
      (error "Unknown features %s" feature-list)))

;;; General features

;; Fill-related

(defmodefeature autofill
  (auto-fill-mode t))
(defmodefeature refill
  (error "Refill isn't working right now")
  (refill-mode t)
  (setq use-hard-newlines t))
(defmodefeature filladapt
  (if (featurep 'filladapt) (turn-on-filladapt-mode)))

;; Spelling-related

;; ispell.el is beautifully written to make it impossible to
;; gracefully deal with a missing ispell program since loading ispell
;; will cause it to fail, but without loading it, you can't find out
;; what program it's going to try to load
(defmacro when-ispell-works (&rest forms)
  `(if (and (boundp 'ispell-program-name)
            (executable-find ispell-program-name))
       (progn
         ,@forms)
     (message "Warning: ispell not found")))
(defmodefeature flyspell-prog
  (when-ispell-works (flyspell-prog-mode)))
(defmodefeature flyspell-full
  (when-ispell-works (flyspell-mode t)))

;; Misc

(defmodefeature plain-newline
  (local-set-key "\C-m" (function newline)))
(defun sh-newline-and-actually-indent ()
  (interactive)
  (sh-newline-and-indent)
  (indent-according-to-mode))
(defmodefeature shell-newline
  (local-set-key "\C-m" (function sh-newline-and-actually-indent)))
(defmodefeature highlight-unhappy
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|XXX\\)\\>"
                             1 font-lock-warning-face prepend))))
(defmodefeature final-newline-ask
  (make-local-variable 'require-final-newline)
  (setq require-final-newline 1))
(defmodefeature final-newline-always
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t))

(provide 'atc-features)
