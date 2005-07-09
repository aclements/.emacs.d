;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming support
;;;
;;; Sets up general programming options and loading of language modes

;;; Non-specific setup
;; Blink those parens
(if (require 'paren nil t)
    (show-paren-mode t))

;; Adapt those fills
(if (require 'filladapt nil t)
    (progn
      ;; Disable built-in adaptive filling
      (setq adaptive-fill-mode nil)
      ;; This is a little weird.  filladapt overloads a bunch of
      ;; autofill's functions, so just by loading it and setting the
      ;; mode variable it will come to life.  The modeline won't be
      ;; updated unless turn-on-filladapt-mode is called from every
      ;; buffer.
      (setq filladapt-mode-line-string " F*")))

;; Tabs are evil.  Use spaces to indent
(setq-default indent-tabs-mode nil)

;; Autoindent after a line
(global-set-key "\C-m" 'newline-and-indent)

;;; General mechanisms for setting up specific modes

;; Set up autoloading of a language mode
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
         ,@forms)
       (add-to-list 'mode-features-alist
                    (cons ',name (function ,func-name))
                    t))))
(defvar mode-features-alist ())

;; Enabled all of the features from feature-list when the modehook is
;; run.  The features will be enabled in the order in which they were
;; defined, not the order in feature-list (note that I don't like
;; elisp enough to make this true across multiple calls to this
;; function).  modehook-maybe-plural may either be a single mode hook,
;; or a list of mode hooks
(defun atc:put-mode-features (modehook-maybe-plural feature-list)
  (if (listp modehook-maybe-plural)
      ;; Iterate over the mode hooks
      (dolist (hook modehook-maybe-plural)
        (atc:put-one-mode-features hook feature-list))
    ;; Just one mode hook
    (atc:put-one-mode-features modehook-maybe-plural feature-list)))

;; Do the real work of atc:put-mode-features for just one modehook
(defun atc:put-one-mode-features (modehook feature-list)
  ;; Iterate over the available features
  (dolist (name-func mode-features-alist)
    (let ((name (car name-func))
          (func (cdr name-func)))
      (when (memq name feature-list)
        (add-hook modehook func t)
        (setq feature-list (delq name feature-list)))))
  ;; Are there any features in the list that weren't registered
  (if (not (null feature-list))
      (error "Unknown features %s" feature-list)))

;;; Mode features

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
(defmodefeature flyspell
  (when-ispell-works (flyspell-prog-mode)))
(defmodefeature flyspell-full
  (when-ispell-works (flyspell-mode t)))

;; Misc general

(defmodefeature plain-newline
  (local-set-key "\C-m" (function newline)))

;; C-related

(defmodefeature c-defun-jump
  (local-set-key "\M-p" (function c-beginning-of-defun))
  (local-set-key "\M-n" (function c-end-of-defun)))
(defmodefeature c-auto-hungry
  (c-toggle-auto-hungry-state 1))
(defmodefeature c-filladapt
  (function c-setup-filladapt))
(defmodefeature c-auto-brace-space
  (defadvice c-electric-brace (around c-auto-brace-space first
                                      (arg) activate)
    (if (or (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if I'm in a literal or if the
        ;; user typed anything other than an open brace
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
(defmodefeature c-auto-close-brace
  (defadvice c-electric-brace (around c-auto-close-brace last
                                      (arg) activate)
    (if (or (not c-auto-newline)
            (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if not in auto mode, if the user
        ;; typed anything other than an open brace, or if the point is
        ;; in a literal
        ad-do-it
      (let ((syntax-type
             (or (save-excursion
                   (c-backward-syntactic-ws)
                   (backward-char)
                   (cond ((looking-at "=") 'assignment)
                         ((looking-at ",") 'list)
                         ((or (looking-at ")\\|;")) 'block)
                         (t nil)))
                 (save-excursion
                   ;; Class, struct, or enum?
                   (c-beginning-of-statement-1)
                   (if (looking-at "typedef\\>")
                       (c-forward-token-1))
                   (if (looking-at "class\\|struct\\|enum\\>")
                       'definition
                     nil))
                 (progn
                   (message "c-auto-close-brace is confused")
                   'block))))
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
            (cond ((or (eq syntax-type 'assignment)
                       (eq syntax-type 'definition))
                   (let ((last-command-char ?\;)
                         (c-cleanup-list (cons 'defun-close-semi
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))
                  ((eq syntax-type 'list)
                   (let ((last-command-char ?,)
                         (c-cleanup-list (cons 'list-close-comma
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))))
          ;; Clean up extra whitespace that may have been inserted
          ;; after the close characters
          (let ((end (point))
                (begin (save-excursion
                         (skip-chars-backward " \t\n")
                         (point))))
            (delete-region begin end)))))))

;; Latex-related

(defmodefeature latex-bindings
  (local-set-key "\M-q" 'LaTeX-fill-paragraph)
  ;; This gets unset by latex-mode
  (local-set-key "\C-m" 'newline-and-indent))
(defmodefeature latex-faces
  ;; Make all of the section faces small
  (dolist (face '(font-latex-title-1-face
                  font-latex-title-2-face
                  font-latex-title-3-face))
    (face-spec-set face '((t (:inherit font-latex-title-4-face))))))

;;; Specific modes

;; C/C++
(setcdr (assoc "\\.h\\'" auto-mode-alist) (function c++-mode))
(atc:put-mode-features 'c-mode-common-hook
                       '(autofill filladapt flyspell
                                  c-defun-jump c-auto-hungry
                                  c-filladapt c-auto-brace-space
                                  c-auto-close-brace))

;; Python
(atc:autoload-mode 'python-mode "python-mode" "\\.py$" "python")
(atc:put-mode-features 'python-mode-hook '(filladapt))

;; MIT Scheme
(atc:autoload-mode 'scheme-mode "xscheme" "\\.scm$")
(atc:put-mode-features 'scheme-mode-hook '(autofill flyspell))

;; AUCTeX
(atc:autoload-mode 'latex-mode "tex-site" "\\.tex$")
(atc:put-mode-features 'LaTeX-mode-hook
                       '(autofill flyspell-full
                                  latex-bindings latex-faces))
;; flyspell only knows about tex-mode by default
(put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)

;; Lisp
(atc:put-mode-features '(lisp-mode-hook emacs-lisp-mode-hook)
                       '(autofill filladapt flyspell))

;; HTML and text
(atc:put-mode-features '(html-mode-hook text-mode-hook)
                       '(autofill flyspell))
(atc:put-mode-features 'text-mode-hook
                       '(plain-newline filladapt))
