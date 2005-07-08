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

;;; Set up specific language modes
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

;; Set up additional features to enable when entering a mode
(defun atc:set-mode-features (modehook feature-list)
  (if (listp modehook)
      ;; Iterate over the mode hooks
      (mapcar (lambda (hook) (atc:set-mode-features hook feature-list))
              modehook)
    ;; Set the features for just one mode hook
    (if (member 'autofill feature-list)
        (add-hook modehook (lambda () (auto-fill-mode t))))
    (if (member 'refill feature-list)
        (progn
          ;; XXX Fix refill.  I think it's not playing well with
          ;; filladapt, but I'm not sure
          (error "Refill isn't working right now")
          (add-hook modehook (lambda () (refill-mode t)))
          (setq use-hard-newlines t)))
    (if (and (member 'filladapt feature-list)
             ;; XXX Need to featurepify more things
             (featurep 'filladapt))
        (add-hook modehook (function turn-on-filladapt-mode)))
    (if (member 'flyspell feature-list)
        (add-hook modehook (lambda ()
                             (when-ispell-works (flyspell-prog-mode)))))
    (if (member 'flyspell-full feature-list)
        (add-hook modehook (lambda ()
                             (when-ispell-works (flyspell-mode t)))))
    (if (member 'plain-newline feature-list)
        (add-hook modehook
                  (lambda () (local-set-key "\C-m" (function newline)))))
    (if (member 'c-defun-jump feature-list)
        (add-hook modehook
                  (lambda ()
                    (local-set-key "\M-p" (function c-beginning-of-defun))
                    (local-set-key "\M-n" (function c-end-of-defun)))))
    (if (member 'c-auto-hungry feature-list)
        (add-hook modehook
                  (lambda () (c-toggle-auto-hungry-state 1))))
    (if (member 'latex-bindings feature-list)
        (add-hook modehook
                  (lambda ()
                    (local-set-key "\M-q" 'LaTeX-fill-paragraph)
                    ;; This gets unset by latex-mode
                    (local-set-key "\C-m" 'newline-and-indent))))
    (if (member 'latex-faces feature-list)
        (add-hook modehook
                  (lambda ()
                    ;; Make all of the section faces small
                    (mapcar
                     (lambda (face)
                       (face-spec-set face
                                      '((t (:inherit font-latex-title-4-face)))))
                     '(font-latex-title-1-face
                       font-latex-title-2-face
                       font-latex-title-3-face)))))
    ))

;; C/C++
(setcdr (assoc "\\.h\\'" auto-mode-alist) (function c++-mode))
(atc:set-mode-features 'c-mode-common-hook
                       '(autofill filladapt flyspell
                                  c-defun-jump c-auto-hungry))

;; Python
(atc:autoload-mode 'python-mode "python-mode" "\\.py$" "python")
(atc:set-mode-features 'python-mode-hook '(filladapt))

;; MIT Scheme
(atc:autoload-mode 'scheme-mode "xscheme" "\\.scm$")
(atc:set-mode-features 'scheme-mode-hook '(autofill flyspell))

;; AUCTeX
(atc:autoload-mode 'latex-mode "tex-site" "\\.tex$")
(atc:set-mode-features 'LaTeX-mode-hook
                       '(autofill flyspell-full
                                  latex-bindings latex-faces))
;; flyspell only knows about tex-mode by default
(put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)

;; Lisp
(atc:set-mode-features '(lisp-mode-hook emacs-lisp-mode-hook)
                       '(autofill filladapt flyspell))

;; HTML and text
(atc:set-mode-features '(html-mode-hook text-mode-hook)
                       '(autofill flyspell))
(atc:set-mode-features 'text-mode-hook
                       '(plain-newline filladapt))
