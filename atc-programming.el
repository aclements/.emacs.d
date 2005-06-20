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
        (add-hook modehook (function flyspell-prog-mode)))
    (if (member 'flyspell-full feature-list)
        (add-hook modehook (lambda () (flyspell-mode t))))
    (if (member 'plain-newline feature-list)
        (add-hook modehook (lambda () (local-set-key "\C-m" 'newline))))
    ))

;; Python
(atc:autoload-mode 'python-mode "python-mode" "\\.py$" "python")
(atc:set-mode-features 'python-mode '(filladapt))

;; MIT Scheme
(atc:autoload-mode 'scheme-mode "xscheme" "\\.scm$")
(atc:set-mode-features 'scheme-mode-hook '(autofill flyspell))

;; AUCTeX
(atc:autoload-mode 'latex-mode "tex-site" "\\.tex$")
(atc:set-mode-features 'LaTeX-mode-hook '(autofill flyspell-full))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key "\M-q" 'LaTeX-fill-paragraph)
            (local-set-key "\C-m" 'newline-and-indent)
            ;; XXX Fix flyspell so it checks more than just comments.
            ;; There's a more fundamental problem here, because this
            ;; _should_ be set right by flyspell.
            (setq flyspell-generic-check-word-p
                  (function tex-mode-flyspell-verify))
            ;; Make all of the section faces small
            (mapcar
             (lambda (face)
               (face-spec-set face
                              '((t (:inherit font-latex-title-4-face)))))
             '(font-latex-title-1-face
               font-latex-title-2-face
               font-latex-title-3-face))))


;; Lisp
(atc:set-mode-features '(lisp-mode-hook emacs-lisp-mode-hook)
                       '(autofill filladapt flyspell))

;; HTML and text
(atc:set-mode-features '(html-mode-hook text-mode-hook)
                       '(autofill flyspell))
(atc:set-mode-features 'text-mode-hook
                       '(plain-newline filladapt))
