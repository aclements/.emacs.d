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

;; Self-explanatory
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; Set up individual language modes

(require 'atc-features)

;; C/C++ is too scary to fit here
(load "atc-cc")

;; Python
(atc:autoload-mode 'python-mode "python-mode" "\\.py$" "python")
(atc:add-mode-features 'python-mode-hook '(filladapt))

;; MIT Scheme
(atc:autoload-mode 'scheme-mode "xscheme" "\\.scm$")
(atc:add-mode-features 'scheme-mode-hook '(autofill flyspell-prog))

;; Lisp
(atc:add-mode-features '(lisp-mode-hook emacs-lisp-mode-hook)
                       '(autofill filladapt flyspell-prog))

;; Shell
(atc:add-mode-features 'sh-mode-hook
                       '(autofill filladapt flyspell-prog
                                  shell-newline))

;; HTML, text, and Subversion log messages
(atc:add-mode-features '(html-mode-hook text-mode-hook)
                       '(autofill flyspell-full))
(atc:add-mode-features 'text-mode-hook
                       '(plain-newline filladapt))
(atc:autoload-mode 'svn-commit-mode "svn-commit-mode"
                   "svn-commit\\(\\.[0-9]+\\)?\\.tmp")

;; Latex (AUCTeX)
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

(atc:autoload-mode 'latex-mode "tex-site" "\\.tex$")
(atc:add-mode-features 'LaTeX-mode-hook
                       '(autofill flyspell-full
                                  latex-bindings latex-faces))

;;; Fix flyspell

;; Flyspell has major issues when replaying keyboard macros.  I don't
;; know if this fix will correctly check all changes made by keyboard
;; macros (I think it will), but it's well worth it
(defadvice flyspell-post-command-hook (around flyspell-in-macros-bug
                                       activate)
  (unless executing-kbd-macro
    ad-do-it))

;; flyspell only knows about tex-mode by default
;(put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)
