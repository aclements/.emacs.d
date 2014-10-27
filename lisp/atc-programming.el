;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming support
;;;
;;; Sets up general programming options and loading of language modes

;;; Non-specific setup

;; Blink those parens
(if (require 'paren nil t)
    (show-paren-mode t))

;; Adapt those fills
;; XXX (2013-05-01) filladapt apparently hasn't been updated since the
;; 90's and since it replaces much of Emacs' filling mechanism, has
;; started breaking more things.
;; (if (require 'filladapt nil t)
;;     (progn
;;       ;; Disable built-in adaptive filling
;;       ;; XXX (2012-09-16) Setting this screws up Python string filling
;;       ;; even if filladapt isn't enabled in python-mode.  The
;;       ;; filladapt documentation doesn't say anything about setting
;;       ;; this, so hopefully this was a mistake.
;; ;;      (setq adaptive-fill-mode nil)
;;       ;; This is a little weird.  filladapt overloads a bunch of
;;       ;; autofill's functions, so just by loading it and setting the
;;       ;; mode variable it will come to life.  The modeline won't be
;;       ;; updated unless turn-on-filladapt-mode is called from every
;;       ;; buffer.
;;       (setq filladapt-mode-line-string " F*")))

;; Tabs are evil.  Use spaces to indent
(setq-default indent-tabs-mode nil)

;; Make tabs visible
(defface show-tab-face '((t :strike-through "#4a4a00"))
  "Face to use to highlight tabs.")

(defun show-tabs ()
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\t
        (vector (make-glyph-code ?\t 'show-tab-face))))

;; Autoindent after a line
(global-set-key "\C-m" 'newline-and-indent)

;; Self-explanatory
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq
 ;; Make auto-fill in programming modes on affect comments
 comment-auto-fill-only-comments t
 ;; When commenting a block with a block-style comment, use a single
 ;; multi-line comment where the start and end are on their own lines
 comment-style 'extra-line)

;; Spiff up compilation mode.  It would be even cooler if the buffer
;; automatically went away when compilation finished with no errors
;; (or possibly never appeared if there were no errors).  See
;; compilation-finish-functions hook.  Also, it would be nice if it
;; automatically scrolled until it hit the first error (Use
;; compilation-filter-hook?), or at least scrolled with the output and
;; then jumped the point to the first error.  (compilation-next-error
;; 1 nil (point-min)) will move point to the first error in the
;; compilation buffer, or give a Lisp error if there are none.
(setq compilation-window-height 10
;;      compilation-context-lines 1
      compilation-scroll-output t)

;; XXX This doesn't auto-hide if there are no errors
(setq compilation-scroll-output 'first-error)

;;; Set up individual language modes

(require 'atc-features)

;; C/C++ is too scary to fit here
(load "atc-cc")

;; Python
(atc:add-mode-features 'python-mode-hook '(flyspell-prog))
(setq python-fill-docstring-style 'pep-257-nn)

;; MIT Scheme
;(atc:autoload-mode 'scheme-mode "xscheme" "\\.scm$")

;; PLT Scheme
(defun scheme-send-buffer ()
  (interactive)
  (scheme-send-region (point-min)
                      (point-max)))
(defmodefeature quack-send-buffer
  (local-set-key "\C-c\C-c" 'scheme-send-buffer)
  ;; Quack inadvertently changes the semantics of scheme-proc in bad
  ;; ways.  It advises it so that, if no current inferior Scheme
  ;; buffer exists, it starts a new one.  However, it has the
  ;; side-effect of switching to this new buffer.  This break
  ;; functions like scheme-send-region, which then try to read from
  ;; the Scheme process buffer instead of the buffer in which they
  ;; were called.
  (defadvice scheme-proc (around atc-ad-fix-quack first nil activate)
    (save-current-buffer
      ad-do-it)))

(atc:autoload-mode 'scheme-mode "quack" "\\.scm$")
(atc:add-mode-features 'scheme-mode-hook '(autofill filladapt
                                                    flyspell-prog
                                                    quack-send-buffer))
(setq quack-fontify-style 'emacs
      ;; Alas, this only works with plt-style fontification
      quack-pretty-lambda-p t
      quack-run-scheme-always-prompts-p t)

;; Lisp
(atc:add-mode-features '(lisp-mode-hook emacs-lisp-mode-hook)
                       '(autofill filladapt flyspell-prog))

;; Shell
(defmodefeature sh-choose-style
  (let ((filename (buffer-file-name)))
    (cond ((not filename))
          ((or (string-match "/jos/" filename)
               (string-match "/6.828/" filename))
           (message "Setting style for 6.828")
           (setq sh-basic-offset 8
                 tab-width 8
                 indent-tabs-mode t)))))

(atc:add-mode-features 'sh-mode-hook
                       '(;autofill
                         sh-choose-style
                         filladapt flyspell-prog))

;; HTML, text, and Subversion log messages
(atc:add-mode-features '(html-mode-hook text-mode-hook)
                       '(autofill flyspell-full))
(atc:add-mode-features 'text-mode-hook
                       '(plain-newline filladapt))
(when (require 'svn-msg-load nil t)
  (require 'svnci-load nil t))

;; Latex (AUCTeX)
(defmodefeature latex-bindings
  (local-set-key "\M-q" 'LaTeX-fill-paragraph)
  ;; This gets unset by latex-mode
  (local-set-key "\C-m" 'newline-and-indent))
(defmodefeature latex-faces
  ;; Add a "problem" title keyword
  (setq font-latex-match-sectioning-1-keywords '("problem"))
  (font-latex-match-sectioning-1-make)
  ;; Disable French and German quotes
  (setq font-latex-quote-list '(("``" "''"))))

(defun tex-choose-default-command ()
  (let ((dir (file-name-directory (buffer-file-name))))
    (if (or (file-exists-p (expand-file-name "Makefile" dir))
            (file-exists-p (expand-file-name "GNUmakefile" dir))
            (file-exists-p (expand-file-name "makefile" dir)))
        (setq TeX-command-default "Make")
        (setq TeX-command-default "Rubber"))))

(atc:autoload-mode 'latex-mode "tex-site" "\\.tex$")
(atc:add-mode-features 'LaTeX-mode-hook
                       '(autofill-code filladapt flyspell-full
                                  latex-bindings latex-faces))
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-command-list
                  '("Make" "make" TeX-run-compile t t)
                  t)
     (add-to-list 'TeX-command-list
                  '("Make PS" "make %f" TeX-run-compile t t)
                  t)

     ;; Always use rubber to compile LaTeX
     (add-to-list 'TeX-expand-list
                  '("%(rubberarg)"
                    (lambda () (if TeX-PDF-mode "--pdf" ""))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(rubberarg) -Wrefs -Wmisc %t"
                    TeX-run-compile nil (latex-mode)))
     (add-hook 'LaTeX-mode-hook
               'tex-choose-default-command)

     ;; Use evince if available
     (when (executable-find "evince")
       (add-to-list 'TeX-expand-list
                    '("%(evincepagelabel)"
                      (lambda ()
                        (if TeX-sync-output-page-function
                            (concat "--page-label="
                                    (funcall TeX-sync-output-page-function)
                                    " ")
                          ""))))
       (add-to-list 'TeX-output-view-style
                    '("^pdf$" "." "evince %(evincepagelabel)%o")))))

;;; Fix flyspell

;; flyspell only knows about tex-mode by default
;; Not necessary with new version of flyspell
;(put 'latex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)

;; Assembly
;;(atc:autoload-mode '8051-mode "8051-mode" "\\.asm$")
;;(atc:add-mode-features '8051-mode-hook '(autofill filladapt
;;                                                  flyspell-prog))

;; RSCC Grammar
(atc:autoload-mode 'rsccg-mode "rsccg-mode" "\\.g$")

;; Haskell
;; XXX
;;(load "~/sys/elisp/extra-pre/haskell-mode-2.1/haskell-site-file")
(defmodefeature haskell-filladapt
  ;; Add Haskell comments to filladapt.
  ;; XXX These variables aren't local, so the only advantage of doing
  ;; this here is that it won't happen until filladapt needs to be
  ;; loaded
  (add-to-list 'filladapt-token-table
               '("-- " haskell-comment))
  (add-to-list 'filladapt-token-match-table
               '(haskell-comment haskell-comment))
  (add-to-list 'filladapt-token-conversion-table
               '(haskell-comment . exact)))
(atc:autoload-mode 'haskell-mode "haskell-mode" "\\.hs$")
(atc:add-mode-features 'haskell-mode-hook
                       '(flyspell-prog filladapt haskell-filladapt))

;; Literate Haskell
(atc:autoload-mode 'latex-mode "tex-site" "\\.lhs$")
(when (load "mmm-auto" t)
  (mmm-add-classes
   '((literate-haskell
      :submode haskell-mode
      :front "\\\\begin[ \t]*{code}\n"
      ;; The \n at the beginning of back prevents the mis-fontification
      ;; of the line matching this regex.  Without this, haskell-mode
      ;; will fontify it when the haskell-mode region is edited
      :back "\n\\\\end[ \t]*{code}")))
  (add-to-list 'mmm-mode-ext-classes-alist
               '(latex-mode "\\.lhs$" literate-haskell)))

;; XML
(if (fboundp 'nxml-mode)
    ;; Emacs 23
    (progn
      (fset 'html-mode 'nxml-mode)
      (add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode)))
  ;; Emacs <= 22
  (when (load "nxml-mode/rng-auto" t)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
    ;; Based on http://www.emacswiki.org/cgi-bin/wiki/NxmlMode
    (fset 'xml-mode 'nxml-mode)
    (fset 'html-mode 'nxml-mode)
    (if (boundp 'magic-mode-alist)
        ;; Emacs 22
        (add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
      ;; Emacs <= 21
      (add-hook 'hack-local-variables-hook
                (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (when (looking-at "^<\\?xml ")
                      (nxml-mode))))))))
(when (fboundp 'nxml-mode)
  (atc:add-mode-features 'nxml-mode-hook
                         '(flyspell-full autofill)))

(defun atc:flyspell-prog-mode ()
  (when-ispell-works (flyspell-prog-mode)))

;; Javascript
(add-hook 'js-mode-hook #'atc:flyspell-prog-mode)

;; Utilities for Elisp programming
(defmacro time-it (&rest code)
  "Time CODE, repeating it to get an accurate timing.

Returns a string giving the pretty-printed time per iteration and
the number of iterations executed."

  (let ((start-time (gensym "start-time"))
        (iters (gensym "iters"))
        (delta (gensym "delta"))
        (i (gensym "i")))
    `(let ((,iters 1) (,delta 0))
       ;; Repeat until we reach a threshold delta
       (while (< ,delta 0.5)
         (let ((,i 0) (,start-time (get-internal-run-time)))
           (while (< ,i ,iters)
             ,@code
             (setq ,i (1+ ,i)))
           (setq ,delta (float-time (time-subtract (get-internal-run-time)
                                                   ,start-time))
                 ,iters (* ,iters 2))))
       (time-it--format ,delta (/ ,iters 2)))))

(defun time-it--format (total iters)
  (let ((factors `((,(* 24 60 60) "day") (,(* 60 60) "hour") (60 "minute")
                   (1 "second") (1e-3 "millisecond") (1e-6 "microsecond")
                   (1e-9 "nanosecond")))
        (secs (/ total iters)))
    (while (and (cdr factors) (< secs (caar factors)))
      (setq factors (cdr factors)))
    (concat (format "%g %s" (/ secs (caar factors)) (cadar factors))
            (if (/= (/ secs (caar factors)) 1) "s")
            " ("
            (if (/= (caar factors) 1) (format "%ss, " secs))
            (format "%d iterations)" iters))))
