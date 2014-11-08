;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C support
;;;
;;; This supports modes derived from cc-mode

(defun c-backward-defun (&optional arg)
  "Move backwards to the beginning of this function declaration.  This
differs from `c-beginning-of-defun' in that it actually takes the
point to the beginning of the function definition instead of just its
block."
  (interactive "p")
  (unless arg (setq arg 1))
  (while (> arg 0)
    (c-beginning-of-defun)
    (c-beginning-of-statement)
    (setq arg (1- arg))))

;;; Define C-specific features

(require 'atc-features)

(defmodefeature c-defun-jump
  (local-set-key "\M-p" (function c-backward-defun))
  (local-set-key "\M-n" (function c-end-of-defun)))

(defmodefeature c-auto-hungry
  (c-toggle-auto-hungry-state 1))

(defmodefeature c-filladapt
  (when (featurep 'filladapt)
    (turn-on-filladapt-mode)
    (c-setup-filladapt)))

(defmodefeature c-magic-punctuation
  (when (require 'c-magic-punctuation nil t)
    (c-magic-punctuation-mode)))

(defmodefeature c-show-func
  (when (require 'show-context-mode nil t)
    (show-context-mode 1)))

(defmodefeature c-compile
  (local-set-key "\C-c\C-c" (function compile)))

(defmodefeature java-find-file
  (require 'java-find-file))

(defmodefeature java-fix-generics
  ;; Setting this to t at least fixes the indentation of braces for
  ;; constructors of generic classes.  It might fix other things, too.
  (setq c-recognize-<>-arglists t))

(defun streambase-style ()
  (c-set-offset 'inline-open 0)
  (c-set-offset 'label '*)
  (c-set-offset 'case-label '*)
  (c-set-offset 'statement-case-intro '*)
  (c-set-offset 'statement-case-open '*)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'access-label '/)

  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

(defun cc-fix-slash-paren ()
  "Fix paren balancing where the open paren appears to be escaped.
This is not valid syntax in C, but can be in C-like languages."
  (make-variable-buffer-local 'parse-sexp-lookup-properties)
  (setq parse-sexp-lookup-properties t)
  (make-variable-buffer-local 'font-lock-syntactic-keywords)
  (setq font-lock-syntactic-keywords
        (cons '("\\(\\\\\\)(" (1 "."))
              font-lock-syntactic-keywords)))

;; Force linux style to use tabs
(eval-after-load 'cc-styles
  '(let ((linux-style (cdr (assoc "linux" c-style-alist))))
     (c-add-style "linux" (cons '(indent-tabs-mode . t) linux-style))))

(defmodefeature c-choose-style
  (let ((filename (buffer-file-name))
        (hostname (system-name)))
    (cond ((not filename))
          ;; XXX As of 2011-08, JOS takes care of its own indentation,
          ;; but I'm keeping this for old code.
          ((or (string-match "/jos/" filename)
               (string-match "/6.828/" filename))
           (message "Setting style for 6.828")
           ;; (c-set-style "gnu")
           ;;(make-local-variable 'c-basic-offset)
           (c-set-style "bsd")
           (make-local-variable 'perl-indent-level)
           (setq perl-indent-level 8
                 tab-width 8
                 indent-tabs-mode t))
          ((string-match "/qemu" filename)
           (message "Setting style for qemu")
           (setq c-basic-offset 4))
          ((string-match "/polyglot[/-]" filename)
           (message "Setting style for Polyglot")
           (setq c-basic-offset 4
                 indent-tabs-mode nil))
          ((string-match "/pub/xtc" filename)
           (message "Setting style for XTC")
           ;; This is not the style used by the XTC codebase
           (setq c-basic-offset 4
                 indent-tabs-mode nil))
          ((string-match "/atcc/" filename)
           (message "Setting style for RSCC/ATCC")
           (cc-fix-slash-paren)
           (c-set-style "bsd")
           (setq c-basic-offset 8
                 tab-width 8
                 indent-tabs-mode t))
          ((string-match ".streambase.com$" hostname)
           (message "Setting style for StreamBase")
           (streambase-style))
          ((and (functionp 'vmstyle-set-c-style)
                (string-match "/bora/" filename))
           (message "Setting style for VMware")
           (vmstyle-set-c-style))
          ((or (string-match "/linux-" filename)
               (string-match "/scale-linux" filename))
           (message "Setting style for Linux kernel")
           (c-set-style "linux")
           (setq indent-tabs-mode t))
          ((string-match "/mcchart/" filename)
           (message "Setting style for MCChart")
           (c-set-style "linux")
           (setq indent-tabs-mode t))
          ((string-match "/postgresql-8" filename)
           (message "Setting style for Postgres")
           (c-set-style "bsd")
           (setq c-basic-offset 4
                 tab-width 4
                 indent-tabs-mode t)
           (c-set-offset 'case-label '+))
          ((string-match (concat "^" (regexp-quote (expand-file-name "~/go")))
                         filename)
           (message "Setting style for Go")
           (c-set-style "bsd")
           (setq comment-start "// "
                 comment-end ""))
          )))
;; Don't decompress .z files.  This stopped working in Emacs 23.2 and
;; I don't use it any more.
;;
;; (mapcar
;;  (lambda (a)
;;    (when (string-match "\\\\\\.g\\?z" (aref a 0))
;;      (aset a 0 (replace-match "\\.gz" t t (aref a 0)))))
;;  jka-compr-compression-info-list)

;;; Set up the mode itself

;; Automatically determine the major mode of header files
(unless (require 'h-auto-mode-load nil t)
  ;; A lot of .h files are actually C++
  (add-to-list 'auto-mode-alist (cons "\\.h\\'" (function c++-mode))))

;; Set C's features
(atc:add-mode-features 'c-mode-common-hook
                       '(autofill flyspell-prog highlight-unhappy
                                  final-newline-always c-defun-jump
                                  c-auto-hungry c-filladapt
                                  ;;c-magic-punctuation
                                  c-choose-style c-show-func
                                  c-compile ))
;; Java features
(atc:add-mode-features 'java-mode-hook
                       '(java-find-file java-fix-generics))
;; Set Perl "features", for those unhappy times...
(atc:add-mode-features 'perl-mode-hook
                       '(flyspell-prog final-newline-always
                         c-choose-style))

(defun atc--set-c-cleanup-list ()
  ;; This is insane.  Since c-cleanup-list is a style variable, if we
  ;; modify it directly during c-mode-common-hook and a file or dir
  ;; local variable sets a new C style (a common practice), then our
  ;; changes will be overridden.  Hence we do it after local variables
  ;; have been applied.
  (add-hook 'hack-local-variables-hook
            (lambda ()
              ;; Undo most of the stupid things auto-newline mode does
              (add-to-list 'c-cleanup-list 'brace-else-brace)
              (add-to-list 'c-cleanup-list 'brace-elseif-brace)
              (add-to-list 'c-cleanup-list 'brace-catch-brace)
              (add-to-list 'c-cleanup-list 'defun-close-semi)
              (add-to-list 'c-cleanup-list 'list-close-comma))
            t t))

(add-hook 'c-mode-common-hook 'atc--set-c-cleanup-list t)
