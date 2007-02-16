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
  (local-set-key "\C-c\C-c" (function compile))
  (setq compilation-window-height 10
        compilation-scroll-output t))

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

(defmodefeature c-choose-style
  (let ((filename (buffer-file-name))
        (hostname (system-name)))
    (cond ((or (string-match "/jos/" filename)
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
          ((string-match "/atcc/" filename)
           (message "Setting style for RSCC/ATCC")
           (setq c-basic-offset 8
                 tab-width 8
                 indent-tabs-mode t))
          ((string-match ".streambase.com$" hostname)
           (message "Setting style for StreamBase")
           (streambase-style)))))
;; Don't decompress .z files
(mapcar
 (lambda (a)
   (when (string-match "\\\\\\.g\\?z" (aref a 0))
     (aset a 0 (replace-match "\\.gz" t t (aref a 0)))))
 jka-compr-compression-info-list)

;;; Set up the mode itself

;; A lot of .h files are actually C++
(defun assoc-set-or-add (alist-var key value)
  (let ((pair (assoc key (symbol-value alist-var))))
    (if pair
        (setcdr pair value)
      (add-to-list alist-var (list key value)))))
(assoc-set-or-add 'auto-mode-alist "\\.h\\'" (function c++-mode))

;; Set C's features
(atc:add-mode-features 'c-mode-common-hook
                       '(autofill flyspell-prog highlight-unhappy
                                  final-newline-always c-defun-jump
                                  c-auto-hungry c-filladapt
                                  c-magic-punctuation c-show-func
                                  c-compile c-choose-style))
;; Set Perl "features", for those unhappy times...
(atc:add-mode-features 'perl-mode-hook
                       '(flyspell-prog final-newline-always
                         c-choose-style))
