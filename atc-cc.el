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
  (function c-setup-filladapt))

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

;;; Set up the mode itself

;; A lot of .h files are actually C++
(setcdr (assoc "\\.h\\'" auto-mode-alist) (function c++-mode))

;; Set C's features
(atc:add-mode-features 'c-mode-common-hook
                       '(autofill filladapt flyspell-prog
                                  highlight-unhappy
                                  final-newline-always c-defun-jump
                                  c-auto-hungry c-filladapt
                                  c-magic-punctuation c-show-func
                                  c-compile))
