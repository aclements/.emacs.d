;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic configuration
;;;
;;; Configures basic global variables and look 'n feel

;;; Set up user information
(setq user-full-name    "Austin Clements"
      user-mail-address "amdragon@mit.edu")

;;; Disable frame fluff and set colors (in case X resources aren't around)
;; These are predicated to avoid autoloading modules solely for the
;; purpose of disabling them (ie, the tool-bar module)
(if tool-bar-mode (tool-bar-mode -1))
(if menu-bar-mode (menu-bar-mode -1))
(if scroll-bar-mode (scroll-bar-mode -1))

;; Set frame colors right now and when a new frame is made
(defun atc:set-frame-colors (frame)
  (modify-frame-parameters frame '((background-color . "Black")
				   (foreground-color . "White")
				   (cursor-color . "Red"))))
(add-hook 'after-make-frame-functions (function atc:set-frame-colors))
(atc:set-frame-colors nil)

;;; Set up buffer look
(setq truncate-partial-width-windows nil
      column-number-mode             t
      frame-title-format             '("%b" " - " invocation-name))
(global-font-lock-mode t)

;;; Enable usability features
(mouse-wheel-mode t)

(if (require 'saveplace nil t)
    (setq-default save-place t))

(toggle-uniquify-buffer-names)
(setq uniquify-buffer-name-style 'forward)

(setq load-warn-when-source-newer t
      inhibit-startup-message     t
      next-line-add-newlines      nil
      x-stretch-cursor            t)

;; Place backup files in ~/.emacs-backup
(if (require 'ebackup nil t)
    ;; Only keep 5 last backup copies around (the default is 10)
    (setq ebackup-max-copies 5))

;; Interpret ANSI color codes in grep output
(require 'compilation-colorization nil t)

;;; Set general settings
;; aspell rocks.  ispell sucks.  Use aspell if it's available
(if (executable-find "aspell")
    (setq ispell-program-name "aspell")
  (setq ispell-program-name "ispell"))

;; The Emacs terminal emulator tends to confuse programs by setting
;; TERM to "Emacs".  It's basically a vt100 emulator (just like every
;; other terminal emulator out there)
(setenv "TERM" "vt100")

;; Ange FTP really slows down filename completion, so disable this
(let ((new-fnha ())
      (fnha file-name-handler-alist))
  (while fnha
    (when (not (string-match "^ange-.*" (symbol-name (cdar fnha))))
      (add-to-list 'new-fnha (car fnha)))
    (setq fnha (cdr fnha)))
  (setq file-name-handler-alist new-fnha))

;;; Set global bindings
(global-set-key "\C-c\g"   (function goto-line))
(global-set-key "\C-x\C-k" (function kill-buffer))
(global-set-key "\C-x\C-b" (function electric-buffer-list))
;; The following is because some terminals use C-h and some use C-?
;; for backspace.  Emacs by default only understands C-?.  I'm sure
;; one of these is the Right Way and I shouldn't even be using Emacs
;; on terminals that do it the other way.
(global-set-key "\C-h"     (function delete-backward-char))

;;; Enable dangerous functions
(put 'narrow-to-region 'disabled nil)
