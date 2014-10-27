;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Amthrax's Emacs customizations
;;;

;; Note to self:  To compile el's:
;; M-x byte-compile-file RET
;; Give it the .el file

(message "Loading init.el")

;; XXX Figure out when auto-fill-function gets set globally to
;; c-do-auto-fill and breaks everything.
(defun amdragon-monitor-auto-fill ()
  (if (eq (default-value 'auto-fill-function) 'c-do-auto-fill)
      (message (propertize ">>>> auto-fill-function set to c-do-auto-fill <<<<"
                           'face '(:background "red" :weight bold)))))
(setq amdragon-monitor-auto-fill-timer
      (run-with-idle-timer 0.1 t #'amdragon-monitor-auto-fill))

;; Set appropriate load-path
(defun atc:add-to-load-path (path &optional append fatal)
  (setq path (expand-file-name path user-emacs-directory))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path append)
    (let ((msg (format "%s does not exist" path)))
      (if fatal (error "%s" msg) (message "%s" msg)))))

(atc:add-to-load-path "lisp" nil t)

(atc:add-to-load-path "extra" 'append)
;; XXX I wish there was a way to load the _latest_ version of these
;; override packages, since mine are sure to get out of date
(atc:add-to-load-path "extra-pre")

(setq atc-load-fast
      (let ((env (getenv "EMACS_LOAD_FAST")))
        (and env (> (length env) 0))))

;; Load customization files
(load "atc-basic")
(atc:setup-user "Austin Clements" "aclements@csail.mit.edu")
(atc:basic-setup-all)
(load "atc-programming")

(load "atc-notmuch" t)

(load "atc-google" t)                   ; Non-fatal if missing
(load "atc-streambase" t)
(load "atc-vmware" t)

(load "git-commit-load" t)
(add-hook 'git-commit-mode-hook 'flyspell-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "navy"))))
 '(mmm-default-submode-face ((((background dark)) (:background "DimGray")) (t (:background "LightGray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-template "setopt csh_null_glob; grep <X> <C> -nH -e <R> <F>")
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(send-mail-function (quote sendmail-send-it)))
