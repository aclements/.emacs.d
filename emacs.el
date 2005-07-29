;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Amthrax's Emacs customizations
;;;

;; Note to self:  To compile el's:
;; M-x byte-compile-file RET
;; Give it the .el file

(message "Loading .emacs")

;; Things to fix
;; * Make run-scheme use xscheme from the get-go
;; * refill in latex-mode
;; * Replace xscheme
;; * lua-mode
;; * Colorize column violations
;; * Try custom-set-variables to make things better behaved
;; * Standardize keys for comment region and compile
;; * Make arrows automatically skip over basic-offset spaces to
;;   emulate tab-like motion
;; * Make compilation-mode better.  It should be able to scroll the
;;   output as it appears until the first error appears, then stay
;;   there.  It should also go away automatically if no errors occur.
;; * isearch should work in the electric buffer list
;;
;;
;; Thing I think are fixed, but need more testing
;; * Flyspell on non-comments
;; * Fix else/catch vs auto-hungry thing
;; * auto-fill in c-mode
;; * ebackup
;; * newline-and-indent in LaTeX
;; * Brace insertion in C++ (look at skeleton.el)
;; * gnuclient/emacsclient with a new frame

;; Set appropriate load-path
(defun atc:add-to-load-path-maybe (path msg append fatal)
  (if (not (file-accessible-directory-p path))
      (if fatal
          (error msg)
        (message msg))
    (add-to-list 'load-path path append)))
(atc:add-to-load-path-maybe
 "~/sys/elisp" "Failed to find elisp directory" nil t)
(atc:add-to-load-path-maybe
 "~/sys/elisp/extra" "Failed to find user-local packages" t nil)
;; XXX I wish there was a way to load the _latest_ version of these
;; override packages, since mine are sure to get out of date
(atc:add-to-load-path-maybe
 "~/sys/elisp/extra-pre" "Failed to find user-local override packages"
 nil nil)

;; Load customization files
(load "atc-basic")
(atc:setup-user "Austin Clements" "amdragon@mit.edu")
(atc:basic-setup-all)
(load "atc-programming")
(load "atc-google" t)                   ; Non-fatal if missing
