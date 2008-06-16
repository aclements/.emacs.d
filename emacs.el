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
;; * magic-buffer-list
;; * Better bookmarks system
;; ** Place overlay arrows by bookmark markers
;; ** Jump to bookmark should display an electric buffer showing the
;;    context of each bookmark (one line above and below).  Either all
;;    bookmarks or just buffer bookmarks.  Order by position in file
;;    (recentness is hard to code, and probably harder to use)
;; ** Integrate with magic-buffer-list.  Hit 'b' on buffer line to
;;    bring up bookmarks menu for that buffer.  Add a getter for
;;    number of bookmarks
;; ** Ability to save bookmarks between sessions, like save-place (the
;;    existing bookmarks system looks like it does this well)
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

;; Make it easy to test for Emacs 22
(setq emacs22 (>= emacs-major-version 22))

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

(unless emacs22
  (atc:add-to-load-path-maybe
   "~/sys/elisp/extra.21"
   "Failed to find Emacs 21 user-local packages" t nil)
  (atc:add-to-load-path-maybe
   "~/sys/elisp/extra-pre.21"
   "Failed to find Emacs 21 user-local override packages" nil nil))

(setq atc-load-fast
      (let ((env (getenv "EMACS_LOAD_FAST")))
        (and env (> (length env) 0))))

;; Load customization files
(load "atc-basic")
(atc:setup-user "Austin Clements" "amdragon@mit.edu")
(atc:basic-setup-all)
(load "atc-programming")

(load "atc-google" t)                   ; Non-fatal if missing
(load "atc-streambase" t)
(load "atc-vmware" t)
