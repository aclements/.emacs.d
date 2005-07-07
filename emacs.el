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
;; * Had to do
;;   (setq flyspell-generic-check-word-p
;;         (function tex-mode-flyspell-verify))
;;   in LaTeX mode (it's not setting it automagically)
;; * Make it not remember places or save backups in subversion log
;;   messages
;; * Brace insertion in C++ (look at skeleton.el)
;; * newline-and-indent in LaTeX
;; * Replace xscheme
;; * lua-mode
;; * gnuclient/emacsclient with a new frame
;; * Colorize TODO, FIXME, and XXX, as well as style violations
;; * Try custom-set-variables to make things better behaved
;; * C-x C-c should be close-dwim (close the current frame unless it's
;;   the only frame, possibly closing gnuclient as well)
;; * Standardize keys for comment region and compile
;;
;;
;; Thing I think are fixed, but need more testing
;; * Flyspell on non-comments
;; * Fix else/catch vs auto-hungry thing
;; * auto-fill in c-mode
;; * ebackup
;; * grep decolorization (really, there should be a colorizing filter)

; Set appropriate load-path
(if (not (file-accessible-directory-p "~/sys/elisp"))
    (error "Failed to find load path")
  (add-to-list 'load-path "~/sys/elisp")
  (if (not (file-accessible-directory-p "~/sys/elisp/extra"))
      (message "Failed to find user-local packages")
    (add-to-list 'load-path "~/sys/elisp/extra")))

; Load customization files
(load "atc-basic")
(load "atc-programming")
(load "atc-google")

;; ;; M-x customize stuff
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(browse-url-browser-function (quote browse-url-mozilla))
;;  '(browse-url-mozilla-program "mozilla-tab")
;;  '(bsh-classpath (quote ("/usr/share/java/")))
;;  '(bsh-jar "/usr/share/java/bsh.jar")
;;  '(c-cleanup-list (quote (brace-else-brace brace-elseif-brace brace-catch-brace scope-operator defun-close-semi)))
;;  '(c-echo-syntactic-information-p t)
;;  '(compilation-scroll-output t)
;;  '(compilation-window-height 15 t)
;;  '(compile-auto-highlight t)
;;  '(compile-command "g++ -lm -lstdc++")
;;  '(desktop-enable nil nil (desktop))
;;  '(ecb-fix-window-size (quote width))
;;  '(ecb-layout-name "left15")
;;  '(ecb-options-version "2.21")
;;  '(ecb-show-only-positioned-tags nil)
;;  '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15")))
;;  '(ecb-show-tags (quote ((include collapsed nil) (parent collapsed nil) (type flattened nil) (variable expanded access) (function flattened access) (rule flattened name) (section flattened nil) (def collapsed name) (t collapsed name))))
;;  '(ecb-source-path (quote (("/" "/") ("/home/amthrax/doc/6.170/psets" "6.170/"))))
;;  '(ecb-tip-of-the-day nil)
;;  '(ecb-toggle-layout-sequence (quote ("left9" "left15")))
;;  '(ecb-truncate-long-names nil)
;;  '(frame-background-mode (quote dark))
;;  '(glasses-face (quote bold))
;;  '(glasses-separate-parentheses-p nil)
;;  '(glasses-separator "")
;;  '(gnus-select-method (quote (nntp "localhost")))
;;  '(highlight-completion-mode nil nil (highlight-completion))
;;  '(ibuffer-shrink-to-minimum-size t)
;;  '(ibuffer-use-other-window t)
;;  '(icomplete-mode t nil (icomplete))
;;  '(icomplete-prospects-length 40)
;;  '(icomplete-show-key-bindings nil)
;;  '(ispell-program-name "aspell")
;;  '(jde-ant-args "-emacs")
;;  '(jde-ant-enable-find t)
;;  '(jde-ant-read-target t)
;;  '(jde-build-function (quote (jde-ant-build)))
;;  '(jde-compiler (quote ("jikes" "")))
;;  '(jde-complete-function (quote jde-complete-minibuf))
;;  '(jde-debugger (quote ("JDEbug")))
;;  '(jde-enable-abbrev-mode nil)
;;  '(jde-gen-comments nil)
;;  '(jde-global-classpath (quote ("/usr/share/java/jde.jar")))
;;  '(jde-help-docsets (quote (("JDK API" "/usr/share/doc/j2sdk1.4-doc/1.4.1/api" nil))))
;;  '(jde-import-auto-collapse-imports t)
;;  '(jde-import-auto-sort t)
;;  '(jde-import-auto-sort-function (quote jde-import-organize))
;;  '(jde-import-collapse-imports-threshold 5)
;;  '(jde-import-group-of-rules (quote (("^javax?\\.") ("^junit\\."))))
;;  '(jde-import-sorted-groups (quote asc))
;;  '(jde-jdk-doc-url "file:///usr/share/doc/j2sdk1.4-doc/1.4.1/api/index.html")
;;  '(jde-jdk-registry (quote (("1.4.x" . "/usr/lib/j2se/1.4/"))))
;;  '(jde-launch-beanshell-on-demand-p t)
;;  '(jde-run-classic-mode-vm t)
;;  '(jde-sourcepath nil)
;;  '(mmm-submode-decoration-level 2)
;;  '(mouse-avoidance-mode (quote animate) nil (avoid))
;;  '(mouse-wheel-mode t nil (mwheel))
;;  '(show-trailing-whitespace nil)
;;  '(speedbar-tag-hierarchy-method (quote (speedbar-trim-words-tag-hierarchy)))
;;  '(speedbar-use-images nil)
;;  '(tempo-interactive t)
;;  '(truncate-partial-width-windows nil)
;;  '(user-full-name "Amthrax Arlan")
;;  '(user-mail-address "amdragon@mit.edu")
;;  '(view-calendar-holidays-initially t t)
;;  '(which-func-mode-global t nil (which-func))
;;  '(which-function-mode t nil (which-func))
;;  '(whitespace-auto-cleanup nil)
;;  '(whitespace-check-indent-whitespace nil)
;;  '(whitespace-global-mode t nil (whitespace))
;;  '(whitespace-rescan-timer-time 15)
;;  '(whitespace-silent t))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(font-latex-title-1-face ((t (:inherit font-latex-title-2-face))))
;;  '(font-latex-title-2-face ((t (:inherit font-latex-title-3-face))))
;;  '(font-latex-title-3-face ((t (:inherit font-latex-title-4-face))))
;;  '(highlight-beyond-fill-column-face ((t (:strike-through "red"))))
;;  '(mmm-cleanup-submode-face ((t (:background "red4"))))
;;  '(mmm-code-submode-face ((t (:background "blue4"))))
;;  '(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
;;  '(mmm-declaration-submode-face ((t (:background "aquamarine4"))))
;;  '(mmm-default-submode-face ((t (:inherit mmm-code-submode-face))))
;;  '(mmm-init-submode-face ((t (:background "green4"))))
;;  '(mmm-output-submode-face ((t (:background "LightGoldenrod4"))))
;;  '(mmm-special-submode-face ((t (:background "pink4"))))
;;  '(trailing-whitespace ((((class color) (background dark)) (:strike-through "red")))))

;; (put 'narrow-to-region 'disabled nil)
