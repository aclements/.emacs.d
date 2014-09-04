;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Google-specific Emacs customizations
;;;

(when (load "/home/build/public/eng/elisp/google.el" t)
  (setq p4-use-p4config-exclusively t)

  (require 'p4-change-mode-load)
  (setq p4-change-diff-command '("g4" "diff")))

  ;; ;; XXX Google has it's own outdated copy of python-mode, so grab
  ;; ;; mine before loading Google
  ;; (require 'python-mode)
  ;; (when (load "~build/public/google/util/google.el" t)
  ;;   ;; Load gtags
  ;;   (when (load "~build/nonconf/google3/tools/tags/gtags.el" t)
  ;;     (setq google-decipher-proto-files t)
  ;;     (global-set-key "\M-." 'google-find-tag))
  ;;   ;; Set compile command
  ;;   (setq compile-command "make-dbg -r")
  ;;   ;; Load unit testing
  ;;   (when (load (concat "~build/nonconf/google3/experimental"
  ;;                       "/users/damonkohler/run_unit_test.el") t)
  ;;     (global-set-key "\C-x\C-u" 'run-unit-test-for-current-buffer))
  ;;   ;; Add perforce to magic-buffer-list
  ;;   (setq magic-buffer-list-view-basic-info
  ;;         (append magic-buffer-list-view-basic-info
  ;;                 '(" "
  ;;                   (10 p4-client
  ;;                       :align left
  ;;                       :trim-align right
  ;;                       :trim ".."))))
  ;;   ))

(when (load "/home/lnagy/emacs/javascript.el" t)
  (load "/home/lnagy/emacs/go-mode.el" t)
  (setq auto-mode-alist (append (list '("\\.go$" . go-mode))
                                auto-mode-alist))
  (add-hook 'go-mode-hook
            (lambda ()
              (setq javascript-indent-level 8
                    indent-tabs-mode t))))

(defun magic-buffer-list-getter-p4-client ()
  "Get the name of the buffer's Perforce client, or nil if there is no
client or no Perforce"
  (if (fboundp 'p4-get-client-name)
      (p4-get-client-name)))
