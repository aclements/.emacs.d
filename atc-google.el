;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Google-specific Emacs customizations
;;;

(when (file-accessible-directory-p "~build/public/google/")
  ;; XXX Google has it's own outdated copy of python-mode, so grab
  ;; mine before loading Google
  (require 'python-mode)
  (when (load "~build/public/google/util/google.el" t)
    ;; Load gtags
    (when (load "~build/nonconf/google3/tools/tags/gtags.el" t)
      (setq google-decipher-proto-files t)
      (global-set-key "\M-." 'google-find-tag))
    ;; Set compile command
    (setq compile-command "make-dbg -r")
    ;; Load unit testing
    (when (load (concat "~build/nonconf/google3/experimental"
                        "/users/damonkohler/run_unit_test.el") t)
      (global-set-key "\C-x\C-u" 'run-unit-test-for-current-buffer))
    ;; Add perforce to magic-buffer-list
    (setq magic-buffer-list-view-basic-info
          (append magic-buffer-list-view-basic-info
                  '(" "
                    (10 p4-client
                        :align left
                        :trim-align right
                        :trim ".."))))
    ))

(defun magic-buffer-list-getter-p4-client ()
  "Get the name of the buffer's Perforce client, or nil if there is no
client or no Perforce"
  (if (fboundp 'p4-get-client-name)
      (p4-get-client-name)))
