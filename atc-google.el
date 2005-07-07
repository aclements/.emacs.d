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
    ))
