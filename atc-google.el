;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Google-specific Emacs customizations
;;;

(when (load "~build/public/google/util/google.el" t)
  ;; Load gtags
  (when (load "~build/nonconf/google3/tools/tags/gtags.el" t)
    (setq google-decipher-proto-files t)
    (global-set-key "\M-." 'google-find-tag))
  )
