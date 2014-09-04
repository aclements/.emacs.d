(when (string-match "^/\\(mts.*\\|exit[0-9]*\\)/home[0-9]?/" (getenv "HOME"))
  ;; SCons scripts
  (add-to-list 'auto-mode-alist (cons "\\.sc\\'" (function python-mode)))

  ;; Save-place is stupidly slow on VMware NFS, mostly from checking
  ;; for unreadable files
  (setq save-place-file (concat "/tmp/.emacs-places-" (user-login-name))
        save-place-forget-unreadable-files nil)

  ;; Simple p5 integration
  (defun p5-edit ()
    (interactive)

    (message "Opening buffer for editing in p5...")
    (let ((buffer (current-buffer))
          (out-buf (generate-new-buffer " *p5 edit output*"))
          (config (current-window-configuration)))
      (let ((split-window-keep-point t))
        (with-selected-window (split-window-vertically -10)
          (switch-to-buffer out-buf t)))
      (let ((res (call-process "p5" nil out-buf t "edit"
                               (file-name-nondirectory
                                (buffer-file-name buffer)))))
        (if (/= res 0)
            (with-current-buffer out-buf
              (insert (format "Process failed with code %s" res)))
          (message "Opening buffer in p5...  Done")
          (with-current-buffer buffer
            (setq buffer-read-only nil)
            (force-mode-line-update))
          (sit-for 2)
          (set-window-configuration config)))))

  (setq vmstyle-enable-c-style nil)     ; Handled in atc-cc.el
  (require 'vmstyle-load)
  (eval-after-load 'cc-mode
    '(define-key c-mode-map "\C-ci" #'vmstyle-prologue-dwim))
  )
