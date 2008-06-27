(when (string-match "^/mts/home[0-9]/" (getenv "HOME"))
  ;; SCons scripts
  (add-to-list 'auto-mode-alist (cons "\\.sc\\'" (function python-mode)))

  ;; Highlight lines longer than 80 characters
  (defface vmware-long-line-face
    '((((class color)) (:background "red4")))
    "Highlight overly long lines")
  (font-lock-add-keywords 
   'c-mode
   '(("^[^\n]\\{80\\}\\(.*\\)$"
      1 'vmware-long-line-face append)))

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
  )
