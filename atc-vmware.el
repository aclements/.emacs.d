(when (string-match "^/\\(mts.*\\|exit[0-9]*\\)/home[0-9]?/" (getenv "HOME"))
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

  ;; Generate function headers
  (defun vmware-function-header ()
    (interactive)

    (push-mark)
    (c-beginning-of-defun)
    (unless (bolp)
      (error "Declaration does not start at the beginning of a line"))
    (let ((beginning (point)))
      (forward-line)
      ;; This way oversimplifies things, but should work 99% of the
      ;; time and should generally bail when it would be wrong.
      (unless (looking-at "\\([a-zA-Z_][a-zA-Z0-9_$]*\\)(")
        (error "This doesn't look like a function name"))
      (let ((name (match-string 1)))
        (goto-char beginning)
        (insert "/*
 *-----------------------------------------------------------------------------
 *
 * " name " --
 *
 *      ")
        (let ((middle (point)))
          (insert "XXX
 *
 * Results:
 *      XXX
 *
 * Side effects:
 *      XXX
 *
 *-----------------------------------------------------------------------------
 */

")
          (goto-char middle)))))

  (defvar vmware-function-header-begin-re "/\\*\n \\*--")
  (defvar vmware-function-header-continuation-re "^[ \t]*\\*?[ \t]*$")
  (defvar vmware-function-header-section-re
    "^[ \t]*\\*\\(-\\| [^ ].*\\(--\\|:\\)[ \t]*$\\)")

  (defvar vmware-function-header-default-indent " *      ")

  (defun vmware-compute-comment-indent ()
    (let (range)
      (when (c-save-buffer-state
                ()
              (setq range (c-literal-limits))
              (when range
                (save-excursion
                  (goto-char (car range))
                  (looking-at vmware-function-header-begin-re))))
        ;; We are.  Go backwards until we find another line of text
        (save-excursion
          (beginning-of-line)
          (forward-line -1)
          (while (looking-at vmware-function-header-continuation-re)
            (forward-line -1))
          ;; What did we hit?
          (when (or (looking-at "^/\\*")
                    (looking-at vmware-function-header-section-re))
            ;; We hit the beginning of the comment or a section
            ;; within the comment.  Find the first real line of text.
            (goto-char (car range))
            (forward-line)
            (beginning-of-line)
            (while (or (looking-at vmware-function-header-continuation-re)
                       (looking-at vmware-function-header-section-re))
              (forward-line)))
          ;; Take our indentation from this line
          (if (looking-at "^[ \t]*\\*?[ \t]*")
              (match-string 0)
            vmware-function-header-default-indent)))))

  (defadvice c-indent-line (around indent-in-function-header activate)
    (or (let ((indent (vmware-compute-comment-indent)))
          (when indent
            (insert indent)
            (length indent)))
        ad-do-it))

  ;; Generate header file headers
  (defun vmware-header-header ()
    (interactive)

    (push-mark)

    (goto-char
     (save-excursion
       (let ((middle (vmware-file-header))
             (restriction-sym (concat
                               "_"
                               (upcase
                                (replace-regexp-in-string
                                 "\\." "_"
                                 (file-name-nondirectory (buffer-file-name))))
                               "_")))
         (insert "#ifndef " restriction-sym "\n"
                 "#define " restriction-sym "\n"
                 "\n"
                 "#define INCLUDE_ALLOW_XXX\n"
                 "#include \"includeCheck.h\"\n"
                 "\n")
         (goto-char (point-max))
         (unless (bolp)
           (insert "\n"))
         (insert "\n#endif // ifndef " restriction-sym "\n")

         middle))))

  (defun vmware-source-header ()
    (interactive)

    (push-mark)

    (goto-char (save-excursion (vmware-file-header))))

  (defun vmware-file-header ()
    (goto-char (point-min))
    (let ((comment-end (save-excursion
                         (while (forward-comment 1) t)
                         (point))))
      (when (re-search-forward "Copyright [0-9]+ VMware" comment-end t)
        (error "This file appears to have a header comment already")))

    ;; Copyright
    (let ((year (number-to-string (nth 5 (decode-time)))))
      (insert "/* **********************************************************
 * Copyright " year " VMware, Inc.  All rights reserved.
 * VMware Confidential
 * **********************************************************/

"))

    ;; Abstract
    (insert "/*
 * " (file-name-nondirectory (buffer-file-name)) " --
 *
 *      ")
    (prog1
        (point)
      (insert "XXX
 */

")))

  (defun vmware-header-dwim ()
    (interactive)

    (cond ((save-excursion
             (unless (eobp)
               (forward-char))
             (c-beginning-of-defun)
             (not (bobp)))
           (vmware-function-header))
          ((string-match "\\.c$" (buffer-file-name))
           (vmware-source-header))
          ((string-match "\\.h$" (buffer-file-name))
           (vmware-header-header))
          (t
           (message "Couldn't determine header type.  Insert header?  [fch?]")
           (let (done)
             (while (not done)
               (let ((key (read-event)))
                 (setq done t)
                 (case key
                   ((?f ?F) (vmware-function-header))
                   ((?c ?C) (vmware-source-header))
                   ((?h ?H) (vmware-header-header))
                   ((??)
                    (message "Header type: (f) Function, (c) Source file, (h) Header file")
                    (setq done nil)))))))))

  (eval-after-load 'cc-mode
    '(define-key c-mode-map "\C-ci" #'vmware-header-dwim))
  )
