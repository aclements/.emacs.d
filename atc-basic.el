;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic configuration
;;;
;;; Configures basic global variables and look 'n feel

;;; Set up user information
(setq user-full-name    "Austin Clements"
      user-mail-address "amdragon@mit.edu")

;;; Disable frame fluff and set colors (in case X resources aren't around)
;; These are predicated to avoid autoloading modules solely for the
;; purpose of disabling them (ie, the tool-bar module)
(if tool-bar-mode (tool-bar-mode -1))
(if menu-bar-mode (menu-bar-mode -1))
(if scroll-bar-mode (scroll-bar-mode -1))

;; Set frame colors right now and when a new frame is made
(defun atc:set-frame-colors (frame)
  (modify-frame-parameters frame '((background-color . "Black")
				   (foreground-color . "White")
				   (cursor-color . "Red"))))
(add-hook 'after-make-frame-functions (function atc:set-frame-colors))
(atc:set-frame-colors nil)

;;; Set up buffer look
(setq truncate-partial-width-windows nil
      column-number-mode             t
      frame-title-format             '("%b" " - " invocation-name))
(global-font-lock-mode t)

;; Move the useful, short, non-static information to earlier in the
;; mode line to make it more visible
(setq-default mode-line-format
      '("-" mode-line-mule-info mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        "    "
        (line-number-mode "L%2l ")
        (column-number-mode "C%2c ")
        (-3 "%p")
        " "
        global-mode-string
        " %[("
        (:eval (mode-line-mode-name))
        mode-line-process
        minor-mode-alist
        "%n"
        ")%]"
        "-%-"))

;;; Enable usability features
(mouse-wheel-mode t)

;; Save buffer positions between sessions
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (if (= (user-uid) 0)
      ;; Being root screws with permissions of saveplace's files, so
      ;; put it somewhere else
      (setq save-place-file (concat save-place-file "-root"))))

;; Uniquify buffer names by prepending path elements
(toggle-uniquify-buffer-names)
(setq uniquify-buffer-name-style 'forward)

;; icomplete improves minibuffer completion usability
(when (require 'icomplete nil t)
  (icomplete-mode 1)
  (setq icomplete-prospects-length 50))

;; Place backup files in ~/.emacs-backup
(when (require 'ebackup nil t)
  ;; Only keep 5 last backup copies around (the default is 10)
  (setq ebackup-max-copies 5))

;; Interpret ANSI color codes in grep output
(require 'compilation-colorization nil t)

;; Misc usability variables
(setq load-warn-when-source-newer t
      inhibit-startup-message     t
      next-line-add-newlines      nil
      x-stretch-cursor            t)

;;; Set general settings
;; aspell rocks.  ispell sucks.  Use aspell if it's available
(if (executable-find "aspell")
    (setq ispell-program-name "aspell")
  (setq ispell-program-name "ispell"))

;; The Emacs terminal emulator tends to confuse programs by setting
;; TERM to "Emacs".  It's basically a vt100 emulator (just like every
;; other terminal emulator out there)
(setenv "TERM" "vt100")

;; Ange FTP really slows down filename completion, so disable it
(let ((fnha file-name-handler-alist))
  (while fnha
    (if (string-match "^ange-.*" (symbol-name (cdar fnha)))
        (setq file-name-handler-alist
              (delq (car fnha) file-name-handler-alist)))
    (setq fnha (cdr fnha))))

;;; Set global bindings
(global-set-key "\C-c\g"   (function goto-line))
(global-set-key "\C-x\C-k" (function kill-buffer))
(global-set-key "\C-x\C-b" (function electric-buffer-list))
;; The following is because some terminals use C-h and some use C-?
;; for backspace.  Emacs by default only understands C-?.  I'm sure
;; one of these is the Right Way and I shouldn't even be using Emacs
;; on terminals that do it the other way.
(global-set-key "\C-h"     (function delete-backward-char))

;;; Enable dangerous functions
(put 'narrow-to-region 'disabled nil)

;;; Set up emacsclient
(when (require 'server nil t)
  ;; Rely on the ec script or the user to start the server in the
  ;; right instance.  This way stray emacs instances don't fight over
  ;; who gets the server.
  ;(server-start)

  ;; Always open a new frame.  XXX I'd rather this not open a new
  ;; frame if the buffer is already open, but this hook doesn't get
  ;; called until after the server has gone and fiddled with the live
  ;; frames
  (add-hook 'server-switch-hook
            (lambda ()
              (let ((buffer (current-buffer)))
                (bury-buffer)
                (let ((pop-up-windows nil)
                      (pop-up-frames t))
                  (pop-to-buffer buffer)))))

  ;; Make kill-buffer just release the client without complaining
  ;; about it
  (remove-hook 'kill-buffer-query-functions
               'server-kill-buffer-query-function))

;;; Improve C-x C-c
(defun kill-server-or-frame-or-emacs (&optional arg)
  "If there are multiple frames open, close only the current frame.
If this is the last frame, just do a `save-buffers-kill-emacs'.  This
has special knowledge of the Emacs server and will attempt to kill
server-controlled buffers in a way that makes it feel like the frames
popped up by new connections are independent Emacsen."
  (interactive "P")
  (when (featurep 'server)
    ;; For each window in this frame, if the buffer in that window
    ;; came from the Emacs server, see if this is the only window
    ;; containing that buffer and kill it if so
    (walk-windows (lambda (wnd)
                    (save-excursion
                      (let ((buffer (window-buffer wnd)))
                        (if (and server-buffer-clients
                                 (null (cdr (get-buffer-window-list buffer))))
                            (kill-buffer buffer)))))
                  nil
                  (selected-frame)))
  ;; If there are multiple frames, kill just this frame
  (let ((frames (frame-list)))
    (if (and frames (cdr frames))
        (delete-frame)
      ;; Last frame, so kill emacs
      (save-buffers-kill-emacs arg))))
(global-set-key "\C-x\C-c" (function kill-server-or-frame-or-emacs))
