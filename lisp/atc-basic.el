;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic configuration
;;;
;;; Configures basic global variables, look 'n feel, and global Emacs
;;; services

(defun atc:setup-user (name address)
  "Set up user information"
  (setq user-full-name    name
        user-mail-address address))

;;; Frame look (in case X resources aren't present)

(defun atc:defluff ()
  "Disabled frame fluff.  This includes the tool bar, menu bar, and
scroll bar."
  (when tool-bar-mode (tool-bar-mode -1))
  (when menu-bar-mode (menu-bar-mode -1))
  (set-scroll-bar-mode nil))

(defun atc:set-faces ()
  ;; Setting this through customize for the default face causes it to
  ;; be lost when the display changes DPI because customize doesn't
  ;; set the :user-spec of the font correctly. DPI changes fire a
  ;; GConf config-changed-event special event, which triggers
  ;; dynamic-setting-handle-config-changed-event with (I think) a
  ;; font-render event, which tries to reset the same font using its
  ;; :user-spec.
  (when (equal (system-name) "austin2")
    ;; 4K work laptop. Need a scalable font.
    (set-frame-font "Source Code Pro 9" nil t)))

(defun atc:set-frame-colors (frame)
  "Set the frame color scheme for `frame'."
  (modify-frame-parameters frame '((background-color . "Black")
				   (foreground-color . "White")
				   (cursor-color . "Red"))))
(defun atc:setup-colors ()
  "Set frame colors right now and whenever a new frame is made.  This
should be done from the Xresources, but this will work if that isn't
present, and this doesn't hurt."
  (if window-system
      (progn
        (add-hook 'after-make-frame-functions (function atc:set-frame-colors))
        (atc:set-frame-colors nil))
    ;; Set the foreground color, but don't set the background color.
    ;; This allows it to be transparent
    (set-foreground-color "White")
    ;; Emacs can't guess the background color of the terminal, so tell
    ;; it
    (setq frame-background-mode 'dark)))

;;; Buffer look

(defun atc:setup-buffer-look ()
  "Sets up miscellaneous aspects of buffer look and feel."
  (setq truncate-partial-width-windows nil
        column-number-mode             t
        frame-title-format             '("%b" " - " invocation-name)))

(defun atc:setup-mode-line ()
  "Move the useful, short, non-static information to earlier in the
mode line to make it more visible.  This also removes some of the
unnecessary whitespace."
  ;; Different Emacs installations apparently have wildly different
  ;; mode-line defaults, even if they look basically the same.  In
  ;; particular, there are many ways of getting the mode list.  This
  ;; does its best to guess the right thing.
  (if (not (boundp 'mode-line-modes))
      (if (fboundp 'mode-line-mode-name)
          (setq mode-line-modes
                '(" %[("
                  (:eval (mode-line-mode-name))
                  mode-line-process
                  minor-mode-alist
                  "%n"
                  ")%]"))
        (setq mode-line-modes
              "Help! atc:setup-mode-line is confused!")))
  (setq-default mode-line-format
                '("-" mode-line-mule-info mode-line-modified
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "    "
                  (line-number-mode
                   (column-number-mode
                    "(%2l,%2c) "
                    "L%2l ")
                   (column-number-mode
                    "C%2l "))
                  (-3 "%p")
                  " "
                  global-mode-string
                  mode-line-modes
                  "-%-")))

(defun atc:setup-mmm ()
  (custom-set-faces
   '(mmm-default-submode-face
     ((((background dark))
       (:background "DimGray"))
      (t (:background "LightGray")))))
  (setq mmm-global-mode 'maybe))

;;; Usability

(defun atc:setup-misc-usability ()
  "Sets up lots of miscellaneous usability features"
  ;; Save buffer positions between sessions
  (when (require 'saveplace nil t)
    (setq-default save-place t)
    (setq save-place-limit 25)
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
    (setq ebackup-max-copies 5)
    ;; XXX Only disable this on sshfs mounts
    (setq file-precious-flag nil))

  ;; Interpret ANSI color codes in grep output
  (require 'compilation-colorization nil t)

  ;; Setup longlines mode for autoload
  (when (locate-library "longlines")
    (autoload 'longlines-mode
      "longlines"
      "Minor mode for automatically wrapping long lines." t)
    (setq longlines-show-hard-newlines t))

  ;; Outed mode
  (when (locate-library "outed-mode")
    (autoload 'outed-mode "outed-mode"
      "Outed major mode for editing outlines." t))

  ;; Misc usability variables
  (setq inhibit-startup-message     t
        x-stretch-cursor            t
        inhibit-startup-buffer-menu  t
        query-replace-lazy-highlight t
        default-indicate-buffer-boundaries t
        mouse-autoselect-window      t
        initial-scratch-message      nil
        sentence-end-double-space    nil)
  ;; `set-fringe-mode' doesn't exist if Emacs is compiled without
  ;; windowing support
  (when (fboundp 'set-fringe-mode)
    (set-fringe-mode 5))

  ;; Improved ps-print defaults
  (setq ps-landscape-mode t
        ps-number-of-columns 2)

  ;; Enable dangerous functions
  (put 'narrow-to-region 'disabled nil)
  
  ;; The Emacs terminal emulator tends to confuse programs by setting
  ;; TERM to "Emacs".  It's basically a vt100 emulator (just like every
  ;; other terminal emulator out there)
  (setenv "TERM" "vt100")

  ;; Don't blink the cursor
  (blink-cursor-mode -1)

  ;; Don't update the cscope database on every query
  (setq cscope-do-not-update-database t)

  ;; Use one frame for ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Disable VC.  I never use it an it can seriously slow down Emacs
  ;; in some situations.
  (setq vc-handled-backends ())

  ;; Color completion
  (when (load "color-completion/color-completion" t)
    (global-color-completion t))

  ;; Literal-mode
  (require 'literal-mode)

  ;; Enable line/column numbers in find-file.
  (require 'find-file-line nil t)

  ;; The git-rebase command editor is simple and elegant and builds on
  ;; everything you already know about your editor. git-rebase-mode
  ;; turns it in to something complicated with its own rules and
  ;; learning curve. Clearly this is progress.
  (setq auto-mode-alist (delete '("/git-rebase-todo\\'" . git-rebase-mode)
                                auto-mode-alist))
  )

;;; General settings

(defun atc:setup-ispell ()
  "Configures the ispell package to use aspell if it's available."
  (if (executable-find "aspell")
      (setq ispell-program-name "aspell")
    (setq ispell-program-name "ispell")))

(defun atc:disable-ange-ftp ()
  "Disables Ange FTP filename completion.  Having this enabled can
really slow things down."
  (let ((fnha file-name-handler-alist))
    (while fnha
      (if (string-match "^ange-.*" (symbol-name (cdar fnha)))
          (setq file-name-handler-alist
                (delq (car fnha) file-name-handler-alist)))
      (setq fnha (cdr fnha)))))

(defun atc:setup-global-bindings ()
  "Sets useful global bindings."
  (if (require 'magic-buffer-list nil t)
      (progn
        (global-set-key "\C-x\C-b" (function magic-buffer-list))
        (global-set-key "\C-xB" (function magic-buffer-list-other-window))
        (global-set-key "\M-r"
                        (function magic-buffer-list-and-select-next))
        (global-set-key "\M-R"
                        (function magic-buffer-list-and-select-prev)))
    (global-set-key "\C-x\C-b" (function electric-buffer-list)))
  ;; The following is because some terminals use C-h and some use C-?
  ;; for backspace.  Emacs by default only understands C-?.  I'm sure
  ;; one of these is the Right Way and I shouldn't even be using Emacs
  ;; on terminals that do it the other way.
  (global-set-key "\C-h"     (function delete-backward-char)))

(defun atc:setup-server ()
  "Sets up the Emacs server.  This doesn't actually start it, relying
on ~/bin/ec to do that when appropriate, but it configures it to open
a new frame on each connection and to not complain about closing
server buffers."
  (when (require 'server nil t)
    ;; Rely on the ec script or the user to start the server in the
    ;; right instance.  This way stray emacs instances don't fight
    ;; over who gets the server.
    ;;(server-start)

    ;; Always open a new frame.  XXX I'm not sure what the right
    ;; behavior is if the buffer is already open, but I know it's not
    ;; what server does.  Either focus the existing buffer, or create
    ;; a new frame containing it without messing with any existing
    ;; frames.
;;     (add-hook 'server-switch-hook
;;               (lambda ()
;;                 (let ((buffer (current-buffer)))
;;                   (bury-buffer)
;;                   (let ((pop-up-windows nil)
;;                         (pop-up-frames t))
;;                     (pop-to-buffer buffer)))))

    ;; Make kill-buffer just release the client without complaining
    ;; about it
    (remove-hook 'kill-buffer-query-functions
                 'server-kill-buffer-query-function)))

(defun kill-server-or-frame-or-emacs (&optional arg)
  "If there are multiple frames open, close only the current frame.
If this is the last frame, just do a `save-buffers-kill-emacs'.  This
has special knowledge of the Emacs server and will attempt to kill
server-controlled buffers in a way that makes it feel like the frames
popped up by new connections are independent Emacsen.

This differs somewhat from `save-buffers-kill-terminal' in that
it will always kill only the current frame, even if it was not
started by emacsclient."
  (interactive "P")
  (if (and (fboundp 'server-save-buffers-kill-terminal)
           (frame-parameter (selected-frame) 'client))
      ;; Frame came from emacsclient
      (server-save-buffers-kill-terminal arg)
    ;; If there are multiple frames, kill just this frame
    (let ((frames (frame-list)))
      (if (and frames (cdr frames))
          (delete-frame)
        ;; Last frame, so kill emacs
        (save-buffers-kill-emacs arg)))))
(defun atc:setup-kill-dwim ()
  "Changes \C-x\C-c to use `kill-server-or-frame-or-emacs'.  This goes
well with `atc:setup-server'."
  (global-set-key "\C-x\C-c" (function kill-server-or-frame-or-emacs)))

;;; Post mode

(defun atc:post-mode-jump-cursor ()
  "Automatically jump the cursor to the Right Place in post-mode"
  (save-match-data
    (when (re-search-backward "^To:\\(\\s *\\)$" nil t)
      (goto-char (match-end 1)))))
(defun atc:post-mode-disable-saveplace ()
  "Disable the saveplace package for post-mode buffers"
  (when (featurep 'saveplace)
    (setq save-place nil)
    ;; If saveplace recognizes this file, it will ignore being
    ;; disabled
    (or save-place-loaded (load-save-place-alist-from-file))
    (let ((cell (assoc buffer-file-name save-place-alist)))
      (if cell
          (setq save-place-alist (remq cell save-place-alist))))))
(defun atc:setup-post-mode ()
  (autoload 'post-mode "post"
    "Major mode for composing email or news with an external agent." t)
  (add-to-list 'auto-mode-alist
               (cons "\\(mutt\\(ng\\)?-[a-zA-Z0-9-.]+-[0-9]+-[0-9]+\\|mutt\\(ng\\)?[a-zA-Z0-9._-]\\{6\\}\\)\\'"
                     #'post-mode)
               t)
  (add-hook 'post-mode-hook (function atc:post-mode-jump-cursor))
  (add-hook 'post-mode-hook (function atc:post-mode-disable-saveplace)))

;;; Org mode

(defun atc:load-org-mode ()
  "Load a recent version of org-mode and set it up."
  (let ((path "~/sys/elisp/extra-pre/org-5.08"))
    (if (not (file-accessible-directory-p path))
        (message "Failed to find recent org-mode")
      (add-to-list 'load-path path)
      (require 'org-install)
      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)))
  (setq org-hide-leading-stars t))

;;; icomplete bug fix

(eval-after-load "icomplete"
  '(defun icomplete-get-keys (func-name)
     "Return strings naming keys bound to `func-name', or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer."
     (if (commandp func-name)
         (save-excursion
           (let* ((sym (intern func-name))
                  (buf (other-buffer nil t))
                  (keys (save-excursion (set-buffer buf) (where-is-internal sym))))
             (if keys
                 (concat "<"
                         (mapconcat 'key-description
                                    (sort keys
                                          #'(lambda (x y)
                                              (< (length x) (length y))))
                                    ", ")
                         ">")))))))

;;; Everything

(defun atc:basic-setup-all ()
  "Sets up everything in atc-basic, except `atc:setup-user'."
  (atc:defluff)
  (atc:set-faces)
  (atc:setup-colors)
  (atc:setup-buffer-look)
  (atc:setup-mode-line)
  (atc:setup-mmm)
  (atc:setup-misc-usability)
  (atc:setup-ispell)
  (atc:disable-ange-ftp)
  (atc:setup-global-bindings)
  (atc:setup-server)
  (atc:setup-kill-dwim)
  (atc:setup-post-mode))
