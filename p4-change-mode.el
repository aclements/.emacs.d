;;; p4-change-mode.el --- major mode for Perforce change specifications

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    29-Aug-2009
;; Version:    0.1

;;; Commentary:

;; For installation instructions, see p4-change-mode-load.el.

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;

(defgroup p4-change nil
  "Major mode for editing Perforce change specifications.")

(defcustom p4-change-diff-command '("p4" "diff")
  "Command and arguments to use for p4 diffs.

The file specification will be given as the last argument."
  :group 'p4-change
  :type '(repeat string))

(defcustom p4-change-move-point t
  "Whether to move point to the Description or View section.

If non-nil, entering p4-change-mode with point at the beginning
of the buffer will move point to the beginning of the Description
or View section."
  :group 'p4-change
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode maps and font lock
;;

(defvar p4-change-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'p4-change-mouse-view-diff)
    (define-key map "\r" #'p4-change-view-diff)
    map))

(defvar p4-change-file-props
  `(face link
    mouse-face highlight
    help-echo "mouse-2: run p4 diff"
    keymap ,p4-change-file-map
    follow-link t
    pointer x-sensitive-text-pointer-shape))

(defvar p4-change-font-lock-keywords
  '(("^[^ \t:]*:" . font-lock-keyword-face)
    ("^#.*" . font-lock-comment-face)
    ("/\\(/[^/ \t\n]*\\)+\\(#[0-9]+\\)?" . p4-change-file-props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; p4 diff
;;

(defun p4-change-mouse-view-diff (event)
  "Run p4 diff on the file specifications under the mouse."

  (interactive "e")
  (let ((posn (event-start event)))
    (select-window (posn-window posn) t)
    (p4-change-view-diff (posn-point posn))))

(defun p4-change-view-diff (point)
  "Run p4 diff on the file specification at point."

  (interactive "d")
  (let* ((begin (previous-single-char-property-change point 'keymap))
         (end (next-single-char-property-change point 'keymap))
         (path (buffer-substring-no-properties begin end))
         (cmd (append p4-change-diff-command (list path)))
         (cmd-string (mapconcat (lambda (x) x) cmd " ")))
    (message "%s..." cmd-string)
    (let ((proc (apply #'start-process "p4diff" (concat "*diff " path "*") cmd)))
      (process-put proc 'cmd-string cmd-string)
      (set-process-sentinel proc #'p4-change-diff-sentinel))))

(defun p4-change-diff-sentinel (p e)
  "A process sentinel for p4 diff commands.

If the process buffer contains any lines other than file
headings, this splits the frame and shows the p4 diff output."

  (when (with-current-buffer (process-buffer p)
          (goto-char (point-min))
          (save-excursion (re-search-forward "^[^=]" nil t)))
    (delete-other-windows)
    (select-window (split-window))
    (switch-to-buffer (process-buffer p)))
  (let ((cmd-string (process-get p 'cmd-string)))
    (message "%s... %s" cmd-string (substring e 0 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode
;;

;;;###autoload
(define-derived-mode p4-change-mode text-mode "P4 change"
  "Major mode for editing Perforce change specifications.

This mode indents all text by one tab, sets up paragraph filling
to account for section headings, and hyperlinks file
specifications to run p4 diff.  If `p4-change-move-point' is
non-nil and point is at the beginning of the buffer, entering
this mode moves point to the View or Description section."

  (set (make-local-variable 'font-lock-defaults)
       '(p4-change-font-lock-keywords nil nil nil nil
         (font-lock-extra-managed-props
          mouse-face help-echo keymap follow-link pointer)))

  ;; All text is indented by a tab
  (setq indent-tabs-mode t
        fill-prefix "\t")

  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-separate "\\|^\\<.*:$"))
  (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)

  (when (and p4-change-move-point
             (bobp)
             (or (re-search-forward "^View:$" nil t)
                 (re-search-forward "^Description:$" nil t)))
    (skip-chars-forward "\n\t")))

;;;###autoload
(setq magic-mode-alist
      (nconc magic-mode-alist
             '(("^# A Perforce \\(Change\\|Client\\) Specification" . p4-change-mode))))

(provide 'p4-change-mode)
