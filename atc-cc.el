;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C support
;;;
;;; This supports modes derived from cc-mode

(defun c-backward-defun (&optional arg)
  "Move backwards to the beginning of this function declaration.  This
differs from `c-beginning-of-defun' in that it actually takes the
point to the beginning of the function definition instead of just its
block."
  (interactive "p")
  (unless arg (setq arg 1))
  (while (> arg 0)
    (c-beginning-of-defun)
    (c-beginning-of-statement)
    (setq arg (1- arg))))

;;; Define C-specific features

(require 'atc-features)

(defmodefeature c-defun-jump
  (local-set-key "\M-p" (function c-backward-defun))
  (local-set-key "\M-n" (function c-end-of-defun)))

(defmodefeature c-auto-hungry
  (c-toggle-auto-hungry-state 1))

(defmodefeature c-filladapt
  (function c-setup-filladapt))

(defmodefeature c-auto-brace-space
  (defadvice c-electric-brace (around c-auto-brace-space activate)
    (if (or (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if I'm in a literal or if the
        ;; user typed anything other than an open brace
        ad-do-it
      ;; Go ahead and put a space here
      (just-one-space)
      (let ((here (point)))
        ad-do-it
        ;; Delete any extra space this may have inserted
        (save-excursion
          (goto-char here)
          (if (looking-at "[ \t]*\n")
              (delete-horizontal-space)))))))

(defmodefeature c-auto-close-brace
  (defadvice c-electric-brace (around c-auto-close-brace activate)
    (if (or (not c-auto-newline)
            (not (eq last-command-char ?{))
            (c-in-literal))
        ;; Don't do anything special if not in auto mode, if the user
        ;; typed anything other than an open brace, or if the point is
        ;; in a literal
        ad-do-it
      (let ((syntax-type
             (or (save-excursion
                   (c-backward-syntactic-ws)
                   (let ((cb (char-before)))
                     (cond ((= cb ?=) 'assignment)
                           ((= cb ?,) 'list)
                           ((or (= cb ?\)) (= cb ?\;)) 'block)
                           (t nil))))
                 (save-excursion
                   ;; Class, struct, or enum?
                   (c-beginning-of-statement-1)
                   (if (looking-at "typedef\\>")
                       (c-forward-token-1))
                   (if (looking-at "\\(class\\|struct\\|enum\\)\\>")
                       'definition
                     nil))
                 (save-excursion
                   ;; Do-while?
                   (c-beginning-of-statement-1)
                   (if (looking-at "do\\>")
                       'do-while))
                 (progn
                   (message "c-auto-close-brace is confused")
                   'block))))
        ad-do-it
        (save-excursion
          (newline)
          ;; Type the close brace
          (let ((last-command-char ?})
                ;; Inhibit cleanup of empty defuns
                (c-cleanup-list
                 (remq 'empty-defun-braces c-cleanup-list)))
            (c-electric-brace arg)
            ;; Insert any additional characters dictated by the
            ;; syntactic context
            (if (eq syntax-type 'do-while)
                (insert " while ()"))
            (cond ((or (eq syntax-type 'assignment)
                       (eq syntax-type 'definition)
                       (eq syntax-type 'do-while))
                   ;; Insert semicolon
                   (let ((last-command-char ?\;)
                         (c-cleanup-list (cons 'defun-close-semi
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))
                  ((eq syntax-type 'list)
                   ;; Insert comma
                   (let ((last-command-char ?,)
                         (c-cleanup-list (cons 'list-close-comma
                                               c-cleanup-list)))
                     (c-electric-semi&comma arg)))))
          ;; Clean up extra whitespace that may have been inserted
          ;; after the close characters
          (let ((end (point))
                (begin (save-excursion
                         (skip-chars-backward " \t\n")
                         (point))))
            (delete-region begin end)))))))

(defmodefeature c-show-func
  (when (require 'show-context-mode nil t)
    (show-context-mode 1)))

(defmodefeature c-bindings
  (local-set-key "\C-c\C-c" (function compile)))

;;; Set up the mode itself

;; A lot of .h files are actually C++
(setcdr (assoc "\\.h\\'" auto-mode-alist) (function c++-mode))

;; Set C's features
(atc:put-mode-features 'c-mode-common-hook
                       '(autofill filladapt flyspell-prog
                                  highlight-unhappy
                                  final-newline-always
                                  c-defun-jump c-auto-hungry
                                  c-filladapt c-auto-brace-space
                                  c-auto-close-brace c-show-func
                                  c-bindings))
