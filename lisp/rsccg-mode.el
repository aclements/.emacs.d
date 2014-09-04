(defconst rsccg-font-lock-keywords
  (let ((rule-name "[-a-zA-Z0-9_+*?,]+"))
    `((,(concat "^\\(" rule-name "\\)[ \t]*:")
       (1 font-lock-variable-name-face))
      (;; An incomplete regex
       "\\(/\\([^/\n]\\|\\\\/\\)*/?\\|\\$\\([^$\n]\\|\\\\\\$\\)*\\$/\\)"
       (0 font-lock-warning-face prepend))
      (;; A complete regex
       "\\(/\\([^/]\\|\\\\/\\)+/\\|\\$\\([^$]\\|\\\\\\$\\)+\\$\\)s?"
       (0 font-lock-constant-face prepend))
      )))

(defvar rsccg-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?/ "\"" st)
    st))

(defvar rsccg-mode nil)
(make-variable-buffer-local 'rsccg-mode)

(define-derived-mode rsccg-mode fundamental-mode "rsccg"
  "Major mode for editing RSCC grammar files."

  ;; Set up font lock
  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (setq font-lock-defaults
        '(rsccg-font-lock-keywords nil t nil nil))

  ;; Commenting
  (mapcar (function make-local-variable)
          '(comment-start comment-end comment-multi-line
                          comment-start-skip))
  (setq comment-start "# "
        comment-end ""
        comment-multi-line nil
        comment-start-skip ";[ \t;]*")

  (setq rsccg-mode t)
  )
