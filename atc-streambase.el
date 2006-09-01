;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Streambase-specific Emacs customizations
;;;

(when (string-match ".streambase.com$" (system-name))
  (unless atc-load-fast
    ;; CEDET
    (load-file "~/sys/elisp/extra/jdee/cedet-1.0pre3/common/cedet.el")

    ;; Semantic
    ;;  (semantic-load-enable-code-helpers)
    ;; (semantic-load-enable-guady-code-helpers)
    ;; (semantic-load-enable-excessive-code-helpers)

    ;; Elib
    (add-to-list 'load-path "~/sys/elisp/extra/jdee/elib")

    ;; JDEE
    (add-to-list 'load-path "~/sys/elisp/extra/jdee/jde-2.3.5.1/lisp")

    (require 'jde)
    (require 'jde-xref)

    ;; XXX Only for Java
    ;;   (global-semantic-stickyfunc-mode 1)
    )

  (setq-default
   jde-jdk-registry '(("1.4.2" . "/usr/java/j2sdk1.4.2_07"))
   jde-complete-function 'jde-complete-minibuf
   jde-complete-insert-method-signature nil)

  ;; Use template for svn commit
  (add-hook 'svn-commit-mode-hook
            (lambda ()
              (unless svn-commit-restored-filename
                (save-excursion
                  (skip-chars-forward "\n")
                  (when (looking-at svn-commit-ignore-regexp)
                    (let ((modified (buffer-modified-p)))
                      (insert "\nRT: \nBB: \n")
                      (set-buffer-modified-p modified))))))))
