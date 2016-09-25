;; Programming support for the Go language

(defun atc:go-mode-setup ()
  (setq sentence-end-double-space nil)

  ;; go-mode integrates godoc and godef support if they're installed
  (atc:want-executable "godoc" "Run go get golang.org/x/tools/cmd/godoc")
  (atc:want-executable "godef" "Run go get github.com/rogpeppe/godef")
  (when (atc:want-fbound 'go-eldoc-setup 'go-eldoc)
    (go-eldoc-setup))
  (when (and (atc:want-fbound 'company-go 'company-go)
             (atc:want-executable "gocode" "Run go get github.com/nsf/gocode"))
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))

  ;; Replace find-tag with go-identify.
  (local-set-key (kbd "M-.") #'go-identify)

  (flyspell-prog-mode))

(eval-after-load 'go-mode
  '(progn
     (add-hook 'go-mode-hook #'atc:go-mode-setup)
     (let* ((have-goimports
             (atc:want-executable "goimports" "Run go get golang.org/x/tools/cmd/goimports"))
            (have-gofmt
             (or have-goimports
                 (atc:want-executable "gofmt" "Add Go to your PATH"))))
       (when have-gofmt
         (add-hook 'before-save-hook #'gofmt-before-save))
       (when have-goimports
         (setq gofmt-command "goimports")))
     (font-lock-add-keywords 'go-mode '(("\\.  " 0 'trailing-whitespace t)))
     (load "~/r/go/src/golang.org/x/tools/cmd/oracle/oracle.el" t)
     (require 'go-identify)
     ))

;; Style for C/assembly code in Go trees
;; XXX Extend c-choose-style?  Still need this for assembly
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match
                   (concat "^" (regexp-quote (expand-file-name "~/go")))
                   (buffer-file-name))
              (setq indent-tabs-mode t
                    c-basic-offset 8))))

;; Company mode (for Go completion)
;; XXX Requires company, company-go
(setq company-tooltip-limit 20)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-go-insert-arguments nil)
(setq company-go-show-annotation t)

(defun go-play ()
  "Create a new Go buffer for playing in."
  (interactive)
  (let ((buf (generate-new-buffer "*go-play*")))
    (switch-to-buffer buf)
    (insert "package main

import \"fmt\"

func main() {
	")
    (let ((pt (point)))
      (insert "fmt.Println(\"Hello world!\")
}
")
      (goto-char pt))
    (go-mode)))

(defun go-run ()
  "Run this buffer."
  (interactive)
  (gofmt)
  (let ((path (make-temp-file "go-play" nil ".go")))
    (unwind-protect
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) path)
          (shell-command (format "go run %s" path) "*go-run*"))
      (delete-file path))))
