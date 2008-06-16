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
      1 'vmware-long-line-face append))))
