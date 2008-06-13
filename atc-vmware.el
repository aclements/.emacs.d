(when (string-match "^/mts/home[0-9]/" (getenv "HOME"))
  ;; SCons scripts
  (add-to-list 'auto-mode-alist (cons "\\.sc\\'" (function python-mode))))
