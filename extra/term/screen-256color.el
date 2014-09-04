;; Support TERM=screen-256color
;; From http://www.emacswiki.org/emacs/GnuScreen

(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
