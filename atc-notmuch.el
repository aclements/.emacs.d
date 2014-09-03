(when (file-directory-p "~/sandbox/notmuch/emacs")
  (setq load-path (cons "~/sandbox/notmuch/emacs" load-path))
  (require 'notmuch)

  ;; XXX Use bold for low-color displays and maybe only for dark
  ;; backgrounds
  (setq notmuch-search-line-faces
        '(("unread" . (:foreground "#55ff55"))
          ("flagged" . (:foreground "#5555ff"))))

  (setq notmuch-search-oldest-first nil)

  (setq notmuch-fcc-dirs "Sent")

  (setq frame-background-mode 'dark)

  (setq message-citation-line-format "On %a, %d %b %Y, %f wrote:"
        message-citation-line-function 'message-insert-formatted-citation-line)

  (require 'notmuch-address)
  (setq notmuch-address-command "/home/amthrax/bin/nottoomuch-addresses.sh")
  (notmuch-address-message-insinuate))
