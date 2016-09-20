(defadvice find-file (around find-file-line
                             (filename &optional wildcards)
                             activate)
  "Given a file name FILE[:N[:[C:?]]], open FILE and go to line N, column C."
  (let (line col)
    (save-match-data
      (when (string-match "\\(.*?\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:?\\)?$" filename)
        (let ((s (match-string 2 filename)))
          (when s (setq line (string-to-number s))))
        (let ((s (match-string 3 filename)))
          (when s (setq col (string-to-number s))))
        (setq filename (match-string 1 filename))))
    ad-do-it
    (when line
      (goto-line line))
    (when col
      (move-to-column (- col 1)))))

(provide 'find-file-line)
