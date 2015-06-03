;; TODO: Selecting a result should show it in the other window, ala
;; compilation-mode or grep. See `compilation-goto-locus'.

;; TODO: Oracle can take a really long time on public identifiers.
;; Build the buffer more incrementally: first get the definition, then
;; do an oracle with just the local package, then do the global oracle
;; (and ignore results in the local package).

;; TODO: Support go-identify in go-identify buffers (and use the same
;; binding as in go-mode). Will require recording enough information
;; to construct the -pos query from the results.

;; TODO: Better declaration stripping. Start from the beginning, walk
;; over balanced things, and strip from the first unbalanced thing to
;; the beginning of the first comment or EOL. OTOH, in the odd case
;; that a function argument list is split over multiple lines, this
;; will eat the whole thing. Maybe it would be better to understand a
;; little declaration syntax.

;; TODO: C-u prefix to prompt for an identifier and treat it as if it
;; were at point. Requires support in oracle.

(require 'json)
(eval-when-compile (require 'cl))

(defvar go-identify--cwd nil)

(defun go-identify ()
  "Show definition, documentation, and uses for identifier at point."
  (interactive)
  (when (not buffer-file-name)
    (error "Cannot use `go-identify' on a buffer without a file name."))
  (when (buffer-modified-p)
    (error "Please save the buffer before invoking `go-identify'."))
  (let* ((identifier (go-identify--identifier-at-point (point)))
         (filename (file-truename buffer-file-name))
         (go-identify--cwd (file-name-directory filename))
         (pos-arg (format "-pos=%s:#%d" filename (1- (position-bytes (point)))))
         (info (go-identify--call-process-json
                "oracle" "-format=json" pos-arg "referrers" "--"))
         (referrers (assq 'referrers info)))
    (push-mark)
    (ring-insert find-tag-marker-ring (point-marker))
    ;; TODO Consider using `with-temp-buffer-window'
    (with-current-buffer (get-buffer-create "*go-identify*")
      (go-identify-mode)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (let ((objpos (cdr (assq 'objpos referrers))))
          (insert "Defined at ")
          (go-identify--insert-path objpos)
          (insert ".\n\n")
          (go-identify--insert-doc objpos)
          (insert "\n\n")
          (go-identify--insert-referrers (cdr (assq 'refs referrers)) identifier)))
      (goto-char (point-min))
      ;; XXX Put point in this buffer?
      ;; XXX Make next-error work
      ;; XXX grep-mode follows links in the other window
      (display-buffer (current-buffer)))))
;; The following works poorly when clicking links if we follow the
;; link in the current window. It would be okay if we followed the
;; link in the other window.
      ;; (let ((w (display-buffer (current-buffer))))
      ;;   (balance-windows)
      ;;   (shrink-window-if-larger-than-buffer w)))))

(defvar go-identify-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-buffer-map special-mode-map))
    map)
  "Key map used in go-identify buffers.")

(define-derived-mode go-identify-mode special-mode "go-identify"
  "Major mode for go-identify buffers.
\\{go-identify-mode-map}"
  (setq buffer-undo-list t
        truncate-lines t))

(defun go-identify--call-process-json (program &rest args)
  "Call PROGRAM synchronously with ARGS and parse its stdout as JSON."
  (with-temp-buffer
    ;; TODO: Handle missing binary, error status, stderr, etc. This
    ;; gives a confusing "End of file during parsing" error from JSON
    ;; if the oracle failed (e.g., because there's no identifier at
    ;; point).
    (with-temp-message "Running oracle..."
      (apply #'call-process program nil (list t nil) nil args))
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'vector)
          (json-key-type 'symbol)
          (json-false nil)
          (json-null nil))
      (json-read))))

(defun go-identify--identifier-at-point (pos)
  (save-excursion
    (goto-char pos)
    (skip-chars-backward "[:word:][:multibyte:]")
    (save-match-data
      (when (looking-at go-identifier-regexp)
        (match-string-no-properties 0)))))

(defun go-identify--parse-path (path-pos)
  "Parse \"PATH[:LINE[:COL]]\" into (list PATH LINE COL)."
  (save-match-data
    (string-match "\\(.*?\\)\\(?::\\([0-9]+\\)\\)?\\(?::\\([0-9]+\\)\\)?$" path-pos)
    (let ((path (match-string 1 path-pos))
          (line (let ((s (match-string 2 path-pos)))
                  (and s (string-to-number s))))
          (col (let ((s (match-string 3 path-pos)))
                 (and s (string-to-number s)))))
      (list path line col))))

(defun go-identify--group-adj (lst same-group-p)
  "Divide LST into a list of lists.

For each pair of adjacent elements A and B in LST, A and B are
put in the same sublist if (SAME-GROUP-P A B) returns non-nil and
separate sublists otherwise. The concatenation of the returned
list of lists is `equal' to LST."

  (if (null lst)
      '()
    (let ((groups (list (list (car lst)))))
      (while (cdr lst)
        (if (funcall same-group-p (car lst) (cadr lst))
            (push (cadr lst) (car groups))
          (push (list (cadr lst)) groups))
        (setq lst (cdr lst)))
      (nreverse (mapcar #'nreverse groups)))))

(defun go-identify--goto (filename &optional line col-byte)
  (find-file filename)
  (when line
    (goto-line line))
  (when col-byte
    (goto-char (byte-to-position (+ (position-bytes (point)) col-byte -1)))))

(defun go-identify--insert-path (path-pos)
  (let* ((path-parts (go-identify--parse-path path-pos))
         (filename (first path-parts))
         (line (second path-parts))
         (col (third path-parts))
         (rel-path (file-relative-name filename go-identify--cwd)))
    (insert-text-button
     (format "%s:%d" rel-path line)
     'follow-link t
     'action (lambda (b)
               (apply #'go-identify--goto (button-get b 'pos)))
     'pos path-parts)))

(defun go-identify--insert-doc (path-pos)
  "Insert the doc comment and definition for the definition at PATH-POS."
  ;; Poor man's godoc. godoc doesn't let us make precise enough
  ;; queries and doesn't expose private identifiers. Too bad.
  (let* ((path-parts (go-identify--parse-path path-pos))
         (filename (first path-parts))
         (line (second path-parts))
         (col (third path-parts))
         (output-buffer (current-buffer))
         (beg (point)))
    (condition-case err
        ;; XXX Link these lines
        (insert
         (with-temp-buffer
           (setq buffer-undo-list t)
           (insert-file-contents filename)
           (go-mode)
           (forward-line (- line 1))
           (let ((end (1+ (line-end-position))))
             (save-restriction
               ;; Narrow to after preceding blank line, if any
               (save-excursion
                 (when (re-search-backward "^[[:space:]]*$" nil t)
                   (narrow-to-region (point) end)))

               ;; Strip { or ( that opens a definition block
               (save-excursion
                 (let ((cs (save-excursion
                             (comment-search-forward (point-max) t))))
                   (if cs
                       (goto-char cs)
                     (end-of-line))
                   (when (looking-back "[{(][[:space:]]*")
                     (delete-region (match-beginning 0) (match-end 0)))))

               ;; Find comment preceding the definition
               (forward-comment -1000)
               (when (not (bolp))
                 ;; We wound up on a trailing comment for something
                 ;; else.
                 (forward-line 1))
               (skip-chars-forward "[:space:]\r\n")
               (forward-line 0)
               (go-identify--get-hunk (1+ (count-lines (point) end)))))))
      (file-error
       (insert (format "%s: %s\n" filename (error-message-string err)))))

    ;; Strip common indentation
    (go-identify--reindent beg (point) "")))

(defun go-identify--insert-referrers (refs identifier)
  ;; XXX Put references from this file first, then other files in this
  ;; package, then other packages.
  (let ((prefs (sort (mapcar #'go-identify--parse-path refs)
                     (lambda (a b)
                       (cond ((not (equal (first a) (first b)))
                              (string-lessp (first a) (first b)))
                             ((/= (second a) (second b))
                              (< (second a) (second b)))
                             (t
                              (< (third a) (third b)))))))
        (output-buffer (current-buffer)))
    (dolist (file-group (go-identify--group-adj
                         prefs (lambda (a b) (equal (first a) (first b)))))
      ;; Open this file
      (let* ((filename (first (first file-group)))
             (rel-filename (file-relative-name filename go-identify--cwd))
             hunks)
        (condition-case err
            (with-temp-buffer
              (setq buffer-undo-list t)
              (insert-file-contents filename)
              (go-mode)
              ;; Get hunks from this file
              (dolist (hunk (go-identify--group-adj
                             file-group (lambda (a b)
                                          (<= (- (second b) (second a)) 2))))
                ;; Insert hunk
                (let* ((first (first hunk))
                       (last (car (last hunk)))
                       (line-begin (- (second first) 1))
                       (line-end (+ (second last) 1))
                       (start (progn
                                (goto-char (point-min))
                                (forward-line (- line-begin 1))
                                (point)))
                       (text (go-identify--get-hunk (1+ (- line-end line-begin))))
                       (context (go-identify--get-context)))
                  (with-current-buffer output-buffer
                    ;; Insert hunk header
                    ;; XXX Add a shortcut number?
                    (insert-text-button
                     (format "%s:%d-%d" rel-filename line-begin line-end)
                     'follow-link t
                     'action (lambda (b)
                               (apply #'go-identify--goto (button-get b 'pos)))
                     'pos first)
                    ;; Say what function the hunk is in
                    ;; XXX Link this part of the line, too
                    (when context
                      (insert (concat " in " context)))
                    (newline)
                    ;; Insert hunk text
                    ;; XXX Link each line
                    (let ((start (point)))
                      (insert text)
                      ;; Highlight occurrences of identifier in this hunk
                      (go-identify--highlight start line-begin identifier hunk)
                      ;; Simplify indentation of this hunk
                      (go-identify--reindent start (point) "\t")
                      (goto-char (point-max)))
                    (newline)
                    (newline)))))
          (file-error
           (insert (format "%s: %s\n" rel-filename (error-message-string err)))))))
    ;; Remove extra newline at end
    (delete-char -1)
    (unless (bolp)
      (newline))))

(defun go-identify--get-hunk (line-count)
  "Return fontified text for the next LINE-COUNT lines in this buffer."
  (let ((beg (point))
        (end (save-excursion
               (forward-line line-count)
               (forward-char -1)
               (point))))
    (font-lock-fontify-region beg end)
    (buffer-substring beg end)))

(defun go-identify--get-context ()
  "Return a single line string indicating the context at point."
  ;; Look for the containing function
  (save-excursion
    (let ((pt (point))
          (beg (progn (forward-char 1) (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (when (< pt end)
        (goto-char beg)
        (go-identify--get-hunk 1)))))

(defun go-identify--highlight (point line-begin identifier prefs)
  "Highlight IDENTIFIER for each parsed reference PREFS.

The line beginning at POINT in the current buffer must correspond
to LINE-BEGIN in the file referenced by the parsed references."

  (save-excursion
    ;; identifier is used solely for its length, since PREFS tells us
    ;; where it starts, but not where it ends.
    ;;
    ;; XXX We could instead just highlight whatever identifier is at
    ;; the given position.
    (let ((identifier-length (length identifier)))
      (dolist (pref prefs)
        (goto-char point)
        (forward-line (- (second pref) line-begin))
        (goto-char (byte-to-position (+ (position-bytes (point)) (third pref) -1)))
        (let ((ov (make-overlay (point) (+ (point) identifier-length))))
          (overlay-put ov 'face 'match))))))

(defun go-identify--reindent (beg end indent)
  "Replace common indentation between BEG and END with INDENT."
  (save-excursion
    (save-match-data
      ;; Find common indentation
      (goto-char beg)
      (let (common)
        (while (and (< (point) end) (not (eobp)))
          (looking-at "[[:blank:]]*")
          (let ((space (match-string-no-properties 0)))
            (cond
             ;; Ignore all-whitespace lines
             ((= (match-end 0) (line-end-position)) t)
             ((null common)
              (setq common space))
             (t
              (setq common (go-identify--common-prefix common space)))))
          (forward-line 1))
        (setq common (or common ""))

        ;; Replace common indentation with a fixed indentation
        (goto-char beg)
        (let ((common-len (length common))
              (end (copy-marker end)))
          (while (and (< (point) end) (not (eobp)))
            (if (looking-at "[[:blank:]]*$")
                ;; Remove all-whitespace line
                (delete-region (point) (match-end 0))
              ;; Remove common indentation
              (delete-region (point) (+ (point) common-len)))
            ;; Add fixed indentation
            (insert-before-markers indent)
            (forward-line 1)))))))

(defun go-identify--common-prefix (a b)
  "Return the longest common prefix of strings A and B."
  (let ((i 0) (lim (min (length a) (length b))))
    (while (and (< i lim) (= (aref a i) (aref b i)))
      (setq i (1+ i)))
    (substring a 0 i)))

(provide 'go-identify)
