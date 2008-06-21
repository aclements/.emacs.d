;; What about updating prototypes?

(defun auto-proto-insert ()
  (interactive)

  (buffer-substring
   (progn (c-beginning-of-defun) (point))
   (progn (c-end-of-statement) (point)))
   )

(defun auto-proto-read-token (&rest tokens)
  (when (looking-at (regexp-opt tokens 'words))
    (goto-char (match-end 0))
    (c-forward-syntactic-ws)
    (intern (match-string 0))))

(defun auto-proto-read-symbol (&rest symbols)
  (when (looking-at (regexp-opt symbols))
    (goto-char (match-end 0))
    (c-forward-syntactic-ws)
    (match-string 0)))

(defun auto-proto-read-name ()
  (when (looking-at "[[:alpha:]_][[:alnum:]_$]*")
    (goto-char (match-end 0))
    (c-forward-syntactic-ws)
    (match-string 0)))

(defun auto-proto-read-s/q/a/f ()
  (or
   (auto-proto-read-token
    ;; Storage
    "typedef" "extern" "static" "auto" "register"
    ;; Function spec
    "inline")
   (auto-proto-read-q/a)))

(defun auto-proto-read-q/a ()
  (auto-proto-read-token
   ;; Qualifier
   "const" "restrict" "volatile"
   ;; XXX Attribute
   ))

(defun auto-proto-read-b/s ()
  (auto-proto-read-token
   ;; Basic type
   "void" "char" "short" "int" "long" "float" "double" "_Bool"
   ;; Sign
   "signed" "unsigned"))

(defun auto-proto-read-s/u/e ()
  (let ((s/u/e (auto-proto-read-token "struct" "union" "enum")))
    (when s/u/e
      (let ((name (auto-proto-read-name)))
        (when (looking-at "{")
          (forward-list)
          (c-forward-syntactic-ws))
        (list s/u/e name)))))

(defmacro auto-proto-p* (parser)
  (let ((gather (gensym "gather"))
        (val (gensym "val")))
    `(let ((,gather nil))
       (let ((,val ,parser))
         (while ,val
           (setq ,gather (cons ,val ,gather))
           (setq ,val ,parser)))
       (nreverse ,gather))))

(defmacro auto-proto-ptry (&rest parser)
  (let ((start (gensym "start"))
        (result (gensym "result")))
    `(let* ((,start (point))
            (,result (progn ,@parser)))
       (unless ,result
         (goto-char ,start))
       ,result)))

(defun auto-proto-read-typeclass ()
  (auto-proto-ptry
    (c-forward-syntactic-ws)              ;XXX
    (catch 'failed
      (let (q1 type q2)
        (setq q1 (auto-proto-p* (auto-proto-read-s/q/a/f)))
        (cond ((setq type (auto-proto-read-b/s))
               (setq q2 (auto-proto-p* (or (auto-proto-read-s/q/a/f)
                                           (auto-proto-read-b/s)))))
              ((setq type (auto-proto-read-s/u/e))
               (setq q2 (auto-proto-p* (auto-proto-read-s/q/a/f))))
              ((setq type (auto-proto-read-name))
               (setq q2 (auto-proto-p* (auto-proto-read-s/q/a/f))))
              (t (throw 'failed nil)))
        (list (nconc q1 q2) type)))))

(defun auto-proto-read-decor (tc)
  (auto-proto-ptry
    (if (auto-proto-read-symbol "*")
        (progn
          (auto-proto-p* (auto-proto-read-q/a))
          (auto-proto-read-decor tc))
      (auto-proto-read-decor1 tc))))

(defun auto-proto-read-decor1 (tc)
  (auto-proto-ptry
    (let (name)
      (prog1
          (cond ((auto-proto-read-symbol "(")
                 (let ((decor (auto-proto-read-decor tc)))
                   (when (and decor
                              (auto-proto-read-symbol ")"))
                     decor)))
                ((setq name (auto-proto-read-name))
                 name))
        (while (looking-at "[[(]")
          (forward-list)
          (c-forward-syntactic-ws))))))

(defun auto-proto-read-decl ()
  (let ((tc (auto-proto-read-typeclass)))
    ;; XXX decor is optional.  Must be followed by ',', '=', ';', or '{'
    (when tc
      (auto-proto-read-decor tc))))
