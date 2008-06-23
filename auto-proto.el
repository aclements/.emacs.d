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

(defun auto-proto-read-basic-declspec (storage)
  ;; Any declaration specifier other than a type specifier (6.7)
  (let (tmp)
    (cond ((setq tmp (auto-proto-read-token
                      ;; Storage-class specifier (6.7.1)
                      "typedef" "extern" "static" "auto" "register"))
           (setcar storage tmp)
           t)
          (t
           (or (auto-proto-read-token
                ;; Function specifier (6.7.4)
                "inline")
               (auto-proto-read-typequal))))))

(defun auto-proto-read-typequal ()
  (auto-proto-read-token
   ;; Type qualifier (6.7.3)
   "const" "restrict" "volatile"
   ;; XXX Attribute
   ))

(defun auto-proto-read-basic-typespec ()
  (auto-proto-read-token
   ;; Type specifier (6.7.2) other than struct/union/enum/typedef name
   "void" "char" "short" "int" "long" "float" "double"
   "signed" "unsigned"
   "_Bool" "_Complex"))

(defun auto-proto-read-sue-typespec ()
  (let ((s/u/e (auto-proto-read-token "struct" "union" "enum")))
    (when s/u/e
      (let* ((name (auto-proto-read-name))
             (is-complete
              (when (looking-at "{")
                (forward-list)
                (c-forward-syntactic-ws))))
        (list s/u/e name is-complete)))))

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

(defun auto-proto-read-declspec ()
  ;; Read a declaration specifier (6.7)
  (auto-proto-ptry
    (c-forward-syntactic-ws)              ;XXX
    (catch 'failed
      (let (s1 typespec s2 tmp (storage (list nil)))
        ;; Gather declaration specifiers other than the type specifier
        (setq s1 (auto-proto-p* (auto-proto-read-basic-declspec storage)))
        ;; Read the type specifier.  We need to do this carefully so
        ;; we can recognize typedef names without having to know the
        ;; typedef names in scope.  Luckily, we can recognize all of
        ;; the cases that are not a typedef name, and if it is a
        ;; typedef name, there can only be one of them in the
        ;; declaration specifier (6.7.2.2).
        (cond ((setq typespec (auto-proto-read-basic-typespec))
               ;; Basic type specifiers can be moshed together with
               ;; other basic type specifiers and other declaration
               ;; specifiers (6.7.2.2)
               (setq typespec (list typespec))
               (setq s2 (auto-proto-p*
                         (cond ((setq tmp (auto-proto-read-basic-typespec))
                                (setq typespec (cons tmp typespec))
                                t)
                               (t (auto-proto-read-basic-declspec storage)))))
               (setq typespec (nreverse typespec)))
              ((setq typespec (or (auto-proto-read-sue-typespec)
                                  ;; Must try typedef names last
                                  (auto-proto-read-name)))
               (unless (consp typespec)
                 (setq typespec (list 'name typespec)))
               ;; A struct, union, enum, or typedef name type
               ;; specifier must be the only type specifier, but may
               ;; be followed by other declaration specifiers
               ;; (6.7.2.2)
               (setq s2 (auto-proto-p* (auto-proto-read-basic-declspec storage))))
              (t (throw 'failed nil)))
        (let ((other-specs (delq t (nconc s1 s2))))
          (list (car storage) (list typespec other-specs)))))))

(defun auto-proto-read-declarator ()
  ;; Read a declarator (6.7.5).  Note that declarator syntax is
  ;; upside-down so we build the type from the outside in as we unwind
  ;; the parser recursion.  The return from this function is a 3-tuple
  ;; of (ROOT-DECLARATOR HOLE-POINTER NAME) where ROOT-DECLARATOR is
  ;; the top of the declarator tree (may be nil) and the car of
  ;; HOLE-POINTER is the bottom of the declarator tree such that
  ;; setting the car will update the tree bottom.
  (auto-proto-ptry
    (if (auto-proto-read-symbol "*")
        ;; Pointer
        (let ((qs (auto-proto-p* (auto-proto-read-typequal)))
              (decor (auto-proto-read-declarator)))
          (when decor
            ;; Fill in the hole with the pointer
            (let ((root (first decor))
                  (hole (second decor))
                  (name (third decor))
                  (new-bottom (list 'ptr nil qs)))
              (if root
                  (setcar hole new-bottom)
                (setq root new-bottom))
              (list root (cdr new-bottom) name))))
      ;; Direct declarator
      (auto-proto-read-direct-declarator))))

(defun auto-proto-read-direct-declarator ()
  (auto-proto-ptry
   (let ((decor
          (let (tmp)
            (cond ((auto-proto-read-symbol "(")
                   ;; Parenthesized declarator
                   (when (and (setq tmp (auto-proto-read-declarator))
                              (auto-proto-read-symbol ")"))
                     tmp))
                  ((setq tmp (auto-proto-read-name))
                   ;; Identifier.  This is the base of the declarator
                   ;; recursion.
                   (list nil nil tmp))))))
     (when decor
       (while (looking-at "[[(]")
         ;; Fill in the declarator hole with the function or array
         (let ((root (first decor))
               (hole (second decor))
               (name (third decor))
               (new-bottom (if (looking-at "(")
                               (list 'fn nil)
                             (list 'arr nil))))
           (if root
               (setcar hole new-bottom)
             (setq root new-bottom))
           (setq decor (list root (cdr new-bottom) name)))
         ;; Skip the arguments or array dimensions
         (forward-list)
         (c-forward-syntactic-ws)))
     decor)))

(defun auto-proto-read-declaration ()
  (let ((ds (auto-proto-read-declspec)))
    (when ds
      (let ((storage (first ds))
            (declspec (second ds))
            (declarator (auto-proto-read-declarator)))
        ;; XXX decor is optional if declspec declares a tag or is a
        ;; typedef.  Must be followed by ',', '=', ';', or '{'
        (when declarator
          (let ((decor (first declarator))
                (hole (second declarator))
                (name (third declarator)))
            (if decor
                (setcar hole declspec)
              (setq decor declspec))
            (list decor name)))))))

;; (fun name type start end)
;; (var name type start end)
;; (type name type start end)

(defun auto-proto-get-proto ()
  ;; Point must be at the beginning of the function declaration
  (let* ((start (point-marker))
         (decl (auto-proto-read-declaration))
         (end (point-marker)))
    (when decl
      (replace-regexp-in-string
       "\\(^[ \t]*\\)\\|\\([ \t]*$\\)" ""
       (replace-regexp-in-string
        "[ \t]*\n[ \t]*" " "
        (buffer-substring start end))))))

;; For a function definition, insert or update prototype in file
;; prologue or header.  Offer to make it static if it is not already
;; marked so either in the definition or in an existing prototype.
;;
;; For a function prototype, do nothing.
;;
;; For a typedef, insert or update prototype in header.
;;
;; For a struct/union/enum type, insert or update prototype in file
;; prologue or header.  Offer to include the definition or not,
;; defaulting to the current state if updating or to not including
;; otherwise.  Or should it always strip the definition?
;;
;; For a variable declaration, if static do nothing.  Otherwise,
;; insert or update an extern declaration in the header.
