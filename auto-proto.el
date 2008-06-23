;; What about updating prototypes?

(eval-when-compile (require 'cl))

(defun auto-proto-insert ()
  (interactive)

  (buffer-substring
   (progn (c-beginning-of-defun) (point))
   (progn (c-end-of-statement) (point)))
   )

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

;; XXX This is goofy.  The c-forward-syntactic-ws should happen first,
;; but this requires a number of changes.
(defun auto-proto-read-token (&rest tokens)
  (when (looking-at (regexp-opt tokens 'words))
    (goto-char (match-end 0))
    (prog1
        (intern (match-string 0))
      (c-forward-syntactic-ws))))

(defun auto-proto-read-symbol (&rest symbols)
  (when (looking-at (regexp-opt symbols))
    (goto-char (match-end 0))
    (prog1
        (match-string 0)
      (c-forward-syntactic-ws))))

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
               (auto-proto-read-typequal)
               (auto-proto-read-attribute))))))

(defun auto-proto-read-typequal ()
  (auto-proto-read-token
   ;; Type qualifier (6.7.3)
   "const" "restrict" "volatile"))

(defun auto-proto-read-attribute ()
  ;; Attribute (GCC attribute syntax)
  (auto-proto-ptry
   (when (and (auto-proto-read-token "__attribute__" "__attribute")
              (looking-at "("))
     (forward-list)
     (c-forward-syntactic-ws)
     'attribute)))

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

(defun auto-proto-read-declspec ()
  ;; Read a declaration specifier (6.7)
  (auto-proto-ptry
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
        (let ((qs (auto-proto-p* (or (auto-proto-read-typequal)
                                     (auto-proto-read-attribute))))
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

(defun auto-proto-end-of-statement ()
  ;; Place point immediately after the next significant semicolon or
  ;; close brace, skipping over any bracket, brace or paren
  ;; constructs.  This is useful for re-synchronizing while parsing.
  (while (and (/= (char-before (point)) ?\;)
              (/= (char-before (point)) ?})
              (c-syntactic-re-search-forward "[[{(;]" nil t))
    (when (/= (char-before (point)) ?\;)
      (backward-char)
      (forward-list))))

(defun auto-proto-read-declaration ()
  ;; Set up the parse.  Change "_" to be a word character, since it's
  ;; part of the identifier syntax.  I don't know why cc-mode makes
  ;; this a symbol character, but it screws up token parsing.
  (let ((stab (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" stab)
    (with-syntax-table stab
      (c-forward-syntactic-ws)

      ;; Read the declaration specifier list
      (let* ((start (point-marker))
             (ds (auto-proto-read-declspec))
             (storage (first ds))
             (declspec (second ds)))
        (when ds
          ;; Read the optional declarator
          (let ((declarator (auto-proto-read-declarator)))
            ;; Ugh, attributes can also appear here.  Just toss them
            ;; out.
            (when declarator
              (auto-proto-p* (auto-proto-read-attribute)))
            ;; Read the symbol following the declarator
            (let* ((end (point-marker))
                   (next
                    (if declarator
                        (auto-proto-read-symbol "," "=" ";" "{")
                      (auto-proto-read-symbol ";")))
                   (has-body (equal next "{")))
              ;; Get past the end of the declaration
              (cond ((or (string= next ",") (string= next "="))
                     ;; Stop after the next significant semicolon
                     (auto-proto-end-of-statement))
                    ((string= next "{")
                     ;; Stop after the end of the body
                     (goto-char end)
                     (c-forward-syntactic-ws)
                     (forward-list)))
              ;; Construct the return value
              (if declarator
                  ;; Fill the declarator hole with the declaration
                  ;; specifier list.
                  (let ((decor (first declarator))
                        (hole (second declarator))
                        (name (third declarator)))
                    (if decor
                        (setcar hole declspec)
                      (setq decor declspec))
                    (list decor storage name has-body start end))
                (list declspec storage nil nil start end)))))))))

(defun auto-proto-decl-type (decl)
  (first decl))

(defun auto-proto-decl-storage (decl)
  (second decl))

(defun auto-proto-decl-name (decl)
  (third decl))

(defun auto-proto-decl-has-body (decl)
  (fourth decl))

(defun auto-proto-decl-extents (decl)
  (cons (fifth decl) (sixth decl)))

(defun auto-proto-get-proto ()
  ;; Point must be at the beginning of the function declaration
  (let* ((decl (auto-proto-read-declaration))
         (extents (auto-proto-decl-extents decl)))
    (when decl
      (replace-regexp-in-string
       "\\(^[ \t]*\\)\\|\\([ \t]*$\\)" ""
       (replace-regexp-in-string
        "[ \t]*\n[ \t]*" " "
        (buffer-substring (car extents) (cdr extents)))))))

(defun auto-proto-summarize ()
  (c-save-buffer-state
      (decls)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((decl (auto-proto-read-declaration)))
          (setq decls (cons decl decls))
          (unless decl
            (auto-proto-end-of-statement)))))
    (nreverse decls)))

(defun auto-proto-summarize-clear ()
  (remove-overlays nil nil 'owner 'auto-proto))

(defun auto-proto-summarize-test ()
  (auto-proto-summarize-clear)
  (let ((summ (auto-proto-summarize)) (nils 0))
    (dolist (decl summ)
      (if (null decl)
          (setq nils (1+ nils))
        (let* ((extents (auto-proto-decl-extents decl))
               (ov (make-overlay (car extents) (cdr extents))))
          (overlay-put ov 'owner 'auto-proto)
          (overlay-put ov 'face
                       '((:background "midnight blue"))))))
    (message "%d failed decls" nils)))

;; For a function definition, insert or update prototype in file
;; prologue or header.  Offer to make it static if it is not already
;; marked so either in the definition or in an existing prototype.
;; (('fn ...) storage name 'body)
;;
;; For a function prototype, do nothing.
;; (('fn ...) storage name nil)
;;
;; For a typedef, insert or update prototype in header.
;; (type 'typedef name nil)
;;
;; For a struct/union/enum type, insert or update prototype in file
;; prologue or header.  Offer to include the definition or not,
;; defaulting to the current state if updating or to not including
;; otherwise.  Or should it always strip the definition?
;; (('struct/union/enum ...) storage nil nil)
;;
;; For a variable declaration, if static do nothing.  Otherwise,
;; insert or update an extern declaration in the header.
;; (type storage name nil)
