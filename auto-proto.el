;; What about updating prototypes?

(eval-when-compile (require 'cl))

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

(defun auto-proto-read-token (&rest tokens)
  (when (looking-at (regexp-opt tokens 'words))
    (goto-char (match-end 0))
    (prog1
        (intern (match-string 0))
      ;; We skip syntactic whitespace after the fact so that we don't
      ;; have to skip backwards over it if we fail to read the token
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
  ;;
  ;; If this is storage specifier, prepends the storage specifier to
  ;; the list kept in the car of STORAGE and returns t.  The t's will
  ;; later be filtered out of the declaration specifier list.
  (let (tmp)
    (cond ((setq tmp (auto-proto-read-token
                      ;; Storage-class specifier (6.7.1)
                      "typedef" "extern" "static" "auto" "register"
                      ;; Function specifier (6.7.4) While this is
                      ;; technically not a storage class specifier, it
                      ;; does act a lot like one, so we include it
                      ;; here.
                      "inline"))
           (setcar storage (cons tmp (car storage)))
           t)
          (t
           (or (auto-proto-read-typequal)
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

  ;; If we're already at the end of a statement, start moving to the
  ;; next.
  (cond ((bobp)
         (forward-char))
        ((or (= (char-before) ?\;) (= (char-before) ?}))
         (unless (eobp)
           (forward-char))))
  (unless (eobp)
    (while (and (/= (char-before) ?\;)
                (/= (char-before) ?})
                (or (c-syntactic-re-search-forward "[[{(;]" nil t)
                    (progn (goto-char (point-max)) nil)))
      ;; If we hit a bracket, brace, or paren, skip to the other side.
      (when (/= (char-before) ?\;)
        (backward-char)
        (forward-list))
      ;; A close brace will be followed by a semicolon in the case of a
      ;; declaration (for example, struct or array initialization),
      ;; though not in the case of a function definition.  Try to
      ;; consume a semicolon before bailing out just because of a close
      ;; brace.
      (when (= (char-before) ?})
        (let ((semi (save-excursion
                      (c-forward-syntactic-ws)
                      (when (= (char-after) ?\;)
                        (1+ (point))))))
          (when semi (goto-char semi)))))))

;; XXX It might be possible to be more lax in parsing to allow for
;; other tokens to appear in the declaration specifier list (for
;; example, macro calls) by taking advantage of the fact that
;; functions returning functions are semantically disallowed.  Without
;; this, "w x (y)(z)" could be either a function called x that returns
;; a function, or a function called y where x is some unrecognized
;; token.
;;
;; Unfortunately, this approach blurs the boundary between the
;; declaration specifier list and the declarator and also complicates
;; reading of type qualifiers in a pointer declarator.  It might help
;; if read-declspec consumes tokens more liberally (ie, up to a star
;; or paren) and returns the last token consumed, or maybe both the
;; parse including the last token and excluding the last token.

(defun auto-proto-read-declaration ()
  ;; Set up the parse.  Change "_" to be a word character, since it's
  ;; part of the identifier syntax.  I don't know why cc-mode makes
  ;; this a symbol character, but it screws up token parsing.
  (let ((stab (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" stab)
    (with-syntax-table stab
      (auto-proto-ptry
       (catch 'return
         (c-forward-syntactic-ws)

         ;; Read the declaration specifier list
         (let* ((start (point-marker))
                (ds (or (auto-proto-read-declspec)
                        (throw 'return nil)))
                (storage (first ds))
                (declspec (second ds)))
           ;; Read the optional declarator
           (let ((declarator (auto-proto-read-declarator)))
             ;; Ugh, attributes can also appear here.  Just toss them
             ;; out.
             (when declarator
               (auto-proto-p* (auto-proto-read-attribute)))
             ;; Read the symbol following the declarator
             (let* ((end (point-marker))
                    (next
                     (or (if declarator
                             (auto-proto-read-symbol "," "=" ";" "{")
                           (auto-proto-read-symbol ";"))
                         (throw 'return nil)))
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
               (let ((after (save-excursion
                              (c-backward-syntactic-ws)
                              (point-marker))))
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
                       (list decor storage name has-body start end after))
                   (list declspec storage nil nil start end after)))))))))))

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

(defun auto-proto-decl-after (decl)
  (seventh decl))

(defun auto-proto-decl-is-typedef (decl)
  (memq 'typedef (auto-proto-decl-storage decl)))

(defun auto-proto-decl-is-tag (decl)
  (and (null (auto-proto-decl-name decl))
       (memq (car-safe (car (auto-proto-decl-type decl)))
             '(struct union enum))))

(defun auto-proto-decl-is-fndef (decl)
  (and (eq (car (auto-proto-decl-type decl)) 'fn)
       (auto-proto-decl-has-body decl)))

(defun auto-proto-decl-is-fnproto (decl)
  (and (eq (car (auto-proto-decl-type decl)) 'fn)
       (not (auto-proto-decl-has-body decl))))

(defun auto-proto-get-proto ()
  ;; Point must be at the beginning of the function declaration
  (let* ((decl (auto-proto-read-declaration))
         (extents (auto-proto-decl-extents decl)))
    (when decl
      (let ((str (buffer-substring (car extents) (cdr extents))))
        (with-temp-buffer
          (c-mode)
          (insert str)
          (goto-char (point-min))
          ;; Replace all syntactic whitespace with a single space.
          ;; XXX This is not intelligent about strings.
          (delete-horizontal-space)
          (while (not (eobp))
            (when (looking-at "[^ \t\n/]+")
              (goto-char (match-end 0)))
            (let ((start (point)))
              (c-forward-syntactic-ws)
              (cond ((/= start (point))
                     (delete-region start (point))
                     (insert " "))
                    ((not (eobp))
                     (forward-char)))))
          (delete-horizontal-space)
          (list (buffer-substring (point-min) (point-max)) decl))))))

(defun auto-proto-summarize ()
  (c-save-buffer-state
      ((progress (make-progress-reporter
                  (format "Scanning declarations in %s... " (buffer-name))
                  (point-min) (point-max)))
       decls)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (progress-reporter-update progress (point))
        (let ((decl (auto-proto-read-declaration)))
          (setq decls (cons decl decls))
          (unless decl
            (auto-proto-end-of-statement)))))
    (progress-reporter-done progress)
    (nreverse decls)))

(defun auto-proto-summarize-clear ()
  (remove-overlays nil nil 'owner 'auto-proto))

(defun auto-proto-summarize-test ()
  (interactive)
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

(defun auto-proto ()
  (interactive)
  (save-excursion
    ;; Get this declaration
    (c-beginning-of-defun)
    (let* ((proto (or (auto-proto-get-proto)
                      (error "Error getting declaration at point")))
           (this-text (first proto))
           (this-decl (second proto)))
      (cond ((eq (car (auto-proto-decl-type this-decl)) 'fn)
             ;; Insert or update function prototype
             (unless (auto-proto-decl-has-body this-decl)
               (auto-proto-error-decl
                this-decl
                "This declaration is already a prototype"))
             (auto-proto-do-function-prototype this-text this-decl))
            (t
             ;; XXX Highlight decl
             (auto-proto-error-decl
              this-decl
              "I don't know how to prototype this"))))))

(defun auto-proto-error-decl (decl fmt &rest args)
  ;; XXX Highlight decl
  (funcall #'error fmt args))

(defun auto-proto-name-alist (decls)
  (mapcar (lambda (decl) (cons (auto-proto-decl-name decl) decl))
          decls))

(defun auto-proto-highlight-decl (decl)
  ;; XXX
  nil)

(defun auto-proto-do-function-prototype (text decl)
  ;; Get the summary of this file so we can figure out if decl already
  ;; has a prototype and what decl is relative to
  (let* ((decl-name (or (auto-proto-decl-name decl)
                        (auto-proto-error-decl
                         decl
                         "This function doesn't have a name")))
         (decl-storage (auto-proto-decl-storage decl))
         (file-summ (auto-proto-summarize))
         (file-protos (remove-if-not #'auto-proto-decl-is-fnproto file-summ))
         (file-proto-names (auto-proto-name-alist file-protos))
         ;; XXX Header file
         (in-file (cdr (assoc decl-name file-proto-names)))
         )
    (cond
     (in-file
      (let ((extents (auto-proto-decl-extents in-file)))
        ;; Point out the prototype being replaced
        (goto-char (car extents))
        (auto-proto-highlight-decl in-file)
        ;; If the prototype is static and the declaration isn't
        ;; already static, make the new prototype static
        (when (and (memq 'static (auto-proto-decl-storage in-file))
                   (not (memq 'static decl-storage)))
          (setq text (concat "static " text)))
        ;; Prompt
        (when (y-or-n-p (format "Replace prototype with\n %s;\n? " text))
          ;; Replace the old prototype
          (delete-region (car extents) (cdr extents))
          (goto-char (car extents))
          (insert text))))
     (t
      (let ((protos
             ;; Should the prototype be in this file or in the header?
             ;; If it's static or inline, it should always be in the
             ;; file.  Otherwise, ask.
             (if (or (memq 'static decl-storage)
                     (memq 'inline decl-storage))
                 file-summ
               ;; XXX
               file-summ))
            ;; Figure out the function's surrounding context
            (pre-context
             (nreverse
              (remove-if-not #'auto-proto-decl-is-fndef file-summ)))
            (post-context nil)
            (decl-start (car (auto-proto-decl-extents decl))))
        ;; Roll pre-context on to post-context until pre-context is a
        ;; list of declarations preceding decl, starting with the one
        ;; immediately preceding it and post-context is a list of
        ;; declarations following decl, starting with the one
        ;; immediately following it.  The context, in priority order,
        ;; is then the concatenation of these
        (while (and pre-context
                    (> (car (auto-proto-decl-extents (car pre-context)))
                       decl-start))
          (setq post-context (cons (car pre-context) post-context))
          (setq pre-context  (cdr pre-context)))
        (when (and pre-context
                   (= (car (auto-proto-decl-extents (car pre-context)))
                      decl-start))
          (setq pre-context (cdr pre-context)))
        ;; Compute the insertion point
        (message "Finding prototype insertion point...")
        (let* ((insertion
                (auto-proto-compute-insertion decl pre-context post-context
                                              protos))
               (where (first insertion))
               (pre (second insertion))
               (post (third insertion)))
          (setq text (concat pre text ";" post))
          ;; Prompt to insert the prototype
          (goto-char where)
          (when (let ((ov (make-overlay where (1+ where))))
                  (unwind-protect
                      (progn
                        (overlay-put ov 'before-string text)
                        ;; XXX
                        (overlay-put ov 'face
                                     '((:background "midnight blue")))
                        ;; XXX Other options: k to put prototype in
                        ;; kill ring and to push the original point
                        (y-or-n-p "Insert prototype? "))
                    (delete-overlay ov)))
            (insert text))))))))

(defun auto-proto-compute-insertion (decl pre-context post-context dest-decls)
  "Return a triple indicating where and how to insert the prototype of decl.

The triple should be of the form

  (POINT BEFORE-STRING AFTER-STRING)

where POINT indicates the where in the buffer to insert the
prototype and BEFORE-STRING and AFTER-STRING indicate strings to
be inserted before and after the prototype, respectively.

DECL is the declaration for which a prototype is being generated.
PRE-CONTEXT is a list of function definitions preceding DECL,
starting with the one immediately preceding DECL.  POST-CONTEXT
is a list of function definitions following DECL, starting with
the one immediately following DECL.  DEST-DECLS is the list of
declarations in the file that the prototype is being inserted
into.  The returned point should be in the context of
DEST-DECLS.

The current buffer will be the buffer destined to contain the
prototype."

  (let* ((votes (auto-proto-compute-insertion-votes
                 decl pre-context post-context dest-decls))
         (where (or (auto-proto-compute-vote votes)
                    (point-min))))
    ;; Deal with whitespace
    ;;
    ;; XXX Insert an extra blank if this is not preceded by a function
    ;; prototype.  Don't insert a following blank if the following
    ;; line is already blank.
    (save-excursion
      (goto-char where)
      (cond ((bolp)
             (list where "" "\n"))
            ((looking-at "[ \t]*$")
             (list (match-end 0) "\n" ""))
            (t
             (while (forward-comment 1) t)
             (skip-chars-backward " \t\n")
             (skip-chars-forward " \t")
             (list (point) "\n" "\n"))))))

(defvar auto-proto-neighbor-weight 1)
(defvar auto-proto-neighbor-falloff 0.8)
(defvar auto-proto-before-fn-weight 1)
(defvar auto-proto-after-type-weight 1)

(defun auto-proto-compute-insertion-votes (decl pre-context post-context
                                                dest-decls)
  (let (votes)
    ;; Compute votes based on the locations of other function prototypes
    (let ((protos (auto-proto-name-alist
                   ;; Filter down dest-decls to just function prototypes
                   (remove-if-not #'auto-proto-decl-is-fnproto dest-decls))))
      ;; Pre-context
      (let ((weight auto-proto-neighbor-weight))
        (dolist (pre pre-context)
          (let ((proto (cdr (assoc (auto-proto-decl-name pre) protos))))
            (when proto
              (push (list 'after (auto-proto-decl-after proto) weight)
                    votes)))
          (setq weight (* weight auto-proto-neighbor-falloff))))
      ;; Post-context
      (let ((weight auto-proto-neighbor-weight))
        (dolist (post post-context)
          (let ((proto (cdr (assoc (auto-proto-decl-name post) protos))))
            (when proto
              (push (list 'before (car (auto-proto-decl-extents proto)) weight)
                    votes)))
          (setq weight (* weight auto-proto-neighbor-falloff)))))

    ;; Prototypes should come after leading pre-processor goo.  Note
    ;; that the weight of this doesn't actually matter because nothing
    ;; else we're considering could possibly appear before this vote.
    (save-excursion
      (goto-char (point-min))
      (c-forward-syntactic-ws)
      (push (list 'after (point) 1) votes))

    ;; Prototypes should come after typedef's and struct's
    (let ((types (remove-if-not
                  (lambda (decl)
                    (or (auto-proto-decl-is-typedef decl)
                        (auto-proto-decl-is-tag decl)))
                  dest-decls)))
      (dolist (type types)
        (push (list 'after (auto-proto-decl-after type)
                    auto-proto-after-type-weight)
              votes)))

    ;; Prototypes should come before functions
    (let ((fns (remove-if-not #'auto-proto-decl-is-fndef dest-decls)))
      (dolist (fn fns)
        (push (list 'before (car (auto-proto-decl-extents fn))
                    auto-proto-before-fn-weight)
              votes)))

    votes))

(defun auto-proto-compute-vote (votes)
  (when votes
    (setq votes (sort votes (lambda (v1 v2) (< (second v1) (second v2)))))
    ;; Ensure votes doesn't start with a before vote.  This eliminates
    ;; a lot of corner cases below.
    (setq votes (cons '(after 1 0) votes))
    (let ((tally 0)
          (best-tally -1.0e+INF)
          (best-value 0))
      (dolist (v votes)
        (let ((point (second v))
              (weight (third v)))
          (case (first v)
            ((after)
             (setq tally (+ tally weight))
             (when (> tally best-tally)
               (setq best-tally tally)
               (setq best-value point)))
            ((before)
             (setq tally (- tally weight)))
            (t (error "Unknown vote %s" v)))))
      best-value)))

  ;; XXX This can be easily misguided.  Perhaps it should be based on
  ;; voting.  Each context entry can vote (before x) or (after x) and
  ;; the mechanism to place it after structs and before global
  ;; variables or function definitions can also weigh in.
  ;; (catch 'done
  ;;   (let ((protos (auto-proto-name-alist dest-decls)))
  ;;   (dolist (c pre-context)
  ;;     (let ((proto (cdr (assoc (auto-proto-decl-name c) protos))))
  ;;       (when proto
  ;;         (goto-char (cdr (auto-proto-decl-extents proto)))
  ;;         (auto-proto-end-of-statement)
  ;;         (while (forward-comment 1) t)
  ;;         (throw 'done
  ;;                (concat (if (bolp) "" "\n")
  ;;                        text
  ;;                        ";\n"))))))))

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
