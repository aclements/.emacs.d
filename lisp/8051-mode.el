;;; Notes:
;; There are four types of comments:
;; * Side comments are introduced by a single semicolon (which isn't
;;   in the left-most column).  These are aligned to `comment-column`.
;; * Block comments are introduced by a single semicolon in the
;;   left-most oclumn.  These are left-aligned and are meant for
;;   commenting out blocks of code.
;; * Code comments are introduced by two semicolons.  These are
;;   lined-up with the instructions.  These are meant to introduce
;;   short blocks of code.
;; * Top comments are introduced by three semicolons.  These are
;;   left-aligned and are meant for top-level introductions, such as
;;   functions.  These are displayed in `8051-top-comment-face`.

;;; Todo:
;; * Fix idle timer so only one exists at a time and it only acts on
;;   the current buffer and only if it's in 8051-mode
;; * Insert labels for functions (function name + . + label)
;; ** Automatically fill in function name
;; ** Make easy to insert label and to call on label
;; * Summary doc in minibuffer? (ie, "mov dst, src - Foo")
;; * Symbolize instructions like on 1-11 of MCS-51 manual
;; * Annotate arguments with their token types
;; ** Easy syntax
;; *** acc (A), dir (8-bit address or SFR), ind8 (@R[01], @SP), ind16
;;     (@DPTR), reg (R[0-7]), imm (numeric constant), dptr (dptr), b
;;     (b), carry (c), ab (ab), pgm (@a+dptr, @a+pc), addr (bareword,
;;     or possibly # followed by non-numeric)
;; ** Op-dependent
;; *** bit (bit-address, looks like a numeric constant), not-bit
;;     (/bit), rel (looks like a label, but has to be close)
;; ** Others
;; *** Annotate psw.n with bit name (see 2-11 in MCS-11)
;; ** How to deal with symbolic constants like SFRs?
;; ** Distinguish between internal and external addresses (could also
;;    do RAM versus ROM, though it's the same for us)
;; * Indicate when argument types aren't valid for instruction
;; * Annotate with execution times of instructions
;; * Make # electric for #include?
;; * Fix paragraph filling
;; * Baud rate calculator?
;; * There are a bunch of synonyms for bit addressable SFRs that
;;   should be highlighted along with the SFRs
;; * Linting
;; ** Make sure procedures aren't calling private labels in other
;;    procedures
;; ** Check for ret at end of procedure
;; *** Distinguish between private procedures and private jumps
;; **** If there aren't any private calls, make sure there's a ret at
;;      the very end of the procedure.  If there are private calls,
;;      make sure there's a ret before the first private label that's
;;      called.
;; **** Perhaps some general rule like every label that's called must
;;      ret before the next label that's not a prefix of the called
;;      label
;; *** Could have multiple exit points (ie, a cjne branch)
;; **** Code path analysis?  Make sure the procedure entry is always
;;      paired with a return still in the procedure and any internal
;;      calls are paired with internal rets
;; ** Prologue and epilog stack pairing
;; ** Check for reti at the end of interrupt handlers
;; ** Check for psw push around interrupt handlers

(require 'cl)

(defvar 8051-instruction-offset 4
  "The column to indent instructions to.")

(defvar 8051-argument-offset (+ 8051-instruction-offset 6)
  "The column to indent arguments to.")

(defvar 8051-equ-offset 12
  "The column to indent equ ops to.")

(defvar 8051-doc-delay 5
  "The number of seconds of idle time to wait for before displaying
documentation about the instruction on the current line")

(defvar 8051-label-history-list nil)

(defconst 8051-top-comment-face '8051-top-comment-face)
(defface 8051-top-comment-face
  '((((class color) (background light))
     (:foreground "red4"))
    (((class color) (background dark))
     (:foreground "red1"))
    (t (:inherit font-lock-comment-face)))
  "Face used for text in block comments."
  :group '8051-mode)

(defconst 8051-opcodes
  ;; http://www.win.tue.nl/~aeb/comp/8051/set8051.html
  '(("acall" "Absolute Call")
    ("add"   "Add Accumulator")
    ("addc"  "Add Accumulator (With Carry)")
    ("ajmp"  "Absolute Jump")
    ("anl"   "Bitwise AND")
    ("cjne"  "Compare and Jump if Not Equal")
    ("clr"   "Clear Register")
    ("cpl"   "Complement Register")
    ("da"    "Decimal Adjust")
    ("dec"   "Decrement Register")
    ("div"   "Divide Accumulator by B")
    ("djnz"  "Decrement Register and Jump if Not Zero")
    ("inc"   "Increment Register")
    ("jb"    "Jump if Bit Set")
    ("jbc"   "Jump if Bit Set and Clear Bit")
    ("jc"    "Jump if Carry Set")
    ("jmp"   "Jump to Address")
    ("jnb"   "Jump if Bit Not Set")
    ("jnc"   "Jump if Carry Not Set")
    ("jnz"   "Jump if Accumulator Not Zero")
    ("jz"    "Jump if Accumulator Zero")
    ("lcall" "Long Call")
    ("ljmp"  "Long Jump")
    ("mov"   "Move Memory")
    ("movc"  "Move Code Memory")
    ("movx"  "Move Extended Memory")
    ("mul"   "Multiply Accumulator by B")
    ("nop"   "No Operation")
    ("orl"   "Bitwise OR")
    ("pop"   "Pop Value From Stack")
    ("push"  "Push Value Onto Stack")
    ("ret"   "Return From Subroutine")
    ("reti"  "Return From Interrupt")
    ("rl"    "Rotate Accumulator Left")
    ("rlc"   "Rotate Accumulator Left Through Carry")
    ("rr"    "Rotate Accumulator Right")
    ("rrc"   "Rotate Accumulator Right Through Carry")
    ("setb"  "Set Bit")
    ("sjmp"  "Short Jump")
    ("subb"  "Subtract From Accumulator With Borrow")
    ("swap"  "Swap Accumulator Nibbles")
    ("xch"   "Exchange Bytes")
    ("xchd"  "Exchange Digits")
    ("xrl"   "Bitwise Exclusive OR")))

(defconst 8051-sfrs
  ;; http://www.8052.com/tutsfr.phtml
  ;; Name     type     bit 8052 description
  '(("p0"     'io      t   nil  "Port 0")
    ("sp"     'other   nil nil  "Stack pointer")
    ("dpl"    'other   nil nil  "Data pointer low")
    ("dph"    'other   nil nil  "Data pointer high")
    ("pcon"   'control nil nil  "Power control")
    ("tcon"   'control t   nil  "Timer control")
    ("tmod"   'control nil nil  "Timer mode")
    ("tl0"    'other   nil nil  "Timer 0 low")
    ("tl1"    'other   nil nil  "Timer 1 low")
    ("th0"    'other   nil nil  "Timer 0 high")
    ("th1"    'other   nil nil  "Timer 1 high")
    ("p1"     'io      t   nil  "Port 1")
    ("scon"   'control t   nil  "Serial control")
    ("sbuf"   'other   nil nil  "Serial control")
    ("p2"     'io      t   nil  "Port 2")
    ("ie"     'control t   nil  "Interrupt enable")
    ("p3"     'io      t   nil  "Port 3")
    ("ip"     'control t   nil  "Interrupt priority")
    ("psw"    'control t   nil  "Program status word")
    ("acc"    'other   t   nil  "Accumulator")
    ("b"      'other   t   nil  "B register")
    ("t2con"  'control t   t    "Timer 2 control")
    ("th2"    'other   nil t    "Timer 2 high")
    ("tl2"    'other   nil t    "Timer 2 low")
    ("rcap2h" 'other   nil t    "Timer 2 capture high")
    ("rcap2l" 'other   nil t    "Timer 2 capture low")))

(defconst 8051-avoid-numbered-labels
  '("acc" "b" "ie" "ip" "p0" "p1" "p2" "p3" "psw" "scon" "t2con"
  "tcon"))

(defconst 8051-avoid-labels
  '("cprl2" "ct2" "cy" "dph" "dpl" "ea" "es" "et0" "et1" "ex0" "ex1"
  "exen2" "exf2" "exti0" "exti1" "f0" "ie0" "ie1" "int0" "int1" "it0"
  "it1" "ov" "p" "pcon" "ps" "pt0" "pt1" "px0" "px1" "rb8" "rcap2h"
  "rcap2l" "rclk" "rd" "ren" "reset" "ri" "sbuf" "sint" "sm0" "sm1"
  "sm2" "sp" "t0" "t1" "tb8" "tclk" "tf0" "tf1" "tf2" "th0" "th1"
  "th2" "ti" "timer0" "timer1" "tl0" "tl1" "tl2" "tmod" "tr0" "tr1"
  "tr2" "txd" "wr"))

;; XXX There might be more valid characters
(defconst 8051-label-regex "[a-zA-Z_][a-zA-Z0-9_.]*")

(defconst 8051-font-lock-keywords
  (let ((keywords (regexp-opt (mapcar 'first 8051-opcodes)))
        (label 8051-label-regex)
        (avoid-label (concat (regexp-opt 8051-avoid-labels) "\\|"
                             "\\(?:"
                             (regexp-opt 8051-avoid-numbered-labels)
                             "\\)\\(?:\.[0-7]\\)?"))
        (bit-sfrs (mapcar 'first (remove-if-not 'third 8051-sfrs)))
        (non-bit-sfrs (mapcar 'first (remove-if 'third 8051-sfrs)))
        (number (concat "\\([0-9]+d?\\>\\|"
                        "[0-9][0-9a-f]*h\\>\\|"
                        "[0-7]+o\\>\\|"
                        "[01]+b\\>\\)")))
    `( ;; Block comments
      ("^\;\;\;+[ \t]*\\(.*[\r\n]?\\)"
       (1 8051-top-comment-face prepend))
      ;; Keyword
      (,(concat "^[ \t]*\\(" keywords "\\)\\>")
       (1 font-lock-keyword-face))
      ;; Label followed by a keyword
      (,(concat "^" label ":[ \t]*\\(" keywords "\\)?\\>")
       (1 font-lock-keyword-face nil t))
      ;; Bad label
      (,(concat "^\\(" avoid-label "\\):")
       (1 font-lock-warning-face))
      ;; Just a label
      (,(concat "^\\(" label "\\):")
       (1 font-lock-function-name-face))
      ;; Include directives
      ("^#\\(include\\)\\([ \t]+[^ \t\n]+\\)?"
       (1 font-lock-builtin-face)
       (2 font-lock-string-face))
      ;; Data pseudo ops
      ("^[ \t]*\\<\\(db\\|dw\\)\\>"
       (1 font-lock-builtin-face))
      ;; Data pseudo op values prefixed with # are an easy error.  XXX
      ;; This only highlights the first one in a line
      (,(concat "^[ \t]*\\(?:db\\|dw\\)[ \t]+\\(?:\\|.*?,[ \t]*\\)"
                "\\(#[^ \t\n,]*\\)")
       (1 font-lock-warning-face))
      ;; Data pseudo op numeric values.  XXX This only highlights the
      ;; first numeric value, which makes it rather silly (how do I
      ;; fix this?)
;;       (,(concat "^[ \t]*\\(?:db\\|dw\\)[ \t]+\\(?:\\|.*,[ \t]*\\)\\<\\("
;;                 number "\\)\\>[ \t]*\\(?:,\\|$\\)")
;;        (1 font-lock-constant-face))
      ;; org pseudo op (hex number)
      ("^[ \t]*\\<\\(org\\)[ \t]*\\([0-9][0-9a-f]*h\\>\\)"
       (1 font-lock-builtin-face)
       (2 font-lock-constant-face))
      ;; org pseudo op (other number)
      ("^[ \t]*\\<\\(org\\)[ \t]*\\([0-9]+d?\\|[0-7]+o\\|[01]+b\\)$"
       (1 font-lock-builtin-face)
       (2 font-lock-warning-face))
      ;; org pseudo op (no number, invalid, but annoying without this)
      ("^[ \t]*\\<\\(org\\)\\>"
       (1 font-lock-builtin-face))
      ;; equ
      (,(concat "^[ \t]*\\(" label "\\)[ \t]+\\(equ\\)\\>")
       (1 font-lock-variable-name-face)
       (2 font-lock-builtin-face))
      ;; equ constant
      (,(concat "^[ \t]*" label "[ \t]+equ[ \t]+"
                "\\(" number "\\)")
       (1 font-lock-constant-face))
      ;; equ with a pound sign is almost certainly a mistake
      (,(concat "^[ \t]*" label "[ \t]+equ[ \t]+\\(#[^ \t\n]*\\)")
       (1 font-lock-warning-face))
      ;; Bad label literal
      (,(concat "\\(#\\(?:" avoid-label "\\)\\>\\)")
       (1 font-lock-warning-face))
      ;; Numeric literals
      (,(concat "#" number)
       (0 font-lock-constant-face))
      ;; Warn about ASCII constants without preceeding # (these are
      ;; memory addresses, which is almost certainly unintended).  XXX
      ;; The only invalid place to do this is in a db/dw.
      ("[^#]\\('.'\\)"
       (1 font-lock-warning-face prepend))
      ;; ASCII literals
      ("\\(#'.'\\)"
       (1 font-lock-constant-face prepend))
      ;; Registers, accumulator, etc.
      (,(concat "\\<\\(r[0-7]\\|@r[01]\\|a\\|c\\|ab\\|"
                "@?dptr\\|@a\\+\\(?:dptr\\|pc\\)\\)\\>")
       (1 font-lock-variable-name-face))
      ;; Bit-addressable SFRs
      (,(concat "\\<\\(\\(?:" (regexp-opt bit-sfrs)
                "\\)\\(?:\.[0-7]\\)?\\)\\>")
       (1 font-lock-variable-name-face))
      ;; Non-bit-addressable SFRs
      (,(concat "\\<\\(" (regexp-opt non-bit-sfrs) "\\)\\>")
       (1 font-lock-variable-name-face))
      ;; Built-in functions (this has to come after arguments or the +
      ;; will confuse @a+dptr)
      ("\\(\\<high\\>\\|\\<low\\>\\|\\+\\|-\\|\\*\\|/\\)"
       (1 font-lock-builtin-face))
      ;; XXX Address literals, equ, invalid bits on bit-addressable
      ;; registers
      )))

(defvar 8051-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?#  "w"  st)
    (modify-syntax-entry ?.  "_"  st)
    (modify-syntax-entry ?@  "w"  st)
    st))

(defvar 8051-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm ":" '8051-electric-colon)
    (define-key mm ";" '8051-electric-semicolon-or-comma-or-pound)
    (define-key mm "," '8051-electric-semicolon-or-comma-or-pound)
    (define-key mm "#" '8051-electric-semicolon-or-comma-or-pound)
    (define-key mm "\C-m" 'reindent-then-newline-and-indent)
    (define-key mm [backspace] '8051-hungry-backspace)
    (define-key mm "\C-c\C-f" '8051-insert-function)
    (define-key mm "\C-c\C-t" '8051-insert-top-header-final)
    (define-key mm "\C-c\C-s" '8051-insert-section)
    (define-key mm "\M->" '8051-find-label)
    mm))

(defvar 8051-mode nil)
(make-variable-buffer-local '8051-mode)

(defvar 8051-doc-timer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8051 mode
;;;

(define-derived-mode 8051-mode fundamental-mode "8051"
  "Major mode for editing 8051 assembly and interacting with rasm.
\\{8051-mode-map}"

  ;; Make variables
  (make-local-variable '8051-doc-showing)
  (setq 8051-doc-showing nil)

  ;; Set up indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function (function 8051-indent-line))

  ;; Set up font lock
  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (setq font-lock-defaults
        '(8051-font-lock-keywords nil t nil nil))

  ;; Commenting
  (mapcar (function make-local-variable)
          '(comment-start comment-end comment-multi-line
                          comment-start-skip))
  (setq comment-start "; "
        comment-end ""
        comment-multi-line nil
        comment-start-skip ";[ \t;]*")

  ;; Paragraph navigation
  (setq paragraph-start "^[ \t\n]*$"
        paragraph-separate "^[ \t\n]*$")

  ;; Auto-documentation
  (unless 8051-doc-timer
    (setq 8051-doc-timer
          (run-with-idle-timer 8051-doc-delay t #'8051-show-doc)))

  (setq 8051-mode t)
  )

(defun 8051-show-doc ()
  (when 8051-mode
    (let ((op (8051-get-opcode)))
      (when (and op (not (string-equal op 8051-doc-showing)))
        (let ((doc (second (assoc op 8051-opcodes))))
          (if doc
              (message "%s - %s" op doc)
            (message "%s - Unknown op code" op))))
      (setq 8051-doc-showing op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line parsing and indenting
;;;

(defun 8051-get-opcode ()
  (let ((class (8051-classify-line)))
    (when (memq class '(instruction label-and-instruction))
      (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t")
        (when (eq class 'label-and-instruction)
          (skip-chars-forward "^:")
          (forward-char)
          (skip-chars-forward " \t"))
        (let ((start (point))
              (end (progn (skip-chars-forward "^ \t\n")
                          (point))))
          (buffer-substring start end))))))

(defun 8051-classify-line ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond ((eolp) 'blank)
          ((looking-at "#include\\|org\\|\\.") 'directive)
          ((looking-at ";;;") 'top-comment)
          ((looking-at ";;") 'code-comment)
          ((looking-at ";") (if (bolp) 'block-comment 'side-comment))
          ((looking-at (concat 8051-label-regex "[ \t]*:"))
           (skip-chars-forward "^:")
           (forward-char)
           (skip-chars-forward " \t")
           (if (and (not (eolp)) (not (looking-at ";")))
               'label-and-instruction
             'label))
          ((looking-at (concat 8051-label-regex "[ \t]+equ\\>"))
           'equ)
          ;; XXX Other pseudo-ops?
          (t 'instruction))))

;; XXX This accidentally inserts a space after side comments.  This
;; wouldn't be such a problem, but I don't know why it's doing that,
;; and it makes typing code comments hard because you can't just hit
;; semicolon twice.
(defun 8051-indent-line ()
  (interactive)

  (let ((class (8051-classify-line))
        (eol (eolp)))
    (if (eq class 'blank)
        ;; Deal with completely blank lines
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to-column 8051-instruction-offset))
      (save-excursion
        (beginning-of-line)
        ;; Labels, top comments, block comments, and directives start
        ;; at column 0
        (when (memq class '(label label-and-instruction top-comment
                                  block-comment directive))
          (delete-horizontal-space))
        ;; Remove space between a label and the colon
        (when (memq class '(label label-and-instruction))
          (skip-chars-forward "^:")
          (delete-horizontal-space)
          (forward-char))
        ;; Indent equ
        (when (eq class 'equ)
          (delete-horizontal-space)
          (indent-to-column
           (- 8051-equ-offset
              (save-excursion
                (search-forward-regexp "\\<equ\\>")
                (- (current-column) 3)))))
        ;; For instructions and code comments, indent to the
        ;; instruction offset
        (when (memq class '(instruction label-and-instruction
                                        code-comment))
          (delete-horizontal-space)
          (indent-to-column 8051-instruction-offset))
        ;; Indent instruction arguments
        (when (memq class '(instruction label-and-instruction))
          (skip-chars-forward "^ \t\n")
          (skip-chars-forward " \t")
          ;; If there are arguments or the cursor was at the end of
          ;; the line, indent for arguments
          (if (or (not (eolp)) eol)
              (progn
                (delete-horizontal-space)
                ;; Make sure there's at least one space
                (insert " ")
                (indent-to-column 8051-argument-offset))
            ;; Otherwise, clean up the trailing spaces
            (delete-horizontal-space)))
        ;; Indent side comment if present
        (when (memq class '(side-comment instruction
                                         label-and-instruction
                                         directive
                                         equ))
          (skip-chars-forward "^;\n")
          (when (looking-at ";")
            (delete-horizontal-space)
            (indent-to-column comment-column)
            ;; Make sure there's a space following the semicolon
            (skip-chars-forward ";")
            (unless (looking-at " \\|\t")
              (insert " "))))))
    ;; XXX Deal better with keeping the cursor in a sane place
    (when eol
        (end-of-line))))

(defun 8051-in-comment-p ()
  (save-excursion
    ;; XXX This doesn't deal with semicolons in strings
    (skip-chars-backward "^;\n")
    (if (bobp)
        nil
      (= (char-before) ?\;))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special keys
;;;

(defun 8051-electric-colon ()
  (interactive)
  (insert last-command-char)
  (unless (8051-in-comment-p)
    (8051-indent-line)
    (newline)
    (8051-indent-line)))

(defun 8051-electric-semicolon-or-comma-or-pound ()
  (interactive)
  (insert last-command-char)
  (8051-indent-line))

(defun 8051-hungry-backspace ()
  (interactive)
  ;; Am I in a comment?
  (if (8051-in-comment-p)
      (backward-delete-char-untabify 1)
    ;; If I'm at the beginning of line, just backspace the newline
    (if (bolp)
        (backward-delete-char 1)
      ;; Am I preceeded by whitespace?  If so, delete it
      (if (memq (char-before) '(?  ?\t))
          (delete-horizontal-space t)
        ;; Behave like regular backspace
        (backward-delete-char-untabify 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template insertion
;;;

(defun 8051-insert-two-blank-lines ()
  ;; Ensure two blank lines
  (delete-region (point)
                 (save-excursion
                   (skip-chars-backward " \t\n")
                   (point)))
  (unless (bobp)
    (newline 3))
  ;; Cleanup whitespace
  (delete-horizontal-space))

(defun 8051-insert-function (func-name)
  (interactive "sFunction name: ")

  (8051-insert-two-blank-lines)
  ;; Cleanup whitespace
  (delete-horizontal-space)
  ;; Insert pre-point content
  (insert (concat ";;; " func-name "\n"
                  ";;; \n"
                  ";;; "))
  ;; Save the point
  (let ((pos (point-marker)))
    ;; Insert post-point content
    (insert (concat "\n"
                    ";;; \n"
                    ";;; Arguments: \n"
                    ";;; Returns: \n"
                    ";;; Modifies: \n"
                    func-name ":\n"))
    (8051-indent-line)
    ;; Return to the point
    (goto-char pos)))

(defun 8051-insert-top-header-lab (lab exercise title)
  (interactive "sLab: \nsExercise: \nsTitle: ")

  (flet ((lrpad (left right)
                (concat left
                        (make-string (- fill-column
                                        (length (concat left right)))
                                     ? )
                        right)))
    ;; Just incase the line is indented
    (delete-horizontal-space)
    ;; Appendix and name
    (insert (lrpad ";;; Appendix A" user-full-name))
    (newline)
    ;; Lab, exercise, and  title
    (insert (lrpad (concat ";;; Lab " lab ", Exercise " exercise)
                   title))
    (newline)))

(defun 8051-insert-top-header-final (appendix title)
  (interactive "sAppendix: \nsTitle: ")

  (flet ((lrpad (left right)
                (concat left
                        (make-string (- fill-column
                                        (length (concat left right)))
                                     ? )
                        right)))
    ;; Just incase the line is indented
    (delete-horizontal-space)
    (insert (lrpad (concat ";;; " user-full-name)
                   "Final Project"))
    (newline)
    ;; Appendix and title
    (insert (lrpad (concat ";;; Appendix " appendix)
                   title))
    (newline)
    ;; Description
    (insert ";;; ")
    (newline)
    (insert ";;; ")
    (save-excursion
      (newline)
      (newline))))
  

(defun 8051-insert-section (section)
  (interactive "sSection: ")

  (8051-insert-two-blank-lines)
  (insert (concat (make-string 66 ?\;) "\n"))
  (insert (concat ";;; " section "\n"))
  (insert (concat ";;; \n\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Label navigation
;;;

(defun 8051-find-label (label)
  (interactive (8051-find-label-interactive "Find label: "))
  (push-mark)
  (goto-char (point-min))
  (unless (re-search-forward (concat "^[ \t]*" label "[ \t]*:")
                             nil t)
    (message "Label \"%s\" not found" label)))

(defun 8051-find-label-interactive (prompt)
  (let* ((default (8051-find-label-get-default))
         (label (completing-read
                 (concat prompt "(default " default ") ")
                 (8051-label-completion-table)
                 nil nil nil '8051-label-history-list default)))
    (setq 8051-label-history-list
          (cons label 8051-label-history-list))
    (list label)))

(defun 8051-find-label-get-default ()
  (save-excursion
    (unless (memq (char-syntax (char-after)) '(?w ?_))
      (skip-syntax-backward "^w_"))
    (skip-syntax-backward "w_")
    (buffer-substring-no-properties
     (point)
     (save-excursion
       (skip-syntax-forward "w_")
       (point)))))

(defun 8051-label-completion-table ()
  (save-excursion
    (goto-char (point-min))
    (let ((table nil))
      (while (re-search-forward
              (concat "^[ \t]*\\(" 8051-label-regex "\\)[ \t]*:")
              nil t)
        (setq table (cons (list (match-string-no-properties 1)) table)))
      table)))
