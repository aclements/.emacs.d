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
;; Insert function from template
;; Insert labels for functions (function name + . + label)
;; * Automatically fill in function name
;; * Make easy to insert label and to call on label
;; Summary doc in minibuffer? (ie, "mov dst, src - Foo")

(require 'cl)

(defvar 8051-instruction-offset 4
  "The column to indent instructions to.")

(defvar 8051-argument-offset 12
  "The column to indent arguments to.")

(defvar 8051-doc-delay 5
  "The number of seconds of idle time to wait for before displaying
documentation about the instruction on the current line")

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
  ;; Name   type     bit description
  '(("p0"   'io      t   "Port 0")
    ("sp"   'other   nil "Stack pointer")
    ("dpl"  'other   nil "Data pointer low")
    ("dph"  'other   nil "Data pointer high")
    ("pcon" 'control nil "Power control")
    ("tcon" 'control t   "Timer control")
    ("tmod" 'control nil "Timer mode")
    ("tl0"  'other   nil "Timer 0 low")
    ("tl1"  'other   nil "Timer 1 low")
    ("th0"  'other   nil "Timer 0 high")
    ("th1"  'other   nil "Timer 1 high")
    ("p1"   'io      t   "Port 1")
    ("scon" 'control t   "Serial control")
    ("sbuf" 'other   nil "Serial control")
    ("p2"   'io      t   "Port 2")
    ("ie"   'control t   "Interrupt enable")
    ("p3"   'io      t   "Port 3")
    ("ip"   'control t   "Interrupt priority")
    ("psw"  'control t   "Program status word")
    ("acc"  'other   t   "Accumulator")
    ("b"    'other   t   "B register")))

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

;; Test this (. in particular)
(defconst 8051-label-regex "[a-zA-Z_][a-zA-Z0-9_.]*")

(defconst 8051-font-lock-keywords
  (let ((keywords (regexp-opt (mapcar 'first 8051-opcodes)))
        (label 8051-label-regex)
        (avoid-label (concat (regexp-opt 8051-avoid-labels) "\\|"
                             "\\(?:"
                             (regexp-opt 8051-avoid-numbered-labels)
                             "\\)\\(?:\.[0-7]\\)?"))
        (bit-sfrs (mapcar 'first (remove-if-not 'third 8051-sfrs)))
        (non-bit-sfrs (mapcar 'first (remove-if 'third 8051-sfrs))))
    `( ;; Block comments
      ("^\;\;\;+[ \t]*\\(.*[\r\n]?\\)"
       (1 8051-top-comment-face prepend))
      ;; Keyword
      (,(concat "^[ \t]*\\(" keywords "\\)\\>")
       (1 font-lock-keyword-face))
      ;; Label followed by a keyword
      (,(concat "^" label "[ \t]*:[ \t]*\\(" keywords "\\)?\\>")
       (1 font-lock-keyword-face nil t))
      ;; Bad label
      (,(concat "^\\(" avoid-label "\\)[ \t]*:")
       (1 font-lock-warning-face))
      ;; Just a label
      (,(concat "^\\(" label "\\)[ \t]*:")
       (1 font-lock-function-name-face))
      ;; Include directives
      ("^#\\(include\\)\\( [^ \t]*\\)"
       (1 font-lock-builtin-face)
       (2 font-lock-string-face))
      ;; Pseudo ops
      ("\\<\\(ord\\|db\\|dw\\)\\>"
       (1 font-lock-builtin-face))
      ;; Built-in functions
      ("\\(\\<high\\>\\|\\<low\\>\\|\\+\\|-\\|\\*\\|/\\)"
       (1 font-lock-builtin-face))
      ;; Bad label literal
      (,(concat "\\(#\\(?:" avoid-label "\\)\\>\\)")
       (1 font-lock-warning-face))
      ;; Numeric literals
      (,(concat "\\(#[0-9]+[dD]?\\>\\|"
                "#[0-9][0-9A-Fa-f]*[hH]\\>\\|"
                "#[0-7]+[oO]\\>\\|"
                "#[01]+[bB]\\>\\)")
       (1 font-lock-constant-face))
      ;; Registers
      ("\\<\\(r[0-7]\\)\\>"
       (1 font-lock-variable-name-face))
      ;; Bit-addressable SFRs
      (,(concat "\\<\\(\\(?:" (regexp-opt bit-sfrs)
                "\\)\\(?:\.[0-7]\\)?\\)\\>")
       (1 font-lock-variable-name-face))
      ;; Non-bit-addressable SFRs
      (,(concat "\\<\\(" (regexp-opt non-bit-sfrs) "\\)\\>")
       (1 font-lock-variable-name-face))
      ;; XXX Address literals, equ
      )))

(defvar 8051-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?\" "\"" st)   ; XXX single quote?
    (modify-syntax-entry ?#  "w"  st)
    (modify-syntax-entry ?.  "w"  st)
    (modify-syntax-entry ?@  "w"  st)
    st))

(defvar 8051-mode-map
  (let ((mm (make-sparse-keymap)))
    (define-key mm ":" '8051-electric-colon)
    (define-key mm ";" '8051-electric-semicolon-or-comma)
    (define-key mm "," '8051-electric-semicolon-or-comma)
    (define-key mm "\C-m" 'reindent-then-newline-and-indent)
    (define-key mm [backspace] '8051-hungry-backspace)
    mm))

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

  ;; Auto-documentation
  (run-with-idle-timer 8051-doc-delay t (function 8051-show-doc))
  )

(defun 8051-show-doc ()
  (let ((op (8051-get-opcode)))
    (when (and op (not (string-equal op 8051-doc-showing)))
      (let ((doc (second (assoc op 8051-opcodes))))
        (if doc
            (message "%s - %s" op doc)
          (message "%s - Unknown op code" op))))
    (setq 8051-doc-showing op)))

(defun 8051-get-opcode ()
  (let ((class (8051-classify-line)))
    (when (memq class '(instruction label-and-instruction))
      (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t")
        (when (eq class 'label-and-instruction)
          (skip-chars-forward "^:")
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
          ((looking-at "#include\\|ord") 'directive)
          ((looking-at ";;;") 'top-comment)
          ((looking-at ";;") 'code-comment)
          ((looking-at ";") (if (bolp) 'block-comment 'side-comment))
          ((looking-at (concat 8051-label-regex "[ \t]*:"))
           (skip-chars-forward "^:")
           (skip-chars-forward " \t")
           (if (and (not (eolp)) (not (looking-at ";")))
               'label-and-instruction
             'label))
          ;; XXX Other pseudo-ops?
          (t 'instruction))))

(defun 8051-indent-line ()
  (interactive)

  (let ((class (8051-classify-line)))
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
        ;; For instructions and code comments, indent to the
        ;; instruction offset
        (when (memq class '(instruction label-and-instruction
                                        code-comment))
          (delete-horizontal-space)
          (indent-to-column 8051-instruction-offset))
        ;; Indent instruction arguments
        (when (memq class '(instruction label-and-instruction))
          (skip-chars-forward "^ \t\n")
          (delete-horizontal-space)
          (when (not (eolp))
            (insert " ")
            (indent-to-column 8051-argument-offset)))
        ;; Indent side comment if present
        (when (memq class '(side-comment instruction
                                         label-and-instruction directive))
          (skip-chars-forward "^;\n")
          (when (looking-at ";")
            (indent-to-column comment-column)
            ;; Make sure there's a space following the semicolon
            (skip-chars-forward ";")
            (unless (looking-at " \\|\t")
              (insert " "))))))))

(defun 8051-electric-colon ()
  (interactive)
  (insert last-command-char)
  (8051-indent-line)
  (newline)
  (8051-indent-line))

(defun 8051-electric-semicolon-or-comma ()
  (interactive)
  (insert last-command-char)
  (8051-indent-line))

(defun 8051-hungry-backspace ()
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (memq (char-before) '(?  ?\t))
        (delete-horizontal-space t)
      (backward-delete-char-untabify 1))))
