;;; literal-mode.el --- Make keys do what they say on the tin

;; Copyright (C) 2015 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    06-Feb-2015
;; Version:    0.1

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

(define-minor-mode literal-mode
  "Toggle literal mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When literal mode is enabled, RET and TAB literally insert
newline and tab characters.  This is useful when pasting into
Emacs running in a terminal."

  :init-value nil
  :lighter " Literal"

  :keymap
  `((,(kbd "RET") . newline)
    (,(kbd "TAB") . self-insert-command)))

(provide 'literal-mode)
