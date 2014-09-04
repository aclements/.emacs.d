;;; svn-msg-load.el --- major mode for Subversion commit messages

;; Copyright (C) 2005-2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    09-Jul-2005
;; Version:    0.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; To install this mode, add the following lines to your .emacs file:
;;   (add-to-list 'load-path "PATH CONTAINING svn-msg-load.el" t)
;;   (require 'svn-msg-load)
;; After this, svn-msg-mode will be used for svn commit messages.

;;; Code:

;; To update this file, evaluate the following form
;;   (let ((generated-autoload-file buffer-file-name)) (update-file-autoloads "svn-msg.el"))


;;;### (autoloads (svn-msg-mode) "svn-msg" "svn-msg.el" (18463 38630))
;;; Generated autoloads from svn-msg.el

(autoload (quote svn-msg-mode) "svn-msg" "\
Major mode for editing svn commit log messages.

This major mode provides support for editing Subversion commit
messages.  It enhances interface integration by hyperlinking file
status lines to diff's that show the changes being checked in.
It enhances editing by customizing paragraph filling,
automatically cleaning up whitespace, and providing font locking.
Finally, it understands Subversion's behavior when commits fail,
offering to restore the failed commit message and clean up the
stale file.

Features
* Modifies paragraph filling to not wrap the information block
  Subversion appends to commit messages
* Automatically removes extraneous trailing whitespace when
  saving so that logs don't contain extra vertical space
* Gratuitous color
* Previews and offers to restore and delete old commit messages
  left by failed commits
* File status lines are hyperlinked to provide easy access to
  diff's between the working copy and the base revision
* Disables backup files and the saveplace package when editing
  commit messages and inhibits flyspell in the file status block

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (cons "svn-commit\\(\\.[0-9]+\\)?\\.tmp" (function svn-msg-mode)))

;;;***

(provide 'svn-msg-load)
