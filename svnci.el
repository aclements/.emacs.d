;;; svnci.el --- interactive Subversion commit and status/diff/add

;; Copyright (C) 2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    02-May-2008
;; Version:    0.1

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
;;   (add-to-list 'load-path "PATH CONTAINING svnci.el" t)
;;   (load "svnci")
;; After this, the svnci and svnci-this commands will be available.

;; See svnci-core.el for documentation.

;;; Code:

;; To update this file, evaluate the following form
;;   (let ((generated-autoload-file buffer-file-name)) (update-file-autoloads "svnci-core.el"))


;;;### (autoloads (svnci-this svnci) "svnci-core" "svnci-core.el"
;;;;;;  (18463 22975))
;;; Generated autoloads from svnci-core.el

(autoload (quote svnci) "svnci-core" "\
Start an svn commit process.

This command constructs and visits a new svn commit message.
This commit message will contain status lines for all of the
files under the current directory, including unversioned files.
Initially, only the files that 'svn commit' would include will be
marked for inclusion in the commit.  If invoked with a prefix
argument (\\[universal-argument]), then this will prompt for which file or path to
include initially.

\\<svnci-stat-line-map>Files can be marked for inclusion or exclusion from the commit by
moving point to a file status line and pressing \\[svnci-toggle-commit].

\\<svnci-mode-map>Once ready to commit, use \\[svnci-commit] to commit the selected
files with the given commit message.

If `svn-msg-offer-to-restore' is non-nil, this first checks for
any existing svnci messages (for example, from failed commits)
and offers to restore them instead of constructing a new
message.

\(fn &optional PATHS NON-RECURSIVE)" t nil)

(autoload (quote svnci-this) "svnci-core" "\
Like `svnci', but include only the currently visited file.

As an added bonus, if the current file is not under version
control, this first prompts to add the file.

\(fn)" t nil)

;;;***

(provide 'svnci)
