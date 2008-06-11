;;; h-auto-mode-load.el --- automatically select major mode of .h files

;; Copyright (C) 2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    06-June-2008
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

;; To install h-auto-mode, add the following lines to your .emacs file:
;;   (add-to-list 'load-path "PATH CONTAINING h-auto-mode-load.el" t)
;;   (require 'h-auto-mode-load)
;; After this, the major mode of .h files will be automatically
;; selected.

;;; Code:

;; To update this file, evaluate the following form
;;   (let ((generated-autoload-file buffer-file-name)) (update-file-autoloads "h-auto-mode.el"))

(add-to-list 'auto-mode-alist (cons "\\.h\\'" #'h-auto-mode))


;;;### (autoloads (h-auto-mode) "h-auto-mode" "h-auto-mode.el" (18512
;;;;;;  4200))
;;; Generated autoloads from h-auto-mode.el

(autoload (quote h-auto-mode) "h-auto-mode" "\
Automatically select a major mode for the current header file.

This works by examining related files in the same directory as
the header file.  First, it sees if another file exists with the
same base name and a different extension and selects the major
mode based on that.  If no such file exists, it takes a vote from
all of the recognized files in the same directory as the header.

\(fn)" t nil)

;;;***

(provide 'h-auto-mode-load)
