;;; -*- lexical-binding: t -*-
;;; js-import-misc.el --- This is an Emacs Lisp file with Emacs Lisp code.

;; Copyright (C) 2020 KarimAziev

;; Author: KarimAziev <karim.aziev@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides some useful utils for js files

;;; Code:

(require 'f)
(require 'js-import-regexp)
(require 'js-import-utils)
(require 'js-import-path)
(require 'js-import-insert)
(require 'js-import-from-path)


(defun js-import-copy-current-path-as-alias ()
  "Copy current buffer path as alias"
  (interactive)
  (let* ((alias (completing-read "Select alias:\s" js-import-alias-map nil t))
         (path (js-import-real-path-to-alias (buffer-file-name) alias)))
    (kill-new path)
    (message "Copied buffer alias path %s" path)))

(defun js-import-copy-current-dir-path()
  "Copy short path (~/) of current buffer's directory"
  (interactive)
  (let ((path (f-short (f-dirname buffer-file-name))))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun js-import-copy-real-buffer-path()
  "Copy short path (~/) of current buffer"
  (interactive)
  (let ((path (f-short buffer-file-name)))
    (kill-new path)
    (message "Copied %s" path)
    path))


(provide 'js-import-misc)
;;; js-import-misc.el ends here
