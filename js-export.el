;;; -*- lexical-binding: t -*-
;;; js-export.el --- This is an Emacs Lisp file with Emacs Lisp code.

;; Copyright (C) 2020 Karim Aziiev

;; Author: Karim Aziiev <karim.aziev@gmail.com>

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

;; Hello world!

;;; Code:

(require 'f)
(require 'json)
(require 'subr-x)
(require 'js-import-from-path)
(require 'js-import-utils)

(defun js-import-show-exports-from-path-at-point()
  (interactive)
  (let* (($inputStr (js-import-get-word-at-point))
         (real-path (js-import-path-to-real $inputStr (f-dirname buffer-file-name))))
    (js-import-from-path $inputStr real-path)))

(defun js-import-open-file-at-point()
  (interactive)
  (when-let (($inputStr (js-import-get-word-at-point)))
    (js-import-path-to-real $inputStr (f-dirname buffer-file-name))))



(provide 'js-export)
;;; js-export.el ends here
