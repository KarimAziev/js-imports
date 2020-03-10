;;; -*- lexical-binding: t -*-
;;; js-import-dependency.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

;; Helm version of js import

;;; Code:

(require 'helm)
(require 'js-import-utils)
(require 'js-import-from-path)

(defclass js-import-dependency-source (helm-source-sync)
  ((candidates :initform 'js-import-get-all-dependencies)
   (nomark :initform t)
   (candidate-number-limit  :initform 15)
   (action :initform 'js-import-dependency)
   (group :initform 'js-import)))

;;;###autoload
(defun js-import-dependency (&optional dependency)
  "Import from node modules"
  (interactive)
  (let* ((module (or dependency (completing-read "Select from node module: " (js-import-get-all-dependencies))))
         (path (js-import-find-node-module-index-path module)))
    (js-import-from-path path module)))

(provide 'js-import-dependency)
;;; js-import-dependency.el ends here
