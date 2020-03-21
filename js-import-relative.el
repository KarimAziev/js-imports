;;; -*- lexical-binding: t -*-
;;; js-import-relative.el --- This is an Emacs Lisp file with Emacs Lisp code.

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
(require 'f)
(require 'json)
(require 'subr-x)
(require 'projectile)
(require 'js-import-utils)
(require 'js-import-from-path)

(defun js-import-sort-relative(candidates)
  (-sort (lambda (path1 path2)
           (let* ((a (s-count-matches "\\.+/" (car path1)))
                  (b (s-count-matches "\\.+/" (car path2)))
                  (result (< a b)))
             result))
         candidates))

(defun js-import-helm-relative-sort-candidate-transformer (candidates)
  (js-import-sort-relative (js-import-helm-relative-ff-transformer candidates)))

(defun js-import-helm-relative-ff-transformer(candidates &optional source)
  (with-current-buffer helm-current-buffer
    (let* ((buffer-dir (f-dirname buffer-file-name))
           (project-name (projectile-project-root)))
      (-map (lambda (it)
              (let* ((path (f-join project-name (format "%s" it)))
                     (relative-path (f-relative path buffer-dir)))
                 (unless (s-matches? "^\\." relative-path)
                         (setq relative-path (concat "./" relative-path)))
                relative-path))

            candidates))))

(defun js-import-relative-candidates (&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (js-import-get-project-files)))

(defclass js-import-relative-source (helm-source-sync)
  ((candidates :initform 'js-import-relative-candidates)
   (candidate-transformer :initform 'js-import-helm-relative-ff-transformer)
   (nomark :initform t)
   (candidate-number-limit  :initform 30)
   (action :initform 'js-import-relative-file-action)
   (group :initform 'js-import)))

(defun js-import-relative-file-action(candidate)
  (let* ((buffer-dir (f-dirname buffer-file-name))
         (expanded-path (f-join buffer-dir candidate))
         (relative-path (js-import-normalize-path candidate)))
    (js-import-from-path relative-path expanded-path)))

;;;###autoload
(defun js-import-relative ()
  "Import from your current project with path relative to current buffer"
  (interactive)
  (save-excursion
    (helm
     :sources (helm-make-source "relative import" 'js-import-relative-source))))

(provide 'js-import-relative)
;;; js-import-relative.el ends here
