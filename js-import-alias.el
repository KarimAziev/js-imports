;;; -*- lexical-binding: t -*-
;;; js-import-alias.el --- This is an Emacs Lisp file with Emacs Lisp code.

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
(require 'subr-x)
(require 'projectile)
(require 'js-import-utils)
(require 'js-import-insert)
(require 'js-import-from-path)

(defvar js-import-alias-candidates nil)
(make-variable-buffer-local 'js-import-alias-candidates)
(defvar js-import-alias-name nil)
(make-variable-buffer-local 'js-import-alias-name)


(defun js-import-alias-make-alias-source(alias)
  (helm-build-sync-source (format "Alias import %s" alias)
    :before-init-hook (lambda()
            (setq js-import-alias-name alias))
    :candidates (js-import-get-alias-files alias)
    :nomark nil
    :group 'js-import
    :action '(("Show exported symbols" . js-import-select-alias-file-action))))

(defun js-import-select-alias-file(candidate)
  "doc"
  (let* ((real-path (f-join (projectile-project-root) candidate)))
    (js-import-from-path (js-import-real-path-to-alias real-path js-import-alias-name) real-path)))

(defun js-import-select-alias-file-action(file)
  (mapc
   'js-import-select-alias-file
   (helm-marked-candidates)))

(defun js-import-alias-make-sources()
  (let ((pl js-import-alias-map)
        (vals ()))
    (while pl
      (push (js-import-alias-make-alias-source (car pl)) vals)
      (setq pl (cddr pl)))
    (nreverse vals)))

;;;###autoload
(defun js-import-alias ()
  "Import from your current project with alias prefix"
  (interactive)
  (save-excursion (with-current-buffer (buffer-name)
                    (helm :sources (js-import-alias-make-sources)))))

(provide 'js-import-alias)
;;; js-import-alias.el ends here
