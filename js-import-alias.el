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
(require 'js-import-path)
(require 'js-import-utils)
(require 'js-import-insert)
(require 'js-import-from-path)

(defun js-import-alias-make-alias-source(alias)
  (let* ((project-dir (projectile-project-root))
         (alias-path (js-import-get-alias-path alias))
         (files (--filter (f-ancestor-of-p alias-path (f-join project-dir it)) (js-import-get-project-files)))
         (slashed-alias (js-import-maybe-slash-alias alias)))

    (message "alias\n %s" alias)
    (helm-build-sync-source (format "Alias import %s" alias)
      :candidates (--map (js-import-real-path-to-alias (f-join project-dir it) alias) files)
      :nomark t
      :candidate-number-limit 25
      :group 'js-import
      :action (lambda(candidate)
                (let ((real-path (f-join alias-path (replace-regexp-in-string (concat "^" slashed-alias) "" candidate)))
                      (alias-path (replace-regexp-in-string "/index$" "" (f-no-ext candidate))))
                  (js-import-from-path real-path alias-path))))))

(defun js-import-alias-make-sources()
  (message "js-import-alias-map\n %s" js-import-alias-map)
  (let ((pl js-import-alias-map)
        (vals  ()))
    (while pl
      (push (js-import-alias-make-alias-source (car pl)) vals)
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun js-import-alias-candidates (&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (js-import-get-project-files)))

;;;###autoload
(defun js-import-alias ()
  "Import from your current project with alias prefix"
  (interactive)
  (with-current-buffer (buffer-name)
    (helm :sources (js-import-alias-make-sources)
          :buffer "js imports from alias")))

(provide 'js-import-alias)
;;; js-import-alias.el ends here
