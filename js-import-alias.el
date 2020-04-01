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

(defvar js-import-alias-name nil)
(make-variable-buffer-local 'js-import-alias-name)
(defvar js-import-aliases nil)
(make-variable-buffer-local 'js-import-aliases)
(defvar js-import-relative-transformer nil)
(make-variable-buffer-local 'js-import-relative-transformer)

(defun js-import-relative-one-by-one(cand)
  (if (s-matches? "^\\.+/" cand)
      cand
    (with-current-buffer helm-current-buffer
      (let* ((path (f-join (projectile-project-root) cand))
             (relative-path (f-relative path (f-dirname buffer-file-name))))
        (unless (s-matches? "^\\.+/" relative-path)
          (setq relative-path (concat "./" relative-path)))
        relative-path))))

(defun js-import-filter-files-by-alias(candidates)
  "doc"
  (let ((project-dir (projectile-project-root))
        (alias-path (js-import-get-alias-path js-import-alias-name)))
    (--filter (and (js-import-filter-pred it) (f-ancestor-of-p alias-path (f-join project-dir it))) candidates)))

(defun js-import-project-files-transformer(candidates &optional source)
  "doc"
  (with-current-buffer helm-current-buffer
    (cond
     (js-import-alias-name
      (js-import-filter-files-by-alias candidates))
     (t (--filter (js-import-filter-pred it) candidates))
     )))

(defun js-import-project-files-filter-one-by-one(cand)
  "doc"
  (with-current-buffer helm-current-buffer
    (cond
     (js-import-alias-name
      (js-import-real-path-to-alias (f-join (projectile-project-root) cand) js-import-alias-name))
     (t (js-import-relative-one-by-one cand)))))

(defun js-import-switch-to-relative(&optional cand)
  (interactive)
  "Toggle relative displaying files"
  (with-current-buffer helm-current-buffer
    (let ((source (helm-get-current-source)))
      (if js-import-alias-name
          (progn
            (setq js-import-relative-transformer 'js-import-relative-one-by-one)
            (setq js-import-alias-name nil)
            (helm-refresh))
        (progn
          (setq js-import-relative-transformer nil)
          (setq js-import-alias-name (car (js-import-get-aliases)))
          (helm-refresh))
        ))))

(defun js-import-switch-to-next-alias(&optional cand)
  (interactive)
  "Switch to next alias"
  (with-current-buffer helm-current-buffer
    (let ((source (helm-get-current-source)))
      (when js-import-alias-name
        (progn
          (setq js-import-alias-name (or (car (cdr (member js-import-alias-name js-import-aliases)))
                                         (car js-import-aliases)))

          (helm-refresh))))))

(defun js-import-switch-to-prev-alias(&optional cand)
  (interactive)
  "Switch to previous alias"
  (with-current-buffer helm-current-buffer
    (let ((source (helm-get-current-source)))
      (when js-import-alias-name
        (progn
          (setq js-import-alias-name (or (car (cdr (member js-import-alias-name (reverse js-import-aliases))))
                                         (car (reverse js-import-aliases))))

          (helm-refresh))))))

(defvar js-import-alias-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "keymap for a helm source.")

(put 'js-import-switch-to-relative 'helm-only t)
(put 'js-import-switch-to-prev-alias 'helm-only t)
(put 'js-import-switch-to-next-alias 'helm-only t)

(defun js-import-select-file-action(file)
  (with-current-buffer helm-current-buffer
    (mapc
     'js-import-from-path
     (helm-marked-candidates))))


(defclass js-import-alias-source (helm-source-sync)
  ((init :initform (lambda()
                     (unless js-import-aliases
                       (setq js-import-aliases (append (js-import-get-aliases))))
                     (unless (and js-import-alias-name (not js-import-relative-transformer))
                       (setq js-import-alias-name (car js-import-aliases)))))
   (candidates :initform 'projectile-current-project-files)
   (candidate-number-limit :initform 40)
   (candidate-transformer :initform 'js-import-project-files-transformer)
   (filter-one-by-one :initform 'js-import-project-files-filter-one-by-one)
   (nomark :initform nil)
   (keymap :initform js-import-alias-keymap)
   (action :initform `(("Show exported symbols" . js-import-select-file-action)
                       (,(substitute-command-keys "Switch to next alias \\<js-import-alias-keymap>`\\[js-import-switch-to-next-alias]'")
                        . js-import-switch-to-next-alias)
                       (,(substitute-command-keys "Switch to previous alias \\<js-import-alias-keymap>`\\[js-import-switch-to-prev-alias]'")
                        . js-import-switch-to-prev-alias)
                       (,(substitute-command-keys "Switch to relative \\<js-import-alias-keymap>`\\[js-import-switch-to-relative]'")
                        . js-import-switch-to-relative)))
   (group :initform 'js-import)))


;;;###autoload
(defun js-import-alias ()
  "Import from your current project with alias prefix"
  (interactive)
  (save-excursion
    (helm
     :sources (helm-make-source "js import project files" 'js-import-alias-source))))

(provide 'js-import-alias)
;;; js-import-alias.el ends here
