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

(defun js-import-relative-one-by-one(path)
  "Transform relative to `projectile-project-root' PATH into relative to the current `buffer-file-name'"
  (if (s-matches? "^\\.+/" path)
      path
    (with-current-buffer helm-current-buffer
      (let* ((path (f-join (projectile-project-root) path))
             (relative-path (f-relative path (f-dirname buffer-file-name))))
        (unless (s-matches? "^\\.+/" relative-path)
          (setq relative-path (concat "./" relative-path)))
        relative-path))))

(defun js-import-filter-files-by-alias(files)
  "A function used to filter FILES based on buffer local `js-import-alias-name'. FILES supposed to be `projectile-current-project-files'"
  (let ((project-dir (projectile-project-root))
        (alias-path (js-import-get-alias-path js-import-alias-name)))
    (--filter (and (js-import-filter-pred it) (f-ancestor-of-p alias-path (f-join project-dir it))) files)))

(defun js-import-project-files-transformer(files &optional source)
  "Filter FILES by extension and one of the aliases, if present."
  (with-current-buffer helm-current-buffer
    (cond
     (js-import-alias-name
      (js-import-filter-files-by-alias files))
     (t (--filter (js-import-filter-pred it) files)))))

(defun js-import-project-files-filter-one-by-one(path)
  "Transform relative to the project root PATH into aliased one or relative to the current `buffer-file-name'"
  (with-current-buffer helm-current-buffer
    (cond
     (js-import-alias-name
      (js-import-real-path-to-alias (f-join (projectile-project-root) path) js-import-alias-name))
     (t (js-import-relative-one-by-one path)))))

(defun js-import-switch-to-relative(&optional cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-alias-name
        (progn
          (setq js-import-relative-transformer 'js-import-relative-one-by-one)
          (setq js-import-alias-name nil)
          (helm-refresh))
      (progn
        (setq js-import-relative-transformer nil)
        (setq js-import-alias-name (car (js-import-get-aliases)))
        (helm-refresh)))))

(defun js-import-switch-to-next-alias(&optional cand)
  "Switch to next alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (when js-import-alias-name
      (setq js-import-alias-name (or (car (cdr (member js-import-alias-name js-import-aliases)))
                                     (car js-import-aliases)))

      (helm-refresh))))

(defun js-import-switch-to-prev-alias(&optional cand)
  "Switch to previous alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (when js-import-alias-name
      (setq js-import-alias-name (or (car (cdr (member js-import-alias-name (reverse js-import-aliases))))
                                     (car (reverse js-import-aliases))))
      (helm-refresh))))


(defun js-import-select-file-action(file)
  (with-current-buffer helm-current-buffer
    (mapc
     'js-import-from-path
     (helm-marked-candidates))))


(defvar js-import-alias-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "Keymap for `js-import-alias-source' source.")

(put 'js-import-switch-to-relative 'helm-only t)
(put 'js-import-switch-to-prev-alias 'helm-only t)
(put 'js-import-switch-to-next-alias 'helm-only t)


(defun js-import-alias-header-name(project-name)
  "A function for display header name. Concatenates `js-import-alias-name' and PROJECT-NAME"
  (with-helm-current-buffer helm-current-buffer
                            (cond
                             (js-import-alias-name
                              (concat (propertize js-import-alias-name 'face 'font-lock-function-name-face) "\s" project-name "/" (lax-plist-get js-import-alias-map js-import-alias-name)))
                             (js-import-relative-transformer
                              (format "%s" project-name))
                             (t project-name))))

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."
  (setq candidate (js-import-path-to-real candidate))
  (let ((buf (get-buffer-create " *js-import persistent*")))
    (cl-flet ((preview (candidate)
                       (switch-to-buffer buf)
                       (setq inhibit-read-only t)
                       (erase-buffer)
                       (insert-file-contents candidate)
                       (let ((buffer-file-name candidate))
                         (set-auto-mode))
                       (font-lock-ensure)
                       (setq inhibit-read-only nil)))
      (if (and (helm-attr 'previewp)
               (string= candidate (helm-attr 'current-candidate)))
          (progn
            (kill-buffer buf)
            (helm-attrset 'previewp nil))
        (preview candidate)
        (helm-attrset 'previewp t)))
    (helm-attrset 'current-candidate candidate)))

(defclass js-import-alias-source (helm-source-sync)
  ((header-name :initform 'js-import-alias-header-name)
   (init :initform (lambda()
                     (unless js-import-aliases
                       (setq js-import-aliases (append (js-import-get-aliases))))
                     (unless (and js-import-alias-name (not js-import-relative-transformer))
                       (setq js-import-alias-name (car js-import-aliases)))))
   (candidates :initform 'projectile-current-project-files)
   (candidate-number-limit :initform 40)
   (persistent-action :initform 'js-import-ff-persistent-action)
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
