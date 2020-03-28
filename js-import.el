;;; -*- lexical-binding: t -*-
;;; js-import.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

(require 'helm)
(require 'projectile)
(require 'js-export)
(require 'js-import-from-path)
(require 'js-import-alias)
(require 'js-import-regexp)
(require 'js-import-dependency)

(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import)

(defvar js-import-file-names-sources
  '("project files" "node modules"))

(defvar js-import-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-import)
    (define-key map (kbd "C-c C-.") 'js-import-edit-buffer-imports)
    (define-key map (kbd "C-c C-d") 'js-import-dependency)
    (define-key map (kbd "C-c C-a") 'js-import-alias)
    (define-key map (kbd "C-c C-b") 'js-import-show-buffer-imports)
    (easy-menu-define js-import-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-import]
        ["Edit current buffer imports" js-import-edit-buffer-imports]
        ["Import alias" js-import-alias]
        ["Show import lines" js-import-show-buffer-imports]
        ["Import depenency" js-import-dependency]))
    map)
  "Keymap for Js-import commands")

(defvar js-import-buffer-source nil)


;;;###autoload
(define-minor-mode js-import-mode
  "js-import-mode is a minor mode for importing.
\\{js-import-mode-map}"
  :lighter " js-import"
  :group 'js-import
  :global nil
  :keymap js-import-command-map)



(defun js-import-dependencies-only-action ()
  (interactive)
  (let ((curr-source (helm-get-current-source)))
    (helm-set-source-filter (-remove-item (helm-attr 'name curr-source) js-import-file-names-sources))))



(defun js-import-reset-source-filter ()
  (interactive)
  (helm-set-source-filter nil))


(defvar js-import-source-imported nil "Current imports in buffer")
(defvar js-import-helm-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-M-n") 'js-import-dependencies-only-action)
    (define-key map (kbd "C-M-p") 'js-import-reset-source-filter)
    ;;(define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "keymap for a helm source.")


(define-key helm-map (kbd "C-M-n") 'js-import-dependencies-only-action)
(define-key helm-map (kbd "C-M-p") 'js-import-reset-source-filter)


;;;###autoload
(defun js-import ()
  "Init imports from your current project"
  (interactive)
  (save-excursion
    (let ((result (helm
                   :keymap js-import-helm-keymap
                   :sources (append (list
                                     (helm-make-source "project files" 'js-import-alias-source)
                                     (helm-make-source "node modules" 'js-import-dependency-source)))
                   :buffer "js import"
                   :prompt "Select path:"))))))


;;;###autoload
(defun js-import-show-buffer-imports()
  "Show current imports lines in buffer"
  (interactive)
  (helm
   :preselect (js-import-get-unreserved-word-at-point)
   :sources (helm-build-in-buffer-source "js imports in buffer"
                   :init (lambda ()
                           (with-current-buffer (helm-candidate-buffer 'global)

                             (insert (with-helm-current-buffer (buffer-substring-no-properties (point-min) (point-max))))
                             (goto-char (point-min))
                             (delete-non-matching-lines js-import-import-regexp)))
                   :get-line #'buffer-substring-no-properties)))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Find all imported symbols in current buffer and propose to jump or edit them"
  (interactive)
  (unless js-import-source-imported
    (setq js-import-source-imported (js-import-make-imports-sources)))

  (helm
   :preselect (js-import-get-unreserved-word-at-point)
   :sources js-import-source-imported))

(provide 'js-import)
;;; js-import.el ends here
