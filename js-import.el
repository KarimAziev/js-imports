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
(require 'js-import-relative)
(require 'js-import-alias)
(require 'js-import-dependency)

(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import
  :group 'languages)

(defvar js-import-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-i") 'js-import-helm)
    (define-key map (kbd "C-c M-e") 'js-import-edit-buffer-imports)
    (define-key map (kbd "C-c M-d") 'js-import-dependency)
    (define-key map (kbd "C-c M-a") 'js-import-alias)
    (define-key map (kbd "C-c M-r") 'js-import-relative)
    (easy-menu-define js-import-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-import-helm]
        ["Edit current buffer imports" js-import-edit-buffer-imports]
        ["Import alias" js-import-alias]
        ["Import relative" js-import-relative]
        ["Import depenency" js-import-dependency]))
    map)
  "Keymap for Js-import commands")

;;;###autoload
(define-minor-mode js-import-mode
  "js-import-mode is a minor mode for importing.
\\{js-import-mode-map}"
  :lighter " js-import"
  :group 'js-import
  :global nil
  :keymap js-import-command-map)

;;;###autoload
(defun js-import-edit-buffer-imports()
  (interactive)
  (helm :sources (js-import-make-imports-sources)))

;;;###autoload
(defun js-import-helm ()
  "Init imports from your current project"
  (interactive)
  (helm :sources (append (js-import-alias-make-sources) (list
                                                           (helm-make-source "node modules" 'js-import-dependency-source)
                                                           (helm-make-source (format "relative exports for %s" (buffer-name)) 'js-import-relative-source)))
          :allow-nest nil
          :prompt "Select path:"
          :buffer "js imports"))

(provide 'js-import)
;;; js-import.el ends here
