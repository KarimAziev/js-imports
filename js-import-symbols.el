;;; -*- lexical-binding: t -*-
;;; js-import-symbols.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

;; Hello world!

;;; Code:

(require 'helm)
(require 'cl-lib)
(require 'js-import-regexp)
(require 'js-import-utils)
(require 'dash)
(require 'subr-x)
(require 's)

(defcustom js-import-symbols-faces
  '(("^\\(type\\|interface\\)$" . font-lock-type-face)
    ("^\\(default\\|function\\|function*\\)$" . font-lock-function-name-face)
    ("^\\(export\\|const\\|let\\|var\\|class\\)$" . font-lock-variable-name-face))
  "Faces in a list of cons cells for showing symbols types in js-symbols-menu"
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

(defvar js-import-regexp-symbols "\\(^\\| +\\)\\(export[\s\t\n]+\\)?\\(default[ \s\t\n]+\\)?\\(const\\|let\\|var\\|type\\|interface\\|function[*]?\\|class\\)[\t\s\n]+\\([a-zZ-A0-9_$,]+\\)")
(defvar js-import-symbols-actions
  (helm-make-actions
   "Go" 'js-import-jump-in-buffer))

(defvar js-import-symbols-in-buffer nil)
(make-variable-buffer-local 'js-import-symbols-in-buffer)
(defvar js-import-symbols-in-buffer-tick nil)
(make-variable-buffer-local 'js-import-symbols-in-buffer-tick)


(defun js-import-symbols-candidates-in-buffer(&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-import-symbols-in-buffer-tick tick)
          js-import-symbols-in-buffer
        (progn
          (setq js-import-symbols-in-buffer-tick tick)
          (setq js-import-symbols-in-buffer (js-import-extract-symbols buffer-file-name)))))))


(defun js-import-symbols-real-to-display(item)
  "A function for displaying ITEM's props - 'display-part and 'real-name props."
  (let ((type-part (mapconcat (lambda (x)
                                (propertize
                                 x 'face
                                 (cl-loop for (p . f) in js-import-symbols-faces
                                          when (string-match p x) return f
                                          finally return x)))
                              (split-string (js-import-get-prop item 'display-part))
                              "\s")))
    (concat type-part " / " (js-import-get-prop item 'real-name))))

(defun js-import-jump-in-buffer(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker"
  (let ((m (js-import-get-prop item 'marker)))
    (goto-char m)
    (recenter-top-bottom)
    (helm-highlight-current-line)
    item))

(defun js-import-extract-symbols(filename)
  "A function which extract js symbols from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char 0)
    (save-excursion
      (js-import-extract-vars filename))))


(defun js-import-extract-vars(&optional real-path)
  "A function extracts variables, types, declarations and returns results in propertized list."
  (let (symbols)
    (while (search-forward-regexp js-import-regexp-symbols nil t 1)
      (when (not (js-import-inside-comment?))
        (let* ((full-match (match-string 0))
               (export-type-1 (match-string 1))
               (export-type-default (match-string 2))
               (var-type (match-string 4))
               (real-name (match-string 5))
               (marker (car (match-data 5)))
               (display-part (s-trim (s-join "\s" (-filter 'stringp (list export-type-1 export-type-default var-type))))))

          (push (js-import-make-index-item (s-trim full-match)
                                           :marker marker
                                           :real-name real-name
                                           :display-part display-part
                                           :export-type (or export-type-default export-type-1)
                                           :var-type var-type
                                           :real-path real-path)
                symbols))))
    (reverse symbols)))

(cl-defun js-import-make-index-item (candidate
                                     &key
                                     display-path
                                     display-name
                                     display-part
                                     export-type
                                     import-type
                                     var-type
                                     real-path
                                     real-name
                                     marker)
  "Utility function to propertize js symbol. See also
`js-import-propertize'."
  (setq candidate (js-import-strip-text-props candidate))
  (js-import-propertize candidate
                        'real-name real-name
                        'display-name (or display-name candidate)
                        'display-path display-path
                        'real-path real-path
                        'display-part display-part
                        'export-type export-type
                        'import-type import-type
                        'var-type var-type
                        'marker marker))

(defun js-import-inside-comment? ()
  "Returns value of comment character in syntax table's or nil otherwise"
  (interactive)
  (nth 4 (syntax-ppss)))


(provide 'js-import-symbols)
;;; js-import-symbols.el ends here
