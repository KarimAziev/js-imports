;;; -*- lexical-binding: t -*-
;;; js-import-utils.el --- This is an Emacs Lisp file with Emacs Lisp code.

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
(require 'js-import-path)

(defun js-import-cut-names(str &optional regexp)
  (let ((reg (or regexp "export[ \t\n\s]\\(const[ \t\n\s]\\)?+\\({[^}]+\\([a-zA-Z0-9]*\\)\\}\\)\\|\\(export[ \t\n\s]+\\(default[ \t\s\n]\\)?\\(const\\|let\\|var\\|function[*]?\\|class\\)?+\\)")))
    (when (stringp str) (-remove 's-blank? (-map 's-trim (split-string str reg t))))))

(defun js-import-kill-thing-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun js-import-delete-import-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun import-backward-exist(path)
  (re-search-backward (concat "from +['\"]" path "['\"]") nil t))

(defun js-import-join-names(symbols)
  (when (and (listp symbols) (<= 1 (length symbols)))
    (s-join ", " symbols)))

(defun js-import-join-imports-names(default-name names)
  (let (parts '())
    (when (stringp names) (push (concat "{ " names" }") parts))
    (when (stringp default-name) (push default-name parts)
          (message "parts %s" parts))
    (s-join ", " (-non-nil parts))))

(defun goto-last-import()
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\| +\\)import[ \t\n]+" nil t)
    (re-search-forward "['\"]" nil t 2)
    (forward-line 1))
  (point)
  )

(defun js-import-get-import-positions(path)
  (save-excursion
    (let ((pos1 (point-min))
          (pos2 (goto-last-import)))
      (when (import-backward-exist path)
        (re-search-forward "['\"]+;?" nil t 2)

        (setq pos2 (point))
        (re-search-backward "\\(^\\| +\\)import[ \t\n]+" nil t)
        (setq pos1 (point)))
      (cons pos1 pos2)
      ))
  )

(defun js-import-narrow-to-import(path)
  (let* ((bounds (js-import-get-import-positions path))
         (pos1 (car bounds))
         (pos2 (cdr bounds)))
    (when (import-backward-exist path)
      (re-search-forward "['\"]+;?" nil t 2)

      (setq pos2 (point))
      (re-search-backward "\\(^\\| +\\)import[ \t\n]+" nil t)
      (setq pos1 (point))
      (narrow-to-region pos1 pos2))))

(defun js-import-delete-whole-import(display-path)
  (let* ((bounds (js-import-get-import-positions display-path))
         (pos1 (car bounds))
         (pos2 (cdr bounds)))
    (save-excursion
      (save-restriction
        (narrow-to-region pos1 pos2)
        (delete-region pos1 pos2)
        (widen)
        (join-line)))))

(defun js-import-delete-imported-name(fullname display-path)
  (save-excursion
    (save-restriction
      (js-import-narrow-to-import display-path)
      (let ((case-fold-search nil))
        (re-search-forward (concat fullname "[_\s\n,]+") nil t 1)
        (replace-match ""))
      (widen))))

(defun js-import-stringify (x)
  "Convert any object to string effeciently.
This is faster than `prin1-to-string' in many cases."
  (cl-typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun js-import-propertize (item &rest properties)
  "Same as `propertize' except that this avoids overriding
existed name with `nil' property."
  (cl-loop for (k v) on properties by 'cddr
           if v append (list k v) into props
           finally return
           (apply 'propertize
                  (js-import-stringify item)
                  props)))

(defun js-import-get-prop (item property)
  "Same as `get-text-property' except that this returns nil if
ITEM is not string."
  (if (stringp item)
      (get-text-property 0 property item)))

(defun js-import-find-exports (&optional content)
  (let* ((regexp "export[ \s\t\n]+\\(default\\|const\\|let\\|var\\|function[*]?\\|class\\)[\t\s\n]+\\([a-zZ-A0-9_,]+\\)?\\|export[ \s\t\n]+\\({[^}]+\\)")
         (all-matches (s-match-strings-all regexp (or content (buffer-string))))
         (match (s-match regexp content))
         (result (js-import-map-matches all-matches)))
    result))

(defun js-import-find-current-imports(display-path)
  (let* ((regexp-imports (concat "import[ \t\\n]+\\([a-zZ-A0-9_]+\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']" display-path "['\"']") )
         (matches (s-match regexp-imports (buffer-string)))
         (default-import-name (nth 1 matches))
         (named-exports (js-import-cut-names (nth 2 matches) ",\\|}\\|{"))
         (alist '()))
    (when named-exports
      (mapc (lambda (n) (push (cons n (. 4)) alist)) named-exports))

    (when default-import-name
      (push (cons default-import-name (. 1)) alist))
    alist))

(defun js-import-find-all-buffer-imports()
  (with-current-buffer (buffer-name)
    (let* ((content (buffer-substring-no-properties (point-min) (goto-last-import)))
           (buffer (buffer-name))
           (regexp "import[ \t\\n]+\\([a-zZ-A0-9_]+\\|*[\t\n\s]+as[\t\n\s][a-zZ-A_$]\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']\\([^['\"]*\\)")
           (all-matches (s-match-strings-all regexp content))
           (result (mapcar (lambda (sublist)
                             (let* ((reversed-list (reverse sublist))
                                    (path (car (last sublist)))
                                    (imports (js-import-cut-names (car sublist) "^import[ \t\n]\\|[ \t\n]from[ \t\n]+.*\\|,[ \s\t\n]*{" ))
                                    (imports-list (mapcar (lambda(str)
                                                            (cond
                                                             ((s-contains? "}" str) (--map (cons (s-trim it) 4) (split-string str ",\\|}\\|{" t)))
                                                             ((s-contains? "*" str) (cons str 16))

                                                             (t (cons str 1))))
                                                          imports)))

                               (cons path (-flatten imports-list))))
                           all-matches)))
      result)))

(cl-defun js-import-make-item (candidate
                               &key
                               display-path
                               type
                               face
                               real-path
                               cell
                               selection-face)
  "Utility function to make js-import item. See also

`js-import-propertize'."
  (let ((splitted-name (split-string candidate "[ \t\s]+as[ \t\s]+")))
    (js-import-propertize candidate
                          'real-name (nth 0 splitted-name)
                          'display-name candidate
                          'display-path display-path
                          'renamed-name (nth 1 splitted-name)
                          'real-path (or real-path (js-import-path-to-real display-path))
                          'js-import-face face
                          'selection-face selection-face
                          'type (or type (cdr cell)))))

(defun js-import-filter-pred(filename)
  (and (not (string-equal (s-replace (projectile-project-root) "" buffer-file-name) filename))
       (js-import-js-file? filename)
       (not (s-matches? js-import-unsaved-file-regexp filename))
       (not (s-matches? js-import-test-file-regexp filename))))

(defun js-import-get-project-files()
  (-filter 'js-import-filter-pred (projectile-current-project-files)))

(defun helm-js-imports-transformer (candidates)
  (cl-loop for (k . v) in candidates
           for parts = (split-string k)
           for disp = (mapconcat (lambda (x)
                                   (propertize
                                    x 'face
                                    (cl-loop for (p . f) in js-import-type-faces
                                             when (string-match p x) return f
                                             when (equal v 1) return 'font-lock-function-name-face
                                             finally return 'default)))
                                 parts "\s")
           collect disp))

(defun js-import-get-export-type(str)
  (cond
   ((s-equals? "*" str) 16)
   ((s-matches? "{[ \t\s\n]?default\\([ \\b$]\\|$\\)" str) 4)
   ((s-matches? "[ \t\s\n]default\\([ \\b$]\\|$\\)" str) 1)
   (t 4)))

(defun js-import-map-matches(matches &optional regexp-exclude)
  (let ((export-list '(("*" . 16)))
        (regexp (or regexp-exclude js-import-regexp-export-exclude-regexp)))
    (mapc (lambda (content) (let* ((item (s-trim (car content)))
                              (names (js-import-cut-names item regexp))
                              (export-type (js-import-get-export-type item))
                              (default-cons (when (equal export-type 1) (list (cons "default" (. export-type)))))
                              (consed-items (or default-cons (--map (cons it (. export-type))
                                                                    names))))
                         (setq export-list (append export-list consed-items))))
          matches)
    export-list))

(defun js-propose-import-name (path cell &optional default-name)
  (let* ((current-name (car cell))
         (export-type (cdr cell))
         (proposed-symbol (s-replace-regexp "[^a-zZ-A0-9$_]" "" (or default-name (s-join "" (reverse (--update-at 0 (capitalize it) (-take-last 2 (--remove (eq it "index") (split-string path "[/,.]")))))))))
         (prompt (format
                  (pcase export-type
                    (1 "Import default as (default: %s): ")
                    (4 "Symbols to import (default: %s): ")
                    (16 "Import all exports as (default: %s): "))
                  proposed-symbol))
         (read-symbols
          (read-string
           prompt
           nil nil proposed-symbol))

         (symbols (car (split-string (string-trim read-symbols))))
         (name (pcase export-type
                 (1 symbols)
                 (4 (format "%s as %s" current-name symbols))
                 (16 (format "%s as %s" current-name symbols)))))
    name))

(provide 'js-import-utils)
;;; js-import-utils.el ends here
