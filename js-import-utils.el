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


(require 'helm)
(require 'projectile)
(require 'f)
(require 'json)
(require 'subr-x)
(require 's)
(require 'js-import-regexp)

(defcustom js-import-alias-map '("" "src")
  "List of pairs (alias and path)"
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-type-faces
  '(("^\\(as\\)$" . font-lock-type-face)
    ("^\\(*\\)$" . font-lock-type-face)
    ("^\\(default\\)$" . font-lock-variable-name-face)
    ("^\\(Function\\|Functions\\|Defuns\\)$" . font-lock-function-name-face)
    ("^\\(Types\\|Provides\\|Requires\\|Classes\\|Class\\|Includes\\|Imports\\|Misc\\)$" . font-lock-type-face))
  "Faces for showing type in helm-js-import-menu.
This is a list of cons cells.  The cdr of each cell is a face to be used,
and it can also just be like \\='(:foreground \"yellow\").
Each car is a regexp match pattern of the imenu type string."
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

(defun js-import-cut-names(str reg)
  (when (stringp str) (-remove 's-blank? (-map 's-trim (split-string str reg t)))))

(defun js-import-kill-thing-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun js-import-import-backward-exist?(path)
  (re-search-backward (concat "from +['\"]" path "['\"]") nil t))

(defun js-import-join-names(symbols)
  (when (and (listp symbols) (<= 1 (length symbols)))
    (s-join ", " symbols)))

(defun js-import-join-imports-names(default-name names)
  (let (parts '())
    (when (stringp names) (push (concat "{ " names" }") parts))
    (when (stringp default-name) (push default-name parts))
    (s-join ", " (-non-nil parts))))

(defun js-import-goto-last-import()
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\| +\\)import[ \t\n]+" nil t)
    (re-search-forward "['\"]" nil t 2)
    (forward-line 1))
  (point))

(defun js-import-get-import-positions(path)
  (save-excursion
    (let ((pos1 (point-min))
          (pos2 (js-import-goto-last-import)))
      (when (js-import-import-backward-exist? path)
        (re-search-forward "['\"]+;?" nil t 2)

        (setq pos2 (point))
        (re-search-backward "\\(^\\| +\\)import[ \t\n]+" nil t)
        (setq pos1 (point)))
      (cons pos1 pos2))))

(defun js-import-narrow-to-import(path)
  (let* ((bounds (js-import-get-import-positions path))
         (pos1 (car bounds))
         (pos2 (cdr bounds)))
    (when (js-import-import-backward-exist? path)
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
  (let ((all-matches (s-match-strings-all js-import-export-regexp (or content (buffer-string)))))
    (js-import-map-matches all-matches js-import-regexp-export-exclude-regexp)))

(defun js-import-find-current-imports(display-path)
  (let* ((matches (s-match (js-import-make-import-regexp-from-path display-path) (buffer-string)))
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
    (let* ((content (buffer-substring-no-properties (point-min) (js-import-goto-last-import)))
           (buffer (buffer-name))
           (all-matches (s-match-strings-all js-import-import-regexp content))
           (result (mapcar (lambda (sublist)
                             (let* ((reversed-list (reverse sublist))
                                    (path (car (last sublist)))
                                    (imports (js-import-cut-names (car sublist) js-import-import-regexp-exclude))
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
  "Get js and ts files from current project"
  (-filter 'js-import-filter-pred (projectile-current-project-files)))

(defun js-import-get-alias-files(alias)
  "Get filtered by alias path js and ts files from current project"
  (let ((project-dir (projectile-project-root))
        (alias-path (js-import-get-alias-path alias)))
    (--filter (and (js-import-filter-pred it) (f-ancestor-of-p alias-path (f-join project-dir it))) (js-import-get-project-files))))

(defun js-import-imports-transformer (candidates)
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
                    (4 "Import { (default: %s) }: ")
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

(defun js-import-get-aliases ()
  "Get list of aliases"
  (let ((pl js-import-alias-map)
        (vals  ()))
    (while pl
      (push (car pl) vals)
      (setq pl  (cddr pl)))
    (nreverse vals)))

(defun js-import-normalize-path(path)
  (funcall (-compose
            'js-import-remove-path-index
            'f-no-ext
            'js-import-remove-double-slashes)
           path))

(defun js-import-maybe-slash-alias(alias)
  (if (or (s-blank? alias) (s-matches? ".*\\/$" alias)) alias (concat alias "/")))

(defun js-import-real-path-to-alias(real-path alias)
  (let ((alias-path (js-import-get-alias-path alias))
        (slashed-alias (js-import-maybe-slash-alias alias)))
    (replace-regexp-in-string (concat "^" alias-path) slashed-alias real-path)))

(defun js-import-get-alias-path(alias)
  (f-slash (f-join (projectile-project-root) (lax-plist-get js-import-alias-map alias))))

(defun js-import-normalize-relative-path (path)
  (funcall (-compose (lambda (str)
                       (cond ((s-blank? str) "./index")
                             ((not (s-matches? "\\.+/" str))
                              (concat "./" str))
                             (t str)))
                     'js-import-normalize-path)
           path))

(defun js-import-remove-path-index (path)
  (let ((cutted-path (replace-regexp-in-string "[/]*index\\(.jsx?$\\|.tsx?$\\)" "" path)))
    (if (s-blank? cutted-path)
        path
      cutted-path)))

(defun js-import-remove-double-slashes (path)
  (replace-regexp-in-string "//"  "/" path))

(defun js-import-expand-path(candidate)
  (f-short (f-expand candidate (projectile-project-root))))

(defun js-import-get-node-modules-path ()
  "Return the path to node-modules."
  (f-join (projectile-project-root) "node_modules"))

(defun js-import-expand-node-modules(module)
  (let ((node-modules-path (f-join (js-import-get-node-modules-path) module)))
    node-modules-path))

(defun js-import-js-file? (filename)
  "Check if FILENAME ends with either .js or .jsx."
  (or (string-suffix-p ".js" filename t)
      (string-suffix-p ".jsx" filename t)
      (string-suffix-p ".ts" filename t)
      (string-suffix-p ".tsx" filename t)))

(defun js-import-get-package-json-path ()
  "Return the path to package.json."
  (f-join (projectile-project-root) "package.json"))

(defun js-import-find-node-module-index-path(module)
  (let ((path (js-import-expand-node-modules module)))
    (cond ((f-exists? (f-join path "es" "index.js"))
           (setq path (f-join path "es/index.js")))
          ((f-exists? (f-join path "src/index.js"))
           (setq path (f-join path "src/index.js")))
          ((f-exists? (f-join path "lib/index.js"))
           (setq path (f-join path "lib/index.js")))
          ((f-exists? (f-join path "es" "index.ts"))
           (setq path (f-join path "es/index.ts")))
          ((f-exists? (f-join path "src/index.ts"))
           (setq path (f-join path "src/index.ts")))
          ((f-exists? (f-join path "lib/index.d.ts"))
           (setq path (f-join path "lib/index.d.ts")))
          ((f-exists? (f-join path "lib/index.d.ts"))
           (setq path (f-join path "lib/index.d.ts")))
          ((f-exists? (f-join path "index.ts"))
           (setq path (f-join path "index.ts")))
          ((f-exists? (f-join path "index.d.ts"))
           (setq path (f-join path "index.d.ts")))
          (t (setq path (f-join path "index.js"))))))

(defun js-import-get-all-dependencies(&optional $package-json-path)
  "Return dependencies, devDependencies and peerDependencies from package-json-path"
  (let ((sections '("dependencies" "peerDependencies" "devDependencies")))
    (--reduce-r-from (append acc (js-import-get-dependencies $package-json-path it)) '() sections)))

(defun js-import-get-dependencies (&optional $package-json-path $section)
  "Return dependencies list from package-json-path in dependencies, devDependencies and peerDependencies sections."
  (when-let ((dependencies-hash (js-import-dependencies-hash $package-json-path $section)))
    (hash-table-keys dependencies-hash)))

(defun js-import-dependencies-hash (&optional $package-json-path $section)
  "Return a dependency hash fetched from package-json-path in section.  If file not found, return nil."
  (let ((package-json-path (or $package-json-path (js-import-get-package-json-path)))
        (section (or $section "dependencies"))
        (json-object-type 'hash-table))
    (when-let ((package-json-content (condition-case nil
                                         (f-read-text package-json-path 'utf-8) (error nil)))
               (dependencies-hash (condition-case nil
                                      (gethash section (json-read-from-string package-json-content)) (error nil))))
      dependencies-hash)))

(defun js-import-is-dependency? (display-path)
  "Check if path is dependency"
  (let ((dependencies-hash (js-import-dependencies-hash))
        (path (car (split-string display-path "/"))))
    (if dependencies-hash
        (gethash path dependencies-hash)
      nil)))




(defun js-import-alias-path-to-real(path)
  (let* ((aliases (js-import-get-aliases))
         (project-dir (projectile-project-root))
         (real-path nil))

    (mapc (lambda(alias)
            (let* ((alias-regexp (concat "^" alias "/"))
                   (alias-path (js-import-get-alias-path alias))
                   (joined-path (f-join alias-path (s-replace-regexp alias-regexp "" path))))

              (when (s-matches? (js-import-remove-double-slashes (concat "^" alias "/")) path)
                (cond
                 ((and (f-ext? joined-path) (f-exists? joined-path))
                  (setq real-path joined-path))
                 ((f-exists? (f-swap-ext joined-path "js") )
                  (setq real-path (f-swap-ext joined-path "js")))
                 ((and (not (f-ext? joined-path)) (f-exists? (f-join joined-path "index.js")) )
                  (setq real-path (f-join joined-path "index.js")))))))
          aliases)
    real-path))


(defun js-import-path-to-real(path)
  (cond ((js-import-is-dependency? path)
         (js-import-find-node-module-index-path path))
        ((s-matches? "^\\." path)
         (let ((filepath (f-short (f-expand path))))
           (cond
            ((f-exists? (concat filepath ".js"))
             (setq filepath (concat filepath ".js"))
             filepath)
            ((f-exists? (concat filepath "/index.js"))
             (setq filepath (concat filepath "/index.js")))
            (t filepath))
           filepath)
         )
        ((when-let (alias-path (js-import-alias-path-to-real path))
           alias-path))))


(provide 'js-import-utils)
;;; js-import-utils.el ends here
