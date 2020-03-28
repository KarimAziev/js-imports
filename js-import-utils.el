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
(require 'cl-lib)
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

(defvar js-import-dependencies-cache-plist '())

(defmacro js-import-compose (args &rest funcs)
  `(funcall (-compose ,@funcs) ,args))


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



(defun js-import-strip-text-props(str)
  "Remove all text properties from string or stringifies symbol"
  (cond ((stringp str)
         (set-text-properties 0 (length str) nil str)
         str)
        ((and str (symbolp str))
         (symbol-name str))
        (nil str)))

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

(cl-defun js-import-make-item (candidate
                               &key
                               display-path
                               type
                               real-path
                               cell)
  "Utility function to make js-import item. See also

`js-import-propertize'."

  (let* ((splitted-name (split-string candidate "[ \t\s]+as[ \t\s]+"))
         (result (js-import-propertize candidate
                                       'real-name (nth 0 splitted-name)
                                       'display-name candidate
                                       'display-path display-path
                                       'renamed-name (nth 1 splitted-name)
                                       'real-path (or real-path (js-import-path-to-real display-path))
                                       'type (or type (cdr cell)))))

    result))


(defun js-import-find-current-imports(display-path)
  (let* ((matches (s-match (js-import-make-import-regexp-from-path display-path) (buffer-substring-no-properties (point-min) (js-import-goto-last-import))))
         (default-import-name (nth 1 matches))
         (named-exports (js-import-cut-names (nth 2 matches) ",\\|}\\|{"))
         (alist '()))
    (when named-exports
      (mapc (lambda (n) (push (cons n (. 4)) alist)) named-exports))

    (when default-import-name
      (push (cons default-import-name (. 1)) alist))
    alist))

(defun js-import-find-all-buffer-imports(&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (let* ((content (buffer-substring-no-properties (point-min) (js-import-goto-last-import)))
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


(defun js-import-filter-pred(filename)
  (and (not (string-equal (s-replace (projectile-project-root) "" buffer-file-name) filename))
       (js-import-is-ext-enabled? filename)
       (not (s-matches? js-import-unsaved-file-regexp filename))
       (not (s-matches? js-import-test-file-regexp filename))))


(defun js-import-get-export-type(str)
  (cond
   ((s-equals? "*" str) 16)
   ((s-matches? "{[ \t\s\n]?\\(default\\)[ \\s\\t]+as[^a-zZ-A0-9_$]" str) 4)
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


(defun js-import-generate-name-from-path(path)
  "Generate name for default export from PATH"
  (js-import-compose path
                     '(lambda(words) (mapconcat 'capitalize words ""))
                     (-partial '-take 2)
                     'reverse
                     's-split-words
                     (-partial 'replace-regexp-in-string js-import-file-index-regexp "")
                     'js-import-remove-ext))

(defun js-propose-import-name (path cell)
  (let* ((current-name (car cell))
         (export-type (cdr cell))
         (proposed-symbol (js-import-generate-name-from-path path))
         (prompt (format
                  (pcase export-type
                    (1 "Import default as (default: %s): ")
                    (4 "Import { (default: %s) }: ")
                    (16 "Import all exports as (default: %s): "))
                  proposed-symbol))
         (read-symbols
          (read-string
           prompt
           proposed-symbol
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
  (js-import-compose path
                  'js-import-remove-double-slashes
                  'js-import-remove-ext
                  'js-import-maybe-remove-path-index))


(defun js-import-maybe-remove-path-index (path)
  (if (js-import-is-index-trimmable? path)
      (replace-regexp-in-string js-import-file-index-regexp "" path)
    path))

(defun js-import-remove-double-slashes (path)
  (replace-regexp-in-string "//"  "/" path))

(defun js-import-real-path-to-alias(real-path alias)
  (let ((alias-path (js-import-get-alias-path alias)))
    (f-join alias (replace-regexp-in-string (concat "^" alias-path) "" real-path))))

(defun js-import-get-alias-path(alias)
  (f-slash (f-join (projectile-project-root) (lax-plist-get js-import-alias-map alias))))


(defun js-import-get-node-modules-path (&optional project-dir)
  "Return the path to node-modules."
  (f-join (or project-dir (projectile-project-root)) "node_modules"))

(defun js-import-expand-node-modules(module &optional project-dir)
  (let ((node-modules-path (f-join (js-import-get-node-modules-path project-dir) module)))
    node-modules-path))


(defun js-import-is-dependency? (display-path &optional project-root)
  "Check if path is dependency"
  (let ((dependencies (or (plist-get js-import-dependencies-cache-plist (or project-root (projectile-project-root))) (js-import-get-all-dependencies)))
        (dirname (car (split-string display-path "/"))))


    (or (f-exists? (js-import-expand-node-modules dirname project-root))
        (-contains? dependencies dirname))))

(defun js-import-is-dir-and-exist(path)
  (and (f-exists? path) (not (f-ext? path))))

(defun js-import-is-package-json(path)
  (string="package.json" (f-filename path)))

(defun js-import-is-ext-enabled? (filename)
  "Check if FILENAME ends with either .js or .jsx."
  (s-matches? "\\.[jt]s\\(x\\)?$" filename))

(defun js-import-is-index-file?(path)
  (s-matches? js-import-file-index-regexp path))

(defun js-import-is-relative?(path)
  (s-matches? "^\\.+/" path))

(defun js-import-is-module-interface(path)
  (s-matches? ".d.ts$" path))

(defun js-import-is-index-trimmable?(path)
  "Check if PATH index can be trimmed"
  (and (js-import-is-index-file? path)
       (< 1 (s-count-matches "/" path))))

(defun js-import-get-package-json-path ()
  "Return the path to package.json."
  (f-join (projectile-project-root) "package.json"))

(defun js-import-remove-ext(path)
  (replace-regexp-in-string "\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)$" "" path))

(defun js-import-find-interfaces(display-path)
  (when-let ((f-exists-p (js-import-expand-node-modules display-path))
             (files (f-files (js-import-expand-node-modules display-path) (lambda(path) (and (js-import-is-module-interface path) (not (js-import-is-index-file? path)))))))
    (--map (f-join display-path (f-filename (js-import-remove-ext it)))
           files)))

(defun js-import-get-all-dependencies(&optional $package-json-path)
  "Return dependencies, devDependencies and peerDependencies from package-json-path"
  (let ((sections '("dependencies" "peerDependencies" "devDependencies")))
    (--reduce-r-from (append (js-import-get-dependencies (js-import-get-package-json-path) it) acc) '() sections)))



(defun js-import-get-dependencies (&optional $package-json-path $section)
  "Return dependencies list from package-json-path in dependencies, devDependencies and peerDependencies sections."
  (when-let ((dependencies-hash (js-import-read-package-json-section $package-json-path $section)))
    (hash-table-keys dependencies-hash)))

(defun js-import-read-package-json-section (&optional $package-json-path $section)
  "Return dependencies list from package-json-path in dependencies, devDependencies and peerDependencies sections."
  (let ((package-json-path (or $package-json-path (js-import-get-package-json-path)))
        (section (or $section "dependencies"))
        (json-object-type 'hash-table))
    (when-let ((package-json-content (condition-case nil
                                         (f-read-text package-json-path 'utf-8) (error nil)))
               (dependencies-hash (condition-case nil
                                      (gethash section (json-read-from-string package-json-content)) (error nil))))
      dependencies-hash)))


(defun js-import-get-dependency-dir(display-path)
  (let ((parts (s-slice-at "/[^/]+$" display-path)))
    (when-let (lastpath (nth 1 parts))
      (setcdr parts (s-replace-regexp "^/" "" lastpath)))
    parts))

(defun js-import-sort-by-ext(files)
  (--sort (let ((aExt (f-ext it))
                (bName (f-filename other))
                (bExt (f-ext other)))
            (cond
             ((and (equal aExt "ts") (not (equal bName "index.d.ts")))
              aExt)
             ((and (equal aExt "js") (not (equal bExt "ts")))
              aExt)))
          files))

(defun js-import-find-index-files(path)
  (f-files path (lambda(file) (or (and (js-import-is-ext-enabled? file)
                                  (-contains? (list "index.d" "index") (f-base file)))
                             (equal "package.json" (f-filename file))
                             ))))

(defun js-import-process-file (fPath)
  "Process the file at fullpath FPATH.
Write result to buffer DESTBUFF."
  (with-temp-buffer
    (goto-char 1)
    (with-output-to-temp-buffer fPath
      (insert-file-contents fPath)
      (print (js-import-find-exports fPath))
      (print (js-import-find-all-buffer-imports)))))


(defun js-import-collect-deep-exports(path)
  (when-let (content (f-read path))
    (when-let ((deep-exports (s-match-strings-all js-import-regexp-export-all-from content)))
      (--map (car (last it)) deep-exports))))


(defun js-import-find-exports (&optional path)
  (when-let (content (f-read path))
    (let ((all-matches (s-match-strings-all js-import-export-regexp content)))
      (js-import-map-matches all-matches js-import-regexp-export-exclude-regexp))))


(defun js-import-find-all-exports (display-path &optional real-path)
  (let* ((curr-path (or real-path buffer-file-name))
         (curr-dir (if (f-ext? curr-path) (f-dirname curr-path) curr-path)))

    (unless real-path (setq real-path (js-import-path-to-real display-path curr-dir)))
    (when-let ((real-path)
               (dir-name (f-dirname real-path))
               (content (f-read real-path)))
      (let ((all-matches (s-match-strings-all js-import-export-regexp content))
            (deep-exports (--map (car (last it)) (s-match-strings-all js-import-regexp-export-all-from content)))
            (result '()))
        (when deep-exports
          (mapc (lambda(path) (push (js-import-find-all-exports path (js-import-path-to-real path dir-name)) result))
                deep-exports))
        (-distinct (-flatten (append result (js-import-map-matches all-matches js-import-regexp-export-exclude-regexp))))
        ))))

(defun js-import-path-to-real(path &optional dir)
  (cond ((js-import-is-relative? path)
         (js-import-path-to-relative path dir))
        ((js-import-is-dependency? path)
         (js-import-maybe-expand-dependency path))
        (t (js-import-alias-path-to-real path))))

(defun js-import-find-index-files-in-path(path)
  (cond
   ((js-import-is-dir-and-exist path)
    (funcall (-compose 'js-import-sort-by-ext 'js-import-find-index-files) path))
   ((js-import-is-package-json path)
    (when-let ((dir (f-dirname path))
               (module (js-import-read-package-json-section path "module")))
      (unless (f-ext? module)
        (setq module (f-swap-ext module "js")))
      (list (f-expand module dir))))
   (t nil)))

(defun js-import-traverse-down(path)
  (let ((filepath (car (js-import-find-index-files-in-path path))))
    (if filepath
        (js-import-traverse-down filepath)
      path)))

(defun js-import-maybe-expand-dependency(display-path &optional $real-path)
  (let ((real-path (or $real-path (js-import-expand-node-modules display-path))))
    (unless (f-ext real-path)
      (setq real-path (js-import-traverse-down real-path)))))


(defun js-import-alias-path-to-real(path)
  (let* ((aliases (js-import-get-aliases))
         (project-dir (projectile-project-root))
         (real-path nil))
    (mapc (lambda(alias)
            (let* ((alias-regexp (if (s-blank? alias) (concat "^" alias) (concat "^" alias "/")))
                   (alias-path (js-import-get-alias-path alias))
                   (joined-path (f-join
                                 alias-path (s-replace-regexp alias-regexp "" path))))
              (when (s-matches? alias-regexp path)
                (cond
                 ((and (f-ext? joined-path) (f-exists? joined-path))
                  (setq real-path joined-path))
                 ((f-exists? (f-swap-ext joined-path "ts") )
                  (setq real-path (f-swap-ext joined-path "ts")))
                 ((f-exists? (f-swap-ext joined-path "js") )
                  (setq real-path (f-swap-ext joined-path "js")))
                 ((and (not (f-ext? joined-path)) (f-exists? (f-join joined-path "index.ts")) )
                  (setq real-path (f-join joined-path "index.ts")))
                 ((and (not (f-ext? joined-path)) (f-exists? (f-join joined-path "index.js")) )
                  (setq real-path (f-join joined-path "index.js")))))))
          aliases)
    real-path))

(defun js-import-path-to-relative(path &optional dir)
  (let ((filepath (f-expand path dir)))
    (cond
     ((f-exists? (concat filepath ".js"))
      (setq filepath (concat filepath ".js"))
      filepath)
     ((f-exists? (concat filepath "/index.js"))
      (setq filepath (concat filepath "/index.js"))))
    filepath))


(defun js-import-which-word ()
  "Find closest to point whole word."
  (interactive)
  (save-excursion
    (let ( $p1 $p2 )
      (if (use-region-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (save-excursion
          (skip-chars-backward "_A-Za-z0-9")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "_A-Za-z0-9")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (buffer-substring-no-properties $p1 $p2))))


(defun js-import-get-path-at-point()
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2)))))
    $inputStr))

(defun js-import-get-unreserved-word-at-point()
  "Returns whole word at point unless its javascript reserved word"
  (interactive)
  (save-excursion
    (when-let* ((whole-word (js-import-which-word))
                (is-enabled (and (not (js-import-word-reserved? whole-word))
                                 (s-matches? js-import-word-chars-regexp whole-word))))
      whole-word)))


(provide 'js-import-utils)
;;; js-import-utils.el ends here
