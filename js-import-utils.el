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

(require 'projectile)
(require 'cl-lib)
(require 'f)
(require 'dash)
(require 'json)
(require 'subr-x)
(require 's)
(require 'js-import-regexp)

(defun -compose (&rest fns)
  "Takes a list of functions and returns a fn that is the
composition of those fns. The returned fn takes a variable
number of arguments, and returns the result of applying
each fn to the result of applying the previous fn to
the arguments (right-to-left)."
  (lambda (&rest args)
    (car (-reduce-r-from (lambda (fn xs) (list (apply fn xs)))
                         args fns))))

(defmacro js-import-compose (args &rest funcs)
  `(funcall (-compose ,@funcs) ,args))

(defmacro js-import-call-with-marked-candidates-prop (func prop)
  "Iter helm-marked-candidates, pluck PROP from every candidate and call FUNC with value of PROP."
  (declare (indent 2) (debug t))
  `(lambda(_cand) (mapc
              (lambda(candidate)
                (funcall ,func (js-import-get-prop candidate ,prop)))
              (helm-marked-candidates))))

(defmacro js-import-filter-plist(prop-symbol test-form plist)
  `(seq-filter (lambda(str) (let ((it (js-import-get-prop str ,prop-symbol)))
                         ,test-form))
               ,plist))

(defmacro js-import-with-marked-candidates (func)
  (declare (indent 2) (debug t))
  `(lambda(&optional _candidate) (mapc ,func (helm-marked-candidates))))

(defun js-import-with-marked (func)
  (lambda(&optional _candidate) (mapc (lambda(c) (funcall func c)) (helm-marked-candidates))))

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
  (let (parts)
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


(defun js-import-filter-pred(filename)
  (and (not (string-equal (s-replace (projectile-project-root) "" buffer-file-name) filename))
       (js-import-is-ext-enabled? filename)
       (not (s-matches? js-import-unsaved-file-regexp filename))
       (not (s-matches? js-import-test-file-regexp filename))))


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
                 (16 (format "* as %s" symbols)))))
    name))

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

(defun js-import-get-node-modules-path (&optional project-dir)
  "Return the path to node-modules."
  (f-join (or project-dir (projectile-project-root)) "node_modules"))

(defun js-import-expand-node-modules(module &optional project-dir)
  (let ((node-modules-path (f-join (js-import-get-node-modules-path project-dir) module)))
    node-modules-path))


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
  (if (js-import-is-relative? path)
      (and (js-import-is-index-file? path)
           (< 1 (s-count-matches "/" path)))
    (js-import-is-index-file? path)))

(defun js-import-get-package-json-path ()
  "Return the path to package.json."
  (f-join (projectile-project-root) "package.json"))

(defun js-import-remove-ext(path)
  (replace-regexp-in-string "\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)$" "" path))

(defun js-import-find-interfaces(display-path)
  (when-let ((f-exists-p (js-import-expand-node-modules display-path))
             (files (f-files (js-import-expand-node-modules display-path) (lambda(path) (and (js-import-is-module-interface path)
                                                                                        (not (js-import-is-index-file? path)))))))
    (mapcar (lambda(it) (f-join display-path (f-filename (js-import-remove-ext it)))) files)))


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


(defun js-import-try-ext(path &optional dir extensions)
  (unless extensions (setq extensions '("d.ts" "ts" "js")))
  (let (ext real-path)
    (while extensions
      (setq ext (pop extensions))
      (setq real-path (if dir (f-expand (f-swap-ext path ext) dir) (f-swap-ext path ext)))
      (if (f-exists? real-path)
          (setq extensions nil)
        (setq real-path nil)))
    real-path))

(defun js-import-try-json-sections(path sections)
  (let (section)
    (while sections
      (setq section (js-import-read-package-json-section path (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))


(defun js-import-join-when-exists(path filename)
  "Returns joined PATH with FILENAME when exists."
  (let ((joined-path (f-join path filename)))
    (when (f-exists? joined-path)
      joined-path)))

(defun js-import-try-find-real-path(path)
  (if (and (f-ext? path) (f-exists? path))
      path
    (or (when-let* ((package-json (js-import-join-when-exists path "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             '("jsnext:main" "module" "types")))
                    (dir (f-dirname path)))
          (if (f-ext? module)
              (f-expand module path)
            (js-import-try-find-real-path (js-import-try-ext module path))))
        (js-import-try-ext path)
        (js-import-try-ext (f-join path "index")))))


(defun js-import-maybe-expand-dependency(display-path &optional $real-path)
  (let ((real-path (or $real-path (js-import-expand-node-modules display-path))))
    (unless (f-ext real-path)
      (setq real-path (js-import-try-find-real-path real-path))
      real-path)))


(defun js-import-path-to-relative(path &optional dir)
  (unless dir (setq dir default-directory))
  (or (js-import-try-ext path dir)
      (js-import-try-ext (f-join path "index") dir)))



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
          (skip-chars-backward "_$A-Za-z0-9")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "_$A-Za-z0-9")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (buffer-substring-no-properties $p1 $p2))))


(defun js-import-get-path-at-point()
  (interactive)
  (save-excursion
    (when-let* ((word (js-import-which-word))
                (meta-word (or (string= "import" word)
                               (string= "export" word)
                               (string= "from" word))))
      (if (string= word "from")
          (search-forward-regexp "['\"]" nil t 1)
        (search-forward-regexp "[ \s\t\n]+from[ \s\t\n]+['\"]" nil t 1)))

    (when (js-import-inside-string-q)
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

        $inputStr))))

(defun js-import-inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    result))

(defun js-import-inside-comment? ()
  "Returns value of comment character in syntax table's or nil otherwise"
  (interactive)
  (nth 4 (syntax-ppss)))

(defun js-import-get-unreserved-word-at-point()
  "Returns camelCased word at point unless it is javascript reserved."
  (interactive)
  (save-excursion
    (when-let* ((whole-word (js-import-which-word))
                (is-enabled (and (not (js-import-word-reserved? whole-word))
                                 (s-matches? js-import-word-chars-regexp whole-word))))
      whole-word)))


(defun js-import-build-help-buffer (feature body)
  (with-current-buffer (get-buffer-create (concat "*js-import-" feature "*"))
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when body
        (save-excursion
          (insert body))))
    (local-set-key (kbd "q") #'quit-window)
    (current-buffer)))


(defmacro js-import-with-help-buffer(buf &rest forms)
  `(let ((buffer (with-current-buffer (get-buffer-create ,buf)
                   (setq buffer-read-only t)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (progn ,@forms))
                   (local-set-key (kbd "q") #'quit-window)
                   (current-buffer))))
     (display-buffer buffer t)
     (if help-window-select
         (progn
           (pop-to-buffer buffer)
           (message "Type \"q\" to restore previous buffer"))
       (message (concat "Type \"q\" in the to close it")))))


(defun js-import-show-help-buffer (feature body)
  (let ((buffer (js-import-build-help-buffer feature body)))
    (display-buffer buffer t)
    (if help-window-select
        (progn
          (pop-to-buffer buffer)
          (message "Type \"q\" to restore previous buffer"))
      (message (concat "Type \"q\" in the " feature " buffer to close it")))))


(defun js-import-strip-duplicates (list)
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

(defun js-import-filter-by-prop(value prop-symbol plist)
  "Filter PLIST by diffing "
  (seq-filter (lambda(str) (string= value (js-import-get-prop str prop-symbol))) plist))

(defun js-import-rename-default-item(item)
  "Renames default imported item "
  (let (real-name new-name confirm overlay end beg)
    (setq real-name (or (js-import-get-prop item 'real-name) (js-import-strip-text-props item)))
    (setq beg (js-import-get-prop item 'marker))
    (goto-char beg)
    (when (string= real-name (js-import-which-word))
      (setq end (+ (point) (length (js-import-which-word))))

      (unwind-protect
          (progn (setq overlay (make-overlay beg end))
                 (make-overlay beg end)
                 (overlay-put overlay 'face 'ag-match-face)
                 (setq new-name (string-trim (read-string
                                              "Rename %s to"
                                              (concat "\s" real-name)
                                              nil)))
                 (if (string-blank-p new-name)
                     (message "New name is blank")
                   (progn
                     (remove-overlays beg end)
                     (delete-region beg end)
                     (insert new-name)
                     (setq end (+ beg (length new-name)))
                     (setq overlay (make-overlay beg end))
                     (overlay-put overlay 'face 'ag-match-face)
                     (setq confirm (yes-or-no-p (format "Rename occurence?"))))))

        (remove-overlays beg end)
        (if (not confirm)
            (when (string= new-name (js-import-which-word))
              (delete-region beg end)
              (insert real-name))
          (progn
            (let ((case-fold-search nil))
              (js-import-goto-last-import)
              (query-replace-regexp (concat "\\_<" real-name "\\_>") new-name))))))))

(defun js-import-rename-as(item)
  "Rename named imports and module imports."
  (let* ((marker (js-import-get-prop item 'marker))
         (full-name (or (js-import-get-prop item 'full-name)
                        (js-import-strip-text-props item)))
         (parts (split-string full-name))
         (real-name (or (js-import-get-prop item 'real-name) (nth 0 parts)))
         (as-word (nth 1 parts))
         (renamed-name (nth 2 parts))
         (prompt (if as-word (format "Rename %s %s" real-name as-word) (format "Rename %s as" real-name)))
         (input (concat "\s" renamed-name))
         (new-name (string-trim (read-string
                                 prompt
                                 input))))

    (when (and (not (string-blank-p new-name))
               marker
               (goto-char marker)
               (string= real-name (js-import-which-word)))
      (skip-chars-forward real-name)

      (if as-word
          (progn
            (skip-chars-forward " \s\t\n")
            (skip-chars-forward "as")
            (skip-chars-forward " \s\t\n")
            (when (and renamed-name (string= renamed-name (js-import-which-word)))
              (delete-region (point) (+ (point) (length renamed-name))))
            (insert new-name))

        (insert (format " as %s" new-name)))

      (js-import-goto-last-import)
      (query-replace-regexp (concat "\\_<" renamed-name "\\_>") (or new-name real-name)))))

(provide 'js-import-utils)
;;; js-import-utils.el ends here
