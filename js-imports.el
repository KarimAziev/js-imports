;;; js-imports.el --- Import for JavaScript files easily -*- lexical-binding: t -*-

;; Copyright (C) 2020 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-imports
;; Keywords: convenience, matching, languages
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; This library provides easy importing and navigation for javascript files.

;;; Code:

(require 'cl-lib)
(require 'json)
(eval-and-compile
  (require 'cc-mode))

(eval-when-compile
  (require 'subr-x))

(defgroup js-imports nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository"
                   "https://github.com/KarimAziev/js-imports")
  :prefix 'js-imports
  :group 'languages)

(defvar js-imports-next-alias-action nil)

(defvar js-imports-prev-alias-action nil)

(defvar js-imports-current-alias nil)

(defvar js-imports-switch-alias-post-command nil)

(defvar js-imports-files-map nil
  "Keymap for files sources.")

(defvar js-imports-helm-imported-symbols-map nil
  "Keymap for symdol sources.")

(defvar js-imports-helm-export-symbols-map nil
  "Keymap for symdol sources.")

(defcustom js-imports-quote "'"
  "Quote type."
  :group 'js-imports
  :type '(choice (const :tag "Double" "\"")
                 (const :tag "Single" "\\'")))

(defcustom js-imports-helm-files-number-limit 30
  "The limit for number of project files to display in `helm' sources."
  :group 'js-imports
  :type 'number)

(defcustom js-imports-helm-dependencies-number-limit 400
  "The limit for number of dependencies to display in `helm' sources."
  :group 'js-imports

  :type 'number)

(defcustom js-imports-package-json-sections
  '("dependencies" "devDependencies")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-imports
  :type '(repeat string))

(defcustom js-imports-node-modules-priority-section-to-read
  '("jsnext:main" "module" "types" "typings")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-imports
  :type '(repeat string))

(defcustom js-imports-node-modules-dir "node_modules"
  "Relative to project root or absolute path to node_modules directory."
  :group 'js-imports
  :type 'string)

(defcustom js-imports-preffered-extensions
  '("d.ts" "ts" "tsx" "jsx" "mjs" "js" "cjs")
  "Preffered suffixes for files with different extension."
  :group 'js-imports
  :type '(repeat string))

(defface js-imports-highlight-face
  '((t (:background "Gold" :foreground "black" :bold t)))
  "Face used to highlight symbol."
  :group 'js-imports)

(defcustom js-imports-normalize-paths-functions
  '(js-imports-remove-double-slashes
    js-imports-remove-ext
    js-imports-maybe-remove-path-index)
  "List of functions to use in `js-imports-normalize-path'."
  :type '(repeat function)
  :group 'js-imports)

(defcustom js-imports-root-ignored-directories
  '("build")
  "A list of directories in project root to ignore."
  :type '(repeat string)
  :group 'js-imports)

(defvar js-imports-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for command `js-imports-mode'.")

(defcustom js-imports-helm-file-actions
  '(("Import" . js-imports-from-path)
    ("Find file" . js-imports-find-file)
    ("Find file other window" . js-imports-find-file-other-window))
  "Default actions for files."
  :group 'js-imports
  :type '(alist :key-type string :value-type function))

(defcustom js-imports-tsconfig-filename "tsconfig.json"
  "Name of tsconfig or jsconfig."
  :type 'string)

(defvar js-imports-open-paren-re "[^=(]{")

(defvar js-imports-closed-paren-re (regexp-opt '("}") t))

(defvar js-imports-current-project-root nil)

(defvar js-imports-current-buffer nil)

(defvar js-imports-project-files nil)

(defvar-local js-imports-buffer-tick nil
  "Buffer modified tick.")
(defvar-local js-imports-current-export-path nil)
(defvar-local js-imports-last-export-path nil)
(defvar-local js-imports-export-candidates-in-path nil)
(defvar-local js-imports-cached-imports-in-buffer nil)

(defvar js-imports-node-modules-source nil
  "Variable keeps source files from node_modules.")

(defvar js-imports-project-files-source nil
  "Variable for source of relative and aliased files without dependencies.")

(defvar js-imports-buffer-files-source nil
  "Variable for source of imported files in the current buffer.")

(defvar js-imports-imported-symbols-source nil)

(defvar js-imports-exports-source nil)

(defvar js-imports-definitions-source nil)

(defun js-imports-expand-alias-path (path &optional base-url)
  "Convert PATH to absolute filename and append slash to PATH.
Without BASE-URL resolve PATH as relative to project directory,
otherwise firstly expand BASE-URL to project directory."
  (setq path (replace-regexp-in-string "\\*[^$]+" "" path))
  (cond
   ((and
     (file-name-absolute-p path)
     (file-exists-p path))
    (js-imports-slash (expand-file-name path)))
   (t
    (if-let* ((root (or js-imports-current-project-root
                        (js-imports-find-project-root)))
              (file (expand-file-name path
                                      (if base-url
                                          (expand-file-name base-url root)
                                        root))))
        (js-imports-slash file)
      path))))

(defun js-imports-normalize-aliases (paths &optional base-url)
  "Convert and sort alist of PATHS to absolute filenames.
First element of each pair in PATHS supposed to be an alias and rest elements as
relative to BASE-URL if provided or project directory."
  (let ((alist (mapcar
                (lambda (it)
                  (let ((alias (js-imports-slash
                                (string-join
                                 (split-string (format "%s" (car it)) "*"))))
                        (alias-paths (cond
                                      ((vectorp (cdr it))
                                       (append (cdr it) nil))
                                      ((listp (cdr it))
                                       (cdr it))
                                      (t `(,(cdr it))))))
                    (setq alias-paths (mapcar
                                       (lambda (p)
                                         (setq p (replace-regexp-in-string "*" "" p))
                                         (js-imports-expand-alias-path
                                          p base-url))
                                       alias-paths))
                    (cons alias alias-paths)))
                paths)))
    (seq-sort-by (lambda (it) (length (car it))) #'> alist)))

(defun js-imports-set-alias (var value &optional &rest _ignored)
  "Set VAR (`js-imports-project-aliases') to VALUE."
  (let ((aliases (js-imports-normalize-aliases value)))
    (set var aliases)))

(defcustom js-imports-project-aliases nil
  "An associated list of ((ALIAS_A . DIRECTORY_A) (ALIAS_B . DIR_B DIR_C))."
  :group 'js-imports
  :set 'js-imports-set-alias
  :type '(alist
          :key-type (string :tag "Alias")
          :value-type (repeat :tag "Path" directory)))

(defun js-imports-make-opt-symbol-regexp (words)
  "Return regexp from WORDS surrounded with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt (if (listp words)
                                 words
                               (list words)) t) "\\_>"))

(defvar js-imports-aliases nil
  "A list of aliases to use in projects.")

(defconst js-imports-file-ext-regexp
  (concat "[\\.]" (regexp-opt js-imports-preffered-extensions) "$")
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-imports-file-index-regexp
  (concat "\\(/\\|^\\)index" "\\($\\|" js-imports-file-ext-regexp "\\)"))

(defconst js-imports-string-re "[\"'][^\"']+[\"']")

(defconst js-imports-var-keywords '("const" "var" "let"))

(defconst js-imports-expression-keywords
  (append js-imports-var-keywords
          '("interface" "type" "class" "enum")))

(defconst js-imports-vars-keywords--re
  (js-imports-make-opt-symbol-regexp js-imports-var-keywords))

(defconst js-imports-expression-keywords--re
  (js-imports-make-opt-symbol-regexp js-imports-expression-keywords))

(defconst js-imports-from-keyword--re
  (js-imports-make-opt-symbol-regexp "from"))

(defconst js-imports-delcaration-keywords
  (append '("function" "function*") js-imports-expression-keywords))

(defvar js-imports-node-starts-keywords
  (append '("export" "import") js-imports-delcaration-keywords))

(defvar js-imports-node-starts-re
  (concat "\\_<" (regexp-opt js-imports-node-starts-keywords t) "\\_>"))

(defconst js-imports-delcaration-keywords--re
  (concat "\\_<" (regexp-opt js-imports-delcaration-keywords t) "\\_>"))

(defconst js-imports-regexp-name
  "_$A-Za-z0-9"
  "Regexp matching the start of a js identifier.")

(defconst js-imports-name-as--re
  "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)\\([\s\t\n]+as[\s\t\n]+\\([_$A-Za-z0-9]+\\)\\)?")

(defconst js-imports-regexp-name-set
  "[_$A-Za-z0-9]"
  "Regexp set matching the start of a js identifier.")

(defconst js-imports-regexp-import-keyword
  (eval-when-compile
    (concat "\\_<" (regexp-opt `(,"import") t) "\\_>"))
  "Regexp matching keyword import.")

(defconst js-imports-esm-export-keyword--re
  (eval-when-compile
    (concat "\\_<" (regexp-opt `(,"export") t) "\\_>"))
  "Regexp matching keyword export.")

(defconst js-imports-cjs-export-keyword--re
  (eval-when-compile
    (concat "\\_<" (regexp-opt `(,"exports") t) "\\_>"))
  "Regexp matching keyword for exports.")

(defconst js-imports-node-modules-regexp
  "\\(/\\|^\\)node_modules\\(/\\|$\\)"
  "Regexp matching path with node_modules.")

(defconst js-imports-reserved-js-words
  '("abstract" "any" "as" "async" "await" "boolean" "bigint"
    "break" "case" "catch" "class" "const" "constructor" "continue"
    "declare" "default" "delete" "do" "else" "enum" "export" "exports"
    "extends" "extern" "false" "finally" "for" "function" "function*"
    "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
    "interface" "keyof" "let" "module" "namespace" "never" "new"
    "null" "number" "object" "of" "private" "protected" "public"
    "readonly" "return" "set" "static" "string" "super" "switch"
    "this" "throw" "true" "try" "type" "typeof" "unknown"
    "var" "void" "while" "yield")
  "List of reserved words in javascript.")

(defun js-imports-reserved-word-p (str)
  "Check if STR is js reserved word."
  (when (stringp str)
    (member str js-imports-reserved-js-words)))

(defcustom js-imports-helm-files-source '(js-imports-buffer-files-source
                                          js-imports-project-files-source
                                          js-imports-node-modules-source)
  "Helm sources for files for command `js-imports'."
  :type '(repeat (choice symbol))
  :group 'js-imports)

(defcustom js-imports-helm-symbol-sources '(js-imports-imported-symbols-source
                                            js-imports-exports-source
                                            js-imports-definitions-source)
  "Helm sources for symbols for command `js-imports-symbols-menu'."
  :type '(repeat (choice symbol))
  :group 'js-imports)

(defmacro js-imports-with-buffer-or-file-content (filename &rest body)
  "Execute BODY in temp buffer with file or buffer content of FILENAME.
Bind FILENAME to variables `buffer-file-name' and `current-path''.
It is also bind `default-directory' into FILENAME's directory."
  (declare (indent 2))
  `(when-let ((current-path ,filename))
     (when (and current-path (file-exists-p current-path))
       (with-temp-buffer
         (erase-buffer)
         (let ((inhibit-read-only t))
           (if (get-file-buffer current-path)
               (progn
                 (let ((inhibit-read-only t))
                   (insert-buffer-substring-no-properties
                    (get-file-buffer current-path))))
             (progn
               (let ((buffer-file-name current-path)
                     (inhibit-read-only t))
                 (insert-file-contents current-path))))
           (with-syntax-table js-imports-mode-syntax-table
             (let* ((buffer-file-name current-path)
                    (default-directory (js-imports-dirname buffer-file-name)))
               (delay-mode-hooks
                 (set-auto-mode)
                 (progn ,@body)))))))))

(defmacro js-imports-with-popup (buff &rest body)
  `(let ((buffer (get-buffer-create ,buff)))
     (with-current-buffer buffer
       (with-current-buffer-window
           buffer
           (cons 'display-buffer-in-side-window
                 '((window-height . fit-window-to-buffer)
                   (preserve-size . (nil . t))))
           (lambda (window _value)
             (with-selected-window window
               (unwind-protect
                   (read-key "Key\s")
                 (when (window-live-p window)
                   (quit-restore-window window 'kill)))))
         ;; Here we generate the popup buffer.
         (setq cursor-type nil)
         (goto-char (point-max))
         (progn ,@body)))))

(defvar js-imports-files-cache (make-hash-table :test 'equal))

(defun js-imports-get-file-cache (cache-key)
  "Return value of CACHE-KEY from a variable `js-imports-files-cache'.
CACHE-KEY should be a filename due to invalidation which uses modification time."
  (let* ((cache (gethash cache-key js-imports-files-cache))
         (cache-tick (and cache (plist-get cache :tick)))
         (tick (file-attribute-modification-time (file-attributes
                                                  cache-key
                                                  'string))))
    (when (equal cache-tick tick)
      (plist-get cache :cache))))

(defun js-imports-set-file-cache (path content)
  "Put CONTENT to hash table using PATH as key."
  (let* ((cache (gethash path js-imports-files-cache))
         (tick (file-attribute-modification-time (file-attributes
                                                  path
                                                  'string))))
    (setq cache (list :tick tick
                      :cache content))
    (puthash path cache
             js-imports-files-cache)
    (plist-get cache :cache)))

(defvar js-imports-json-hash (make-hash-table :test 'equal))

(defun js-imports-read-json (file &optional json-type)
  "Read the JSON object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'plist))
             (cache (gethash (format "%s:%s" file json-object-type)
                             js-imports-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time (file-attributes
                                                      file
                                                      'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (js-imports-with-buffer-or-file-content
                    file
                    (js-imports-remove-comments)
                  (when-let ((str (buffer-substring-no-properties
                                   (point-min)
                                   (point-max))))
                    (json-read-from-string str))))
          (setq cache (list :tick tick
                            :json content-json))
          (puthash file cache js-imports-json-hash))
        (plist-get cache :json))
    (error (message "Cannot read %s" file))))

(defun js-imports-read-tsconfig (&optional project-root tsconfig-name)
  "Expand TSCONFIG-NAME to PROJECT-ROOT and return alist of aliases and paths."
  (unless project-root (setq project-root (js-imports-find-project-root)))
  (unless tsconfig-name (setq tsconfig-name js-imports-tsconfig-filename))
  (let ((config)
        (compiler-options)
        (found)
        (base-url)
        (extends (if tsconfig-name
                     (and project-root (expand-file-name tsconfig-name
                                                         project-root))
                   (seq-find 'file-exists-p
                             (and project-root
                                  (expand-file-name "tsconfig.json"
                                                    project-root)
                                  (expand-file-name "jsconfig.json"
                                                    project-root))))))
    (while (and (not found)
                extends
                (file-exists-p extends))
      (setq config (js-imports-read-json extends 'alist))
      (setq compiler-options (cdr-safe (assoc 'compilerOptions config)))
      (setq base-url (cdr (assoc 'baseUrl compiler-options)))
      (setq found (cdr (assoc 'paths (cdr-safe compiler-options))))
      (unless found
        (setq extends
              (and extends (assoc 'extends config)
                   (expand-file-name (cdr (assoc 'extends config))
                                     (js-imports-dirname extends))))))
    (setq base-url (and base-url extends
                        (expand-file-name
                         base-url
                         (js-imports-dirname extends))))
    (js-imports-normalize-aliases found base-url)))

(defun js-imports-read-package-json-section (&optional package-json-path
                                                       section)
  "Read a SECTION from PACKAGE-JSON-PATH and return its hash.
By default PACKAGE-JSON-PATH is a value of `js-imports-find-package-json'.
Default section is `dependencies'"
  (unless section (setq section "dependencies"))
  (let ((path (or package-json-path (js-imports-find-package-json)))
        (json-object-type 'hash-table))
    (when-let ((content
                (condition-case nil
                    (decode-coding-string
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (setq buffer-file-coding-system 'binary)
                       (insert-file-contents-literally path)
                       (buffer-substring-no-properties
                        (point-min) (point-max)))
                     'utf-8)
                  (error nil))))
      (condition-case nil
          (gethash section (json-read-from-string content))
        (error nil)))))

(defun js-imports-init-project ()
  "Initialize project by setting buffer, finding root and aliases."
  (let ((root (js-imports-find-project-root)))
    (setq js-imports-current-buffer (current-buffer))
    (when (and js-imports-current-project-root
               (not (equal root js-imports-current-project-root)))
      (setq js-imports-aliases nil
            js-imports-current-alias nil))
    (setq js-imports-current-project-root root)
    (setq js-imports-aliases (delete-dups
                              (js-imports-get-aliases)))
    (when js-imports-current-alias
      (unless (member js-imports-current-alias js-imports-aliases)
        (setq js-imports-current-alias nil)))
    (setq js-imports-project-files
          (js-imports-find-project-files
           js-imports-current-project-root))))

(defun js-imports-find-project-files (&optional project-root)
  "Return files of PROJECT-ROOT without node_modules."
  (unless project-root
    (setq project-root (or js-imports-current-project-root
                           (js-imports-find-project-root))))
  (if (and js-imports-current-project-root
           (string-match-p js-imports-current-project-root
                           default-directory))
      (let ((files)
            (processed-dirs)
            (re (concat ".+" js-imports-file-ext-regexp))
            (node-modules (js-imports-find-node-modules))
            (dir (replace-regexp-in-string "/$" "" default-directory))
            (proj-root (replace-regexp-in-string
                        "/$"
                        ""
                        js-imports-current-project-root)))
        (push dir processed-dirs)
        (while (string-match-p proj-root dir)
          (setq files (append files
                              (reverse
                               (directory-files-recursively
                                dir
                                re
                                nil
                                (lambda (it)
                                  (not (or
                                        (member it processed-dirs)
                                        (string= it dir)
                                        (string= (or node-modules "") it))))))))
          (setq dir (expand-file-name ".." dir))
          (push dir processed-dirs))
        files)
    (message "Cannot find project files")
    nil))

(defun js-imports-get-aliases ()
  "Return sorted list of ALIASES."
  (setq js-imports-project-aliases
        (seq-sort-by
         (lambda (it) (length (car it)))
         #'>
         (or (js-imports-read-tsconfig)
             (js-imports-normalize-aliases
              js-imports-project-aliases))))
  (setq js-imports-aliases
        (seq-sort-by 'length #'> (mapcar 'car js-imports-project-aliases))))

(defun js-imports-get-all-modules (&optional project-root)
  (let* ((project-files (js-imports-find-project-files project-root))
         (node-modules (js-imports-node-modules-candidates project-root))
         (aliases (js-imports-get-aliases))
         (relative-files (js-imports-transform-files-to-relative
                          default-directory project-files))
         (transformed-files (mapcan (lambda (a)
                                      (js-imports-transform-files-to-alias
                                       a project-files))
                                    aliases)))
    (append transformed-files node-modules relative-files)))

(defun js-imports-project-files-transformer (files &optional _source)
  "Filter and transform FILES to aliased or relative."
  (with-current-buffer js-imports-current-buffer
    (let* ((current-file (buffer-file-name js-imports-current-buffer))
           (current-dir (js-imports-dirname current-file)))
      (setq files (seq-remove (lambda (filename) (string= filename current-file))
                              files))
      (if js-imports-current-alias
          (js-imports-transform-files-to-alias js-imports-current-alias files)
        (js-imports-transform-files-to-relative current-dir files)))))

(defun js-imports-get-file-variants (&optional path dir)
  (let* ((real-path (js-imports-path-to-real path dir))
         (relative (js-imports-path-to-relative real-path
                                                (or dir
                                                    default-directory)))
         (aliased (mapcar (lambda (it) (js-imports-transform-file-to-alias
                                   real-path
                                   it))
                          (js-imports-get-aliases))))
    (seq-uniq (seq-remove 'null (push relative aliased)))))

(defun js-imports-transform-file-to-alias (filename alias)
  (when-let* ((absolute-p (and filename (file-name-absolute-p filename)))
              (paths (cdr (assoc alias js-imports-project-aliases)))
              (alias-path (seq-find (lambda (parent)
                                      (js-imports-string-match-p
                                       (concat "^" parent)
                                       filename))
                                    paths)))
    (js-imports-normalize-path (replace-regexp-in-string
                                alias-path
                                (js-imports-slash alias)
                                filename))))

(defun js-imports-transform-files-to-alias (alias files)
  (seq-remove 'null (mapcar (lambda (it)
                              (js-imports-transform-file-to-alias it alias))
                            files)))

(defun js-imports-transform-files-to-relative (dir files)
  (mapcar (lambda (path) (js-imports-normalize-path
                     (js-imports-path-to-relative
                      path
                      dir)))
          files))

(defun js-imports-find-project-root (&optional dir)
  (unless dir (setq dir default-directory))
  (let ((parent (expand-file-name ".." dir)))
    (unless (or (string= parent dir)
                (string= dir "")
                (string= dir "/"))
      (if (file-exists-p (expand-file-name "package.json" dir))
          dir
        (js-imports-slash (js-imports-find-project-root parent))))))

(defun js-imports-directory-files (dir &optional recursive re include-dirs pred)
  "Return files in DIR that matches value of the variable
`js-imports-file-ext-regexp'.
Optional argument RECURSIVE non-nil means to search recursive."
  (unless re (setq re js-imports-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir re include-dirs pred)
    (directory-files dir t re t)))

(defun js-imports-join-file (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (not (file-name-absolute-p (car args)))))
    (mapc (lambda (arg)
            (unless (null arg)
              (setq path (expand-file-name arg path))))
          args)
    (if relative (file-relative-name path) path)))

(defun js-imports-dirname (path)
  "Return the parent directory to PATH."
  (let (parent)
    (setq parent (file-name-directory
                  (directory-file-name
                   (expand-file-name path default-directory))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (js-imports-relative-p path)
          (file-relative-name parent)
        (directory-file-name parent)))))

(defun js-imports-path-to-relative (path &optional dir)
  "Transform PATH into relative to the DIR (default: ‘default-directory’).
If PATH is a relative file, it will be returned without changes."
  (if (js-imports-relative-p path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-imports-relative-p relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-imports-slash (str)
  "Append slash to non-empty STR unless one already."
  (cond ((string= "" str) str)
        ((string= "/" str) "")
        ((stringp str)
         (if (string-match "/$" str)
             str
           (concat str "/")))))

(defun js-imports-normalize-path (path)
  (apply 'js-imports-compose-from
         (append
          (list path)
          js-imports-normalize-paths-functions)))

(defun js-imports-remove-double-slashes (path)
  (replace-regexp-in-string "//"  "/" path))

(defun js-imports-maybe-remove-path-index (path)
  (if (js-imports-index-trimmable-p path)
      (replace-regexp-in-string js-imports-file-index-regexp "" path)
    path))

(defun js-imports-index-trimmable-p (path)
  "Check if PATH index can be trimmed."
  (if (js-imports-relative-p path)
      (and (js-imports-is-index-file-p path)
           (< 1 (js-imports-count-matches "/" path)))
    (js-imports-is-index-file-p path)))

(defun js-imports-remove-ext (path)
  (replace-regexp-in-string js-imports-file-ext-regexp "" path))

(defun js-imports-is-index-file-p (path)
  (js-imports-string-match-p js-imports-file-index-regexp path))

(defun js-imports-relative-p (path)
  (js-imports-string-match-p "^\\.+/" path))

(defun js-imports-path-to-real (path &optional dir)
  (when (and path (stringp path))
    (setq path (js-imports-strip-text-props path))
    (cond ((and (js-imports-string-match-p
                 js-imports-file-ext-regexp path)
                (file-exists-p path)
                (not (js-imports-relative-p path)))
           path)
          ((js-imports-relative-p path)
           (js-imports-relative-to-real path dir))
          ((js-imports-dependency-p path (js-imports-find-project-root))
           (js-imports-node-module-to-real path))
          (t (js-imports-alias-path-to-real path)))))

(defun js-imports-get-ext (str)
  (when-let ((ext-pos (string-match
                       "\\.\\(\\(d\\.\\)?[a-zZ-A0-9]+\\)$"
                       str)))
    (substring str (1+ ext-pos))))

(defun js-imports-sort-by-exts (files &optional extensions)
  (setq extensions (or extensions js-imports-preffered-extensions))
  (seq-sort-by (lambda (a)
                 (if-let ((ext (js-imports-get-ext a)))
                     (or (seq-position extensions ext 'string=) -1)
                   -1))
               #'>
               files))

(defun js-imports-relative-to-real (path &optional dir)
  (when (or (string= path ".")
            (string= path ".."))
    (setq path (js-imports-slash path)))
  (let* ((base (file-name-base path))
         (non-dir (file-name-nondirectory path))
         (ext (js-imports-get-ext path))
         (up (if (string-empty-p base)
                 path
               (replace-regexp-in-string
                (format "/%s\\(\\.[a-zZ-A0-9]+\\)*$" base) "" path)))
         (parent (expand-file-name up (or dir default-directory)))
         (files (js-imports-sort-by-exts
                 (directory-files parent nil
                                  (js-imports-make-file-base-re
                                   non-dir)
                                  t)))
         (found (car files))
         (result (and found (expand-file-name found parent))))
    (cond ((and (null ext)
                result
                (file-directory-p result))
           (when-let ((index
                       (car
                        (directory-files
                         result nil "^index\\(\\.[a-zZ-A0-9]+\\)+$"))))
             (expand-file-name index result)))
          (t result))))

(defun js-imports-alias-path-to-real (path)
  "Convert aliased PATH to absolute file name."
  (let (aliases alias-cell alias-paths alias alias-regexp real-path)
    (setq aliases (js-imports-get-aliases))
    (while (setq alias (pop aliases))
      (setq alias-regexp (if (js-imports-string-blank-p alias)
                             (concat "^" alias)
                           (concat "^" (js-imports-slash alias))))
      (setq alias-cell (assoc alias js-imports-project-aliases))
      (setq alias-paths (cdr alias-cell))
      (let (alias-path)
        (while (setq alias-path (pop alias-paths))
          (let* ((joined-path (js-imports-join-file
                               alias-path
                               (replace-regexp-in-string
                                alias-regexp "" path)))
                 (found-path (if (and
                                  joined-path
                                  (js-imports-get-ext joined-path)
                                  (file-exists-p joined-path))
                                 joined-path
                               (or (js-imports-try-ext joined-path)
                                   (js-imports-try-ext
                                    (js-imports-join-file
                                     joined-path "index"))))))
            (when (and found-path (file-exists-p found-path))
              (setq real-path found-path)
              (setq aliases nil))))))
    real-path))

(defun js-imports-add-ext (path ext)
  (if (js-imports-string-match-p js-imports-file-ext-regexp path)
      path
    (concat path "." ext)))

(defun js-imports-try-ext (path &optional dir extensions)
  "A function tries to join into PATH every element from EXTENSIONS
from left to right until first existing file will be found or nil otherwise.
If optional argument DIR is passed, PATH will be firstly expanded as relative."
  (unless extensions (setq extensions js-imports-preffered-extensions))
  (let (ext real-path)
    (while extensions
      (setq ext (pop extensions))
      (setq real-path (js-imports-add-ext path ext))
      (when dir
        (setq real-path (expand-file-name real-path dir)))
      (if (file-exists-p real-path)
          (setq extensions nil)
        (setq real-path nil)))
    real-path))

(defun js-imports-next-or-prev-alias (&optional direction)
  "Set value for variable `js-imports-current-alias'."
  (unless direction (setq direction 1))
  (let* ((aliases (if (> direction 0)
                      js-imports-aliases
                    (reverse js-imports-aliases))))
    (setq js-imports-current-alias (if js-imports-current-alias
                                       (car (cdr
                                             (member js-imports-current-alias
                                                     aliases)))
                                     (car aliases))))
  (when js-imports-switch-alias-post-command
    (funcall js-imports-switch-alias-post-command)))

(defun js-imports-get-package-json-modules (&optional project-root)
  (when-let*
      ((root (or project-root (js-imports-find-project-root)))
       (package-json-path
        (expand-file-name
         "package.json" (if (js-imports-string-match-p
                             js-imports-node-modules-regexp
                             root)
                            (car
                             (split-string
                              root
                              js-imports-node-modules-regexp))
                          root)))
       (package-json (js-imports-read-json package-json-path 'hash-table)))
    (seq-remove 'null
                (mapcan (lambda (section)
                          (when-let ((hash (gethash section
                                                    package-json)))
                            (hash-table-keys hash)))
                        js-imports-package-json-sections))))

(defun js-imports-find-node-modules-submodules (node-modules-path modules)
  (let ((submodules))
    (dolist (element modules)
      (unless (js-imports-string-contains-p "/" element)
        (setq submodules (append submodules
                                 (js-imports-find-interfaces
                                  element
                                  node-modules-path)))))
    submodules))

(defun js-imports-node-modules-candidates (&optional project-root)
  "Return dependencies of PROJECT-ROOT from package json."
  (unless project-root (setq project-root (js-imports-find-project-root)))
  (when (js-imports-string-match-p js-imports-node-modules-regexp project-root)
    (setq project-root (car
                        (split-string project-root
                                      js-imports-node-modules-regexp))))
  (let ((modules (js-imports-get-package-json-modules project-root)))
    (if-let* ((node-modules-path (js-imports-find-node-modules project-root))
              (submodules (or (js-imports-get-file-cache node-modules-path)
                              (js-imports-set-file-cache
                               node-modules-path
                               (js-imports-find-node-modules-submodules
                                node-modules-path
                                modules)))))
        (append modules submodules)
      modules)))

(defun js-imports-find-interfaces (display-path dir)
  (when-let* ((real-path (js-imports-join-when-exists dir display-path))
              (files (seq-remove 'js-imports-is-index-file-p
                                 (js-imports-directory-files
                                  real-path nil "\\.d.ts$"))))
    (mapcar (lambda (it) (js-imports-join-file
                          display-path
                          (js-imports-compose-from it
                                                   'file-name-nondirectory
                                                   'directory-file-name
                                                   'js-imports-remove-ext)))
            files)))

(defun js-imports-find-node-modules (&optional project-dir)
  "Return the path to node-modules."
  (if (file-name-absolute-p js-imports-node-modules-dir)
      js-imports-node-modules-dir
    (when-let ((root (or project-dir (js-imports-find-project-root))))
      (setq root (car (split-string root js-imports-node-modules-regexp)))
      (js-imports-join-when-exists root js-imports-node-modules-dir))))

(defun js-imports-node-module-to-real (module &optional project-root)
  (when-let* ((node-modules (or (js-imports-find-node-modules project-root)
                                (js-imports-find-node-modules)))
              (real-path (js-imports-join-file node-modules module)))
    (unless (js-imports-string-match-p
             js-imports-file-ext-regexp real-path)
      (setq real-path (js-imports-try-find-real-path real-path))
      real-path)))

(defun js-imports-dependency-p (module &optional project-root)
  "Check if DISPLAY-PATH is dependency in PROJECT-ROOT.
Dependencies are recognized by `package.json' or `node_modules' of
PROJECT-ROOT."
  (let ((dependencies (js-imports-node-modules-candidates project-root))
        (dirname (car (split-string module "/"))))
    (or (member module dependencies)
        (member dirname dependencies)
        (when-let ((node-dir (js-imports-find-node-modules project-root)))
          (or (js-imports-join-when-exists node-dir module)
              (js-imports-join-when-exists node-dir dirname))))))

(defun js-imports-try-find-real-path (path)
  (if (or (null path) (and (js-imports-string-match-p
                            js-imports-file-ext-regexp path)
                           (file-exists-p path)))
      path
    (or (when-let* ((package-json (js-imports-join-when-exists
                                   path
                                   "package.json"))
                    (module
                     (js-imports-try-json-sections
                      package-json
                      js-imports-node-modules-priority-section-to-read)))
          (if (js-imports-string-match-p js-imports-file-ext-regexp
                                         module)
              (expand-file-name module path)
            (js-imports-try-find-real-path (js-imports-try-ext module path))))
        (when-let* ((dir (js-imports-join-file path "src"))
                    (exists (file-exists-p dir))
                    (files (js-imports-directory-files dir)))
          (if (= 1 (length files))
              (car files)
            (seq-find (lambda (it) (js-imports-string-match-p
                                    js-imports-file-index-regexp
                                    it))
                      files)))
        (js-imports-try-ext path)
        (js-imports-try-ext (js-imports-join-file path "index"))
        (when-let* ((package-json (js-imports-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-imports-try-json-sections
                             package-json
                             '("main"))))
          (if (js-imports-string-match-p js-imports-file-ext-regexp
                                         module)
              (expand-file-name module path)
            (js-imports-try-find-real-path
             (js-imports-try-ext module path)))))))

(defun js-imports-find-package-json ()
  "Return the path to package.json."
  (when-let ((root (js-imports-find-project-root)))
    (js-imports-join-file root "package.json")))

(defun js-imports-try-json-sections (path sections)
  (let (section)
    (while sections
      (setq section (js-imports-read-package-json-section
                     path
                     (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))

(defun js-imports-join-when-exists (&rest args)
  "Return joined ARGS when exists."
  (let ((joined-path (apply 'js-imports-join-file args)))
    (when (file-exists-p joined-path)
      joined-path)))

(defun js-imports-find-imported-files ()
  "Return list of with imported imported paths in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (symbols)
      (with-syntax-table js-imports-mode-syntax-table
        (while (js-imports-re-search-forward
                js-imports-regexp-import-keyword nil t 1)
          (when-let ((path (js-imports-get-path-at-point)))
            (push path symbols))))
      (cl-remove-duplicates (reverse symbols) :test 'string=))))

(defun js-imports-make-file-base-re (file)
  (let ((ext (js-imports-get-ext file))
        (filebase (js-imports-trim-ext file))
        (ext-re))
    (setq ext-re (if ext (concat "[\\.]" (regexp-opt (list ext)) "$")
                   js-imports-file-ext-regexp))
    (concat "\\(/\\|^\\)" filebase "\\($\\|" ext-re "\\)")))

(defun js-imports-trim-ext (file)
  (if-let ((ext (js-imports-get-ext file)))
      (replace-regexp-in-string (concat "\\." ext "$") "" file)
    file))

(defun js-imports-preselect-file ()
  "Preselect function for file sources."
  (if-let ((path (with-current-buffer js-imports-current-buffer
                   (or js-imports-last-export-path
                       js-imports-current-export-path
                       (when (> (point-max) (point))
                         (js-imports-get-path-at-point))))))
      (regexp-quote path)
    ""))

(defun js-imports-extract-es-imports (real-path)
  (js-imports-with-buffer-or-file-content real-path
      (goto-char 0)
    (let (symbols)
      (while (js-imports-re-search-forward
              js-imports-regexp-import-keyword nil t 1)
        (js-imports-skip-whitespace-forward)
        (let (display-path imports)
          (cond ((js-imports-looking-at "*")
                 (let (beg end renamed-name)
                   (setq beg (point))
                   (forward-char)
                   (skip-chars-forward "\s\t")
                   (setq end (point))
                   (when (js-imports-looking-at "as")
                     (skip-chars-forward "as")
                     (setq end (point))
                     (skip-chars-forward "\s\t")
                     (setq renamed-name (js-imports-which-word))
                     (if (or (js-imports-reserved-word-p renamed-name)
                             (js-imports-invalid-name-p renamed-name))
                         (setq renamed-name "")
                       (skip-chars-forward js-imports-regexp-name)))
                   (setq end (point))
                   (push (js-imports-make-item
                          (format "%s" (buffer-substring-no-properties beg end))
                          :type 16
                          :real-name "*"
                          :as-name renamed-name
                          :pos beg)
                         imports)))
                ((js-imports-get-word-if-valid)
                 (when-let ((name (js-imports-get-word-if-valid)))
                   (push (js-imports-make-item
                          name
                          :type 1
                          :real-name name
                          :as-name name
                          :pos (point))
                         imports)
                   (skip-chars-forward name)
                   (js-imports-skip-whitespace-forward)
                   (skip-chars-forward ",")
                   (js-imports-skip-whitespace-forward))))
          (when (looking-at-p "{")
            (let ((map-func (lambda (item) (js-imports-propertize
                                            item
                                            :type 4
                                            :pos
                                            (js-imports-get-prop item :start))))
                  (named-items (js-imports-extract-esm-braced-symbols
                                js-imports-name-as--re)))
              (setq named-items (mapcar map-func named-items))
              (setq imports (append imports named-items))))
          (unless (looking-at-p "[\"']")
            (js-imports-re-search-forward js-imports-from-keyword--re nil t 1)
            (js-imports-skip-whitespace-forward)
            (when (looking-at-p "[\"']")
              (save-excursion
                (forward-char 1)
                (setq display-path (js-imports-get-path-at-point)))
              (setq imports (mapcar (lambda (it)
                                      (js-imports-propertize it
                                                             :display-path
                                                             display-path
                                                             :real-path
                                                             real-path
                                                             :import t))
                                    imports))
              (setq symbols (append symbols imports))))))
      symbols)))

(defun js-imports-extract-esm-braced-symbols (&optional re)
  (save-excursion
    (when-let* ((brace-start (when (looking-at-p "{") (point)))
                (brace-end (save-excursion (forward-list) (point))))
      (let (items)
        (save-restriction
          (narrow-to-region brace-start brace-end)
          (js-imports-remove-comments)
          (while (js-imports-re-search-forward re nil t 1)
            (when-let* ((start (match-beginning 0))
                        (end (match-end 0))
                        (parts (split-string (buffer-substring-no-properties
                                              start end)))
                        (full-name (mapconcat 'identity parts "\s")))
              (push (js-imports-make-item full-name
                                          :real-name (car parts)
                                          :as-name (car (reverse parts))
                                          :start start
                                          :end end)
                    items))))
        items))))

(defun js-imports-init-exports-candidates ()
  "Search exports in a file.
File is specified in the variable `js-imports-current-export-path.'."
  (with-current-buffer js-imports-current-buffer
    (setq js-imports-export-candidates-in-path
          (js-imports-extract-all-exports (or js-imports-current-export-path
                                              buffer-file-name)))
    js-imports-export-candidates-in-path))

(defun js-imports-extract-all-exports (&optional init-path)
  "Return exports in PATH defined with CommonJs syntax."
  (let* ((esm-exports)
         (cjs-exports)
         (external-paths)
         (path)
         (processed-paths)
         (map-result (lambda (result)
                       (cond ((listp result)
                              (setq result (seq-remove 'null result))
                              (setq esm-exports (append result esm-exports)))
                             ((= 16 (js-imports-get-prop result :type))
                              (when-let ((external-path (js-imports-path-to-real
                                                         (js-imports-get-prop
                                                          result
                                                          :display-path))))
                                (unless (or (member external-path
                                                    processed-paths)
                                            (member external-path
                                                    external-paths))
                                  (push external-path external-paths))))
                             ((not (null result))
                              (push result cjs-exports))))))
    (push (js-imports-path-to-real init-path) external-paths)
    (while (setq path (pop external-paths))
      (push path processed-paths)
      (when path
        (js-imports-with-buffer-or-file-content path
            (goto-char (point-min))
          (while
              (js-imports-re-search-forward
               js-imports-esm-export-keyword--re nil t 1)
            (unless (or (js-imports-inside-comment-p)
                        (js-imports-inside-string-p (point))
                        (not (looking-at "[\s\t\n]\\|[/][*/]"))
                        (save-excursion
                          (skip-chars-backward "export")
                          (unless (<= (point) (point-min))
                            (backward-char 1)
                            (not (looking-at "[\s\t\n;/*]")))))
              (js-imports-skip-whitespace-forward)
              (when-let ((result (js-imports-make-esm-export-at-point
                                  buffer-file-name)))
                (funcall map-result result))))
          (unless esm-exports
            (let ((export-depth)
                  (cjs-locals))
              (while (js-imports-re-search-forward
                      js-imports-cjs-export-keyword--re (point-max) t 1)
                (let ((depth (nth 0 (syntax-ppss (point)))))
                  (when (or (not export-depth)
                            (> export-depth depth))
                    (setq export-depth depth)
                    (setq cjs-locals nil))
                  (js-imports-skip-whitespace-forward)
                  (when-let ((result (js-imports-extract-cjs-exports
                                      buffer-file-name)))
                    (cond ((listp result)
                           (setq result (seq-remove 'null result))
                           (setq cjs-locals (append result cjs-locals)))
                          ((= 16 (js-imports-get-prop result :type))
                           (when-let ((external-path (js-imports-path-to-real
                                                      (js-imports-get-prop
                                                       result
                                                       :display-path))))
                             (unless (or (member external-path
                                                 processed-paths)
                                         (member external-path
                                                 external-paths))
                               (push external-path external-paths))))
                          ((not (null result))
                           (push result cjs-locals))))))
              (when cjs-locals
                (setq cjs-exports (seq-uniq
                                   (append cjs-exports cjs-locals)))))))))
    (mapcar (lambda (it) (js-imports-propertize it :export t))
            (reverse (seq-remove 'null (append esm-exports cjs-exports))))))

(defun js-imports-make-esm-export-at-point (&optional path)
  "Return exports in PATH defined with ES Module syntax."
  (cond ((looking-at-p "\\*")
         (let ((start (point))
               (as-name)
               (full-name)
               (end))
           (forward-char 1)
           (js-imports-skip-whitespace-forward)
           (pcase (js-imports-which-word)
             ("as" (progn (re-search-forward "as" nil t 1)
                          (js-imports-skip-whitespace-forward)
                          (setq as-name (js-imports-get-word-if-valid))
                          (skip-chars-forward as-name)
                          (setq end (point))
                          (setq full-name (concat "* as" as-name))
                          (js-imports-make-item full-name
                                                :type 4
                                                :as-name as-name
                                                :real-name "*"
                                                :real-path path
                                                :end end
                                                :pos start)))
             ("from" (when-let ((from (js-imports-get-path-at-point)))
                       (js-imports-make-item "*"
                                             :type 16
                                             :as-name "*"
                                             :real-name "*"
                                             :display-path from
                                             :real-path path
                                             :pos start))))))
        ((looking-at-p "{")
         (let ((symbols (js-imports-extract-esm-braced-symbols
                         js-imports-name-as--re))
               (map-func
                (lambda (it) (let ((type
                                    (if (equal "default"
                                               (js-imports-get-prop it
                                                                    :as-name))
                                        1 4))
                                   (pos (js-imports-get-prop it :start)))
                               (js-imports-propertize it
                                                      :type type
                                                      :real-path path
                                                      :pos pos))))
               (from-path (progn
                            (forward-list)
                            (js-imports-skip-whitespace-forward)
                            (when (js-imports-looking-at "from")
                              (skip-chars-forward "from")
                              (js-imports-skip-whitespace-forward)
                              (forward-char 1)
                              (js-imports-get-path-at-point)))))
           (setq symbols (if from-path
                             (mapcar (lambda (it)
                                       (js-imports-propertize (funcall
                                                               map-func it)
                                                              :display-path
                                                              from-path))
                                     symbols)
                           (mapcar map-func symbols)))
           symbols))
        ((looking-at js-imports-regexp-name-set)
         (let* ((stack (js-imports-skip-reserved-words "\s\t\\*"))
                (default (car (assoc "default" stack)))
                (real-name (or (car (seq-find (lambda (it)
                                                (let ((name (car it)))
                                                  (js-imports-valid-identifier-p
                                                   name)))
                                              stack))
                               default))
                (var-type (car
                           (seq-find (lambda (it)
                                       (member
                                        (car it)
                                        js-imports-delcaration-keywords))
                                     stack)))
                (as-name (or real-name default)))
           (js-imports-make-item
            (or as-name real-name)
            :type (if default 1 4)
            :real-name (or real-name default as-name)
            :real-path path
            :as-name as-name
            :var-type var-type
            :pos (point))))))

(defun js-imports-extract-cjs-exports (path)
  (cond ((looking-at-p "=\\([\s\t\n]+?\\)require[ \t('\"]")
         (js-imports-re-search-forward "require" nil t 1)
         (js-imports-skip-whitespace-forward)
         (when (looking-at "([\"']")
           (re-search-forward "([\"']"))
         (when-let ((from (js-imports-get-path-at-point)))
           (js-imports-make-item "*"
                                 :type 16
                                 :as-name "*"
                                 :real-name "*"
                                 :display-path from
                                 :real-path path
                                 :pos (point))))
        ((looking-at-p "=\\([\s\t]+?\\){")
         (js-imports-re-search-forward "=" nil t 1)
         (js-imports-skip-whitespace-forward)
         (when-let* ((items (js-imports-parse-object-keys)))
           (when (looking-at-p "{")
             (forward-list))
           (mapcar (lambda (cell)
                     (let ((name (car cell)))
                       (js-imports-make-item
                        name
                        :pos (cdr cell)
                        :as-name name
                        :type 4
                        :real-path path
                        :real-name name)))
                   items)))
        ((looking-at-p "=[^=]")
         (forward-char 1)
         (js-imports-skip-whitespace-forward)
         (when-let ((real-name (js-imports-get-word-if-valid)))
           (js-imports-make-item
            real-name
            :type 1
            :real-name real-name
            :as-name real-name
            :real-path path
            :pos (point))))
        ((looking-at-p "[.]")
         (forward-char 1)
         (when-let ((as-name (js-imports-which-word)))
           (js-imports-make-item
            as-name
            :type (if (string= as-name "default")  1 4)
            :as-name as-name
            :real-name (progn
                         (js-imports-re-search-forward "=" nil t 1)
                         (js-imports-skip-whitespace-forward)
                         (or (js-imports-get-word-if-valid)
                             as-name))
            :real-path path
            :pos (point))))))

(defun js-imports-parse-object-keys ()
  "Return object keys at point."
  (when-let ((start (when (looking-at-p "{")
                      (1+ (point))))
             (end (save-excursion (forward-list) (1- (point))))
             (re (concat js-imports-regexp-name-set "[\s\t\n]*[:(,]")))
    (let (children)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (js-imports-remove-comments)
          (while (js-imports-re-search-forward re nil t 1)
            (let (prop delimiter)
              (setq delimiter (char-before))
              (save-excursion
                (backward-char)
                (skip-chars-backward "\s\t\n")
                (setq prop (js-imports-which-word))
                (when prop
                  (skip-chars-backward prop)
                  (push (cons prop (point)) children)))
              (cond ((char-equal delimiter ?:)
                     (skip-chars-forward "\s\t\n")
                     (when (looking-at-p js-imports-regexp-name-set)
                       (skip-chars-forward js-imports-regexp-name)
                       (skip-chars-forward "\s\t\n."))
                     (when (looking-at-p "=>")
                       (forward-char 2)
                       (skip-chars-forward "\s\t\n"))
                     (if (looking-at-p "[({[]")
                         (progn
                           (while (looking-at-p "[({[]")
                             (forward-list)
                             (skip-chars-forward "\s\t\n")
                             (when (looking-at-p "=>")
                               (forward-char 2))
                             (skip-chars-forward "\s\t\n.")
                             (skip-chars-forward js-imports-regexp-name)))
                       (re-search-forward "," nil t 1)))
                    ((char-equal delimiter ?\()
                     (backward-char)
                     (forward-list)
                     (skip-chars-forward "\s\t\n")
                     (forward-list)
                     (forward-char))
                    ((char-equal delimiter ?,)
                     (progn
                       (save-excursion
                         (skip-chars-forward "\s\t\n")
                         (when-let ((word (js-imports-which-word))
                                    (pos (point)))
                           (skip-chars-forward word)
                           (skip-chars-forward "\s\t\n")
                           (push (cons word pos) children))))))))))
      (when children
        (seq-uniq (cl-remove 'null children))))))

(defun js-imports-parse-destructive ()
  "Return object values."
  (when-let ((start (when (looking-at-p "{") (point)))
             (end (save-excursion (forward-list) (point))))
    (save-excursion
      (save-restriction
        (let (parent children)
          (goto-char end)
          (when (looking-at-p "[\s\t\n]*=[\s\t\n]*")
            (save-excursion
              (re-search-forward "[\s\t\n]*=[\s\t\n]*" nil t 1)
              (setq parent (js-imports-which-word))))
          (narrow-to-region (1+ start) (1- end))
          (js-imports-remove-comments)
          (skip-chars-backward "},\s\t\n")
          (push (js-imports-maybe-make-child-at-point parent) children)
          (while (re-search-backward "," nil t 1)
            (skip-chars-backward "},\s\t\n")
            (push (js-imports-maybe-make-child-at-point parent) children))
          (seq-remove 'null children))))))

(defun js-imports-skip-reserved-words (&optional separators)
  (unless separators (setq separators "\s\t\\*"))
  (let* ((stack)
         (prev)
         (word))
    (while (and
            (not (equal prev (point)))
            (js-imports-reserved-word-p (setq word (js-imports-which-word))))
      (setq prev (point))
      (skip-chars-forward word)
      (js-imports-skip-whitespace-forward)
      (skip-chars-forward separators)
      (push (cons word prev) stack))
    (when-let ((id (js-imports-which-word)))
      (push (cons id (point)) stack))
    (seq-remove 'null stack)))

(defun js-imports-kill-thing-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun js-imports-import-backward-exist-p (path)
  (re-search-backward (concat "from +['\"]" path "['\"]") nil t))

(defun js-imports-join-names (symbols)
  (when (and (listp symbols) (<= 1 (length symbols)))
    (string-join symbols ", ")))

(defun js-imports-join-imports-names (default-name names)
  (let (parts)
    (when (stringp names) (push (concat "{ " names" }") parts))
    (when (stringp default-name) (push default-name parts))
    (js-imports-join ", " (seq-remove 'null parts))))

(defun js-imports-goto-last-import ()
  (goto-char (point-min))
  (while (js-imports-re-search-forward js-imports-regexp-import-keyword nil t)
    (re-search-forward "['\"]" nil t 2)
    (forward-line 1))
  (point))

(defun js-imports-get-es-imports-bounds ()
  "Return a cons with bounds of import stament of PATH."
  (save-excursion
    (goto-char (point-min))
    (let (alist)
      (while (js-imports-re-search-forward
              js-imports-regexp-import-keyword nil t)
        (let ((beg (match-beginning 0))
              (end))
          (js-imports-skip-whitespace-forward)
          (unless (looking-at-p "[\"']")
            (js-imports-re-search-forward js-imports-from-keyword--re nil t 1)
            (js-imports-skip-whitespace-forward))
          (when (looking-at-p "[\"']")
            (forward-sexp 1)
            (setq end (point))
            (js-imports-skip-whitespace-forward)
            (when (looking-at ";")
              (setq end (1+ (point))))
            (push (cons beg end)
                  alist))))
      (reverse alist))))

(defun js-imports-inside-import-p (&optional position)
  "Return import bounds if POSITION inside import statement."
  (let ((pos (or position (point)))
        (imports (js-imports-get-es-imports-bounds))
        (result))
    (setq result (seq-find (lambda (it) (and (>= pos (car it))
                                             (<= pos (cdr it))))
                           imports))
    result))

(defun js-imports-extract-import-path-bounds (&optional import-bounds)
  "Return path of import statement specified in IMPORT-BOUNDS."
  (when-let ((end (cdr import-bounds)))
    (save-excursion
      (goto-char end)
      (skip-chars-backward ";\s\t\n")
      (js-imports-skip-whitespace-backward)
      (when (looking-back "[\"']" 1)
        (let ((p2 (1- (point)))
              (p1))
          (backward-sexp 1)
          (setq p1 (1+ (point)))
          (cons p1 p2))))))

(defun js-imports-get-import-positions (path)
  "Return a cons with bounds of import stament of PATH."
  (save-excursion
    (let ((pos1 (point-min))
          (pos2 (js-imports-goto-last-import)))
      (when (js-imports-import-backward-exist-p path)
        (re-search-forward "['\"]+;?" nil t 2)
        (setq pos2 (point))
        (re-search-backward js-imports-regexp-import-keyword nil t)
        (setq pos1 (point)))
      (cons pos1 pos2))))

(defun js-imports-make-item (candidate &rest plist)
  (apply 'js-imports-propertize candidate plist))

(defun js-imports-propertize (item &rest properties)
  "Stringify and `propertize' ITEM with PROPERTIES."
  (cl-loop for (k v) on properties by 'cddr
           if v append (list k v) into props
           finally return
           (apply 'propertize
                  (js-imports-stringify item)
                  props)))

(defun js-imports-get-prop (str property)
  "Return the value of zero position's PROPERTY in STR."
  (if (stringp str)
      (get-text-property 0 property str)))

(defun js-imports-strip-text-props (item)
  "If ITEM is string, return it without text properties.
   If ITEM is symbol, return it is `symbol-name.'
   Otherwise return nil."
  (cond ((stringp item)
         (set-text-properties 0 (length item) nil item)
         item)
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-imports-stringify (x)
  "Convert any object to string."
  (cl-typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun js-imports-skip-whitespace-forward ()
  (skip-chars-forward "[\s\t\n]")
  (when (and (looking-at "\\(//\\)\\|\\(/[*]\\)")
             (js-imports-re-search-forward "." nil t 1))
    (unless (>= (point-min) (point))
      (backward-char 1))
    (skip-chars-forward "[\s\t\n]")))

(defun js-imports-skip-whitespace-backward ()
  (skip-chars-backward "[\s\t\n]")
  (when (and (looking-back "\\(//\\)\\|\\(/[*]\\)" 1)
             (js-imports-re-search-backward "." nil t 1))
    (unless (>= (point-min) (point))
      (forward-char 1))
    (skip-chars-backward "[\s\t\n]")))

(defun js-imports-get-word-if-valid ()
  "Return word at point if it is valid and not reservered, otherwise nil."
  (when-let* ((word (js-imports-which-word))
              (valid (js-imports-valid-identifier-p word)))
    word))

(defun js-imports-looking-at (str)
  (when-let ((word (js-imports-which-word)))
    (string= word str)))

(defun js-imports-which-word (&optional re)
  "Find closest to point whole word."
  (unless re (setq re js-imports-regexp-name))
  (unless (= (point) (point-max))
    (save-excursion
      (let (p1 p2 word)
        (save-excursion
          (skip-chars-backward re)
          (setq p1 (point))
          (right-char)
          (skip-chars-forward re)
          (setq p2 (point)))
        (when (< p1 (point))
          (goto-char p1))
        (setq word (buffer-substring-no-properties p1 p2))
        word))))

(defun js-imports-get-path-at-point ()
  (save-excursion
    (when-let* ((word (js-imports-which-word))
                (meta-word (or (string= "import" word)
                               (string= "export" word)
                               (string= "from" word))))
      (if (string= word "from")
          (search-forward-regexp "['\"]" nil t 1)
        (search-forward-regexp "[\s\t\n]+from[\s\t\n]+['\"]" nil t 1)))
    (when (js-imports-inside-string-p)
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (let (p0 p1 p2 stops)
          (setq stops "^ \t\n\"`'‘’“”|[]{}·")
          (setq p0 (point))
          (skip-chars-backward stops)
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward stops)
          (setq p2 (point))
          (goto-char p0)
          (buffer-substring-no-properties p1 p2))))))

(defun js-imports-string-match-p (regexp str &optional start)
  "Return t if STR matches REGEXP, otherwise return nil."
  (when (and (not (null str)) (stringp str))
    (not (null (string-match-p regexp str start)))))

(defun js-imports-join (separator strings)
  "Join strings in STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

(defun js-imports-string-contains-p (needle str &optional ignore-case)
  "Return t if STR contains NEEDLE, otherwise return nil.
If IGNORE-CASE is non-nil, the comparison will ignore case differences."
  (let ((case-fold-search ignore-case))
    (not (null (string-match-p (regexp-quote needle) str)))))

(defun js-imports-string-blank-p (str)
  "Return t if STR is nil or empty, otherwise return nil."
  (or (null str) (string= "" str)))

(defun js-imports-count-matches (regexp str &optional start end)
  "Count occurrences of REGEXP in STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))

(defun js-imports-inside-string-p (&optional pos)
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (with-syntax-table js-imports-mode-syntax-table
    (nth 3 (syntax-ppss (or pos (point))))))

(defun js-imports-skip-string ()
  (with-syntax-table js-imports-mode-syntax-table
    (while (js-imports-inside-string-p)
      (forward-char))))

(defun js-imports-inside-comment-p ()
  "Return value of comment character in syntax table's or nil otherwise."
  (with-syntax-table js-imports-mode-syntax-table
    (let ((comment-start "//")
          (comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
          (comment-use-syntax t)
          (result (nth 4 (syntax-ppss))))
      result)))

(defun js-imports-invalid-name-p (str)
  "Return t when STR mathces any characters which are not allowed in js."
  (js-imports-string-match-p (concat "[" "^" js-imports-regexp-name "]") str))

(defun js-imports-valid-identifier-p (str)
  "Return t if STR is a valid variable name, otherwise nil."
  (not (or
        (null str)
        (js-imports-invalid-name-p str)
        (js-imports-reserved-word-p str))))

(defun js-imports-generate-name-from-path (path)
  "Generate name for default or module import from PATH."
  (let* ((split-path (lambda (str) (split-string str "[ \f\t\n\r\v/.-]")))
         (map-capitalize (lambda (p) (mapcar 'capitalize p)))
         (take-two (lambda (parts) (seq-take parts 2))))
    (js-imports-compose-from path
                             'string-join
                             take-two
                             'reverse
                             map-capitalize
                             'seq-uniq
                             split-path
                             'js-imports-maybe-remove-path-index
                             'js-imports-remove-ext)))

(defun js-imports-compose-from (arg &rest funcs)
  "Performs right-to-left unary function composition."
  (seq-reduce (lambda (xs fn)
                (funcall fn xs))
              (reverse funcs) arg))

(defun js-imports-maybe-make-child-at-point (&optional parent)
  (when-let ((valid-id (js-imports-get-word-if-valid)))
    (skip-chars-backward valid-id)
    (skip-chars-backward "\s\t\n")
    (js-imports-make-item valid-id
                          :pos (point)
                          :real-name (and (looking-back ":" 0)
                                          (save-excursion
                                            (backward-char 1)
                                            (js-imports-get-word-if-valid)))
                          :parent parent
                          :as-name valid-id)))

(cl-defun js-imports-highlight-word (&key pos limit buffer face secs jump)
  "Jumps to BEG and highlight word at point."
  (unless buffer (setq buffer (current-buffer)))
  (setq buffer (get-buffer-create buffer))
  (unless pos (setq pos (point)))
  (when (and jump (not (= pos (point))))
    (goto-char pos))
  (unless face (setq face 'js-imports-highlight-face))
  (with-current-buffer buffer
    (let* ((buffer-name (if (bufferp buffer) (intern (buffer-name buffer))
                          (intern buffer)))
           (end (+ pos (or limit (length (js-imports-which-word)))))
           (overlay (get buffer-name 'overlay)))
      (when overlay
        (delete-overlay overlay))
      (setq overlay (make-overlay pos end buffer))
      (put buffer-name 'overlay overlay)
      (overlay-put overlay 'face face)
      (unwind-protect
          (progn
            (when (and overlay (overlayp overlay))
              (move-overlay overlay pos end)))
        (run-with-timer (or 1 secs) nil (lambda (o) (when (and (not (null o))
                                                               (overlayp o))
                                                      (delete-overlay o)))
                        overlay)))))

(defun js-imports-extract-parent-arguments (&optional parens-positions)
  (save-excursion
    (with-syntax-table js-imports-mode-syntax-table
      (let ((open-parens (or parens-positions (nth 9 (syntax-ppss (point)))))
            (children))
        (dotimes (idx (length open-parens))
          (let ((paren-pos (nth idx open-parens))
                (parent)
                (items))
            (goto-char paren-pos)
            (skip-chars-backward "\s\t\n")
            (setq parent
                  (save-excursion
                    (js-imports-re-search-backward
                     js-imports-delcaration-keywords--re nil t 1)
                    (when (looking-at js-imports-delcaration-keywords--re)
                      (skip-chars-forward (js-imports-which-word))
                      (skip-chars-forward "\s\t\n*")
                      (setq parent (js-imports-get-word-if-valid)))))
            (when parent
              (when (looking-back "=>" 1)
                (backward-char 2)
                (skip-chars-backward "\s\t\n")
                (when-let ((child
                            (js-imports-maybe-make-child-at-point parent)))
                  (push child items)))
              (when-let ((end-pos (when (save-excursion
                                          (backward-char 1) (looking-at ")"))
                                    (point)))
                         (start (progn (backward-list 1) (point))))
                (forward-char)
                (skip-chars-forward "\s\t\n")
                (cond ((looking-at "{")
                       (setq items (append
                                    items
                                    (js-imports-parse-destructive))))
                      ((looking-at js-imports-regexp-name-set)
                       (goto-char (1- end-pos))
                       (skip-chars-backward ",\s\t\n")
                       (while (and (or (looking-at ",")
                                       (looking-at ")"))
                                   (not (looking-at "(")))
                         (if (looking-at ",")
                             (skip-chars-backward "\s\t\n,")
                           (skip-chars-backward "\s\t\n"))
                         (let ((child
                                (js-imports-maybe-make-child-at-point
                                 parent)))
                           (push child items)
                           (skip-chars-backward child)
                           (skip-chars-backward "\s\t\n"))
                         (unless
                             (js-imports-re-search-backward
                              "," start t 1)
                           (goto-char start))))))
              (setq children (append children items)))))
        children))))

(defun js-imports-next-declaration-or-scope (&optional pos)
  (unless pos (setq pos (point)))
  (let (declaration-start
        scope-start
        scope-end
        winner
        init-depth
        declaration-depth)
    (save-excursion
      (unless (> pos (point-max))
        (goto-char pos))
      (with-syntax-table js-imports-mode-syntax-table
        (setq init-depth (nth 0 (syntax-ppss (point))))
        (when-let ((found
                    (js-imports-re-search-forward
                     js-imports-open-paren-re nil t 1)))
          (backward-char 1)
          (while (> (nth 0 (syntax-ppss (point))) init-depth)
            (backward-char 1))
          (setq scope-start (point))
          (forward-list)
          (setq scope-end
                (if (looking-at ";")
                    (1+ (point))
                  (point)))
          (progn (goto-char scope-start)
                 (js-imports-skip-whitespace-backward)
                 (and (char-equal (char-before (point)) ?>)
                      (char-equal (char-before (1- (point))) ?=)
                      (js-imports-re-search-backward
                       js-imports-expression-keywords--re nil t 1))
                 (forward-word)
                 (js-imports-skip-whitespace-forward)
                 (when (js-imports-valid-identifier-p (js-imports-which-word))
                   (setq scope-start (point)))))))
    (save-excursion
      (goto-char pos)
      (with-syntax-table js-imports-mode-syntax-table
        (save-excursion
          (when (looking-at js-imports-delcaration-keywords--re)
            (skip-chars-forward js-imports-regexp-name))
          (when (js-imports-re-search-forward
                 js-imports-delcaration-keywords--re nil t 1)
            (if (js-imports-inside-string-p)
                (js-imports-skip-string)
              (progn (setq declaration-depth (nth 0 (syntax-ppss)))
                     (skip-chars-backward js-imports-regexp-name)
                     (setq declaration-start (point))))))))
    (if declaration-start
        (setq winner (if (and scope-start scope-end
                              (or (and (< scope-start declaration-start)
                                       (> scope-end declaration-start))
                                  (> declaration-depth init-depth)))
                         scope-end
                       declaration-start))
      (setq winner scope-end))
    (when winner (goto-char winner))))

(defun js-imports-previous-declaration-or-skope (&optional pos)
  (unless pos (setq pos (point)))
  (let (declaration-start scope-start scope-end winner)
    (with-syntax-table js-imports-mode-syntax-table
      (goto-char pos)
      (when (js-imports-re-search-backward js-imports-closed-paren-re nil t 1)
        (setq scope-end (1+ (point)))
        (when (nth 1 (syntax-ppss (point)))
          (goto-char (nth 1 (syntax-ppss (point))))
          (setq scope-start (point))
          (skip-chars-backward "\s\t\n,"))))
    (with-syntax-table js-imports-mode-syntax-table
      (goto-char pos)
      (when (and (js-imports-re-search-backward
                  js-imports-delcaration-keywords--re nil t 1)
                 (not (js-imports-inside-string-p)))
        (unless (js-imports-inside-string-p)
          (setq declaration-start (point)))))
    (goto-char pos)
    (if (and declaration-start scope-end scope-start)
        (setq winner (if (> scope-end declaration-start)
                         scope-start
                       declaration-start))
      (setq winner (or scope-start declaration-start)))
    (when winner (goto-char winner))))

(defun js-imports-previous-declaration (&optional pos)
  (let ((init-pos (or pos (point)))
        (curr-pos))
    (goto-char init-pos)
    (while (and (not curr-pos)
                (js-imports-previous-declaration-or-skope))
      (when (js-imports-declaration-at-point)
        (setq curr-pos (point))))
    (when (null curr-pos)
      (goto-char init-pos))
    (unless (equal curr-pos init-pos)
      curr-pos)))

(defun js-imports-looking-at-function-expression ()
  (and (js-imports-looking-at "function")
       (< (point-min) (point))
       (save-excursion
         (js-imports-skip-whitespace-backward)
         (let ((c (char-before (point))))
           (or (char-equal c ?=)
               (char-equal c ?|)
               (char-equal c ??)
               (char-equal c ?:))))))

(defun js-imports-declaration-at-point ()
  (when (and (looking-at js-imports-delcaration-keywords--re)
             (not (js-imports-looking-at-function-expression)))
    (let ((var-type (js-imports-which-word)))
      (save-excursion
        (js-imports-re-search-forward var-type nil t 1)
        (when (looking-at "\\*")
          (forward-char 1))
        (js-imports-skip-whitespace-forward)
        (when (looking-at "\\*")
          (forward-char 1)
          (js-imports-skip-whitespace-forward))
        (when-let* ((word (js-imports-which-word))
                    (pos (point)))
          (if (js-imports-valid-identifier-p word)
              (js-imports-make-item word
                                    :pos pos
                                    :var-type var-type
                                    :real-name word
                                    :as-name word)
            (cond
             ((string= word "{")
              (let (parent children display-path)
                (when (looking-at-p "{")
                  (save-excursion
                    (forward-list)
                    (when (looking-at-p "[\s\t\n]*=[\s\t\n]*")
                      (js-imports-re-search-forward
                       "[\s\t\n]*=[\s\t\n]*" nil t 1)
                      (setq parent (js-imports-which-word))
                      (setq display-path
                            (and (string= parent "require")
                                 (js-imports-re-search-forward
                                  "([\s\t]*" nil t 1)
                                 (looking-at "['\"]")
                                 (buffer-substring-no-properties
                                  (1+ (point))
                                  (progn
                                    (forward-sexp 1)
                                    (1- (point))))))))
                  (setq children (js-imports-parse-destructive))
                  (when display-path
                    (setq children (mapcar
                                    (lambda (it)
                                      (js-imports-propertize
                                       it
                                       :var-type nil
                                       :parent parent
                                       :import t
                                       :display-path display-path
                                       :type 4))
                                    children))))
                children)))))))))

(defun js-imports-extract-definitions (&optional path position)
  "Extract visible on POSITION declarations in PATH.
By default PATH is taken from a variable `buffer-file-name'.
Default value for POSITION also current point position."
  (unless path (setq path buffer-file-name))
  (unless position (setq position (if (equal path buffer-file-name)
                                      (point)
                                    (point-max))))
  (let (ids depth depth-position)
    (js-imports-with-buffer-or-file-content path
        (goto-char position)
      (setq ids (ignore-errors (js-imports-extract-parent-arguments)))
      (skip-chars-forward js-imports-regexp-name)
      (while (js-imports-previous-declaration)
        (unless depth
          (setq depth-position (point))
          (setq depth (nth 0 (syntax-ppss depth-position))))
        (when-let ((decl (js-imports-declaration-at-point)))
          (if (listp decl)
              (setq ids (append ids decl))
            (push decl ids))))
      (unless (or (null depth) (null depth-position))
        (save-excursion
          (goto-char depth-position)
          (while (ignore-errors (js-imports-next-declaration-or-scope))
            (when-let ((decl (js-imports-declaration-at-point)))
              (if (listp decl)
                  (setq ids (append ids decl))
                (push decl ids)))))))
    (mapcar (lambda (it) (js-imports-propertize it :real-path path))
            ids)))

(defun js-imports-find-definition (item)
  (let ((stack)
        (current-item item))
    (while (and current-item
                (not (js-imports-get-prop current-item :var-type)))
      (let ((item-path (js-imports-get-prop current-item :real-path)))
        (push current-item stack)
        (setq current-item
              (cond ((js-imports-get-prop current-item :import)
                     (when-let* ((from-path (js-imports-get-prop current-item
                                                                 :display-path))
                                 (dir (js-imports-dirname item-path))
                                 (path (js-imports-path-to-real from-path dir))
                                 (exports (js-imports-extract-all-exports path))
                                 (item-type (js-imports-get-prop
                                             current-item :type))
                                 (export-name (js-imports-get-prop
                                               current-item :real-name)))
                       (pcase item-type
                         (1 (js-imports-find-by-prop :type item-type exports))
                         (4 (js-imports-find-by-prop :as-name export-name
                                                     exports)))))
                    ((and (js-imports-get-prop current-item :display-path)
                          (js-imports-get-prop current-item :export))
                     (when-let* ((from-path (js-imports-get-prop current-item
                                                                 :display-path))
                                 (dir (js-imports-dirname item-path))
                                 (path (js-imports-path-to-real from-path dir))
                                 (exports (js-imports-extract-all-exports path))
                                 (export-name (js-imports-get-prop
                                               current-item :real-name))
                                 (item-type (if (equal export-name "default") 1
                                              (js-imports-get-prop
                                               current-item :type))))
                       (pcase item-type
                         (1 (js-imports-find-by-prop :type item-type exports))
                         (4 (js-imports-find-by-prop :as-name export-name
                                                     exports)))))
                    ((js-imports-get-prop current-item :export)
                     (let* ((path (js-imports-get-prop current-item :real-path))
                            (pos (js-imports-get-prop current-item :pos))
                            (definitions (js-imports-extract-definitions
                                          path pos))
                            (real-name (js-imports-get-prop
                                        current-item :real-name)))
                       (or (js-imports-find-by-prop
                            :real-name real-name definitions)
                           (js-imports-find-by-prop
                            :as-name real-name
                            (js-imports-extract-es-imports path))))))))
      current-item)
    (setq current-item (or current-item (pop stack)))
    (if (and current-item stack)
        (js-imports-propertize current-item :stack stack)
      current-item)))

(defun js-imports-jump-to-item-in-buffer (item &optional buffer)
  "Jumps to ITEM in buffer. ITEM must be propertized with a keyword `pos'."
  (when-let ((pos (js-imports-get-prop item :pos)))
    (js-imports-highlight-word :pos pos :buffer buffer :jump t)
    item))

(defun js-imports-jump-to-item-other-window (item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop :pos."
  (unless (js-imports-get-prop item :pos)
    (setq item (js-imports-display-to-real-exports item)))
  (when-let ((pos (js-imports-get-prop item :pos))
             (item-path (or (js-imports-get-prop item :real-path)
                            (js-imports-path-to-real (js-imports-get-prop
                                                      item
                                                      :display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (js-imports-jump-to-item-in-buffer item)
    item))

(defun js-imports-transform-symbol (c &optional margin)
  (let* ((display-path (js-imports-get-prop c :display-path))
         (export (and (js-imports-get-prop c :export)
                      (if display-path
                          "Reexport"
                        "Export")))
         (import (and (js-imports-get-prop c :import)
                      "Import"))
         (var (js-imports-get-prop c :var-type))
         (default (and (equal
                        (js-imports-get-prop c :type)
                        1)
                       "Default"))
         (parts (string-trim
                 (mapconcat
                  (lambda (it) (propertize (capitalize
                                            (string-trim it))
                                           'face
                                           'font-lock-function-name-face))
                  (seq-remove 'null (list
                                     export
                                     import
                                     default
                                     var))
                  "\s")))
         ;; (indents (make-string (or margin 1) ?\s))
         (name (replace-regexp-in-string " default$" ""
                                         (or (js-imports-get-prop c :as-name)
                                             c)))
         (path-part (cond ((and import)
                           (concat "from\s" display-path))
                          (t "")))
         (result (string-join
                  (seq-remove 'null (list
                                     (and margin (make-string margin ?\s))
                                     (string-trim (or parts ""))
                                     name
                                     path-part))
                  "\s")))
    (apply 'propertize (append (list result)
                               (text-properties-at 0 c)))))

(defun js-imports-map-stack (cands)
  (seq-map-indexed
   'js-imports-transform-symbol
   (seq-remove 'null cands)))

(defun js-imports-find-export-definition (export-symbol)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop :pos."
  (unless (js-imports-get-prop export-symbol :pos)
    (setq export-symbol (js-imports-display-to-real-exports export-symbol)))
  (when-let* ((definition (js-imports-find-definition export-symbol))
              (pos (js-imports-get-prop definition :pos))
              (item-path (or (js-imports-get-prop definition :real-path)
                             (js-imports-path-to-real (js-imports-get-prop
                                                       definition
                                                       :display-path)))))
    (find-file-other-window item-path)
    (js-imports-jump-to-item-in-buffer definition)
    definition))

(defun js-imports-jump-to-symbol-action (it)
  (when-let ((item (js-imports-find-definition it)))
    (let ((stack (js-imports-get-prop item :stack)))
      (when stack
        (push item stack)
        (setq stack (js-imports-compose-from stack
                                             'js-imports-map-stack
                                             'reverse))
        (setq item
              (completing-read "Jump:\s" stack nil t))))
    (when-let ((pos (and item (js-imports-get-prop
                               item :pos))))
      (find-file
       (js-imports-get-prop item :real-path))
      (goto-char pos)
      (js-imports-highlight-word))))

(defun js-imports-imported-candidates-in-buffer (&optional buffer)
  "Returns imported symbols in BUFFER which are cached and stored
in a buffer local variable `js-imports-cached-imports-in-buffer'.
   Cache are invalidated when `buffer-modified-tick' is changed."
  (with-current-buffer (or buffer js-imports-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-imports-buffer-tick tick)
          js-imports-cached-imports-in-buffer
        (progn
          (setq js-imports-buffer-tick tick)
          (setq js-imports-cached-imports-in-buffer
                (seq-remove 'js-imports-reserved-word-p
                            (js-imports-extract-es-imports buffer-file-name)))
          js-imports-cached-imports-in-buffer)))))

(defun js-imports-exported-candidates-transformer (candidates)
  "Remove duplicates and imported members from CANDIDATES plist."
  (with-current-buffer js-imports-current-buffer
    (let (imports exports)
      (setq imports
            (js-imports-filter-with-prop
             :display-path
             js-imports-current-export-path
             (js-imports-imported-candidates-in-buffer
              js-imports-current-buffer)))
      (setq exports (if imports (js-imports-filter-exports candidates imports)
                      candidates))
      (setq exports (cl-remove-duplicates exports))
      (setq exports (mapcar (lambda (c) (js-imports-propertize
                                         c :display-path
                                         js-imports-current-export-path))
                            exports))
      exports)))

(defun js-imports-export-filtered-candidate-transformer (candidates
                                                         &optional
                                                         _source)
  (mapcar (lambda (c) (js-imports-get-prop c :as-name))
          candidates))

(defun js-imports-filter-with-prop (property value items)
  "Return filtered ITEMS with members whose PROPERTY equals VALUE."
  (let ((comparator (cond ((numberp value) '=)
                          ((stringp value) 'string=)
                          (t 'equal))))
    (seq-filter (lambda (str) (funcall comparator
                                  (js-imports-get-prop str property)
                                  value))
                items)))

(defun js-imports-find-by-prop (property value list)
  "Find item in LIST whose PROPERTY equals VALUE."
  (seq-find (lambda (str) (equal (js-imports-get-prop str property) value))
            list))

(defun js-imports-preselect-symbol ()
  "Preselect function for symbols."
  (or (when-let ((pos (> (point-max) (point)))
                 (symbol (js-imports-which-word)))
        (unless (or (js-imports-invalid-name-p symbol)
                    (js-imports-reserved-word-p symbol))
          symbol))
      ""))

(defun js-imports-display-to-real-exports (str)
  "Find STR in the variable `js-imports-export-candidates-in-path'."
  (with-current-buffer js-imports-current-buffer
    (seq-find (lambda (elt) (string= str (js-imports-get-prop elt :as-name)))
              js-imports-export-candidates-in-path)))

(defun js-imports-display-to-real-imports (item)
  "Find ITEM in the variable `js-imports-cached-imports-in-buffer.'."
  (with-current-buffer js-imports-current-buffer
    (seq-find (lambda (elt) (equal elt item))
              js-imports-cached-imports-in-buffer item)))

(defun js-imports-filter-exports (exports imports)
  "Return EXPORTS plist with only those members that are not in IMPORTS plist."
  (let* ((find-in-imports (lambda (key value) (js-imports-find-by-prop key value
                                                                  imports)))
         (by-type (lambda (elt) (let ((type (js-imports-get-prop elt :type))
                                      (name (js-imports-get-prop elt :as-name)))
                                  (pcase type
                                    (1
                                     (funcall find-in-imports :type type))
                                    (4
                                     (funcall find-in-imports :as-name name))
                                    (16 (< 0 (length imports))))))))
    (seq-remove by-type exports)))

(defun js-imports-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js-imports-re-search-forward'."
  (let ((parse)
        str-terminator)
    (while (> count 0)
      (with-syntax-table js-imports-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((setq str-terminator (nth 3 parse))
               (when (eq str-terminator t)
                 (setq str-terminator ?/))
               (re-search-forward
                (concat "\\([^\\]\\|^\\)" (string str-terminator))
                (point-at-eol) t))
              ((nth 7 parse)
               (forward-line))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
               (re-search-forward "\\*/"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-imports-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring comments and strings."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js-imports-re-search-backward-inner)
               ((> count 0) #'js-imports-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun js-imports-re-search-backward-inner (regexp &optional bound count)
  "Helper for `js-imports-re-search-backward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table js-imports-mode-syntax-table
        (re-search-backward regexp bound)
        (when (and (> (point) (point-min))
                   (save-excursion (backward-char) (looking-at "/[/*]")))
          (forward-char))
        (setq parse (syntax-ppss))
        (cond ((nth 8 parse)
               (goto-char (nth 8 parse)))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?/) (eq (char-after) ?*)))
               (re-search-backward "/\\*"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-imports-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring strings and comments."
  (js-imports-re-search-forward regexp bound noerror (if count (- count) -1)))

(defun js-imports-remove-comments (&optional buffer-start buffer-end)
  "Replaces comments in buffer beetween START and END with empty lines."
  (let ((comments (js-imports-get-comments-bounds buffer-start buffer-end))
        (cell))
    (save-excursion
      (while (setq cell (pop comments))
        (when-let* ((start (car cell))
                    (end (or (cdr cell)
                             (save-excursion (goto-char start)
                                             (end-of-line)
                                             (point)))))
          (let ((content (buffer-substring-no-properties start end))
                (lines))
            (setq lines (split-string content "\n"))
            (goto-char start)
            (delete-region start end)
            (insert (string-join (mapcar
                                  (lambda (it)
                                    (string-join
                                     (append
                                      (make-vector
                                       (1+ (length it)) "")
                                      nil)
                                     "\s"))
                                  lines) "\n"))))))))

(defun js-imports-get-comments-bounds (&optional start end)
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (with-syntax-table js-imports-mode-syntax-table
        (let (comments)
          (goto-char start)
          (while (re-search-forward "\\(/\\*\\)\\|\\(//\\)\\|[\"'`]" nil t 1)
            (if (save-excursion
                  (backward-char 1)
                  (looking-at "[\"'`]"))
                (js-imports-skip-string)
              (save-excursion
                (backward-char 1)
                (if (looking-at-p "\\*")
                    (progn
                      (let* ((p1 (1- (point)))
                             (p2 (re-search-forward "\\(\\*/\\)" nil t 1)))
                        (push (cons p1 p2) comments)))
                  (let* ((p1 (1- (point)))
                         (p2 (point-at-eol)))
                    (push (cons p1 p2) comments))))))
          comments)))))

(defun js-imports-rename-import (candidate)
  "Rename imported CANDIDATE in buffer."
  (save-excursion
    (save-restriction
      (pcase (js-imports-get-prop candidate :type)
        (1 (js-imports-rename-default-item candidate))
        (4 (js-imports-rename-as candidate))
        (16 (js-imports-rename-as candidate))))))

(defun js-imports-rename-default-item (item)
  "Renames default imported ITEM."
  (let (real-name new-name overlay end beg)
    (setq real-name (or (js-imports-get-prop item :real-name)
                        (js-imports-strip-text-props item)))
    (setq beg (js-imports-get-prop item :pos))
    (goto-char beg)
    (when (string= real-name (js-imports-which-word))
      (setq end (+ (point) (length (js-imports-which-word))))
      (unwind-protect
          (progn (setq overlay (make-overlay beg end))
                 (make-overlay beg end)
                 (overlay-put overlay 'face 'ag-match-face)
                 (setq new-name (read-string
                                 "Rename %s to" (concat "\s" real-name)))
                 (setq new-name (string-trim new-name))
                 (if (string-blank-p new-name)
                     (progn
                       (remove-overlays beg end)
                       (let ((case-fold-search nil)
                             (regexp (concat "\\_<" real-name "\\_>")))
                         (query-replace-regexp regexp new-name)))))
        (remove-overlays beg end)))))

(defun js-imports-rename-as (item)
  "Rename named imports and module imports."
  (let* ((pos (js-imports-get-prop item :pos))
         (full-name (js-imports-strip-text-props item))
         (parts (split-string full-name))
         (real-name (nth 0 parts))
         (as-word (nth 1 parts))
         (renamed-name (nth 2 parts))
         (prompt (if as-word
                     (format "Rename %s %s" real-name as-word)
                   (format "Rename %s as" real-name)))
         (input (concat "\s" renamed-name))
         (new-name (string-trim (read-string prompt input))))
    (when (and (not (string-blank-p new-name))
               pos
               (goto-char pos)
               (string= real-name (js-imports-which-word)))
      (skip-chars-forward real-name)
      (if as-word
          (progn
            (skip-chars-forward "\s\t\n")
            (skip-chars-forward "as")
            (skip-chars-forward "\s\t\n")
            (when (and renamed-name
                       (string= renamed-name (js-imports-which-word)))
              (query-replace-regexp (concat "\\_<" renamed-name "\\_>")
                                    new-name)))
        (progn
          (insert (format " as %s" new-name))
          (query-replace-regexp (concat "\\_<" real-name "\\_>") new-name))))))

(defun js-imports-items-from-string (&optional str)
  (let* ((parts (split-string str "{"))
         (default (unless (or
                           (null (nth 0 parts))
                           (string-blank-p (nth 0 parts)))
                    (nth 0 parts)))
         (default-type (unless (null default)
                         (if (string-match-p "\\*" default)
                             16 1)))
         (named (unless (or
                         (null (nth 1 parts))
                         (string-blank-p (nth 1 parts)))
                  (nth 1 parts)))
         (default-as-name (and (numberp default-type)
                               (car (cdr (split-string default))))))
    (when (and default-as-name
               (not (js-imports-valid-identifier-p default-as-name)))
      (setq default-as-name nil))
    (when (and default-as-name
               (= default-type 16))
      (setq default (format "* as %s" default-as-name)))
    (list default named)))

(defun js-imports-ivy-insert-or-view-export (item)
  "Add ITEM into existing or new import statement."
  (if-let* ((real (js-imports-display-to-real-exports item))
            (definition (js-imports-find-definition real))
            (exit (and (boundp 'ivy-exit)
                       (not ivy-exit))))
      (progn (view-file-other-window
              (js-imports-get-prop definition
                                   :real-path))
             (goto-char (js-imports-get-prop definition :pos))
             (js-imports-highlight-word))
    (js-imports-insert-import item)))

(defun js-imports-insert-import (candidate)
  "Insert CANDIDATE into existing or new import statement."
  (save-excursion
    (if-let* ((real (js-imports-display-to-real-exports candidate))
              (type (or (js-imports-get-prop real :type)))
              (display-path js-imports-last-export-path)
              (as-name (js-imports-get-prop real :as-name))
              (default-name (if (or (= type 4)
                                    (js-imports-valid-identifier-p as-name))
                                as-name
                              (js-imports-generate-name-from-path
                               display-path)))
              (prompt (pcase type
                        (1 (format "Import default as (default %s)\s"
                                   default-name))
                        (4 (format "Import (default %s)\s"
                                   as-name))
                        (16 (format "Import * as (default %s)\s"
                                    default-name))))
              (confirmed-name (read-string
                               prompt nil nil default-name))
              (full-name (pcase type
                           (1 confirmed-name)
                           (4 (if (string= default-name confirmed-name)
                                  default-name
                                (format "%s as %s" as-name confirmed-name)))
                           (16 (if (string= default-name confirmed-name)
                                   default-name
                                 (format "* as %s" confirmed-name))))))
        (pcase type
          (4 (js-imports-insert-exports nil full-name display-path))
          (_ (js-imports-insert-exports full-name nil display-path)))
      (let ((items (js-imports-items-from-string (format "%s" candidate))))
        (js-imports-insert-exports (pop items)
                                   (pop items)
                                   js-imports-last-export-path)))))

(defun js-imports-insert-exports (default-name named-list path)
  (let ((names (if (stringp named-list)
                   named-list
                 (js-imports-join-names named-list)))
        (imports (reverse (js-imports-find-imported-files))))
    (save-excursion
      (js-imports-goto-last-import)
      (if (member path imports)
          (js-imports-add-to-current-imports path default-name names)
        (let (module project-root)
          (setq project-root (js-imports-find-project-root))
          (cond ((js-imports-relative-p path)
                 (let* ((dir (file-name-directory path))
                        (pred (lambda (it) (string= dir
                                                    (file-name-directory it)))))
                   (setq module (seq-find pred imports))
                   (unless module
                     (setq module (seq-find 'js-imports-relative-p imports)))))
                ((js-imports-dependency-p path)
                 (when-let ((dependencies (js-imports-node-modules-candidates
                                           project-root)))
                   (setq module (seq-find (lambda (it) (member it dependencies))
                                          imports)))
                 (unless module
                   (goto-char (point-min))))
                (t (let ((pred (lambda (it) (not (js-imports-relative-p it)))))
                     (setq module (seq-find pred imports))
                     (unless module
                       (when-let* ((relative (seq-find 'js-imports-relative-p
                                                       imports))
                                   (bounds (js-imports-get-import-positions
                                            module)))
                         (goto-char (car bounds)))))))
          (when module
            (goto-char (cdr (js-imports-get-import-positions module)))
            (forward-line))
          (insert "import " (js-imports-join-imports-names
                             default-name names)
                  " from " js-imports-quote path js-imports-quote ";\n")))
      (js-imports-goto-last-import)
      (unless (looking-at-p "\n")
        (newline-and-indent)))))

(defun js-imports-add-to-current-imports (path default-name &optional names)
  (when-let* ((bounds (js-imports-get-import-positions path))
              (start (car bounds))
              (end (cdr bounds)))
    (save-excursion
      (save-restriction
        (goto-char start)
        (narrow-to-region start end)
        (forward-word)
        (js-imports-skip-whitespace-forward)
        (if (looking-at-p "\\*")
            (progn
              (goto-char end)
              (skip-chars-forward ";")
              (newline-and-indent)
              (insert "import " (js-imports-join-imports-names
                                 default-name names)
                      " from " js-imports-quote path js-imports-quote ";"))
          (when default-name
            (when (looking-at-p js-imports-regexp-name-set)
              (js-imports-kill-thing-at-point 'sexp))
            (insert default-name))
          (when (or (looking-at-p js-imports-regexp-name-set)
                    default-name)
            (skip-chars-forward js-imports-regexp-name-set)
            (js-imports-skip-whitespace-forward)
            (unless (looking-at-p ",")
              (js-imports-re-search-backward js-imports-regexp-name-set nil t 1)
              (forward-char 1)
              (insert ", "))
            (skip-chars-forward ",")
            (js-imports-skip-whitespace-forward))
          (when names
            (if (looking-at-p "{")
                (progn (js-imports-re-search-forward "}" nil t 1)
                       (backward-char 1)
                       (js-imports-skip-whitespace-backward)
                       (let ((separator (if (save-excursion (backward-char 1)
                                                            (looking-at-p ","))
                                            " "
                                          ", ")))
                         (insert separator names)))
              (insert "{" names "}\s"))))))))

(defun js-imports-delete-import-statetement (candidate)
  "Remove whole import statement of CANDIDATE.
CANDIDATE should be propertizied with property `display-path'."
  (when-let* ((path (js-imports-get-prop candidate :display-path))
              (bounds (js-imports-get-import-positions path)))
    (delete-region (car bounds) (cdr bounds))
    (join-line)))

(defun js-imports-delete-import-item (candidate)
  "Remove CANDIDATE from import statement in buffer."
  (let* ((display-path (js-imports-get-prop candidate :display-path))
         (type (js-imports-get-prop candidate :type))
         (other-imports (js-imports-filter-with-prop
                         :display-path display-path
                         js-imports-cached-imports-in-buffer))
         (whole-import-bounds (js-imports-get-import-positions display-path))
         (beg (car whole-import-bounds))
         (end (cdr whole-import-bounds)))
    (setq other-imports (remove candidate other-imports))
    (remove-overlays (car whole-import-bounds) (cdr whole-import-bounds))
    (unwind-protect
        (let (p1 p2 overlay)
          (if (or (= type 16)
                  (not other-imports))
              (progn
                (setq overlay (make-overlay beg end))
                (overlay-put overlay 'face 'js-imports-highlight-face)
                (when (yes-or-no-p "Delete whole import?")
                  (remove-overlays beg end)
                  (delete-region beg end)))
            (save-excursion
              (goto-char (js-imports-get-prop candidate :pos))
              (setq p1 (point))
              (re-search-forward candidate nil t 1)
              (setq p2 (point))
              (skip-chars-forward ",\s\n\t")
              (setq p2 (point))
              (when (looking-at-p "}")
                (setq p2 (point))
                (goto-char p1)
                (skip-chars-backward "\s\t\n")
                (backward-char)
                (when (looking-at-p "{")
                  (setq p2 (1+ p2))
                  (setq p1 (point))
                  (skip-chars-backward  "\s\t\n")
                  (backward-char))
                (when (looking-at-p ",")
                  (setq p1 (point))))
              (setq overlay (make-overlay p1 p2))
              (overlay-put overlay 'face 'js-imports-highlight-face)
              (when (yes-or-no-p "Delete?")
                (remove-overlays p1 p2)
                (delete-region p1 p2)))))
      (remove-overlays beg end))))

(cl-defun js-imports-completing-read (prompt
                                      collection
                                      &key
                                      action
                                      history
                                      require-match)
  (interactive)
  (let* ((minibuffer-completion-table collection)
         (minibuffer-completion-confirm (unless (eq require-match t)
                                          require-match))
         (base-keymap (if require-match
                          minibuffer-local-must-match-map
                        minibuffer-local-completion-map))
         (keymap (make-composed-keymap
                  minibuffer-local-must-match-map base-keymap))
         (preselect (car minibuffer-completion-table))
         (prompt (if (string-empty-p (format "%s" preselect))
                     prompt
                   (concat prompt "\s" (format "(default %s)" preselect))))
         (result))
    (setq result (read-from-minibuffer
                  prompt
                  nil
                  keymap
                  t
                  history
                  preselect))
    (when action
      (funcall action result))
    result))

(defun js-imports-ivy-read-file-name (&optional preselect input)
  (require 'ivy)
  (when (fboundp 'ivy-read)
    (ivy-read
     (js-imports-make-files-prompt)
     (append (js-imports-project-files-transformer
              (or js-imports-project-files
                  (js-imports-find-project-files))))
     :preselect (or preselect (js-imports-preselect-file))
     :require-match t
     :caller 'js-imports-ivy-read-file-name
     :initial-input input
     :keymap js-imports-files-map
     :action (lambda (it) (if (and (boundp 'ivy-exit)
                                   ivy-exit)
                              (funcall-interactively 'js-imports-from-path it)
                            (js-imports-find-file it))))))

(defvar ivy-last)

(defvar ivy-exit)

(defun js-imports-ivy-setup ()
  (require 'ivy)
  (setq js-imports-files-map (make-sparse-keymap))
  (when (fboundp 'ivy-state-current)
    (setq js-imports-switch-alias-post-command
          (lambda ()
            (let ((input (when (boundp 'ivy-text)
                           ivy-text)))
              (progn
                (put 'quit 'error-message "")
                (run-at-time nil nil
                             (lambda ()
                               (put 'quit 'error-message "Quit")
                               (with-demoted-errors "Error: %S"
                                 (js-imports-ivy-read-file-name
                                  (when (and (fboundp 'ivy-state-current)
                                             (boundp 'ivy-last))
                                    (ivy-state-current ivy-last))
                                  input))))
                (abort-recursive-edit))))))
  (setq js-imports-next-alias-action
        (lambda () (interactive)
          (funcall 'js-imports-next-or-prev-alias 1)))
  (setq js-imports-prev-alias-action
        (lambda () (interactive)
          (funcall 'js-imports-next-or-prev-alias -1)))
  (define-key js-imports-files-map (kbd "C->")
    js-imports-next-alias-action)
  (define-key js-imports-files-map (kbd "C-<")
    js-imports-prev-alias-action)
  (define-key js-imports-files-map (kbd "C-c o")
    (lambda ()
      (interactive)
      (when-let ((filename (and (fboundp 'ivy-state-current)
                                (ivy-state-current ivy-last))))
        (ivy-set-action
         `(lambda (x)
            (funcall 'js-imports-find-file-other-window x)
            (ivy-set-action ',(ivy-state-action ivy-last))))
        (setq ivy-exit 'done)
        (exit-minibuffer))))
  (when (fboundp 'ivy-set-actions)
    (ivy-set-actions
     'js-imports
     '(("f" js-imports-find-file
        "find file")
       ("j"
        js-imports-find-file-other-window
        "find file other window"))))
  (when (fboundp 'ivy-set-display-transformer)
    (ivy-set-display-transformer
     'js-imports-symbols-menu
     'js-imports-transform-symbol))
  (when (fboundp 'ivy-set-sources)
    (ivy-set-sources
     'js-imports-ivy-read-file-name
     '((js-imports-find-imported-files)
       (original-source)
       (js-imports-node-modules-candidates)))))

(defun js-imports-helm-setup ()
  (require 'helm)
  (setq js-imports-files-map (make-sparse-keymap))
  (js-imports-helm-reset-sources)
  (when (boundp 'helm-map)
    (setq js-imports-switch-alias-post-command
          (when (and (fboundp 'helm-refresh))
            'helm-refresh))
    (set-keymap-parent js-imports-files-map helm-map)
    (define-key js-imports-files-map (kbd "C-c o")
      (lambda () (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-imports-find-file
           (helm-get-selection)))))
    (define-key js-imports-files-map (kbd "C->")
      (lambda ()
        (interactive)
        (funcall 'js-imports-next-or-prev-alias 1)))
    (define-key js-imports-files-map (kbd "C-<")
      (lambda () (interactive)
        (funcall 'js-imports-next-or-prev-alias -1)))
    (define-key js-imports-files-map (kbd "C-c C-o")
      (lambda () (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-imports-find-file-other-window
           (helm-get-selection)))))
    (setq js-imports-helm-export-symbols-map
          (js-imports-build-helm-exports-keymap))
    (setq js-imports-helm-imported-symbols-map
          (js-imports-build-helm-imported-keymap))
    (put 'js-imports-files-map 'helm-only t)
    (put 'js-imports-helm-export-symbols-map 'helm-only t)
    (put 'js-imports-helm-imported-symbols-map 'helm-only t))
  (when (fboundp 'helm-make-source)
    (when (fboundp 'helm-candidate-buffer)
      (setq js-imports-buffer-files-source
            (helm-make-source
             "Imported files"
             'helm-source-in-buffer
             :init
             (lambda ()
               (with-current-buffer
                   (helm-candidate-buffer 'global)
                 (let ((items (with-current-buffer
                                  js-imports-current-buffer
                                (js-imports-find-imported-files))))
                   (mapc (lambda (it) (insert it)
                           (newline-and-indent))
                         items))
                 (goto-char (point-min))))
             :get-line #'buffer-substring-no-properties
             :action 'js-imports-helm-file-actions
             :keymap js-imports-files-map
             :group 'js-imports
             :persistent-action #'js-imports-view-file
             :mode-line (list "Imports"))))
    (setq js-imports-project-files-source
          (helm-make-source
           "Project files"
           'helm-source-sync
           :group 'js-imports
           :mode-line (list "File(s)")
           :candidate-number-limit js-imports-helm-files-number-limit
           :action 'js-imports-helm-file-actions
           :persistent-action #'js-imports-view-file
           :keymap js-imports-files-map
           :candidates #'js-imports-find-project-files
           :filtered-candidate-transformer
           #'js-imports-project-files-transformer))
    (setq js-imports-node-modules-source
          (helm-make-source
           "Node Modules" 'helm-source-sync
           :candidates #'js-imports-node-modules-candidates
           :candidate-number-limit js-imports-helm-dependencies-number-limit
           :action 'js-imports-helm-file-actions
           :mode-line (list "Dependencies")
           :keymap js-imports-files-map
           :persistent-action #'js-imports-view-file
           :group 'js-imports)))
  (when (and (boundp 'helm-map)
             (fboundp 'helm-make-source))
    (setq js-imports-imported-symbols-source
          (helm-make-source
           "Imported"
           'helm-source-sync
           :candidates 'js-imports-imported-candidates-in-buffer
           :candidate-transformer
           (lambda (candidates)
             (with-current-buffer js-imports-current-buffer
               (when js-imports-current-export-path
                 (setq candidates
                       (js-imports-filter-with-prop
                        :display-path
                        js-imports-current-export-path
                        candidates))))
             candidates)
           :action '(("Jump" . js-imports-jump-to-item-in-buffer)
                     ("Rename" . js-imports-rename-import)
                     ("Quick delete" . js-imports-delete-import-item)
                     ("Delete whole import" .
                      js-imports-delete-import-statetement))
           :persistent-action (lambda (it)
                                (js-imports-jump-to-item-in-buffer
                                 (js-imports-display-to-real-imports it)))
           :keymap js-imports-helm-imported-symbols-map
           :volatile t
           :display-to-real #'js-imports-display-to-real-imports
           :persistent-help "Show symbol"
           :marked-with-props 'withprop))
    (setq js-imports-exports-source
          (helm-make-source
           "Exports" 'helm-source-sync
           :header-name (lambda (_name)
                          (with-current-buffer js-imports-current-buffer
                            (format "Exports in %s"
                                    (or js-imports-current-export-path
                                        buffer-file-name))))
           :candidates 'js-imports-init-exports-candidates
           :candidate-transformer 'js-imports-exported-candidates-transformer
           :filtered-candidate-transformer
           #'js-imports-export-filtered-candidate-transformer
           :marked-with-props 'withprop
           :volatile t
           :keymap 'js-imports-helm-export-symbols-map
           :action '()
           :action-transformer
           (lambda (_candidate _actions)
             (if (with-current-buffer js-imports-current-buffer
                   js-imports-current-export-path)
                 '(("Import" . (lambda (_it)
                                 (let ((marked (helm-marked-candidates)))
                                   (dotimes (i (length marked))
                                     (let ((item (nth i marked)))
                                       (js-imports-insert-import item))))))
                   ("Jump to export" . js-imports-jump-to-item-other-window)
                   ("Jump to definition" . js-imports-find-export-definition))
               '(("Jump" . (lambda (it)
                             (js-imports-jump-to-item-in-buffer
                              (js-imports-display-to-real-exports it)))))))
           :persistent-action
           (lambda (c)
             (when-let ((item (js-imports-display-to-real-exports c)))
               (setq item (js-imports-find-definition item))
               (when (and (js-imports-get-prop item :pos))
                 (view-file (js-imports-get-prop
                             item
                             :real-path))
                 (goto-char (js-imports-get-prop item :pos))
                 (js-imports-highlight-word))))))
    (setq js-imports-definitions-source
          (helm-make-source
           "Definitions" 'helm-source-sync
           :candidates (lambda () (with-current-buffer js-imports-current-buffer
                               (js-imports-extract-definitions
                                buffer-file-name (point))))
           :marked-with-props 'withprop
           :volatile t
           :action '(("Jump" .
                      (lambda (_it)
                        (when (fboundp 'helm-get-selection)
                          (js-imports-jump-to-item-in-buffer
                           (helm-get-selection nil
                                               'withprop))))))))))

(defun js-imports-view-file (candidate)
  (if-let ((file (js-imports-path-to-real
                  candidate)))
      (view-file file)
    (message "Couldn't find %s" candidate)))

(defun js-imports-build-helm-exports-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map)
                         helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key
      map (kbd "C-c o")
      (lambda () (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-imports-jump-to-item-other-window
           (helm-get-selection nil 'withprop)))))
    (define-key
      map
      (kbd "C-c C-j")
      (lambda () (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-imports-find-export-definition
           (helm-get-selection nil 'withprop)))))
    map))

(defun js-imports-build-helm-imported-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map)
                         (fboundp 'helm-run-after-exit)
                         (fboundp 'helm-get-selection)
                         (boundp 'helm-alive-p)
                         (fboundp 'helm-refresh)
                         (fboundp 'helm-execute-persistent-action)
                         (fboundp 'helm-attrset)
                         helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key map
      (kbd "M-d") (lambda ()
                    (interactive)
                    (when (and (boundp 'helm-alive-p)
                               helm-alive-p
                               (fboundp 'helm-refresh)
                               (fboundp 'helm-execute-persistent-action)
                               (fboundp 'helm-attrset))
                      (helm-attrset
                       'js-imports-delete-import-item
                       '(js-imports-delete-import-item . never-split))
                      (helm-execute-persistent-action
                       'js-imports-delete-import-item)
                      (js-imports-init-exports-candidates)
                      (helm-refresh))))
    (define-key map
      (kbd "M-D") (lambda ()
                    (interactive)
                    (when (and (boundp 'helm-alive-p)
                               helm-alive-p
                               (fboundp 'helm-refresh)
                               (fboundp 'helm-execute-persistent-action)
                               (fboundp 'helm-attrset))
                      (helm-attrset
                       'js-imports-delete-import-statetement
                       '(js-imports-delete-import-statetement . never-split))
                      (helm-execute-persistent-action
                       'js-imports-delete-import-statetement)
                      (helm-refresh))))
    (define-key map
      (kbd "M-r") (lambda ()
                    (interactive)
                    (when (fboundp 'helm-exit-and-execute-action)
                      (helm-exit-and-execute-action
                       'js-imports-rename-import))))
    map))

(defun js-imports-set-completion (var value &optional &rest _ignored)
  "Set VAR to VALUE."
  (pcase value
    ('ivy (funcall 'js-imports-ivy-setup))
    ('helm (funcall 'js-imports-helm-setup))
    ('default (message "default")))
  (set var value))

(defcustom js-imports-completion-system
  (pcase completing-read-function
    ('ivy-completing-read 'ivy)
    ('helm-comp-read 'helm)
    (_ 'default))
  "Which completion system to use."
  :group 'js-imports
  :set 'js-imports-set-completion
  :type '(choice (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)
                 (const :tag "Default" default)))

(add-variable-watcher 'js-imports-completion-system
                      'js-imports-set-completion)

(defun js-imports-make-files-prompt ()
  "Generate prompt for read file functions."
  (when js-imports-current-project-root
    (let ((project-name (car
                         (reverse (split-string
                                   (directory-file-name
                                    js-imports-current-project-root) "/")))))
      (concat project-name "\s" "files" "\s" (or js-imports-current-alias
                                                 "./")))))

(defun js-imports-skip-semicolon ()
  (let ((p (if (looking-at ";")
               (1+ (point))
             (or (save-excursion
                   (js-imports-skip-whitespace-forward)
                   (when (looking-at ";")
                     (point)))
                 (point)))))
    (goto-char p)
    p))

(defun js-imports-goto-end-brackets ()
  (let ((winner)
        (beg (point))
        (words)
        (w))
    (save-excursion
      (while (looking-at js-imports-node-starts-re)
        (let ((w (js-imports-which-word)))
          (push w words)
          (skip-chars-forward w))
        (js-imports-skip-whitespace-forward)
        (when (looking-at "{")
          (forward-list 1)
          (js-imports-skip-whitespace-forward)))
      (js-imports-skip-whitespace-forward)
      (setq beg (point))
      (let* ((depth (nth 0 (syntax-ppss (point))))
             (next-node
              (save-excursion
                (when
                    (js-imports-re-search-forward
                     js-imports-node-starts-re nil t 1)
                  (skip-chars-forward "a-z")
                  (let ((p (point)))
                    (skip-chars-backward "a-z")
                    (when
                        (and
                         (equal (nth 0 (syntax-ppss (point)))
                                depth)
                         (not
                          (js-imports-looking-at-function-expression)))
                      (point))))))
             (brackets (save-excursion (js-imports-re-search-forward
                                        "[<(;{]" nil t 1))))
        (cond ((and (null next-node)
                    (null brackets))
               (goto-char (point-max))
               (skip-chars-backward "\s\t\n")
               (js-imports-skip-whitespace-backward))
              ((and (null brackets)
                    next-node)
               (goto-char next-node)
               (skip-chars-backward "a-z")
               (js-imports-skip-whitespace-backward))
              ((and next-node brackets
                    (> brackets next-node))
               (goto-char next-node)
               (skip-chars-backward "a-z")
               (js-imports-skip-whitespace-backward))
              (t
               (progn (js-imports-re-search-forward "[<(;{]" nil t 1)
                      (when (looking-back "<")
                        (forward-char -1)
                        (forward-list 1)
                        (forward-char 1)
                        (js-imports-re-search-forward "[({;]" nil t 1))
                      (when (looking-back "(" 0)
                        (forward-char -1)
                        (forward-list 1)
                        (js-imports-re-search-forward "[{;]" nil t 1))
                      (when (looking-back "{" 0)
                        (forward-char -1)
                        (forward-list 1)))))
        (setq winner (if (equal beg (point))
                         nil
                       (progn
                         (js-imports-skip-semicolon)
                         (point))))))
    (when winner
      (goto-char winner))
    winner))

(defun js-imports-get-prev-node-start-if-matches (re)
  (save-excursion
    (js-imports-skip-whitespace-backward)
    (when (looking-back re 0)
      (skip-chars-backward js-imports-regexp-name-set)
      (point))))

(defun js-imports-goto-start-of-node ()
  (let ((beg))
    (save-excursion
      (skip-chars-backward
       js-imports-regexp-name-set)
      (unless (looking-at js-imports-node-starts-re)
        (js-imports-re-search-backward
         js-imports-node-starts-re
         nil t 1))
      (cond
       ((js-imports-looking-at-function-expression)
        (js-imports-re-search-backward js-imports-vars-keywords--re nil t 1)
        (setq beg (or (js-imports-get-prev-node-start-if-matches
                       js-imports-esm-export-keyword--re)
                      (point))))
       ((looking-at js-imports-node-starts-re)
        (setq beg (or (js-imports-get-prev-node-start-if-matches
                       js-imports-esm-export-keyword--re)
                      (point))))
       (t (js-imports-re-search-backward
           js-imports-node-starts-re
           nil t 1)
          (when (looking-at js-imports-node-starts-re)
            (setq beg (or (js-imports-get-prev-node-start-if-matches
                           js-imports-esm-export-keyword--re)
                          (point)))))))
    (when beg (goto-char beg))
    beg))

(defun js-imports-get-bounds-of-exp-at-point ()
  (let ((beg)
        (end))
    (save-excursion
      (when (setq beg
                  (js-imports-goto-start-of-node))
        (setq end (cond
                   ((and (looking-at js-imports-regexp-import-keyword)
                         (js-imports-re-search-forward js-imports-string-re
                                                       nil t 1))
                    (js-imports-skip-semicolon)
                    (point))
                   (t (js-imports-goto-end-brackets))))))
    (when (and beg end)
      (cons beg end))))

;;;###autoload
(defun js-imports-mark-it ()
  (interactive)
  (when-let ((bounds (js-imports-get-bounds-of-exp-at-point)))
    (goto-char (car bounds))
    (push-mark (cdr bounds) nil t)
    (activate-mark)))

;;;###autoload
(defun js-imports ()
  "Read a filename to extract exports and add selected ones in current buffer.
Addional actions to open or preview are configured for `helm' and `ivy'."
  (interactive)
  (js-imports-init-project)
  (pcase js-imports-completion-system
    ('helm
     (require 'helm)
     (when (and (fboundp 'helm)
                (fboundp 'helm-attr))
       (unless js-imports-node-modules-source
         (js-imports-helm-setup))
       (helm
        :sources js-imports-helm-files-source
        :buffer "*helm js import*"
        :preselect (js-imports-preselect-file)
        :prompt (js-imports-make-files-prompt))))
    ('ivy (js-imports-ivy-read-file-name))
    (_ (let ((module (funcall completing-read-function
                              (js-imports-make-files-prompt)
                              (js-imports-get-all-modules))))
         (js-imports-from-path module)))))

;;;###autoload
(defun js-imports-from-path (&optional path)
  "Make completions with exports from PATH.
Add selected choices to existing or new import statement."
  (interactive)
  (unless path
    (js-imports-init-project)
    (setq path (read-file-name "File:\s")))
  (with-current-buffer js-imports-current-buffer
    (when (file-name-absolute-p path)
      (setq path (js-imports-propertize
                  path :display-path
                  (funcall
                   completing-read-function
                   "Transform to\s"
                   (js-imports-get-file-variants path
                                                 default-directory)))))
    (when-let ((display-path (or (js-imports-get-prop path :display-path)
                                 path)))
      (setq js-imports-current-export-path display-path)
      (setq js-imports-last-export-path display-path)))
  (js-imports-init-exports-candidates)
  (cond
   ((and (eq js-imports-completion-system 'helm)
         (fboundp 'helm))
    (helm
     :preselect (js-imports-preselect-symbol)
     :sources '(js-imports-exports-source
                js-imports-imported-symbols-source)))
   ((and (eq js-imports-completion-system 'ivy)
         (fboundp 'ivy-read))
    (let ((choices (js-imports-export-filtered-candidate-transformer
                    (js-imports-exported-candidates-transformer
                     js-imports-export-candidates-in-path)))
          (imports (js-imports-filter-with-prop
                    :display-path
                    js-imports-current-export-path
                    (js-imports-imported-candidates-in-buffer
                     js-imports-current-buffer))))
      (ivy-read
       (if (null imports)
           "Import\s"
         (concat "Import" "\s" (mapconcat (lambda (it) (format "%s" it))
                                          imports
                                          ",\s")
                 ",\s"))
       choices
       :require-match nil
       :caller 'js-imports-from-path
       :preselect (js-imports-preselect-symbol)
       :multi-action (lambda (marked)
                       (dolist (it marked)
                         (js-imports-insert-import it)))
       :action 'js-imports-ivy-insert-or-view-export)))
   (t (let ((choices (js-imports-export-filtered-candidate-transformer
                      (js-imports-exported-candidates-transformer
                       js-imports-export-candidates-in-path))))
        (if (null choices)
            (message "No exports found")
          (js-imports-completing-read
           "Symbols\s" choices
           :require-match t
           :action 'js-imports-insert-import))))))

;;;###autoload
(defun js-imports-jump-to-definition ()
  "Deep jump to a definition of symbol at point through re-exports and renamings."
  (interactive)
  (js-imports-init-project)
  (if-let ((name (and
                  (not (js-imports-inside-string-p))
                  (js-imports-get-word-if-valid))))
      (let (real-name as-name)
        (save-excursion (skip-chars-backward name)
                        (if (save-excursion
                              (js-imports-skip-whitespace-backward)
                              (js-imports-looking-at "as"))
                            (progn
                              (setq as-name name)
                              (js-imports-re-search-backward "as" nil t 1)
                              (js-imports-skip-whitespace-backward)
                              (setq real-name (js-imports-get-word-if-valid)))
                          (progn
                            (setq real-name name)
                            (js-imports-re-search-forward real-name)
                            (js-imports-skip-whitespace-forward)
                            (if (not (js-imports-looking-at "as"))
                                (setq as-name real-name)
                              (js-imports-re-search-forward "as" nil t 1)
                              (js-imports-skip-whitespace-forward)
                              (setq as-name (js-imports-get-word-if-valid))))))
        (when-let ((item (and (or real-name as-name)
                              (or (js-imports-find-by-prop
                                   :real-name real-name
                                   (js-imports-extract-definitions
                                    buffer-file-name (point)))
                                  (js-imports-find-by-prop
                                   :as-name as-name
                                   (js-imports-extract-es-imports
                                    buffer-file-name))
                                  (js-imports-find-by-prop
                                   :real-name real-name
                                   (js-imports-extract-all-exports
                                    buffer-file-name))))))
          (unless (js-imports-get-prop item :var-type)
            (setq item (js-imports-find-definition item)))
          (when item
            (find-file (js-imports-get-prop item :real-path))
            (progn (goto-char (js-imports-get-prop item :pos))
                   (js-imports-highlight-word)))))
    (js-imports-find-file-at-point)))

(defun js-imports-find-file (&optional file)
  "An action for command `js-imports' to open FILE."
  (let ((path (js-imports-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file path)
      (message "Could't find %s" file))))

(defun js-imports-find-file-other-window (&optional file)
  "An action for command `js-imports' to open FILE in other window."
  (let ((path (js-imports-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file-other-window path)
      (message "Could't find %s" file))))

;;;###autoload
(defun js-imports-find-file-at-point ()
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (let (path)
    (save-excursion
      (unless (js-imports-inside-string-p)
        (beginning-of-line))
      (setq path (js-imports-get-path-at-point)))
    (when path
      (js-imports-find-file path))))

;;;###autoload
(defun js-imports-symbols-menu ()
  "Jump or refactor to exported, imported and definitions in current buffer."
  (interactive)
  (js-imports-init-project)
  (setq js-imports-current-export-path nil)
  (pcase js-imports-completion-system
    ('helm (when (and (fboundp 'helm))
             (helm
              :preselect (js-imports-preselect-symbol)
              :sources js-imports-helm-symbol-sources)))
    ('ivy (when (fboundp 'ivy-read)
            (let ((choices (append
                            (js-imports-extract-es-imports buffer-file-name)
                            (js-imports-extract-all-exports buffer-file-name)
                            (js-imports-extract-definitions
                             buffer-file-name (point-max)))))
              (ivy-read "Jump to\s"
                        choices
                        :preselect (js-imports-preselect-symbol)
                        :caller 'js-imports-symbols-menu
                        :action (lambda (it)
                                  (if (and (boundp 'ivy-exit)
                                           ivy-exit)
                                      (js-imports-jump-to-symbol-action it)
                                    (js-imports-jump-to-item-in-buffer
                                     it
                                     js-imports-current-buffer)))))))
    (_ (when-let* ((choices (append (js-imports-extract-all-exports
                                     buffer-file-name)
                                    (js-imports-extract-es-imports
                                     buffer-file-name)
                                    (js-imports-extract-definitions
                                     buffer-file-name (point-max))))
                   (symbol (completing-read "Select symbol"
                                            choices nil t
                                            (js-imports-preselect-symbol))))
         (setq symbol (mapcar (lambda (it) (car (member it choices)))
                              symbol))
         (when-let ((item (js-imports-find-definition symbol)))
           (when (js-imports-get-prop item :var-type)
             (find-file (js-imports-get-prop item :real-path))
             (progn (goto-char (js-imports-get-prop item :pos))
                    (js-imports-highlight-word))))))))

;;;###autoload
(defun js-imports-change-completion ()
  "Customize or temporarly set one of available completions systems:
`helm' `ivy' or default."
  (interactive)
  (let* ((choices '(helm ivy default))
         (result (completing-read (format "Change completion:\s (Current: %s)"
                                          js-imports-completion-system)
                                  choices))
         (func (if (yes-or-no-p "Save? ")
                   'customize-save-variable
                 'js-imports-set-completion)))
    (funcall func 'js-imports-completion-system (intern-soft result))))

;;;###autoload
(defun js-imports-transform-import-path-at-point ()
  "Replace path of import statement at point to aliased one or relative.
Inside import statement generates completions with available replacements, e.g:
`import someExport from '../../enums' to `import someExport from '@/enums''.'"
  (interactive)
  (js-imports-init-project)
  (with-current-buffer js-imports-current-buffer
    (save-excursion
      (when-let* ((bounds (js-imports-inside-import-p (point)))
                  (path-bounds (js-imports-extract-import-path-bounds bounds))
                  (path (buffer-substring-no-properties
                         (car path-bounds)
                         (cdr path-bounds)))
                  (variants (js-imports-get-file-variants
                             path default-directory))
                  (choice (delete path (completing-read
                                        (format "Replace %s with\s" path)
                                        variants))))
        (goto-char (car path-bounds))
        (delete-char (length path))
        (insert choice)))))

;;;###autoload
(defun js-imports-transform-relative-imports-to-aliases ()
  "Replace relative paths in import statemetents to aliased ones.
An exception is made for paths from current buffer directory.

For example `import someExport from '../enums' transforms to
`import someExport from '@/enums', but keeps `import someExport from './enums'."
  (interactive)
  (js-imports-init-project)
  (save-excursion
    (with-current-buffer js-imports-current-buffer
      (goto-char (point-min))
      (while (js-imports-re-search-forward
              js-imports-regexp-import-keyword nil t 1)
        (when-let* ((bounds (js-imports-inside-import-p (point)))
                    (path-bounds (js-imports-extract-import-path-bounds bounds))
                    (path (buffer-substring-no-properties
                           (car path-bounds)
                           (cdr path-bounds)))
                    (variants (and (js-imports-relative-p path)
                                   (string-match-p "\\.\\./" path)
                                   (seq-remove 'js-imports-relative-p
                                               (js-imports-get-file-variants
                                                path default-directory))))
                    (choice (if (> (length variants) 1)
                                (completing-read
                                 (format "Replace %s with\s" path)
                                 variants)
                              (car variants))))
          (goto-char (car path-bounds))
          (delete-char (length path))
          (insert choice))))))

;;;###autoload
(defun js-imports-reset-cache ()
  "Remove cache from variables `js-imports-files-cache' and `js-imports-json-hash'."
  (interactive)
  (setq js-imports-project-aliases nil)
  (maphash (lambda (key _value)
             (remhash key js-imports-files-cache))
           js-imports-files-cache)
  (maphash (lambda (key _value)
             (remhash key js-imports-json-hash))
           js-imports-json-hash)
  (with-current-buffer (or js-imports-current-buffer (current-buffer))
    (setq js-imports-cached-imports-in-buffer nil)
    (setq js-imports-buffer-tick nil)))

;;;###autoload
(defun js-imports-helm-reset-sources ()
  "Reset `helm' sources."
  (interactive)
  (setq js-imports-buffer-files-source nil
        js-imports-project-files-source nil
        js-imports-imported-symbols-source nil
        js-imports-exports-source nil
        js-imports-definitions-source nil
        js-imports-node-modules-source nil
        js-imports-project-files-source nil))

;;;###autoload
(defun js-imports-version ()
  "Return `js-imports' version if `pkg-info' is installed.
If called interactively also show the version in echo area."
  (interactive)
  (if (and (require 'pkg-info nil t)
           (fboundp 'pkg-info-version-info))
      (let ((version (pkg-info-version-info 'js-imports)))
        (when (called-interactively-p 'interactive)
          (message "js-imports %s" version))
        version)
    (error
     "Package pkg-info is required for determining `js-imports' version")))

;;;###autoload
(defun js-imports-project-info ()
  (interactive)
  (let ((vars '(js-imports-current-alias
                js-imports-current-buffer
                js-imports-current-export-path
                js-imports-current-project-root
                js-imports-tsconfig-filename
                js-imports-completion-system
                js-imports-project-aliases
                js-imports-aliases
                js-imports-buffer-tick
                js-imports-last-export-path
                js-imports-export-candidates-in-path
                js-imports-cached-imports-in-buffer)))
    (js-imports-with-popup "*js-imports*"
                           (dolist (v vars)
                             (let ((val (js-imports-stringify
                                         (symbol-value v))))
                               (insert (propertize
                                        (js-imports-stringify v)
                                        'face
                                        'font-lock-variable-name-face)
                                       ": " val)
                               (newline-and-indent))))))

(defvar js-imports-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-imports)
    (define-key map (kbd "C-c C-.") 'js-imports-symbols-menu)
    (define-key map (kbd "C-c C-j") 'js-imports-jump-to-definition)
    (easy-menu-define js-imports-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-imports]
        ["Jump to symbol in buffer" js-imports-symbols-menu]
        ["Jump to definition" js-imports-jump-to-definition]))
    map)
  "Keymap for `js-imports' mode.")

;;;###autoload
(define-minor-mode js-imports-mode
  "js-imports-mode is a minor mode for importing.
\\{js-imports-mode-map}"
  :lighter " js-imports"
  :group 'js-imports
  :keymap js-imports-mode-map)

(provide 'js-imports)
;;; js-imports.el ends here
