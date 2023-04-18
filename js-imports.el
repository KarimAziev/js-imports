;;; js-imports.el --- Import JavaScript files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-imports
;; Keywords: languages
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide importing and navigation for JavaScript files.

;;; Commands

;; M-x `js-imports'
;;      Read a project file, parse exports from it and ask which to import.
;;
;;      During file completion, you can cycle between relative and aliased filenames:
;;      `C->' - select the next alias,
;;      `C-<' - select the previous alias.

;; M-x `js-imports-jump-to-definition'
;;      Jump to a definition of symbol at the point.

;; M-x `js-imports-symbols-menu'
;;      Jump to a identifier in current buffer.

;; M-x `js-imports-find-file-at-point'
;;      Find a file when cursor are placed inside string.

;; M-x `js-imports-mark-it'
;;      Mark node at the point.

;; M-x `js-imports-transform-relative-imports-to-aliases'
;;      Replace relative paths from other directory than current to aliased ones.
;;      Only relative paths from current buffer directory.
;;
;;      For example, ‘import someExport from '../enums' transforms to
;;      ‘import someExport from '@/enums', but keeps ‘import someExport from ’./enums’.

;; M-x `js-imports-transform-import-path-at-point'
;;      Replace path of import statement at the point to aliased one or relative.
;;      Inside import statement generates completions with available replacements, e.g:
;;     import { a, b } from '../fileA' => import { a, b } from '@/fileA'

;; M-x `js-imports-change-completion'
;;      Customize or temporarily set one of the available completions systems.

;;; Customization

;; `js-imports-completion-system'
;;      Which completion system to use.

;; `js-imports-helm-symbol-sources'
;;      Helm sources for symbols for command ‘js-imports-symbols-menu’.

;; `js-imports-helm-files-source'
;;      Helm sources for files for command ‘js-imports’.

;; `js-imports-project-aliases'
;;      An associated list of ((ALIAS_A . DIRECTORY_A) (ALIAS_B . DIR_B DIR_C)).

;; `js-imports-tsconfig-filename'
;;      Name of tsconfig or jsconfig.

;; `js-imports-helm-file-actions'
;;      Default actions for files.

;; `js-imports-modules-default-names'
;;      Alist mapping module path to default and namespace import name.

;; `js-imports-root-ignored-directories'
;;      A list of directories in project root to ignore.

;; `js-imports-normalize-paths-functions'
;;      List of functions to use in ‘js-imports-normalize-path’.

;; `js-imports-preffered-extensions'
;;      File extensions to resolve.

;; `js-imports-node-modules-dir'
;;      Relative to project root or absolute path to node_modules directory.

;; `js-imports-node-modules-priority-section-to-read'
;;      Package-json sections to retrieve candidates from node_modules.

;; `js-imports-package-json-sections'
;;      Package-json sections to retrieve candidates from node_modules.

;; `js-imports-helm-dependencies-number-limit'
;;      Limit for number of dependencies displayed.

;; `js-imports-helm-files-number-limit'
;;      Limit for number of project files displayed in helm source.

;; `js-imports-quote'
;;      Quote type to use in imports.

;;; Code:

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
  :group 'convenience)

(defvar js-imports-current-alias nil)

(defvar js-imports-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'js-imports-select-next-alias)
    (define-key map (kbd "C-<") #'js-imports-select-prev-alias)
    map)
  "Keymap for files sources.")

(defvar js-imports-ivy-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'js-imports-ivy-preview-file-exports)
    (define-key map
                (kbd "C-c M-o")
                #'js-imports-ivy-find-file-other-window)
    map)
  "Minibuffer files keymap for `ivy'.")

(defvar js-imports-helm-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-o") #'js-imports-helm-find-file)
    (define-key map (kbd "C-c C-o") #'js-imports-helm-find-file-other-window)
    map)
  "Minibuffer files keymap for `helm'.")

(defvar js-imports-file-completions-maps
  '((ivy-completing-read . js-imports-ivy-file-map)
    (helm-comp-read . js-imports-helm-file-map))
  "Additional files keymap to compose with `js-imports-file-map'.")

(defvar js-imports-helm-imported-symbols-map nil
  "Keymap for symbol sources.")

(defvar js-imports-helm-export-symbols-map nil
  "Keymap for symbol sources.")

(defcustom js-imports-quote "'"
  "Quote type to use in imports."
  :group 'js-imports
  :type '(choice (const :tag "Double" "\"")
                 (const :tag "Single" "\\'")))

(defcustom js-imports-helm-files-number-limit 30
  "Limit for number of project files displayed in helm source."
  :group 'js-imports
  :type 'number)

(defcustom js-imports-helm-dependencies-number-limit 400
  "Limit for number of dependencies displayed."
  :group 'js-imports
  :type 'number)

(defcustom js-imports-package-json-sections
  '("dependencies" "devDependencies" "peerDependencies")
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
  '("ts" "tsx" "jsx" "es6" "es" "mjs" "js" "cjs" "ls" "sjs" "iced" "liticed"
    "json")
  "File extensions to resolve."
  :group 'js-imports
  :type '(repeat string))

(defface js-imports-highlight-face
  '((t (:background "Gold" :foreground "black" :bold t)))
  "Face used to highlight symbol."
  :group 'js-imports)

(defcustom js-imports-normalize-paths-functions
  '(js-imports-remove-ext
    js-imports-maybe-remove-path-index)
  "List of functions to use in `js-imports-normalize-path'."
  :type '(repeat function)
  :group 'js-imports)

(defcustom js-imports-root-ignored-directories
  '("build")
  "A list of directories in project root to ignore."
  :type '(repeat string)
  :group 'js-imports)

(defcustom js-imports-modules-default-names nil
  "Alist mapping module path to default and namespace import name."
  :type '(alist
          :key-type (string :tag "Module path")
          :value-type (string :tag "Default import name"))
  :group 'js-imports)

(defvar js-imports-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
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
  "Helm source of node_modules.")

(defvar js-imports-project-files-source nil
  "Helm source of the relative and aliased files without dependencies.")

(defvar js-imports-buffer-files-source nil
  "Helm source of imported files in the current buffer.")

(defvar js-imports-imported-symbols-source nil)

(defvar js-imports-exports-source nil)

(defvar js-imports-definitions-source nil)

(declare-function helm-make-source "helm-source.el")
(declare-function helm-run-after-exit "helm-core.el")
(declare-function helm-get-selection "helm-core.el")
(declare-function helm-attrset "helm-core.el")
(declare-function helm-refresh "helm-core.el")
(declare-function helm-execute-persistent-action "helm-core.el")

(defvar ivy-last)

(defmacro js-imports--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro js-imports--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(js-imports--pipe ,@(reverse functions)))

(defmacro js-imports--or (&rest functions)
  "Return an unary function that invoke FUNCTIONS until first non-nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (or
            ,@(mapcar (lambda (v) (if (symbolp v)
                                 `(,v it)
                               `(funcall ,v it)))
                      functions))))

(defmacro js-imports--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (and
                 ,@(mapcar (lambda (v) (if (symbolp v)
                                           `(,v it)
                                         `(funcall ,v it)))
                           functions))))

(defmacro js-imports--partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

(defmacro js-imports--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defun js-imports-expand-alias-path (path &optional base-url)
  "Convert PATH to the absolute filename and append a slash to PATH.
Without BASE-URL, resolve PATH as relative to the project directory.
Otherwise, firstly expand BASE-URL to the project directory."
  (when-let ((filepath
              (when path (replace-regexp-in-string "\\*[^$]+" "" path))))
    (cond
     ((and (file-name-absolute-p filepath)
           (file-exists-p filepath))
      (if (file-directory-p filepath)
          (js-imports-slash filepath)
        filepath))
     (t
      (if-let* ((root (or js-imports-current-project-root
                          (js-imports-find-project-root)))
                (file (expand-file-name filepath
                                        (if base-url
                                            (expand-file-name base-url root)
                                          root))))
          (js-imports-slash file)
        filepath)))))

(defun js-imports-normalize-aliases (paths &optional base-url)
  "Convert and sort alist of PATHS to absolute filenames.
First element of each pair in PATHS supposed to be an alias and rest elements as
relative to BASE-URL if provided or project directory."
  (let ((alist (mapcar
                (lambda (it)
                  (let ((alias (js-imports-slash
                                (string-join
                                 (split-string (format "%s" (car it))
                                               "\\*"))))
                        (alias-paths (cond
                                      ((vectorp (cdr it))
                                       (append (cdr it) nil))
                                      ((listp (cdr it))
                                       (cdr it))
                                      (t `(,(cdr it))))))
                    (setq alias-paths (mapcar
                                       (lambda (p)
                                         (setq p (replace-regexp-in-string
                                                  "\\*" "" p))
                                         (js-imports-expand-alias-path
                                          p base-url))
                                       alias-paths))
                    (cons alias alias-paths)))
                paths)))
    (seq-sort-by (js-imports--compose length car) #'> alist)))

(defun js-imports-set-alias (var value &rest _ignored)
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
  (concat "\\.\\("
          (string-join
           '("\\(d\\.\\)?tsx?"
             "jsx" "es6" "es"
             "mjs" "js" "cjs" "ls"
             "sjs" "iced" "liticed" "json")
           "\\|")
          "\\)\\'")
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-imports-file-index-regexp
  (concat "\\(/\\|^\\)" "index"
          (concat "\\($\\|" js-imports-file-ext-regexp "\\)"))
  "Regexp matching index file.")

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
  "A character set matching an identifier.
Supposed to use as argument of `skip-chars-forward'.")

(defconst js-imports-name-as--re
  "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)\\([\s\t\n]+as[\s\t\n]+\\([_$A-Za-z0-9]+\\)\\)?")

(defconst js-imports-regexp-name-set
  "[_$A-Za-z0-9]"
  "Regexp matching the start of an identifier.")

(defconst js-imports-regexp-import-keyword
  (eval-and-compile
    (concat "\\_<" (regexp-opt `(,"import") t) "\\_>"))
  "Regexp matching keyword import.")

(defconst js-imports-esm-export-keyword--re
  (eval-and-compile
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
  '("abstract" "any" "as" "async" "async*" "await" "boolean" "bigint"
    "break" "case" "catch" "class" "const" "constructor" "continue"
    "declare" "default" "delete" "do" "else" "enum" "export" "exports"
    "extends" "extern" "false" "finally" "for" "function" "function*"
    "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
    "interface" "keyof" "let" "module" "namespace" "never" "new"
    "null" "number" "object" "of" "private" "protected" "public"
    "readonly" "return" "set" "static" "string" "super" "switch"
    "this" "throw" "true" "try" "type" "typeof" "unknown"
    "var" "void" "while" "yield")
  "List of reserved words in JavaScript.")

(defun js-imports-reserved-word-p (str)
  "Check if STR is a reserved keyword.
Keywords specified in the variable `js-imports-reserved-js-words'."
  (when (stringp str)
    (member str js-imports-reserved-js-words)))

(defcustom js-imports-helm-files-source '(js-imports-buffer-files-source
                                          js-imports-project-files-source
                                          js-imports-node-modules-source)
  "Helm sources for files for command `js-imports'."
  :type '(repeat (choice symbol))
  :group 'js-imports)

(defcustom js-imports-helm-symbol-sources
  '(js-imports-imported-symbols-source
    js-imports-exports-source
    js-imports-definitions-source)
  "Helm sources for symbols for command `js-imports-symbols-menu'."
  :type '(repeat (choice symbol))
  :group 'js-imports)

(defun js-imports-syntax-propertize-regexp (end)
  "Propertize regexp syntax and goto END position."
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      (goto-char (nth 8 ppss))
      (when (looking-at
             "/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)")
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defun js-imports-syntax-propertize (start end)
  "Propertize text between START and END with JavaScript syntax rules."
  (goto-char start)
  (js-imports-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
         (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (js-imports-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b")))
   (point) end))

;; files

(defmacro js-imports-with-temp-buffer (&rest body)
  "Evaluate BODY in temporarily buffer with JavaScript syntax."
  `(with-temp-buffer
     (erase-buffer)
     (progn
       (set-syntax-table js-imports-mode-syntax-table)
       (setq-local open-paren-in-column-0-is-defun-start nil)
       (setq-local syntax-propertize-function #'js-imports-syntax-propertize)
       (setq-local parse-sexp-ignore-comments t)
       (setq-local comment-start "// ")
       (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
       (setq-local comment-end "")
       (syntax-ppss-flush-cache (point-min))
       (js-imports-syntax-propertize (point-min) (point-max))
       ,@body)))

(defmacro js-imports-with-buffer-or-file-content (filename &rest body)
  "Execute BODY in temporarily buffer with file or buffer content of FILENAME.
Bind FILENAME to the variable `buffer-file-name', and FILENAME's directory
as `default-directory'."
  (declare (indent 2))
  `(when (and ,filename (file-exists-p ,filename))
     (js-imports-with-temp-buffer
      (save-excursion (insert (with-temp-buffer
                                (let ((inhibit-read-only t))
                                  (erase-buffer)
                                  (if-let ((buff (get-file-buffer ,filename)))
                                      (insert-buffer-substring-no-properties
                                       buff)
                                    (insert-file-contents ,filename))
                                  (buffer-string)))))
      (let ((buffer-file-name ,filename)
            (default-directory (funcall
                                (js-imports--compose
                                 #'js-imports-slash
                                 #'js-imports-dirname)
                                ,filename)))
        ,@body))))

(defmacro js-imports-with-popup (buffer &rest body)
  "Evaluate BODY with the BUFFER and show that buffer."
  `(let ((buff (get-buffer-create ,buffer)))
     (with-current-buffer buff
       (with-current-buffer-window
           buff
           (cons 'display-buffer-in-side-window
                 '((window-height . fit-window-to-buffer)
                   (preserve-size . (nil . t))))
           (lambda (window _value)
             (with-selected-window window
               (unwind-protect
                   (read-key "Key\s")
                 (when (window-live-p window)
                   (quit-restore-window window 'kill)))))
         (setq cursor-type nil)
         (goto-char (point-max))
         (progn ,@body)))))

(defvar js-imports-files-cache (make-hash-table :test 'equal))

(defun js-imports-get-file-cache (cache-key)
  "Return value of CACHE-KEY from a variable `js-imports-files-cache'.
CACHE-KEY should be a file name."
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
  "Read the json object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'plist))
             (cache-key (format "%s:%s" file json-object-type))
             (cache (gethash cache-key js-imports-json-hash))
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
          (puthash cache-key cache js-imports-json-hash))
        (plist-get cache :json))
    (error (message "Cannot read %s" file)
           nil)))

(defun js-imports-read-tsconfig (&optional project-root tsconfig-name)
  "Expand TSCONFIG-NAME to PROJECT-ROOT and return alist of aliases and paths."
  (unless project-root (setq project-root (js-imports-find-project-root)))
  (unless tsconfig-name (setq tsconfig-name js-imports-tsconfig-filename))
  (let ((config)
        (compiler-options)
        (found)
        (base-url)
        (extends (seq-find #'file-exists-p
                           (and project-root
                                (delete nil
                                        (list
                                         tsconfig-name
                                         (expand-file-name "tsconfig.json"
                                                           project-root)
                                         (expand-file-name "jsconfig.json"
                                                           project-root)))))))
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
    (unless js-imports-current-project-root
      (setq js-imports-current-project-root
            default-directory))
    (setq js-imports-project-files
          (js-imports-find-project-files
           js-imports-current-project-root))))

(defun js-imports-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns output if exit status is zero."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (let ((status (apply #'call-process command nil t nil
                           args)))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (message "js-imports: %s" result) nil))))))

(defun js-imports-list-gitignored-dirs ()
  "Return list of directories to exclude."
  (when-let* ((default-directory
               (locate-dominating-file default-directory
                                       (lambda (dir)
                                         (seq-find
                                          (lambda (it)
                                            (file-exists-p (concat dir
                                                                   it)))
                                          '(".gitignore"
                                            "node_modules"
                                            "package.json"
                                            ".git")))))
              (dirs (ignore-errors
                      (split-string
                       (js-imports-call-process "git" "ls-files"
                                                "--others"
                                                "--ignored"
                                                "--exclude-standard"
                                                "--directory")
                       "\n" t))))
    (mapcar #'expand-file-name dirs)))


(defun js-imports-find-project-files (&optional project-root)
  "Return files of PROJECT-ROOT without node_modules."
  (unless project-root
    (setq project-root (js-imports-find-project-root)))
  (let ((files)
        (processed-dirs (append (seq-filter #'file-directory-p
                                            (js-imports-list-gitignored-dirs))
                                (mapcar (lambda (it)
                                          (if (file-name-absolute-p it)
                                              (expand-file-name it)
                                            (expand-file-name it project-root)))
                                        js-imports-root-ignored-directories)))
        (node-modules (js-imports-find-node-modules project-root))
        (dir (replace-regexp-in-string "/$" "" default-directory))
        (proj-root (replace-regexp-in-string
                    "/$"
                    ""
                    js-imports-current-project-root)))
    (push dir processed-dirs)
    (while (not (file-equal-p dir proj-root))
      (when (file-readable-p dir)
        (setq files (append files
                            (reverse
                             (directory-files-recursively
                              dir
                              js-imports-file-ext-regexp
                              nil
                              (lambda (it)
                                (let ((result (and (not
                                                    (or
                                                     (string= it dir)
                                                     (member it processed-dirs)
                                                     (member (concat it "/")
                                                             processed-dirs)
                                                     (string=
                                                      (or node-modules "") it)))
                                                   (file-readable-p it))))
                                  result)))))))
      (setq dir (expand-file-name ".." dir))
      (push dir processed-dirs))
    files))

(defun js-imports-get-aliases ()
  "Return a sorted list of file aliases."
  (setq js-imports-project-aliases
        (seq-sort-by
         (js-imports--compose length car)
         #'>
         (or (js-imports-read-tsconfig)
             (js-imports-normalize-aliases
              js-imports-project-aliases))))
  (setq js-imports-aliases
        (seq-sort-by #'length #'> (mapcar #'car js-imports-project-aliases))))

(defun js-imports-project-files-transformer (files &optional _source)
  "Filter and transform FILES to aliased or relative."
  (with-current-buffer js-imports-current-buffer
    (let* ((current-file (buffer-file-name js-imports-current-buffer))
           (current-dir (js-imports-dirname current-file)))
      (setq files (delete current-file files))
      (if js-imports-current-alias
          (js-imports-transform-files-to-alias js-imports-current-alias files)
        (js-imports-transform-files-to-relative current-dir files)))))

(defun js-imports-get-file-variants (path directory)
  "Return list of transformations for PATH - relative to DIRECTORY and aliased."
  (let ((real-path (js-imports-path-to-real path directory)))
    (let ((relative (js-imports-path-to-relative real-path
                                                 (or directory
                                                     default-directory)))
          (aliased (mapcar (lambda (it)
                             (js-imports-transform-file-to-alias
                              real-path
                              it))
                           (js-imports-get-aliases)))
          (files))
      (setq files (seq-uniq (delete nil (push relative aliased))))
      (append (mapcar #'js-imports-normalize-path files) files))))

(defun js-imports-transform-file-to-alias (filename alias)
  "Convert FILENAME to version with ALIAS."
  (when-let* ((absolute-p (and filename (file-name-absolute-p filename)))
              (paths (cdr (assoc alias js-imports-project-aliases)))
              (alias-path (seq-find (lambda (parent)
                                      (js-imports-string-match-p
                                       (concat "^" parent)
                                       filename))
                                    paths)))
    (replace-regexp-in-string
     alias-path
     (js-imports-slash alias)
     filename)))

(defun js-imports-transform-files-to-alias (alias files)
  "Transform FILES to converted to version with ALIAS."
  (delete
   nil
   (mapcar (js-imports--rpartial js-imports-transform-file-to-alias alias)
           files)))

(defun js-imports-transform-files-to-relative (directory files)
  "Convert FILES to relative to DIRECTORY."
  (mapcar (js-imports--rpartial js-imports-path-to-relative directory)
          files))

(defun js-imports-find-project-root (&optional directory)
  "Traverse up as long as package.json will be found, starting at DIRECTORY."
  (unless directory (setq directory default-directory))
  (let ((parent (expand-file-name ".." directory)))
    (unless (or (string= parent directory)
                (string= directory "")
                (string= directory "/"))
      (if (file-exists-p (expand-file-name "package.json" directory))
          directory
        (js-imports-slash (js-imports-find-project-root parent))))))

(defun js-imports-directory-files (dir &optional
                                       recursive regexp include-dirs)
  "Return files in DIR whose names match REGEXP.
Default value of REGEXP is specified in a variable `js-imports-file-ext-regexp'.
Optional argument RECURSIVE non-nil means to search recursively,
PRED can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into)."
  (unless regexp (setq regexp js-imports-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir regexp include-dirs)
    (directory-files dir t regexp t)))

(defun js-imports-join-file (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (not (file-name-absolute-p (car args)))))
    (mapc (lambda (arg)
            (when arg
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
  "Apply functions from `js-imports-normalize-paths-functions' to PATH."
  (seq-reduce
   (lambda (acc fn) (funcall fn acc))
   (reverse js-imports-normalize-paths-functions)
   path))

(defun js-imports-maybe-remove-path-index (path)
  "Trim index with extension from PATH."
  (replace-regexp-in-string
   "/index$"
   ""
   (js-imports-remove-ext path)))

(defun js-imports-remove-ext (path)
  "Trim extension for PATH."
 (replace-regexp-in-string
   "\\(\\.d\\)?[\\.]\\(?:cjs\\|es6?\\|iced\\|jsx?\\|l\\(?:iticed\\|s\\)\\|mjs\\|sjs\\|ts\\|tsx\\)$"
   "" path))

(defun js-imports-is-index-file-p (path)
  "Return t if base name of PATH equals index, nil otherwise."
  (js-imports-string-match-p js-imports-file-index-regexp path))

(defun js-imports-relative-p (path)
  "Return t if PATH is relative, nil otherwise."
  (js-imports-string-match-p "^\\(\\(\\.\\)?[\\.]\\)\\(/\\|$\\)"
                             path))

(defun js-imports-dependency-p (module &optional project-root)
  "Check if MODULE is dependency of PROJECT-ROOT.
Dependencies are recognized by `package.json' or `node_modules' of
PROJECT-ROOT."
  (or (member module (js-imports-node-modules-candidates project-root))
      (let ((node-dir (js-imports-find-node-modules project-root)))
        (and node-dir
             (file-exists-p
              (expand-file-name (car (split-string module "/"))
                                node-dir))))))

(defun js-imports-path-to-real (path &optional dir)
  "Resolve PATH to absolute filename.
Optional argument DIR is used as default directory."
  (when (and path (stringp path))
    (setq path (js-imports-strip-text-props path))
    (when-let ((result
                (cond ((file-name-absolute-p path)
                       (car (js-imports-resolve-paths path)))
                      ((js-imports-relative-p path)
                       (car (js-imports-resolve-paths path dir)))
                      ((js-imports-dependency-p path
                                                (js-imports-find-project-root))
                       (js-imports-node-module-to-real path))
                      (t
                       (car (js-imports-alias-path-to-real path))))))
      (if (and (file-exists-p result)
               (not (file-directory-p result)))
          result
        nil))))

(defun js-imports-sort-by-exts (files &optional extensions)
  "Sort FILES by EXTENSIONS or `js-imports-preffered-extensions'."
  (setq extensions (or extensions js-imports-preffered-extensions))
  (seq-sort-by (lambda (a)
                 (if-let ((ext (and a (file-name-extension a))))
                     (or (seq-position extensions ext #'string=) -1)
                   -1))
               #'>
               files))

(defun js-imports-get-path-ext-candidates (path)
  "Return files filtered by base name of PATH from the parent directory of PATH.
PATH should be an absolute filename without extension."
  (let ((parts (reverse (split-string path "/")))
        (module-re)
        (parent-dir))
    (setq module-re (concat "\\(/\\|^\\)" (pop parts)
                            js-imports-file-ext-regexp))
    (setq parent-dir (concat (or (string-join
                                  (reverse parts) "/") "") "/"))
    (directory-files parent-dir t module-re)))

(defun js-imports-resolve-paths (path &optional dir)
  "Resolve PATH in DIR and return list of existing files."
  (let ((fullpath (expand-file-name path dir)))
    (js-imports-sort-by-exts
     (if (file-exists-p fullpath)
         (if (not (file-directory-p fullpath))
             (list fullpath)
           (or (js-imports-get-path-ext-candidates
                fullpath)
               (js-imports-get-path-ext-candidates
                (expand-file-name "index"
                                  fullpath))))
       (js-imports-get-path-ext-candidates fullpath)))))

(defun js-imports-alias-path-to-real (path)
  "Convert aliased PATH to absolute file name."
  (when-let ((alias-cell (seq-find (lambda (it)
                                     (string-match-p
                                      (concat "^" (car it)) path))
                                   js-imports-project-aliases)))
    (let* ((alias (car alias-cell))
           (paths (cdr alias-cell))
           (trimmed-path (if (string-empty-p alias)
                             path
                           (replace-regexp-in-string
                            "^/" "" (replace-regexp-in-string
                                     (concat  "^" alias)
                                     ""
                                     path)))))
      (delete nil (mapcan (lambda (it)
                            (js-imports-resolve-paths trimmed-path it))
                          paths)))))

(defun js-imports-add-ext-if-not (file extension)
  "Add EXTENSION to FILE if it doesn't match `js-imports-file-ext-regexp'."
  (if (js-imports-string-match-p js-imports-file-ext-regexp file)
      file
    (concat file "." extension)))

(defun js-imports-try-ext (path &optional dir)
  "Add to PATH extensions and return the first existing result or nil otherwise.
Extensions is stored in a variable `js-imports-preffered-extensions'.
With the optional argument DIR expand PATH to DIR."
  (let ((expanded-path (if dir (expand-file-name path dir) path)))
    (if (js-imports-string-match-p js-imports-file-ext-regexp path)
        expanded-path
      (seq-find #'file-exists-p
                (mapcar (apply-partially #'js-imports-add-ext-if-not
                                         expanded-path)
                        js-imports-preffered-extensions)))))

(defun js-imports-next-or-prev-alias (&optional direction)
  "Cycle over `js-imports-aliases' and set alias to `js-imports-current-alias'.
Optional argument DIRECTION should be a number.
If DIRECTION is a positive return next or first element.
If DIRECTION is a negative return previous or last element."
  (unless direction (setq direction 1))
  (let* ((aliases (if (> direction 0)
                      js-imports-aliases
                    (reverse js-imports-aliases))))
    (setq js-imports-current-alias (if js-imports-current-alias
                                       (car (cdr
                                             (member js-imports-current-alias
                                                     aliases)))
                                     (car aliases)))))

(defun js-imports-get-package-json-modules (&optional project-root)
  "Return dependencies of PROJECT-ROOT from package.json."
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
    (delete nil
            (mapcan (lambda (section)
                      (when-let ((hash (gethash section
                                                package-json)))
                        (hash-table-keys hash)))
                    js-imports-package-json-sections))))

(defun js-imports-extract-subpackages (path)
  "Return directories in PATH with package.json."
  (let ((dirs (and path
                   (file-directory-p path)
                   (seq-filter #'file-directory-p
                               (mapcar
                                (js-imports--rpartial
                                 #'expand-file-name
                                 path)
                                (funcall
                                 (js-imports--compose
                                  (apply-partially #'delete "node_modules")
                                  (apply-partially #'delete "..")
                                  (apply-partially #'delete ".")
                                  #'directory-files)
                                 path))))))
    (append dirs (mapcan #'js-imports-extract-subpackages dirs))))

(defun js-imports-find-node-modules-submodules (node-modules-path modules)
  "Extract nested packages from MODULES.
Every element in MODULES should be listed in packages.json.
NODE-MODULES-PATH is used to expand path of MODULES."
  (let ((submodules)
        (prefix-regexp (concat "^" (js-imports-slash node-modules-path))))
    (dolist (element modules)
      (let ((path (expand-file-name element node-modules-path))
            (dirs))
        (setq dirs
              (mapcar (lambda (it) (replace-regexp-in-string prefix-regexp ""
                                                        it))
                      (js-imports-extract-subpackages path)))
        (setq submodules (append dirs submodules))))
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

(defun js-imports-find-node-modules (&optional project-dir)
  "Return the path to node-modules for PROJECT-DIR."
  (if (file-name-absolute-p js-imports-node-modules-dir)
      js-imports-node-modules-dir
    (when-let ((root (or project-dir (js-imports-find-project-root))))
      (setq root (car (split-string root js-imports-node-modules-regexp)))
      (js-imports-join-when-exists root js-imports-node-modules-dir))))

(defun js-imports-node-module-to-real (module &optional project-root)
  "Find MODULE from PROJECT-ROOT in node_modules and return it full path."
  (when-let* ((node-modules (or (js-imports-find-node-modules project-root)
                                (js-imports-find-node-modules)))
              (real-path (js-imports-join-file node-modules module)))
    (if (and (js-imports-string-match-p js-imports-file-ext-regexp real-path)
             (file-name-absolute-p real-path))
        real-path
      (js-imports-try-find-real-path real-path))))

(defun js-imports-try-find-real-path (path)
  "Resolve PATH as dependency."
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
            (seq-find #'js-imports-is-index-file-p files)))
        (car (js-imports-resolve-paths path))
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

(defun js-imports-try-json-sections (json-file sections)
  "Read JSON-FILE and return first section from SECTIONS."
  (let (section)
    (while sections
      (setq section (js-imports-read-package-json-section
                     json-file
                     (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))

(defun js-imports-join-when-exists (&rest args)
  "Return joined ARGS when exists."
  (let ((joined-path (apply #'js-imports-join-file args)))
    (when (file-exists-p joined-path)
      joined-path)))

;; parsing

(defun js-imports-find-imported-files ()
  "Return list of with imported imported paths in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (imported-files)
      (with-syntax-table js-imports-mode-syntax-table
        (while (js-imports-re-search-forward
                js-imports-regexp-import-keyword nil t 1)
          (when-let ((path (js-imports-get-path-at-point)))
            (push path imported-files))))
      (reverse imported-files))))

(defun js-imports-preselect-file ()
  "Preselect function for file sources."
  (if-let ((path (with-current-buffer js-imports-current-buffer
                   (or js-imports-last-export-path
                       js-imports-current-export-path
                       (when (> (point-max) (point))
                         (js-imports-get-path-at-point))))))
      (regexp-quote path)
    ""))

(defun js-imports-parse-es-imports (&optional real-path)
  "Extract imported symbols in REAL-PATH."
  (goto-char 0)
  (let (symbols)
    (while (js-imports-re-search-forward js-imports-regexp-import-keyword
                                         nil
                                         t
                                         1)
      (js-imports-forward-whitespace)
      (let (display-path
            imports)
        (cond ((js-imports-looking-at "*")
               (let (beg
                     end
                     renamed-name)
                 (setq beg (point))
                 (forward-char)
                 (skip-chars-forward "\s\t")
                 (setq end (point))
                 (when (js-imports-looking-at "as")
                   (skip-chars-forward "as")
                   (setq end (point))
                   (skip-chars-forward "\s\t")
                   (setq renamed-name (js-imports-which-word))
                   (if (not (js-imports-valid-identifier-p renamed-name))
                       (setq renamed-name "")
                     (skip-chars-forward js-imports-regexp-name)))
                 (setq end (point))
                 (push (js-imports-make-item
                        (format "%s"
                                (buffer-substring-no-properties
                                 beg end))
                        :type 16
                        :real-name "*"
                        :as-name renamed-name
                        :start beg)
                       imports)))
              ((looking-at "\\_<\\(type\\)\\_>")
               (skip-chars-forward "type")
               (js-imports-forward-whitespace)
               (if (or (looking-at ",")
                       (looking-at "from"))
                   (push (js-imports-make-item "type"
                                               :type 1
                                               :real-name "type"
                                               :as-name "type"
                                               :start (point))
                         imports)))
              ((js-imports-get-word-if-valid)
               (when-let ((name (js-imports-get-word-if-valid)))
                 (push (js-imports-make-item name
                                             :type 1
                                             :real-name name
                                             :as-name name
                                             :start (point))
                       imports)
                 (skip-chars-forward name)
                 (js-imports-forward-whitespace)
                 (skip-chars-forward ",")
                 (js-imports-forward-whitespace))))
        (when (looking-at-p "{")
          (let ((map-func
                 (lambda (item)
                   (js-imports-propertize item
                                          :type 4
                                          :start
                                          (js-imports-get-prop item :start))))
                (named-items
                 (js-imports-extract-esm-braced-symbols
                  js-imports-name-as--re)))
            (setq named-items (mapcar map-func named-items))
            (setq imports (append imports named-items))))
        (unless (looking-at-p "[\"']")
          (js-imports-re-search-forward js-imports-from-keyword--re nil t 1)
          (js-imports-forward-whitespace)
          (when (looking-at-p "[\"']")
            (save-excursion
              (forward-char 1)
              (setq display-path (js-imports-get-path-at-point)))
            (setq imports
                  (mapcar
                   (lambda (it)
                     (js-imports-propertize it
                                            :display-path display-path
                                            :real-path real-path
                                            :import t))
                   imports))
            (setq symbols (append symbols imports))))))
    symbols))

(defun js-imports-extract-es-imports (real-path)
  "Return imported with ES syntax symbols in REAL-PATH."
  (js-imports-with-buffer-or-file-content real-path
      (js-imports-parse-es-imports real-path)))

(defun js-imports-extract-esm-braced-symbols (&optional regexp)
  "At start of braces extract items matching REGEXP."
  (save-excursion
    (when-let* ((brace-start (when (looking-at-p "{") (point)))
                (brace-end (save-excursion (forward-list) (point))))
      (let (items)
        (save-restriction
          (narrow-to-region brace-start brace-end)
          (js-imports-remove-comments)
          (while (js-imports-re-search-forward regexp nil t 1)
            (when-let* ((start (match-beginning 0))
                        (end (match-end 0))
                        (parts (split-string (buffer-substring-no-properties
                                              start end)))
                        (full-name (mapconcat #'identity parts "\s")))
              (push (js-imports-make-item full-name
                                          :real-name (car parts)
                                          :as-name (car (reverse parts))
                                          :start start
                                          :end end)
                    items))))
        items))))

(defun js-imports-get-next-char (&optional nth position)
  "Return concatenated string with NTH chars after POSITION.
Default value for NTH is 1 and for POSITION - value of current the point."
  (let* ((beg (or position (point)))
         (end (+ (or nth 1) beg)))
    (when (> (point-max) end)
      (buffer-substring-no-properties beg end))))

(defun js-imports-get-prev-char (&optional nth position)
  "Return concatenated string with NTH chars before POSITION.
Default value for NTH is 1 and for POSITION - value of current the point."
  (let* ((end (or position (point)))
         (beg (- end (or nth 1))))
    (when (>= beg (point-min))
      (buffer-substring-no-properties
       beg end))))

(defun js-imports-get-next-char-or-word ()
  "Return word or character after current the point."
  (when-let ((char (js-imports-get-next-char)))
    (if (string-match-p "[_$A-Za-z0-9]" char)
        (js-imports-which-word)
      char)))

(defun js-imports-init-exports-candidates ()
  "Search for exported symbols in a file `js-imports-current-export-path'.
If `js-imports-current-export-path' is nil, use current buffer file name."
  (with-current-buffer js-imports-current-buffer
    (setq js-imports-export-candidates-in-path
          (when (or js-imports-current-export-path
                    buffer-file-name)
            (js-imports-extract-all-exports (or js-imports-current-export-path
                                                buffer-file-name))))
    js-imports-export-candidates-in-path))

(defun js-imports-extract-all-exports (&optional init-path)
  "Return all found exports in INIT-PATH and reexported modules."
  (let* ((esm-exports)
         (cjs-exports)
         (external-paths)
         (path)
         (processed-paths)
         (map-result (lambda (result)
                       (cond ((listp result)
                              (setq result (delete nil result))
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
                             ((and result)
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
                          (unless (bobp)
                            (backward-char 1)
                            (not (looking-at "[\s\t\n;/*]")))))
              (js-imports-forward-whitespace)
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
                  (js-imports-forward-whitespace)
                  (when-let ((result (js-imports-extract-cjs-exports
                                      buffer-file-name)))
                    (cond ((listp result)
                           (setq result (delete nil result))
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
                          ((and result)
                           (push result cjs-locals))))))
              (when cjs-locals
                (setq cjs-exports (seq-uniq
                                   (append cjs-exports cjs-locals)))))))))
    (mapcar (js-imports--rpartial
             js-imports-propertize :export t)
            (reverse (delete nil (append esm-exports cjs-exports))))))

(defun js-imports-parse-export-clause (&optional path)
  "Return list of exports between braces at point.
Propertize every item with :real-path using PATH as value, and
:start, :end, :type, :display-path (optional)."
  (when-let ((symbols
              (js-imports-extract-esm-braced-symbols js-imports-name-as--re))
             (map-func
              (lambda (it)
                (let ((type
                       (if (equal "default" (js-imports-get-prop it :as-name))
                           1
                         4))
                      (pos (js-imports-get-prop it :start)))
                  (js-imports-propertize it
                                         :type type
                                         :real-path path
                                         :start pos)))))
    (let ((from-path
           (progn
             (forward-list)
             (js-imports-forward-whitespace)
             (when (js-imports-looking-at "from")
               (skip-chars-forward "from")
               (js-imports-forward-whitespace)
               (forward-char 1)
               (js-imports-get-path-at-point)))))
      (setq symbols
            (if from-path
                (mapcar
                 (lambda (it)
                   (js-imports-propertize (funcall map-func it)
                                          :display-path from-path))
                 symbols)
              (mapcar map-func symbols)))
      symbols)))

(defun js-imports-make-esm-export-at-point (&optional path)
  "Return exports in PATH defined with ES Module syntax."
  (cond ((looking-at-p "\\*")
         (let ((start (point))
               (as-name)
               (full-name)
               (end))
           (forward-char 1)
           (js-imports-forward-whitespace)
           (pcase (js-imports-which-word)
             ("as" (progn
                     (re-search-forward "as" nil t 1)
                     (js-imports-forward-whitespace)
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
                                           :start start)))
             ("from" (when-let ((from (js-imports-get-path-at-point)))
                       (js-imports-make-item "*"
                                             :type 16
                                             :as-name "*"
                                             :real-name "*"
                                             :display-path from
                                             :real-path path
                                             :start start))))))
        ((looking-at-p "{")
         (js-imports-parse-export-clause path))
        ((looking-at js-imports-regexp-name-set)
         (let* ((stack (js-imports-skip-reserved-words "\s\t\\*"))
                (default (car (assoc "default" stack)))
                (real-name
                 (or (car
                      (seq-find
                       (js-imports--compose
                        js-imports-valid-identifier-p
                        car)
                       stack))
                     default))
                (var-type
                 (car
                  (seq-find
                   (lambda (it)
                     (member (car it) js-imports-delcaration-keywords))
                   stack)))
                (as-name (or real-name default)))
           (if (looking-at "{")
               (js-imports-parse-export-clause path)
             (js-imports-make-item (or as-name real-name)
                                   :type
                                   (if default
                                       1
                                     4)
                                   :real-name (or real-name default as-name)
                                   :real-path path
                                   :as-name as-name
                                   :var-type var-type
                                   :start (point)))))))

(defun js-imports-extract-cjs-exports (path)
  "Parse node at the point if it is defined in common JS syntax.
Return propertized string where PATH is added as :display-path"
  (cond ((looking-at-p "=\\([\s\t\n]+?\\)require[ \t('\"]")
         (js-imports-re-search-forward "require" nil t 1)
         (js-imports-forward-whitespace)
         (when (looking-at "([\"']")
           (re-search-forward "([\"']"))
         (when-let ((from (js-imports-get-path-at-point)))
           (js-imports-make-item "*"
                                 :type 16
                                 :as-name "*"
                                 :real-name "*"
                                 :display-path from
                                 :real-path path
                                 :start (point))))
        ((looking-at-p "=\\([\s\t]+?\\){")
         (js-imports-re-search-forward "=" nil t 1)
         (js-imports-forward-whitespace)
         (when-let* ((items (js-imports-parse-object-keys)))
           (when (looking-at-p "{")
             (forward-list))
           (mapcar (lambda (cell)
                     (let ((name (car cell)))
                       (js-imports-make-item
                        name
                        :start (cdr cell)
                        :as-name name
                        :type 4
                        :real-path path
                        :real-name name)))
                   items)))
        ((looking-at-p "=[^=]")
         (forward-char 1)
         (js-imports-forward-whitespace)
         (when-let ((real-name (js-imports-get-word-if-valid)))
           (js-imports-make-item
            real-name
            :type 1
            :real-name real-name
            :as-name real-name
            :real-path path
            :start (point))))
        ((looking-at-p "[.]")
         (forward-char 1)
         (when-let ((as-name (js-imports-which-word)))
           (js-imports-make-item
            as-name
            :type (if (string= as-name "default")  1 4)
            :as-name as-name
            :real-name (progn
                         (js-imports-re-search-forward "=" nil t 1)
                         (js-imports-forward-whitespace)
                         (or (js-imports-get-word-if-valid)
                             as-name))
            :real-path path
            :start (point))))))

(defun js-imports-parse-object-keys ()
  "Return object keys at the point."
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
                (skip-chars-backward "\s\t\n\r\f\v")
                (setq prop (js-imports-which-word))
                (when prop
                  (skip-chars-backward prop)
                  (push (cons prop (point)) children)))
              (cond ((char-equal delimiter ?:)
                     (skip-chars-forward "\s\t\n\r\f\v")
                     (when (looking-at-p js-imports-regexp-name-set)
                       (skip-chars-forward js-imports-regexp-name)
                       (skip-chars-forward "\s\t\n."))
                     (when (looking-at-p "=>")
                       (forward-char 2)
                       (skip-chars-forward "\s\t\n\r\f\v"))
                     (if (looking-at-p "[({[]")
                         (progn
                           (while (looking-at-p "[({[]")
                             (forward-list)
                             (skip-chars-forward "\s\t\n\r\f\v")
                             (when (looking-at-p "=>")
                               (forward-char 2))
                             (skip-chars-forward "\s\t\n.")
                             (skip-chars-forward js-imports-regexp-name)))
                       (re-search-forward "," nil t 1)))
                    ((char-equal delimiter ?\()
                     (backward-char)
                     (forward-list)
                     (skip-chars-forward "\s\t\n\r\f\v")
                     (forward-list)
                     (forward-char))
                    ((char-equal delimiter ?,)
                     (progn
                       (save-excursion
                         (skip-chars-forward "\s\t\n\r\f\v")
                         (when-let ((word (js-imports-which-word))
                                    (pos (point)))
                           (skip-chars-forward word)
                           (skip-chars-forward "\s\t\n\r\f\v")
                           (push (cons word pos) children))))))))))
      (when children
        (seq-uniq (delete nil children))))))

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
          (while (re-search-backward "[,}]" nil t 1)
            (when (looking-at "}")
              (forward-char 1)
              (backward-list 1))
            (skip-chars-backward js-imports-regexp-name)
            (push (js-imports-maybe-make-child-at-point parent) children))
          (delete nil children))))))

(defun js-imports-skip-reserved-words (&optional separators)
  "Skip reserved JavaScript keywords divided SEPARATORS.
Default value for SEPARATORS is whitespaces and * char."
  (unless separators (setq separators "\s\t\\*"))
  (let* ((stack)
         (prev)
         (word))
    (while (and
            (not (equal prev (point)))
            (js-imports-reserved-word-p (setq word (js-imports-which-word))))
      (setq prev (point))
      (skip-chars-forward word)
      (js-imports-forward-whitespace)
      (skip-chars-forward separators)
      (push (cons word prev) stack))
    (when-let ((id (js-imports-which-word)))
      (push (cons id (point)) stack))
    (delete nil stack)))

(defun js-imports-join-imports-names (default-name names)
  "Concatenates DEFAULT-NAME with NAMES."
  (when (and names (listp names))
    (setq names (string-join names ", ")))
  (mapconcat #'string-trim
             (delete
              nil
              `(,default-name ,(when names
                                 (concat "{ " (string-trim
                                               (replace-regexp-in-string
                                                "}\\|{"
                                                ""
                                                names))
                                         " }"))))
             ", "))

(defun js-imports-goto-last-import ()
  "Jump to the end of last esm import."
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
        (let ((beg (- (point)
                      (length "import")))
              (end))
          (js-imports-forward-whitespace)
          (unless (looking-at-p "[\"']")
            (js-imports-re-search-forward js-imports-from-keyword--re nil t 1)
            (js-imports-forward-whitespace))
          (when (looking-at-p "[\"']")
            (forward-char 1)
            (js-imports-skip-string)
            (setq end (point))
            (js-imports-forward-whitespace)
            (when (looking-at ";")
              (setq end (1+ (point))))
            (push (cons beg end)
                  alist))))
      (reverse alist))))

(defun js-imports-inside-import-p (&optional position)
  "Return import bounds if POSITION inside import statement."
  (let ((pos (or position (point))))
    (seq-find
     (js-imports--and
      (js-imports--compose (js-imports--partial >= pos) car)
      (js-imports--compose (js-imports--partial <= pos) cdr))
     (js-imports-get-es-imports-bounds))))

(defun js-imports-extract-import-path-bounds (&optional import-bounds)
  "Return path of import statement specified in IMPORT-BOUNDS."
  (when-let ((end (cdr import-bounds)))
    (save-excursion
      (goto-char end)
      (skip-chars-backward ";\s\t\n")
      (js-imports-backward-whitespace)
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
      (when (re-search-backward (concat "from +['\"]" path "['\"]") nil t)
        (re-search-forward "['\"]+;?" nil t 2)
        (setq pos2 (point))
        (re-search-backward js-imports-regexp-import-keyword nil t)
        (setq pos1 (point)))
      (cons pos1 pos2))))

(defun js-imports-make-item (candidate &rest plist)
  "Propertize CANDIDATE with filtered PLIST."
  (let ((pl plist)
        (key)
        (filtered-pl))
    (while (setq key (pop pl))
      (when-let ((val (pop pl)))
        (push val filtered-pl)
        (push key filtered-pl)))
    (apply #'js-imports-propertize candidate filtered-pl)))

(defun js-imports-propertize (item &rest props)
  "Stringify and `propertize' ITEM with PROPS."
  (apply #'propertize
         (js-imports-stringify item)
         props))

(defun js-imports-get-prop (str property)
  "Return the value of zero position's PROPERTY in STR."
  (if (stringp str)
      (get-text-property 0 property str)))

(defun js-imports-strip-text-props (item)
  "If ITEM is a string, return it without text properties.
If ITEM is a symbol, return it is `symbol-name.'
Otherwise, return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str) nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-imports-stringify (x)
  "Convert X to string."
  (cond
   ((stringp x)
    x)
   ((stringp x)
    (symbol-name x))
   ((numberp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun js-imports-looking-at-comment-p (&optional max)
  "Return if the point located at the start of comment.
Optional argument MAX defines a limit."
  (and (> (or max (1- (point-max)))
          (point))
       (car (member (buffer-substring-no-properties
                     (point)
                     (+ 2 (point)))
                    '("#!" "/*" "//")))))

(defun js-imports-forward-whitespace (&optional skip-chars)
  "Move the point forward accross SKIP-CHARS and comments.
Returns the distance traveled, either zero or positive."
  (unless skip-chars (setq skip-chars "\s\t\n\r\f\v"))
  (let ((total (skip-chars-forward skip-chars))
        (max (1- (point-max))))
    (while (and (>= max (point))
                (pcase (js-imports-looking-at-comment-p max)
                  ("//" (forward-line 1) t)
                  ("/*" (js-imports-re-search-forward "\\([*]/\\)" nil t 1))
                  ("#!" (when (js-imports-re-search-forward "." nil t 1)
                          (forward-char -1)))))
      (setq total (+ total (skip-chars-forward skip-chars))))
    total))

(defun js-imports-backward-whitespace (&optional skip-chars)
  "Move the point backward accross SKIP-CHARS and comments.
Returns the distance traveled, either zero or positive."
  (unless skip-chars (setq skip-chars "\s\t\n\r\f\v"))
  (let ((total (skip-chars-backward skip-chars))
        (min (1+ (point-min)))
        (pos))
    (while (and (> (point) min)
                (or (js-imports-inside-comment-p)
                    (equal (js-imports-get-prev-char) "/"))
                (setq pos
                      (js-imports-re-search-backward
                       "[^\s\t\n\r\f\v]" nil t 1)))
      (setq total (+ total (skip-chars-backward skip-chars))))
    (when pos
      (goto-char pos)
      (unless (looking-at "//\\|/[*]\\|#!")
        (forward-char 1)))
    total))

(defun js-imports-get-word-if-valid ()
  "Return word at the point if it is valid and not reserved, otherwise nil."
  (when-let ((word (js-imports-which-word)))
    (when (js-imports-valid-identifier-p word)
      word)))

(defun js-imports-looking-at (name)
  "Return t if the NAME and the word under the cursor have identical contents."
  (when-let ((word (js-imports-which-word)))
    (string= word name)))

(defun js-imports-get-rebounds-at-point (&optional rechars)
  "Get bounds of thing at the point depending on RECHARS."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward (or rechars "*_$A-Za-z0-9"))
                (point)))
           (b (save-excursion
                (skip-chars-forward (or rechars "*_$A-Za-z0-9"))
                (point))))
      (if (string-blank-p
           (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun js-imports-which-word (&optional regexp)
  "Return string with a thing at the point matched REGEXP or nil otherwise."
  (when-let ((bounds (js-imports-get-rebounds-at-point regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun js-imports-get-path-at-point ()
  "Return closest module path at the point."
  (save-excursion
    (when-let* ((word (js-imports-which-word))
                (meta-word (or (string= "import" word)
                               (string= "export" word)
                               (string= "from" word))))
      (if (string= word "from")
          (search-forward-regexp "['\"]" nil t 1)
        (search-forward-regexp "[\s\t\n]+from[\s\t\n]+['\"]" nil t 1)))
    (when (js-imports-inside-string-p)
      (let (p0 p1 p2 stops)
        (setq stops "^ \t\n\"`'‘’“”|[]{}·")
        (setq p0 (point))
        (skip-chars-backward stops)
        (setq p1 (point))
        (goto-char p0)
        (skip-chars-forward stops)
        (setq p2 (point))
        (goto-char p0)
        (buffer-substring-no-properties p1 p2)))))

(defun js-imports-string-match-p (regexp str &optional start)
  "Check STR for a match with REGEXP and return t or nil whether it exists."
  (when (and str (stringp str)
             (string-match-p regexp str start))
    t))

(defun js-imports-inside-string-p (&optional position)
  "Return non-nil if the point POSITION inside string, else nil.
Result depends on syntax table's string quote character."
  (with-syntax-table js-imports-mode-syntax-table
    (nth 3 (syntax-ppss (or position (point))))))

(defun js-imports-skip-string ()
  "Jumps to the end of string."
  (with-syntax-table js-imports-mode-syntax-table
    (while (nth 8 (syntax-ppss (point)))
      (forward-char 1))))

(defun js-imports-inside-comment-p ()
  "Return value of comment character in syntax table's or nil otherwise."
  (with-syntax-table js-imports-mode-syntax-table
    (let ((comment-start "//")
          (comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
          (comment-use-syntax t)
          (result (nth 4 (syntax-ppss))))
      result)))

(defun js-imports-valid-identifier-p (string)
  "Return t if STRING is a valid variable name, otherwise nil."
  (not (or
        (null string)
        (js-imports-string-match-p (concat "[" "^" js-imports-regexp-name "]")
                                   string)
        (js-imports-reserved-word-p string))))

(defun js-imports-generate-name-from-path (path)
  "Generate name for default or module import from PATH."
  (funcall
   (js-imports--compose #'string-join
                       (js-imports--rpartial #'seq-take 2)
                       #'reverse
                       (js-imports--partial #'mapcar #'capitalize)
                       #'seq-uniq
                       (js-imports--rpartial #'split-string "[ \f\t\n\r\v/.-]")
                       #'js-imports-maybe-remove-path-index
                       #'js-imports-remove-ext)
   path))

(defun js-imports-maybe-make-child-at-point (&optional parent)
  "Parse argument at the point of PARENT."
  (when-let ((valid-id (js-imports-get-word-if-valid)))
    (skip-chars-backward valid-id)
    (prog1 (js-imports-make-item valid-id
                                 :start (point)
                                 :real-name
                                 (and (looking-back ":" 0)
                                      (save-excursion
                                        (backward-char 1)
                                        (js-imports-get-word-if-valid)))
                                 :parent parent
                                 :var-type (or parent "argument")
                                 :as-name valid-id)
      (skip-chars-backward "\s\t\n\r\f\v"))))

(defun js-imports-highlight-word (&optional start buffer)
  "Jumps to START in the BUFFER and highlight word at the point."
  (unless buffer (setq buffer (current-buffer)))
  (setq buffer (get-buffer-create buffer))
  (unless start (setq start (point)))
  (with-current-buffer buffer
    (goto-char start)
    (let* ((buffer-name (if (bufferp buffer) (intern (buffer-name buffer))
                          (intern buffer)))
           (end (+ start (length (js-imports-which-word))))
           (overlay (get buffer-name 'overlay)))
      (when overlay
        (delete-overlay overlay))
      (setq overlay (make-overlay start end buffer))
      (put buffer-name 'overlay overlay)
      (overlay-put overlay 'face 'js-imports-highlight-face)
      (unwind-protect
          (progn
            (when (and overlay (overlayp overlay))
              (move-overlay overlay start end)))
        (run-with-timer 1 nil (lambda (o) (when (and o
                                                (overlayp o))
                                       (delete-overlay o)))
                        overlay)))))

(defun js-imports-extract-parent-arguments (&optional parens-positions)
  "Extract arguments between PARENS-POSITIONS."
  (save-excursion
    (with-syntax-table js-imports-mode-syntax-table
      (let ((open-parens (or parens-positions (nth 9 (syntax-ppss (point)))))
            (children))
        (dotimes (idx (length open-parens))
          (let ((paren-pos (nth idx open-parens))
                (parent)
                (items))
            (goto-char paren-pos)
            (skip-chars-backward "\s\t\n\r\f\v")
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
                (skip-chars-backward "\s\t\n\r\f\v")
                (when-let ((child
                            (js-imports-maybe-make-child-at-point parent)))
                  (push child items)))
              (when-let ((end-pos (when (save-excursion
                                          (backward-char 1) (looking-at ")"))
                                    (point)))
                         (start (progn (backward-list 1) (point))))
                (forward-char)
                (skip-chars-forward "\s\t\n\r\f\v")
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
                           (skip-chars-backward "\s\t\n\r\f\v"))
                         (let ((child
                                (js-imports-maybe-make-child-at-point
                                 parent)))
                           (push child items)
                           (skip-chars-backward child)
                           (skip-chars-backward "\s\t\n\r\f\v"))
                         (unless
                             (js-imports-re-search-backward
                              "," start t 1)
                           (goto-char start))))))
              (setq children (append children items)))))
        children))))

(defun js-imports-next-declaration-or-scope (&optional pos)
  "Move POS to next node or scope start."
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
                 (js-imports-backward-whitespace)
                 (and (char-equal (char-before (point)) ?>)
                      (char-equal (char-before (1- (point))) ?=)
                      (js-imports-re-search-backward
                       js-imports-expression-keywords--re nil t 1))
                 (forward-word)
                 (js-imports-forward-whitespace)
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
  "Go to next declaration or scope starting from POS."
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
        (setq winner (if (>= scope-end declaration-start)
                         scope-start
                       declaration-start))
      (setq winner (or scope-start declaration-start)))
    (when winner (goto-char winner))))

(defun js-imports-previous-declaration (&optional pos)
  "Go to previous declaration starting from POS."
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
  "Return t if current node is function expression."
  (and (js-imports-looking-at "function")
       (< (point-min) (point))
       (save-excursion
         (js-imports-backward-whitespace)
         (let ((c (char-before (point))))
           (or (char-equal c ?=)
               (char-equal c ?|)
               (char-equal c ??)
               (char-equal c ?:))))))

(defun js-imports-declaration-at-point ()
  "Parse node when the symbol under the point is a declaration keyword."
  (when (and (looking-at js-imports-delcaration-keywords--re)
             (not (js-imports-looking-at-function-expression)))
    (let ((var-type (js-imports-which-word)))
      (save-excursion
        (js-imports-re-search-forward var-type nil t 1)
        (when (looking-at "\\*")
          (forward-char 1))
        (js-imports-forward-whitespace)
        (when (looking-at "\\*")
          (forward-char 1)
          (js-imports-forward-whitespace))
        (if-let* ((word (js-imports-get-word-if-valid)))
            (js-imports-make-item word
                                  :start (point)
                                  :var-type var-type
                                  :real-name word
                                  :as-name word)
          (cond
           ((looking-at "{")
            (let (parent children display-path)
              (when (looking-at-p "{")
                (save-excursion
                  (forward-list)
                  (js-imports-forward-whitespace)
                  (when (looking-at-p "=[^=]")
                    (forward-char 1)
                    (js-imports-forward-whitespace)
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
                                  (js-imports--rpartial
                                   js-imports-propertize
                                   :var-type nil
                                   :parent parent
                                   :import t
                                   :display-path display-path
                                   :type 4)
                                  children))))
              children))))))))

(defun js-imports-extract-definitions (&optional path position)
  "Extract visible on POSITION declarations in PATH.
By default PATH is taken from a variable `buffer-file-name'.
Default value for POSITION also current the point position."
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
    (mapcar (js-imports--rpartial js-imports-propertize
                                  :real-path path)
            ids)))

(defun js-imports-find-definition (item)
  "Search ITEM definition."
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
                            (pos (js-imports-get-prop current-item :start))
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
  "Jumps to ITEM in the BUFFER in current window.
ITEM must be propertized with a keyword `pos'."
  (when-let ((pos (js-imports-get-prop item :start)))
    (js-imports-highlight-word pos buffer)
    item))

(defun js-imports-jump-to-item-other-window (item)
  "Jumps to ITEM in the BUFFER in other window.
ITEM must be propertized with a keyword `pos'."
  (unless (js-imports-get-prop item :start)
    (setq item (js-imports-display-to-real-exports item)))
  (when-let ((pos (js-imports-get-prop item :start))
             (item-path (or (js-imports-get-prop item :real-path)
                            (js-imports-path-to-real (js-imports-get-prop
                                                      item
                                                      :display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (js-imports-jump-to-item-in-buffer item)
    item))

(defun js-imports-transform-symbol (item &optional len)
  "Annotate ITEM and pad with spaces on the left to LEN."
  (let* ((display-path (js-imports-get-prop item :display-path))
         (export (and (js-imports-get-prop item :export)
                      (if display-path
                          "Reexport"
                        "Export")))
         (import (and (js-imports-get-prop item :import)
                      "Import"))
         (var (js-imports-get-prop item :var-type))
         (default (and (equal
                        (js-imports-get-prop item :type)
                        1)
                       "Default"))
         (parts (string-trim
                 (mapconcat
                  (js-imports--compose
                   (js-imports--rpartial
                    propertize 'face 'font-lock-function-name-face)
                   capitalize
                   string-trim)
                  (delete nil (list
                               export
                               import
                               default
                               var))
                  "\s")))
         (name (replace-regexp-in-string " default$" ""
                                         (or (js-imports-get-prop item :as-name)
                                             item)))
         (path-part (cond ((and import)
                           (concat "from\s" display-path))
                          (t "")))
         (result (string-join
                  (delete nil (list
                               (and len (make-string len ?\s))
                               (string-trim (or parts ""))
                               name
                               path-part))
                  "\s")))
    (apply #'propertize (append (list result)
                                (text-properties-at 0 item)))))

(defun js-imports-map-stack (items)
  "Annotate propertized ITEMS."
  (seq-map-indexed
   #'js-imports-transform-symbol
   (delete nil items)))

(defun js-imports-find-export-definition (export-symbol)
  "Find and jump to definition of EXPORT-SYMBOL."
  (unless (js-imports-get-prop export-symbol :start)
    (setq export-symbol (js-imports-display-to-real-exports export-symbol)))
  (when-let* ((definition (js-imports-find-definition export-symbol))
              (pos (js-imports-get-prop definition :start))
              (item-path (or (js-imports-get-prop definition :real-path)
                             (js-imports-path-to-real (js-imports-get-prop
                                                       definition
                                                       :display-path)))))
    (find-file-other-window item-path)
    (js-imports-jump-to-item-in-buffer definition)
    definition))

(defun js-imports-jump-to-symbol-action (symbol)
  "Jump to SYMBOL definition or export."
  (when-let ((item (js-imports-find-definition symbol)))
    (let ((stack (js-imports-get-prop item :stack)))
      (when stack
        (push item stack)
        (setq stack (funcall (js-imports--compose #'js-imports-map-stack
                                                 #'reverse)
                             stack))
        (setq item
              (completing-read "Jump:\s" stack nil t))))
    (when-let ((pos (and item (js-imports-get-prop
                               item :start))))
      (find-file
       (js-imports-get-prop item :real-path))
      (goto-char pos)
      (js-imports-highlight-word))))

(defun js-imports-imported-candidates-in-buffer (&optional buffer)
  "Return imported symbols from BUFFER.
Symbols are stored in the variable `js-imports-cached-imports-in-buffer'.
Cache are invalidated when `buffer-modified-tick' is changed."
  (with-current-buffer (or buffer js-imports-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-imports-buffer-tick tick)
          js-imports-cached-imports-in-buffer
        (progn
          (setq js-imports-buffer-tick tick)
          (setq js-imports-cached-imports-in-buffer
                (seq-remove #'js-imports-reserved-word-p
                            (js-imports-extract-es-imports buffer-file-name)))
          js-imports-cached-imports-in-buffer)))))

(defun js-imports-exported-candidates-transformer (candidates)
  "Remove duplicates and imported members from CANDIDATES."
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
      (setq exports (seq-uniq exports))
      (setq exports (mapcar (js-imports--rpartial
                             js-imports-propertize
                             :display-path
                             js-imports-current-export-path)
                            exports))
      exports)))

(defun js-imports-export-filtered-candidate-transformer (candidates
                                                         &optional
                                                         _source)
  "Extract :as-name property from CANDIDATES."
  (mapcar (js-imports--rpartial js-imports-get-prop :as-name)
          candidates))

(defun js-imports-filter-with-prop (property value items)
  "Return filtered ITEMS with members whose PROPERTY equals VALUE."
  (seq-filter
   (js-imports--compose
    (js-imports--partial
     (cond ((numberp value) #'=)
           ((stringp value) #'string=)
           (t #'equal))
     value)
    (js-imports--rpartial
     js-imports-get-prop property))
   items))

(defun js-imports-find-by-prop (property value items)
  "Find item in ITEMS whose PROPERTY equals VALUE."
  (seq-find
   (js-imports--compose
    (js-imports--partial #'equal value)
    (js-imports--rpartial #'js-imports-get-prop property))
   items))

(defun js-imports-display-to-real-exports (str)
  "Find STR in the variable `js-imports-export-candidates-in-path'."
  (js-imports-find-by-prop
   :as-name
   str
   (if js-imports-current-buffer
       (buffer-local-value
        'js-imports-export-candidates-in-path
        js-imports-current-buffer)
     js-imports-export-candidates-in-path)))

(defun js-imports-display-to-real-imports (item)
  "Find ITEM in the variable `js-imports-cached-imports-in-buffer.'."
  (car (member item (if js-imports-current-buffer
                        (buffer-local-value
                         'js-imports-cached-imports-in-buffer
                         js-imports-current-buffer)
                      js-imports-cached-imports-in-buffer))))

(defun js-imports-filter-exports-pred (elt imports)
  "Return t if ELT is a member of IMPORTS."
  (let ((type (js-imports-get-prop elt :type)))
    (pcase type
      (1
       (js-imports-find-by-prop :type type imports))
      (4
       (js-imports-find-by-prop :as-name
                                (js-imports-get-prop elt :as-name)
                                imports))
      (16
       (< 0 (length imports))))))

(defun js-imports-filter-exports (exports imports)
  "Return EXPORTS plist with only those members that are not in IMPORTS plist."
  (seq-remove (js-imports--rpartial js-imports-filter-exports-pred
                                    imports)
              exports))

(defun js-imports-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `js-imports-re-search-forward'.
Search forward from the point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
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
                (line-end-position) t))
              ((nth 7 parse)
               (forward-line))
              ((or (nth 4 parse)
                   (and (eq (char-before) ?\/)
                        (eq (char-after) ?\*)))
               (re-search-forward "\\*/"))
              (t
               (setq count (1- count)))))))
  (point))

(defun js-imports-re-search-forward (regexp &optional bound noerror count)
  "Search forward from the point for REGEXP ignoring comments and strings.
The optional arguments BOUND, NOERROR, COUNT has the same meaning
as for `re-search-forward'."
  (let ((case-fold-search nil))
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
           (signal (car err) (cdr err))))))))

(defun js-imports-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `js-imports-re-search-backward'.
Search backward from the point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
the search.  The match found must not end after that position.

A value of nil means search to the end of the accessible portion of
the buffer.

The optional argument COUNT is a number that indicates the
search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table js-imports-mode-syntax-table
        (re-search-backward regexp bound)
        (when (and (not (bobp))
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

(defun js-imports-re-search-backward (re &optional bound noerror count)
  "Search backward from the point for RE ignoring strings and comments.
The optional arguments BOUND, NOERROR, COUNT has the same meaning
as `re-search-backward'."
  (let ((case-fold-search nil))
    (js-imports-re-search-forward re bound noerror (if count (- count) -1))))

(defun js-imports-remove-comments ()
  "Replace comments in buffer with empty lines."
  (let ((comments (js-imports-get-comments-bounds))
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
            (insert
             (mapconcat
              (js-imports--compose
               (js-imports--rpartial string-join "\s")
               (js-imports--rpartial append nil)
               (js-imports--rpartial make-vector "")
               1+
               length)
              lines
              "\n"))))))))

(defun js-imports-get-comments-bounds ()
  "Return alist of bounds of comments in the current buffer."
  (save-excursion
    (save-restriction
      (with-syntax-table js-imports-mode-syntax-table
        (let (comments)
          (goto-char (point-min))
          (while (re-search-forward comment-start-skip nil t 1)
            (let ((beg (match-beginning 0))
                  (str (match-string-no-properties 0))
                  (end)
                  (stx))
              (setq stx (syntax-ppss (point)))
              (setq end
                    (cond ((nth 4 stx)
                           (pcase (string-trim str)
                             ("/*" (while
                                       (when (re-search-forward "[*]/" nil t 1)
                                         (nth 4 (syntax-ppss (point)))))
                              (point))
                             ("//"
                              (end-of-line)
                              (point))))))
              (when (and beg end)
                (push (cons beg end) comments))))
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
    (setq beg (js-imports-get-prop item :start))
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
  "Rename ITEM from named imports."
  (let* ((pos (js-imports-get-prop item :start))
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
            (skip-chars-forward "\s\t\n\r\f\v")
            (skip-chars-forward "as")
            (skip-chars-forward "\s\t\n\r\f\v")
            (when (and renamed-name
                       (string= renamed-name (js-imports-which-word)))
              (query-replace-regexp (concat "\\_<" renamed-name "\\_>")
                                    new-name)))
        (progn
          (insert (format " as %s" new-name))
          (query-replace-regexp
           (concat "\\_<" real-name "\\_>") new-name))))))

(defun js-imports-ivy-insert-or-view-export (item)
  "Add ITEM into existing or new import statement."
  (if-let* ((exit (and (boundp 'ivy-exit)
                       (null ivy-exit)))
            (real (js-imports-display-to-real-exports item))
            (definition (js-imports-find-definition real)))
      (progn (view-file-other-window
              (js-imports-get-prop definition
                                   :real-path))
             (goto-char (js-imports-get-prop definition :start))
             (js-imports-highlight-word))
    (js-imports-insert-import (string-trim item))))

(defun js-imports-insert-import (candidate)
  "Insert CANDIDATE into existing or new import statement."
  (let ((args (if-let*
                  ((real (js-imports-display-to-real-exports candidate))
                   (type (or (js-imports-get-prop real :type)
                             (and (equal candidate "*")
                                  16)))
                   (as-name (js-imports-get-prop real :as-name))
                   (confirmed-name
                    (pcase type
                      (1 (js-imports-read-default-import-name
                          (if (js-imports-valid-identifier-p candidate)
                              candidate
                            js-imports-last-export-path)
                          "Import default as" ))
                      (16 (js-imports-read-default-import-name
                           js-imports-last-export-path
                           "Import * as" ))
                      (4 (read-string
                          (format "Import (default %s)\s" as-name)
                          as-name nil as-name))))
                   (full-name
                    (pcase type
                      ((or 1 16) confirmed-name)
                      (4
                       (if (equal as-name confirmed-name)
                           as-name
                         (format "%s as %s" as-name confirmed-name))))))
                  (pcase type
                    (4 (list nil full-name))
                    (_ (list full-name nil)))
                (let* ((parts (split-string (string-trim candidate) "[}{]"))
                       (named (unless (or
                                       (null (nth 1 parts))
                                       (string-blank-p (nth 1 parts)))
                                (string-join
                                 (split-string (nth 1 parts)
                                               "[ \f\t\n\r\v,]+" t) ", ")))
                       (default (unless (or
                                         (null (nth 0 parts))
                                         (string-blank-p (nth 0 parts)))
                                  (nth 0 parts)))
                       (default-fullname
                        (when default
                          (if (string-match-p "\\*" default)
                              (format "* as %s"
                                      (js-imports-read-namespace-import-name
                                       default))
                            default))))
                  (list default-fullname named)))))
    (save-excursion
      (apply
       #'js-imports-insert-exports (append
                                    args
                                    (list js-imports-last-export-path))))))

(defvar js-imports-defaults-history (make-hash-table :test 'equal))

(defun js-imports-read-default-import-name (path &optional prompt)
  "Read string with PROMPT and generated from PATH default import name."
  (let ((value (if-let ((generated
                         (or
                          (cdr
                           (assoc path
                                  js-imports-modules-default-names))
                          (gethash path
                                   js-imports-defaults-history
                                   (js-imports-generate-name-from-path
                                    path)))))
                   (read-string (concat (string-join
                                         (delete
                                          nil
                                          `(,prompt
                                            ,(format "(default %s)"
                                                     generated)))
                                         "\s")
                                        "\s")
                                generated nil generated)
                 (read-string (or prompt "\s")))))
    (unless (string-empty-p value)
      (puthash path value js-imports-defaults-history))
    value))

(defun js-imports-read-namespace-import-name (candidate)
  "Check CANDIDATE is namespace and prompt for local name."
  (when-let ((pos (string-match-p "\\*" candidate)))
    (let ((parts (split-string (substring-no-properties candidate
                                                        (1+ pos))
                               nil t))
          (as-name))
      (setq as-name
            (when (equal (car parts) "as")
              (pop parts)
              (when (js-imports-valid-identifier-p (car parts))
                (pop parts))))
      (or as-name
          (js-imports-read-default-import-name
           (or js-imports-last-export-path "")
           "Import * as")))))

(defun js-imports-insert-exports (default-name named-list path)
  "Add DEFAULT-NAME and NAMED-LIST to new or existing import with PATH."
  (let ((names (if (stringp named-list)
                   named-list
                 (when (and (listp named-list) (<= 1 (length named-list)))
                   (string-join named-list ", "))))
        (imports (reverse (js-imports-find-imported-files))))
    (save-excursion
      (js-imports-goto-last-import)
      (if (member path imports)
          (js-imports-add-to-current-imports path default-name names)
        (let (module project-root)
          (setq project-root (js-imports-find-project-root))
          (setq module
                (cond
                 ((js-imports-relative-p path)
                  (let ((dir (file-name-directory path)))
                    (or (seq-find (js-imports--compose
                                   (js-imports--rpartial string= dir)
                                   file-name-directory)
                                  imports)
                        (seq-find #'js-imports-relative-p imports))))
                 ((js-imports-dependency-p path)
                  (if-let ((dependencies (js-imports-node-modules-candidates
                                          project-root)))
                      (seq-find (lambda (it) (member it dependencies))
                                imports)
                    (goto-char (point-min))
                    nil))
                 (t (let ((pred (lambda (it) (not (js-imports-relative-p it)))))
                      (setq module (seq-find pred imports))
                      (unless module
                        (when-let* ((relative (seq-find #'js-imports-relative-p
                                                        imports))
                                    (bounds (js-imports-get-import-positions
                                             module)))
                          (goto-char (car bounds))))))))
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
  "Add DEFAULT-NAME and NAMES to existing import statement with PATH."
  (when-let* ((bounds (js-imports-get-import-positions path))
              (start (car bounds))
              (end (cdr bounds)))
    (save-excursion
      (save-restriction
        (goto-char start)
        (narrow-to-region start end)
        (forward-word)
        (js-imports-forward-whitespace)
        (if (looking-at-p "\\*")
            (progn
              (goto-char end)
              (skip-chars-forward ";")
              (newline-and-indent)
              (insert "import " (js-imports-join-imports-names
                                 default-name names)
                      " from " js-imports-quote path js-imports-quote ";"))
          (when default-name
            (when-let ((default-bounds (js-imports-get-rebounds-at-point)))
              (delete-region (car default-bounds) (cdr default-bounds)))
            (insert default-name))
          (when (or (looking-at-p js-imports-regexp-name-set)
                    default-name)
            (skip-chars-forward js-imports-regexp-name)
            (js-imports-forward-whitespace)
            (unless (looking-at-p ",")
              (js-imports-re-search-backward js-imports-regexp-name-set nil t 1)
              (forward-char 1)
              (insert ", "))
            (skip-chars-forward ",")
            (js-imports-forward-whitespace))
          (when names
            (if (looking-at-p "{")
                (progn (js-imports-re-search-forward "}" nil t 1)
                       (backward-char 1)
                       (js-imports-backward-whitespace)
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
              (goto-char (js-imports-get-prop candidate :start))
              (setq p1 (point))
              (re-search-forward candidate nil t 1)
              (setq p2 (point))
              (skip-chars-forward ",\s\n\t")
              (setq p2 (point))
              (when (looking-at-p "}")
                (setq p2 (point))
                (goto-char p1)
                (skip-chars-backward "\s\t\n\r\f\v")
                (backward-char)
                (when (looking-at-p "{")
                  (setq p2 (1+ p2))
                  (setq p1 (point))
                  (skip-chars-backward  "\s\t\n\r\f\v")
                  (backward-char))
                (when (looking-at-p ",")
                  (setq p1 (point))))
              (setq overlay (make-overlay p1 p2))
              (overlay-put overlay 'face 'js-imports-highlight-face)
              (when (yes-or-no-p "Delete?")
                (remove-overlays p1 p2)
                (delete-region p1 p2)))))
      (remove-overlays beg end))))

(defvar js-imports-ivy-file-actions
  '(("f" js-imports-find-file "find file")
    ("j" js-imports-find-file-other-window "find file other window")))

(defun js-imports-ivy-setup ()
  "Setup sources and keymaps for ivy completion."
  (require 'ivy)
  (when (fboundp 'ivy-set-actions)
    (ivy-set-actions 'js-imports js-imports-ivy-file-actions))
  (when (fboundp 'ivy-set-display-transformer)
    (ivy-set-display-transformer
     'js-imports-symbols-menu
     'js-imports-transform-symbol)))

;;;###autoload
(defun js-imports-select-next-alias ()
  "Select the next alias and exit minibuffer."
  (interactive)
  (js-imports-next-or-prev-alias 1)
  (js-imports-switch-alias-hook))

;;;###autoload
(defun js-imports-select-prev-alias ()
  "Select the previous alias and exit minibuffer."
  (interactive)
  (js-imports-next-or-prev-alias -1)
  (js-imports-switch-alias-hook))

(defun js-imports-get-files-map ()
  "Return keymap for files minibuffer for reading files."
  (let ((map (copy-keymap js-imports-file-map))
        (special-map (and (boundp 'js-imports-completion-system)
                          (cdr (assq js-imports-completion-system
                                     js-imports-file-completions-maps)))))
    (when (symbolp special-map)
      (setq special-map (symbol-value special-map)))
    (when (and special-map (keymapp special-map))
      (setq map (make-composed-keymap (list map special-map))))
    map))

(defun js-imports-setup-minibuffer-files ()
  "Setup minibuffer for reading files."
  (use-local-map
   (let ((map (js-imports-get-files-map)))
     (set-keymap-parent map (current-local-map))
     map)))

(defun js-imports-read-project-files (&optional caller)
  "Read a project file in the minibuffer with completion.

CALLER is a symbol to identify the caller for ~ivy-read~.

During completion, such keybindings are available:
\\{js-imports-files-map}."
  (minibuffer-with-setup-hook 'js-imports-setup-minibuffer-files
    (when caller (setq this-command caller))
    (if (boundp 'js-imports-completion-system)
        (funcall js-imports-completion-system
                 (js-imports-make-files-prompt)
                 (append (js-imports-project-files-transformer
                          (or js-imports-project-files
                              (js-imports-find-project-files)))
                         (js-imports-node-modules-candidates))
                 nil
                 t)
      (completing-read (js-imports-make-files-prompt)
                       (append (js-imports-project-files-transformer
                                (or js-imports-project-files
                                    (js-imports-find-project-files)))
                               (js-imports-node-modules-candidates))
                       nil
                       t))))

(defun js-imports-read-file (&optional caller)
  "Read a project file.
CALLER is a symbol to identify the caller to `ivy-read` uniquely."
  (let ((file (js-imports-read-project-files caller)))
    (if (memq this-command '(js-imports-select-next-alias
                             js-imports-select-prev-alias))
        (setq file (js-imports-read-file caller))
      file)))

(defvar ivy-exit)
(defvar ivy-last)

;;;###autoload
(defun js-imports-ivy-find-file-other-window ()
  "Find file and exit minibuffer during ivy completion."
  (interactive)
  (require 'ivy)
  (when (fboundp 'ivy-exit-with-action)
    (ivy-exit-with-action 'js-imports-find-file-other-window)))

;;;###autoload
(defun js-imports-ivy-preview-file-exports ()
  "Show the exported symbols in popup during ivy completion."
  (interactive)
  (require 'ivy)
  (let ((cand (js-imports-path-to-real (when (fboundp 'ivy-state-current)
                                         (ivy-state-current ivy-last))))
        (exports))
    (setq exports (js-imports-extract-all-exports cand))
    (js-imports-with-popup "*js-imports-file*"
                           (dolist (it exports)
                             (insert "\n"
                                     (js-imports-transform-symbol it))))))

(defvar helm-map)
(defun js-imports-helm-find-file ()
  "Action for helm to find file."
  (require 'helm)
  (when (fboundp 'helm-run-after-exit)
    (helm-run-after-exit
     'js-imports-find-file
     (when (fboundp 'helm-get-selection)
       (helm-get-selection)))))

(defun js-imports-helm-find-file-other-window ()
  "Action for helm to find file in other window."
  (require 'helm)
  (when (fboundp 'helm-run-after-exit)
    (helm-run-after-exit
     'js-imports-find-file-other-window
     (when (fboundp 'helm-get-selection)
       (helm-get-selection)))))

(defun js-imports-helm-setup ()
  "Setup sources and keymaps for helm completion."
  (require 'helm)
  (let ((files-map (js-imports-get-files-map)))
    (js-imports-helm-reset-sources)
    (setq js-imports-helm-export-symbols-map
          (js-imports-build-helm-exports-keymap))
    (setq js-imports-helm-imported-symbols-map
          (js-imports-build-helm-imported-keymap))
    (put 'js-imports-file-map 'helm-only t)
    (put 'js-imports-helm-export-symbols-map 'helm-only t)
    (put 'js-imports-helm-imported-symbols-map 'helm-only t)
    (setq js-imports-buffer-files-source
          (helm-make-source
              "Imported files"
              'helm-source-in-buffer
            :init
            (lambda ()
              (with-current-buffer
                  (when (fboundp 'helm-candidate-buffer)
                    (helm-candidate-buffer 'global))
                (let ((items (with-current-buffer
                                 js-imports-current-buffer
                               (js-imports-find-imported-files))))
                  (mapc (lambda (it) (insert it)
                          (newline-and-indent))
                        items))
                (goto-char (point-min))))
            :get-line #'buffer-substring-no-properties
            :action 'js-imports-helm-file-actions
            :keymap files-map
            :group 'js-imports
            :persistent-action #'js-imports-view-file
            :mode-line (list "Imports")))
    (setq js-imports-project-files-source
          (helm-make-source
              "Project files"
              'helm-source-sync
            :group 'js-imports
            :mode-line (list "File(s)")
            :candidate-number-limit js-imports-helm-files-number-limit
            :action 'js-imports-helm-file-actions
            :persistent-action #'js-imports-view-file
            :keymap files-map
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
            :keymap files-map
            :persistent-action #'js-imports-view-file
            :group 'js-imports))
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
            :persistent-action (js-imports--compose
                                js-imports-jump-to-item-in-buffer
                                js-imports-display-to-real-imports)
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
            :candidates #'js-imports-init-exports-candidates
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
                    ("Jump to definition" .
                     js-imports-find-export-definition))
                '(("Jump" . (lambda (it)
                              (js-imports-jump-to-item-in-buffer
                               (js-imports-display-to-real-exports it)))))))
            :persistent-action
            (lambda (c)
              (when-let ((item (js-imports-display-to-real-exports c)))
                (setq item (js-imports-find-definition item))
                (when (and (js-imports-get-prop item :start))
                  (view-file (js-imports-get-prop
                              item
                              :real-path))
                  (goto-char (js-imports-get-prop item :start))
                  (js-imports-highlight-word))))))
    (setq js-imports-definitions-source
          (helm-make-source
              "Definitions" 'helm-source-sync
            :candidates
            (lambda () (with-current-buffer js-imports-current-buffer
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

(defvar js-imports-setup-functions-alist
  '((ivy-completing-read . js-imports-ivy-setup)
    (helm-comp-read . js-imports-helm-setup)))

(defun js-imports-set-completion (var value &rest _ignored)
  "Set VAR to VALUE."
  (when-let ((func (assq value js-imports-setup-functions-alist)))
    (funcall (cdr func)))
  (set var value))

(defcustom js-imports-completion-system 'ido-completing-read
  "Which completion system to use."
  :group 'js-imports
  :set 'js-imports-set-completion
  :type '(radio
          (const :tag "Ido" ido-completing-read)
          (const :tag "Helm" helm-comp-read)
          (const :tag "Ivy" ivy-completing-read)
          (const :tag "Default" completing-read-default)
          (function :tag "Custom function")))

(add-variable-watcher 'js-imports-completion-system
                      'js-imports-set-completion)

(defun js-imports-switch-alias-hook ()
  "Function to call after switch alias."
  (if (active-minibuffer-window)
      (pcase (car (assq js-imports-completion-system
                        js-imports-file-completions-maps))
        ('helm-comp-read (if (active-minibuffer-window)
                             (helm-refresh)
                           (js-imports)))
        (_ (exit-minibuffer)))
    (funcall-interactively #'js-imports)))

(defun js-imports-view-file (path)
  "Transform PATH to real file and view FILE in View mode."
  (with-current-buffer js-imports-current-buffer
    (if-let ((file (js-imports-path-to-real path)))
        (view-file file)
      (message "Couldn't find %s" path))))

(defun js-imports-build-helm-exports-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map)
                         helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key
     map (kbd "C-c M-o")
     (lambda () (interactive)
       (when (and (fboundp 'helm-run-after-exit)
                  (fboundp 'helm-get-selection))
         (helm-run-after-exit
          #'js-imports-jump-to-item-other-window
          (helm-get-selection nil 'withprop)))))
    (define-key
     map
     (kbd "C-c M-j")
     (lambda () (interactive)
       (when (and (fboundp 'helm-run-after-exit)
                  (fboundp 'helm-get-selection))
         (helm-run-after-exit
          #'js-imports-find-export-definition
          (helm-get-selection nil
                              'withprop)))))
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

(defun js-imports-make-files-prompt ()
  "Generate prompt for read file functions."
  (unless js-imports-current-project-root
    (js-imports-init-project))
  (let ((project-name (if js-imports-current-project-root
                          (car (reverse (split-string
                                         (directory-file-name
                                          js-imports-current-project-root)
                                         "/")))
                        default-directory)))
    (concat project-name "\s" "files" "\s" (or js-imports-current-alias
                                               "./"))))

(defun js-imports-skip-semicolon ()
  "Jump to next char if it is semicolon ignoring whitespace and comments."
  (let ((p (if (looking-at ";")
               (1+ (point))
             (or (save-excursion
                   (js-imports-forward-whitespace)
                   (when (looking-at ";")
                     (1+ (point))))
                 (point)))))
    (goto-char p)
    p))

(defun js-imports-goto-end-of-node ()
  "Jump to the end of parent node."
  (let ((winner)
        (beg (point))
        (words))
    (save-excursion
      (while (looking-at js-imports-node-starts-re)
        (let ((w (js-imports-which-word)))
          (push w words)
          (skip-chars-forward w))
        (js-imports-forward-whitespace)
        (when (looking-at "{")
          (forward-list 1)
          (js-imports-forward-whitespace)))
      (js-imports-forward-whitespace)
      (setq beg (point))
      (let* ((depth (nth 0 (syntax-ppss (point))))
             (next-node
              (save-excursion
                (when
                    (js-imports-re-search-forward
                     js-imports-node-starts-re nil t 1)
                  (skip-chars-backward "a-z")
                  (when
                      (and
                       (equal (nth 0 (syntax-ppss (point)))
                              depth)
                       (not
                        (js-imports-looking-at-function-expression)))
                    (point)))))
             (brackets (save-excursion (js-imports-re-search-forward
                                        "[<(;{]" nil t 1))))
        (cond ((and (null next-node)
                    (null brackets))
               (goto-char (point-max))
               (skip-chars-backward "\s\t\n\r\f\v")
               (js-imports-backward-whitespace))
              ((and (null brackets)
                    next-node)
               (goto-char next-node)
               (skip-chars-backward "a-z")
               (js-imports-backward-whitespace))
              ((and next-node brackets
                    (> brackets next-node))
               (goto-char next-node)
               (skip-chars-backward "a-z")
               (js-imports-backward-whitespace))
              (t
               (progn (js-imports-re-search-forward "[<(;{]" nil t 1)
                      (when (looking-back "<" 0)
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

(defun js-imports-get-prev-node-start-if-matches (regexp)
  "Get position of previous word matched REGEXP or nil."
  (save-excursion
    (js-imports-backward-whitespace)
    (when (looking-back regexp 0)
      (skip-chars-backward js-imports-regexp-name)
      (point))))

(defun js-imports-goto-start-of-node ()
  "Jump to start of node."
  (let ((beg))
    (save-excursion
      (skip-chars-backward
       js-imports-regexp-name)
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
  "Return bounds of node at the point."
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
                   (t (js-imports-goto-end-of-node))))))
    (when (and beg end)
      (cons beg end))))

;;;###autoload
(defun js-imports-mark-it ()
  "Mark node at the point."
  (interactive)
  (when-let ((bounds (js-imports-get-bounds-of-exp-at-point)))
    (goto-char (car bounds))
    (push-mark (cdr bounds) nil t)
    (activate-mark)))

;;;###autoload
(defun js-imports ()
  "Read a project file, parse exports from it and ask which to import.

During file completion, you can cycle between relative and aliased filenames:
\\<js-imports-file-map>\
`\\[js-imports-select-next-alias]' - select the next alias,
`\\[js-imports-select-prev-alias]' - select the previous alias."
  (interactive)
  (js-imports-init-project)
  (pcase js-imports-completion-system
    ('helm-comp-read
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
    (_ (let ((module (js-imports-read-file 'js-imports)))
         (when-let ((setup-fn (cdr (assoc js-imports-completion-system
                                          js-imports-setup-functions-alist))))
           (funcall setup-fn))
         (when (eq (current-buffer) js-imports-current-buffer)
           (when (active-minibuffer-window)
             (exit-minibuffer))
           (funcall-interactively #'js-imports-from-path module))))))

;;;###autoload
(defun js-imports-from-path (&optional path)
  "Parse exports in PATH and add them to the existing or new import statement."
  (interactive)
  (unless path
    (js-imports-init-project)
    (setq path (or js-imports-current-export-path (read-file-name "File:\s"))))
  (with-current-buffer js-imports-current-buffer
    (when (file-name-absolute-p path)
      (setq path (js-imports-propertize
                  path :display-path
                  (completing-read
                   "Transform to\s"
                   (js-imports-get-file-variants path
                                                 default-directory)))))
    (when-let ((display-path (js-imports-normalize-path
                              (or (js-imports-get-prop path :display-path)
                                  path))))
      (setq js-imports-current-export-path display-path)
      (setq js-imports-last-export-path display-path)))
  (js-imports-init-exports-candidates)
  (cond
   ((and (eq js-imports-completion-system 'helm-comp-read)
         (fboundp 'helm))
    (helm
     :preselect (js-imports-get-word-if-valid)
     :sources '(js-imports-exports-source
                js-imports-imported-symbols-source)))
   ((and (eq js-imports-completion-system 'ivy-completing-read)
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
         (concat "Import" "\s" (mapconcat (lambda (it) (format "%s" it)) imports
                                          ",\s")
                 ",\s"))
       choices
       :require-match nil
       :caller 'js-imports-from-path
       :preselect (js-imports-get-word-if-valid)
       :multi-action (lambda (marked)
                       (dolist (it marked)
                         (js-imports-insert-import it)))
       :action 'js-imports-ivy-insert-or-view-export)))
   (t (let ((choices (js-imports-export-filtered-candidate-transformer
                      (js-imports-exported-candidates-transformer
                       js-imports-export-candidates-in-path)))
            (cand))
        (setq cand (funcall js-imports-completion-system "Import\s" choices))
        (js-imports-insert-import cand)
        (setq this-command 'js-imports-from-path)))))

;;;###autoload
(defun js-imports-jump-to-definition ()
  "Jump to a definition of symbol at the point."
  (interactive)
  (js-imports-init-project)
  (if-let ((name (and
                  (not (js-imports-inside-string-p))
                  (js-imports-get-word-if-valid))))
      (let (real-name as-name)
        (save-excursion (skip-chars-backward name)
                        (if (save-excursion
                              (js-imports-backward-whitespace)
                              (js-imports-looking-at "as"))
                            (progn
                              (setq as-name name)
                              (js-imports-re-search-backward "as" nil t 1)
                              (js-imports-backward-whitespace)
                              (setq real-name (js-imports-get-word-if-valid)))
                          (progn
                            (setq real-name name)
                            (js-imports-re-search-forward real-name)
                            (js-imports-forward-whitespace)
                            (if (not (js-imports-looking-at "as"))
                                (setq as-name real-name)
                              (js-imports-re-search-forward "as" nil t 1)
                              (js-imports-forward-whitespace)
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
            (progn (goto-char (js-imports-get-prop item :start))
                   (js-imports-highlight-word)))))
    (js-imports-find-file-at-point)))

(defun js-imports-find-file (&optional file)
  "An action for command `js-imports' to open FILE."
  (let ((path (js-imports-path-to-real file)))
    (if (and path (file-exists-p path))
        (progn (find-file path)
               (when (minibuffer-window-active-p (minibuffer-window))
                 (abort-recursive-edit)))
      (message "Could't find %s" file))))

(defun js-imports-find-file-other-window (&optional file)
  "An action for command `js-imports' to open FILE in other window."
  (let ((path (js-imports-path-to-real file)))
    (if (and path (file-exists-p path))
        (progn (find-file-other-window path)
               (when (minibuffer-window-active-p (minibuffer-window))
                 (abort-recursive-edit)))
      (message "Could't find %s" file))))

;;;###autoload
(defun js-imports-find-file-at-point ()
  "Find a file when cursor are placed inside string."
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
  "Jump to a identifier in current buffer."
  (interactive)
  (js-imports-init-project)
  (setq js-imports-current-export-path nil)
  (pcase js-imports-completion-system
    ('helm-comp-read (when (fboundp 'helm)
                       (helm
                        :preselect (js-imports-get-word-if-valid)
                        :sources js-imports-helm-symbol-sources)))
    ('ivy-completing-read
     (let ((choices (append
                     (js-imports-extract-es-imports buffer-file-name)
                     (reverse
                      (js-imports-extract-definitions
                       buffer-file-name (point)))
                     (js-imports-extract-all-exports buffer-file-name))))
       (when (fboundp 'ivy-read)
         (ivy-read "Jump to\s"
                   choices
                   :preselect (js-imports-get-word-if-valid)
                   :caller 'js-imports-symbols-menu
                   :action (lambda (it)
                             (if (active-minibuffer-window)
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
                   (symbol (funcall js-imports-completion-system
                                    "Select symbol"
                                    choices nil t
                                    (js-imports-get-word-if-valid))))
         (setq symbol (mapcar (lambda (it) (car (member it choices)))
                              symbol))
         (when-let ((item (js-imports-find-definition symbol)))
           (when (js-imports-get-prop item :var-type)
             (find-file (js-imports-get-prop item :real-path))
             (progn (goto-char (js-imports-get-prop item :start))
                    (js-imports-highlight-word))))))))

;;;###autoload
(defun js-imports-change-completion ()
  "Customize or temporarily set one of the available completions systems."
  (interactive)
  (let* ((alist (mapcar #'cddr
                        (cdr
                         (get 'js-imports-completion-system 'custom-type))))
         (result (completing-read (format "Change completion:\s (Current: %s)"
                                          js-imports-completion-system)
                                  alist))
         (func))
    (setq result (if (equal result "Custom function")
                     (read--expression "Function:\s")
                   (cadr (assoc result alist))))
    (setq func (if (yes-or-no-p "Save it for future sessions?")
                   #'customize-save-variable
                 #'js-imports-set-completion))
    (funcall func 'js-imports-completion-system result)))

;;;###autoload
(defun js-imports-transform-import-path-at-point ()
  "Replace path of import statement at the point to aliased one or relative.
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
  "Replace relative paths from other directory than current to aliased ones.
Only relative paths from current buffer directory.

For example, `import someExport from '../enums' transforms to
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
                                   (seq-remove #'js-imports-relative-p
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
  "Purge project cache."
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
If called interactively, also show the version in the echo area."
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
  "Display project info."
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
    (js-imports-with-popup
     "*js-imports*"
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
    (define-key map (kbd "C-c M-i") #'js-imports)
    (define-key map (kbd "C-c .") #'js-imports-symbols-menu)
    (define-key map (kbd "C-c M-j") #'js-imports-jump-to-definition)
    (easy-menu-define js-imports-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-imports]
        ["Jump to symbol in buffer" js-imports-symbols-menu]
        ["Jump to definition" js-imports-jump-to-definition]))
    map)
  "Keymap for `js-imports' mode.")

;;;###autoload
(define-minor-mode js-imports-alias-fix-mode
  "Transform relative imports to aliased on save when this mode is turned on."
  :lighter " js alias"
  :global nil
  (if js-imports-alias-fix-mode
      (add-hook
       'before-save-hook 'js-imports-transform-relative-imports-to-aliases
       nil 'local)
    (remove-hook
     'before-save-hook 'js-imports-transform-relative-imports-to-aliases
     'local)))

;;;###autoload
(define-minor-mode js-imports-mode
  "A minor mode for importing in JavaScript.
\\{js-imports-mode-map}"
  :lighter " js-imports"
  :group 'js-imports
  :keymap js-imports-mode-map)

(provide 'js-imports)
;;; js-imports.el ends here
