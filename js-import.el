;;; js-import.el --- Import for JavaScript files easily -*- lexical-binding: t -*-

;; Copyright (C) 2020 Karim Aziiev <karim.aziev@gmail.com>

;; Author: Karim Aziiev <karim.aziev@gmail.com>
;; URL: https://github.com/KarimAziev/js-import
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

(require 'helm)
(require 'cl-lib)
(require 'json)
(eval-when-compile
  (require 'subr-x))

(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import
  :group 'languages)

(defcustom js-import-alias-map '("" "src")
  "List of pairs (alias and path)."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-quote "'"
  "Quote type."
  :group 'js-import
  :type '(choice (const :tag "Double" "\"")
                 (const :tag "Single" "\\'")))

(defcustom js-import-files-number-limit 30
  "The limit for number of project files displayed."
  :group 'js-import
  :type 'number)

(defcustom js-import-dependencies-number-limit 400
  "The limit for number of dependencies files displayed."
  :group 'js-import
  :type 'number)

(defcustom js-import-package-json-sections '("dependencies" "devDependencies")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-node-modules-priority-section-to-read
  '("jsnext:main" "module" "types")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-node-modules-dir "node_modules"
  "Relative to project root or absolute path to node_modules directory."
  :group 'js-import
  :type 'string)

(defcustom js-import-preffered-extensions '("d.ts" "ts" "js" "tsx" "jsx")
  "Preffered suffixes for files with different extension."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-buffer "*helm js import*"
  "Name of `js-import' buffer."
  :group 'js-import
  :type 'string)

(defcustom js-import-symbols-faces
  '(("^\\(type\\|interface\\)$" .
     font-lock-type-face)
    ("^\\(function\\|function*\\)$" .
     font-lock-function-name-face)
    ("^\\(export\\|default\\)$" .
     font-lock-builtin-face)
    ("^\\(import\\|const\\|let\\|var\\|class\\)$" .
     font-lock-variable-name-face))
  "Faces in a list of cons cells for showing symbols types."
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

(defface js-import-highlight-face
  '((t (:background "#e52b50" :foreground "white")))
  "Face used to highlight symbol."
  :group 'js-import)

(defvar js-import-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-import)
    (define-key map (kbd "C-c C-.") 'js-import-show-symbols)
    (define-key map (kbd "C-c C-d") 'js-import-dependency)
    (define-key map (kbd "C-c C-a") 'js-import-alias)
    (define-key map (kbd "C-c C-j") 'js-import-jump-to-definition)
    (define-key map (kbd "C-.") 'js-import-find-file-at-point)
    (easy-menu-define js-import-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-import]
        ["Edit current buffer imports" js-import-edit-buffer-imports]
        ["Import alias" js-import-alias]
        ["Import depenency" js-import-dependency]
        ["Find file at point" js-import-find-file-at-point]))
    map)
  "Keymap for `js-import' mode.")

(defvar js-import-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for command `js-import-mode'.")

;;;###autoload
(define-minor-mode js-import-mode
  "js-import-mode is a minor mode for importing.
\\{js-import-mode-map}"
  :lighter " js-import"
  :group 'js-import
  :global nil
  :keymap js-import-mode-map)

(defvar js-import-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'js-import-find-file-and-exit)
    (define-key map (kbd "C-c C-o") 'js-import-find-file-other-window-and-exit)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "Keymap for files sources in Helm.")
(put 'js-import-files-map 'helm-only t)

(defun js-import-with-files-map(name command)
  (substitute-command-keys
   (concat name "\s\\<js-import-files-map>`\\" "[" (symbol-name command) "]")))

(defcustom js-import-file-actions
  (helm-make-actions
   (js-import-with-files-map "Import from file(s)"
                             'js-import-marked-files)
   'js-import-marked-files
   (js-import-with-files-map "Find file"
                             'js-import-find-file-and-exit)
   'js-import-find-file-and-exit
   (js-import-with-files-map "Find file other window"
                             'js-import-find-file-other-window-and-exit)
   'js-import-find-file-other-window-and-exit
   (js-import-with-files-map "Next alias"
                             'js-import-switch-to-next-alias)
   'js-import-switch-to-next-alias
   (js-import-with-files-map "Prev alias"
                             'js-import-switch-to-prev-alias)
   'js-import-switch-to-prev-alias
   (js-import-with-files-map "Relative"
                             'js-import-switch-to-relative)
   'js-import-switch-to-relative)
  "Default actions for files."
  :group 'js-import
  :type '(alist :key-type string :value-type function))

(defun js-import-with-symbols-map(name command)
  (substitute-command-keys
   (concat
    name
    "\s\\<js-import-imported-symbols-map>`\\" "["
    (symbol-name command) "]")))

(defvar js-import-imported-symbols-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-d") 'js-import-delete-persistent)
    (define-key map (kbd "M-D") 'js-import-delete-whole-import-persistent)
    (define-key map (kbd "M-r") 'js-import-rename-item)
    map)
  "Keymap for symdol sources.")
(put 'js-import-imported-symbols-map 'helm-only t)

(defcustom js-import-symbol-actions
  (helm-make-actions
   (js-import-with-symbols-map "Jump"
                               'js-import-jump-to-item-in-buffer)
   'js-import-jump-to-item-in-buffer
   (js-import-with-symbols-map "Rename"
                               'js-import-rename-import)
   'js-import-rename-import
   (js-import-with-symbols-map "Add imports"
                               'js-import-from-path)
   'js-import-from-path
   (js-import-with-symbols-map "Quick delete"
                               'js-import-delete-persistent)
   'js-import-delete-imported-item
   (js-import-with-symbols-map "Delete whole import"
                               'js-import-delete-whole-import-persistent)
   'js-import-delete-whole-import)
  "Actions for imported symbols in buffer."
  :group 'js-import
  :type '(alist :key-type string :value-type function))

(defcustom js-import-export-items-actions
  (helm-make-actions
   "Import"                       'js-import-insert-marked
   "Import as "                   'js-import-insert-import-as
   "Go"                           'js-import-jump-to-item-other-window)
  "Actions for inserting exports."
  :group 'js-import
  :type '(alist :key-type string :value-type function))

(defconst js-import-enabled-extension-regexp "\\.[jt]s\\(x\\)?$")
(defconst js-import-delcaration-keywords
  '("const" "var" "let" "function" "function*" "interface" "type" "class"))

(defconst js-import-delcaration-keywords--re
  (concat "\\_<" (regexp-opt js-import-delcaration-keywords t) "\\_>"))

(defconst js-import-regexp-name
  "_$A-Za-z0-9"
  "Regexp matching the start of a js identifier.")

(defconst js-import-regexp-name-set
  (concat "[" js-import-regexp-name "]")
  "Regexp set matching the start of a js identifier.")

(defconst js-import-regexp-name-with-separators
  (concat js-import-regexp-name ",\s\n\t")
  "Regexp matching js identifier's chars with separators.")

(defconst js-import-regexp-import-keyword
  "\\(^\\| +\\)import[ \t\n]+"
  "Regexp matching keyword import.")

(defconst js-import-regexp-export-keyword
  "\\(^\\| +\\)export[ \t]+"
  "Regexp matching keyword export.")

(defconst js-import-regexp-cjs-exports-keyword
  "\\(^\\| +\\|\\.\\|;\\)exports[ \s\t['\".]+"
  "Regexp matching keyword exports.")

(defconst js-import-node-modules-regexp
  "\\(/\\|^\\)node_modules\\(\\/\\|$\\)"
  "Regexp matching path with node_modules.")

(defconst js-import-file-ext-regexp
  "\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)$"
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-import-file-index-regexp
  "\\(/\\|^\\)\\index\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)?$")

(defconst js-import-reserved-js-words
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

(defun js-import-reserved-word-p(str &optional reserved-list)
  "Check if STR is js reserved word."
  (unless reserved-list (setq reserved-list js-import-reserved-js-words))
  (when (stringp str)
    (member str reserved-list)))

(defvar js-import-dependencies-cache (make-hash-table :test 'equal))
(defvar js-import-dependencies-cache-tick nil)
(defvar js-import-current-alias nil)
(defvar js-import-current-project-root nil)
(defvar js-import-aliases nil)
(defvar js-import-current-buffer nil)

(defvar js-import-current-export-path nil)
(make-variable-buffer-local 'js-import-current-export-path)
(defvar js-import-current-export-real-path nil)
(make-variable-buffer-local 'js-import-current-export-real-path)
(defvar js-import-export-candidates-in-path nil)
(make-variable-buffer-local 'js-import-export-candidates-in-path)
(defvar js-import-cached-imports-in-buffer nil)
(make-variable-buffer-local 'js-import-cached-imports-in-buffer)
(defvar js-import-cached-imports-in-buffer-tick nil)
(make-variable-buffer-local 'js-import-cached-imports-in-buffer-tick)
(defvar-local js-import-cached-exports-in-buffer nil)
(defvar-local js-import-cached-exports-in-buffer-tick nil)
(defvar js-import-last-export-path nil)
(make-variable-buffer-local 'js-import-last-export-path)

(defvar js-import-dependency-source-name "Node Modules")
(defvar js-import-buffer-source-name "Imports in")
(defvar js-import-files-source-name "Project files")
(defvar js-import-node-modules-source nil
  "Variable keeps source files from node_modules.")
(defvar js-import-project-files-source nil
  "Variable for source of relative and aliased files without dependencies.")
(defvar js-import-buffer-files-source nil
  "Buffer local variable for source of all imported files in a buffer.")

(defcustom js-import-files-source '(js-import-buffer-files-source
                                    js-import-project-files-source
                                    js-import-node-modules-source)
  "Preferred sources for command `js-import'."
  :type '(repeat (choice symbol))
  :group 'js-import)

;;;###autoload
(defun js-import ()
  "Preconfigured helm for selecting files.
Run all sources defined in option `js-import-files-source'."
  (interactive)
  (js-import-init-project)
  (unless js-import-buffer-files-source
    (setq js-import-buffer-files-source
          (helm-make-source
              js-import-buffer-source-name
              'js-import-source-imported-files)))
  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source
              js-import-dependency-source-name
              'js-import-source-node-modules)))
  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source
              js-import-files-source-name
              'js-import-source-project-files)))
  (helm-attr 'candidates js-import-node-modules-source t)
  (save-excursion
    (helm
     :sources js-import-files-source
     :buffer js-import-buffer
     :preselect (js-import-preselect)
     :prompt "Select a file: ")))

;;;###autoload
(defun js-import-alias ()
  "Import from project files without dependencies."
  (interactive)
  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source
              js-import-files-source-name
              'js-import-source-project-files)))
  (save-excursion
    (helm
     :sources 'js-import-project-files-source
     :buffer js-import-buffer
     :preselect (js-import-preselect)
     :prompt "Select a file: ")))

;;;###autoload
(defun js-import-dependency ()
  "Import from node modules."
  (interactive)
  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source
              js-import-dependency-source-name
              'js-import-source-node-modules)))
  (save-excursion
    (helm :sources js-import-node-modules-source
          :preselect (js-import-preselect))))

(defclass js-import-source-project-files (helm-source-sync)
  ((header-name :initform 'js-import-files-header-name)
   (init :initform (lambda() (with-helm-current-buffer (js-import-init-project))))
   (candidates :initform #'js-import-find-project-files)
   (filtered-candidate-transformer
    :initform #'js-import-project-files-transformer)
   (action :initform 'js-import-file-actions)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (candidate-number-limit :initform js-import-files-number-limit)
   (mode-line :initform (list "File(s)"))
   (keymap :initform js-import-files-map)
   (group :initform 'js-import)))

(defun js-import-init-project()
  "Initialize project by setting buffer, finding root and aliases."
  (setq js-import-current-buffer (current-buffer))
  (setq js-import-current-project-root (js-import-find-project-root))
  (setq js-import-aliases (js-import-get-aliases
                           js-import-current-project-root))
  (when (and js-import-current-alias
             (not (member js-import-current-alias js-import-aliases)))
    (setq js-import-current-alias nil)))

(defun js-import-files-header-name(_name)
  "A function for display header name for project files."
  (with-helm-current-buffer
    (when-let ((dir (or (js-import-find-project-root) default-directory)))
      (if js-import-current-alias
          (progn
            (concat
             (propertize js-import-current-alias 'face 'font-lock-builtin-face)
             "\s" (abbreviate-file-name dir) "/"
             (plist-get js-import-alias-map js-import-current-alias)))
        (if buffer-file-name
            (progn
              (setq dir (js-import-slash dir))
              (format "relative to %s"
                      (replace-regexp-in-string dir "" buffer-file-name)))
          "relative files")))))

(defun js-import-find-project-files()
  "Return project files without dependencies."
  (let* ((root (js-import-slash (js-import-find-project-root)))
         (alias-path (js-import-get-alias-path js-import-current-alias root))
         (dirs (directory-files (or alias-path root) t))
         (priority-dir (if (and alias-path
                                (js-import-string-match-p
                                 (concat "^" root)
                                 alias-path))
                           alias-path
                         default-directory))
         (dir-pred (lambda(it)
                     (and (file-directory-p it)
                          (not (or (string= it root)
                                   (js-import-string-contains-p
                                    "node_modules" it)
                                   (js-import-string-match-p "/\\." it)
                                   (js-import-string-match-p "\\.+" it))))))
         (files))
    (setq dirs (reverse (push priority-dir dirs)))
    (setq dirs (cl-remove-duplicates dirs :test 'string=))
    (setq dirs (seq-filter dir-pred dirs))
    (setq dirs (reverse dirs))
    (mapc (lambda(dir) (setq files (append files (js-import-directory-files dir t))))
          dirs)
    files))

(defun js-import-get-alias-path(alias &optional project-root)
  (when-let ((alias-path (plist-get js-import-alias-map alias)))
    (if (file-exists-p alias-path)
        alias-path
      (js-import-join-file (or project-root
                               (js-import-find-project-root))
                           alias-path))))

(defun js-import-project-files-transformer(files &optional _source)
  "Filter FILES by extension and one of the aliases, if present."
  (setq files (seq-uniq files))
  (let ((slashed-alias (js-import-slash js-import-current-alias))
        (alias-path (js-import-slash (js-import-get-alias-path
                                      js-import-current-alias))))
    (setq files (seq-remove
                 (lambda(filename)
                   (string= filename (buffer-file-name
                                      js-import-current-buffer)))
                 files))
    (if alias-path
        (progn
          (setq files (seq-filter (lambda(filename)
                                    (if (file-name-absolute-p filename)
                                        (js-import-string-match-p alias-path
                                                                  filename)
                                      filename))
                                  files))
          (mapcar (lambda(path) (js-import-normalize-path
                            (replace-regexp-in-string alias-path slashed-alias
                                                      path)))
                  files))
      (mapcar (lambda(path) (js-import-normalize-path (js-import-path-to-relative
                                                  path)))
              files))))

(defun js-import-find-project-root (&optional dir)
  (unless dir (setq dir default-directory))
  (let ((parent (expand-file-name ".." dir)))
    (unless (or (string= parent dir)
                (string= dir "/"))
      (if (file-exists-p (expand-file-name "package.json" dir))
          dir
        (js-import-slash (js-import-find-project-root parent))))))

(defun js-import-directory-files(dir &optional recursive regexp)
  "Return files in DIR that matches value of the variable
`js-import-enabled-extension-regexp'.
Optional argument RECURSIVE non-nil means to search recursive."
  (unless regexp (setq regexp js-import-enabled-extension-regexp))
  (if recursive
      (directory-files-recursively dir regexp nil)
    (directory-files dir t regexp t)))

(defun js-import-join-file (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (not (file-name-absolute-p (car args)))))
    (mapc (lambda (arg)
            (setq path (expand-file-name arg path)))
          args)
    (if relative (file-relative-name path) path)))

(defun js-import-dirname (path)
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
      (if (js-import-relative-p path)
          (file-relative-name parent)
        (directory-file-name parent)))))

(defun js-import-path-to-relative(path &optional dir)
  "Transform PATH into relative to the DIR (default: ‘default-directory’).
If PATH is a relative file, it will be returned without changes."
  (if (js-import-relative-p path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-import-relative-p relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-import-slash(str)
  "Append slash to non-empty STR unless one already."
  (if (or (null str)
          (js-import-string-match-p "/$" str)
          (js-import-string-blank-p str))
      str
    (concat str "/")))

(defun js-import-normalize-path(path)
  (js-import-compose-from path
                          'js-import-remove-double-slashes
                          'js-import-remove-ext
                          'js-import-maybe-remove-path-index))

(defun js-import-remove-double-slashes (path)
  (replace-regexp-in-string "//"  "/" path))

(defun js-import-maybe-remove-path-index (path)
  (if (js-import-index-trimmable-p path)
      (replace-regexp-in-string js-import-file-index-regexp "" path)
    path))

(defun js-import-index-trimmable-p(path)
  "Check if PATH index can be trimmed."
  (if (js-import-relative-p path)
      (and (js-import-is-index-file-p path)
           (< 1 (js-import-count-matches "/" path)))
    (js-import-is-index-file-p path)))

(defun js-import-remove-ext(path)
  (replace-regexp-in-string js-import-file-ext-regexp "" path))

(defun js-import-is-index-file-p(path)
  (js-import-string-match-p js-import-file-index-regexp path))

(defun js-import-relative-p(path)
  (js-import-string-match-p "^\\.+/" path))

(defun js-import-get-aliases (&optional project-root)
  "Get list of aliases of PROJECT-ROOT without real paths."
  (let ((root (or project-root (js-import-find-project-root)))
        (pl js-import-alias-map)
        (vals))
    (while pl
      (when-let* ((alias (car pl))
                  (path (plist-get js-import-alias-map alias))
                  (exists (file-exists-p (js-import-join-file root path))))
        (push alias vals))
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun js-import-path-to-real(path &optional dir)
  (when (and path (stringp path))
    (setq path (js-import-strip-text-props path))
    (cond ((and (js-import-string-match-p
                 js-import-enabled-extension-regexp path)
                (file-exists-p path)
                (not (js-import-relative-p path)))
           path)
          ((js-import-relative-p path)
           (js-import-relative-to-real path dir))
          ((js-import-dependency-p path (js-import-find-project-root))
           (js-import-node-module-to-real path))
          (t (js-import-alias-path-to-real path)))))

(defun js-import-relative-to-real(path &optional dir)
  (unless dir (setq dir default-directory))
  (or (js-import-try-ext path dir)
      (js-import-try-ext (js-import-join-file path "index") dir)))

(defun js-import-alias-path-to-real(path)
  "Convert aliased PATH to absolute file name."
  (let (aliases alias real-path)
    (setq aliases (js-import-get-aliases))
    (while aliases
      (setq alias (pop aliases))
      (let* ((alias-regexp (if (js-import-string-blank-p alias)
                               (concat "^" alias)
                             (concat "^" alias "\\(/\\|$\\)" )))
             (alias-path (js-import-get-alias-path alias))
             (joined-path (js-import-join-file
                           alias-path
                           (replace-regexp-in-string alias-regexp "" path)))
             (found-path (if (and (js-import-string-match-p
                                   js-import-enabled-extension-regexp
                                   joined-path)
                                  (file-exists-p joined-path))
                             joined-path
                           (or (js-import-try-ext joined-path)
                               (js-import-try-ext
                                (js-import-join-file joined-path "index"))))))
        (when (and found-path (file-exists-p found-path))
          (setq real-path found-path)
          (setq aliases nil))))
    real-path))

(defun js-import-add-ext(path ext)
  (if (js-import-string-match-p js-import-enabled-extension-regexp path)
      path
    (concat path "." ext)))

(defun js-import-try-ext(path &optional dir extensions)
  "A function tries to join into PATH every element from EXTENSIONS
from left to right until first existing file will be found or nil otherwise.
If optional argument DIR is passed, PATH will be firstly expanded as relative."
  (unless extensions (setq extensions js-import-preffered-extensions))
  (let (ext real-path)
    (while extensions
      (setq ext (pop extensions))
      (setq real-path (js-import-add-ext path ext))
      (when dir
        (setq real-path (expand-file-name real-path dir)))
      (if (file-exists-p real-path)
          (setq extensions nil)
        (setq real-path nil)))
    real-path))

(defun js-import-switch-to-relative(&optional _cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias nil)
    (setq js-import-current-alias (car js-import-aliases)))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

(defun js-import-switch-to-next-alias(&optional _cand)
  "Switch to next alias in `js-import-aliases' list."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias (car (cdr
                                          (member js-import-current-alias
                                                  js-import-aliases))))
    (setq js-import-current-alias (car js-import-aliases)))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

(defun js-import-switch-to-prev-alias(&optional _cand)
  "Switch to previous alias in `js-import-aliases' list."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias (car (cdr (member
                                               js-import-current-alias
                                               (reverse js-import-aliases)))))
    (setq js-import-current-alias (car (reverse js-import-aliases))))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

(defclass js-import-source-node-modules (helm-source-sync)
  ((candidates :initform #'js-import-node-modules-candidates)
   (candidate-number-limit :initform js-import-dependencies-number-limit)
   (action :initform 'js-import-file-actions)
   (mode-line :initform (list "Dependencies"))
   (keymap :initform js-import-files-map)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (group :initform 'js-import)))

(defun js-import-node-modules-candidates(&optional project-root)
  "Return dependencies of PROJECT-ROOT from package json."
  (unless project-root (setq project-root (js-import-find-project-root)))
  (unless js-import-dependencies-cache
    (setq js-import-dependencies-cache (make-hash-table :test 'equal)))
  (when (js-import-string-match-p js-import-node-modules-regexp project-root)
    (setq project-root (car
                        (split-string project-root
                                      js-import-node-modules-regexp))))
  (let* ((package-json (js-import-join-file project-root "package.json"))
         (package-json-tick (file-attribute-modification-time (file-attributes
                                                               package-json
                                                               'string)))
         (project-cache (gethash project-root js-import-dependencies-cache)))
    (when (or (not (equal js-import-dependencies-cache-tick package-json-tick))
              (not project-cache))
      (remhash project-root js-import-dependencies-cache)
      (let (submodules modules progress-reporter max)
        (dolist-with-progress-reporter (section js-import-package-json-sections
                                                modules)
            "Reading package.json"
          (when-let ((hash (js-import-read-package-json-section package-json
                                                                section)))
            (setq modules (append modules (hash-table-keys hash)))))
        (setq max (length modules))
        (setq progress-reporter
              (make-progress-reporter "Scaning node modules" 0  max))
        (when-let ((node-modules (js-import-find-node-modules project-root)))
          (dotimes (k max)
            (let ((element (nth k modules)))
              (sit-for 0.01)
              (unless (js-import-string-contains-p "/" element)
                (setq submodules (append submodules
                                         (js-import-find-interfaces
                                          element
                                          node-modules))))
              (progress-reporter-update progress-reporter k))))
        (setq modules (append modules submodules))
        (puthash project-root modules js-import-dependencies-cache)
        (setq js-import-dependencies-cache-tick package-json-tick)
        (progress-reporter-done progress-reporter)))
    (gethash project-root js-import-dependencies-cache)))

(defun js-import-find-interfaces(display-path dir)
  (when-let* ((real-path (js-import-join-when-exists dir display-path))
              (files (seq-remove 'js-import-is-index-file-p
                                 (js-import-directory-files
                                  real-path nil "\\.d.ts$"))))
    (mapcar (lambda(it) (js-import-join-file
                    display-path
                    (js-import-compose-from it
                                            'file-name-nondirectory
                                            'directory-file-name
                                            'js-import-remove-ext)))
            files)))

(defun js-import-find-node-module-path ()
  "Find node_modules."
  (let* ((dir (locate-dominating-file default-directory
                                      "node_modules")))
    (if dir (concat (file-name-as-directory dir)
                    "node_modules")
      default-directory)))

(defun js-import-find-node-modules (&optional project-dir)
  "Return the path to node-modules."
  (if (file-name-absolute-p js-import-node-modules-dir)
      js-import-node-modules-dir
    (when-let ((root (or project-dir (js-import-find-project-root))))
      (setq root (car (split-string root js-import-node-modules-regexp)))
      (js-import-join-when-exists root js-import-node-modules-dir))))

(defun js-import-node-module-to-real(module &optional project-root)
  (when-let* ((node-modules (or (js-import-find-node-modules project-root)
                                (js-import-find-node-modules)))
              (real-path (js-import-join-file node-modules module)))
    (unless (js-import-string-match-p
             js-import-enabled-extension-regexp real-path)
      (setq real-path (js-import-try-find-real-path real-path))
      real-path)))

(defun js-import-dependency-p (module &optional project-root)
  "Check if DISPLAY-PATH is dependency in PROJECT-ROOT.
Dependencies are recognized by `package.json' or `node_modules' of
PROJECT-ROOT."
  (let ((dependencies (js-import-node-modules-candidates project-root))
        (dirname (car (split-string module "/"))))
    (or (member module dependencies)
        (member dirname dependencies)
        (when-let ((node-dir (js-import-find-node-modules project-root)))
          (or (js-import-join-when-exists node-dir module)
              (js-import-join-when-exists node-dir dirname))))))

(defun js-import-try-find-real-path(path)
  (if (or (null path) (and (js-import-string-match-p
                            js-import-enabled-extension-regexp path)
                           (file-exists-p path)))
      path
    (or (when-let* ((dir (js-import-join-file path "src"))
                    (exists (file-exists-p dir))
                    (files (js-import-directory-files dir)))
          (if (= 1 (length files))
              (car files)
            (seq-find (lambda(it) (js-import-string-match-p
                              js-import-file-index-regexp
                              it))
                      files)))
        (when-let* ((package-json (js-import-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             js-import-node-modules-priority-section-to-read)))
          (if (js-import-string-match-p js-import-enabled-extension-regexp
                                        module)
              (expand-file-name module path)
            (js-import-try-find-real-path (js-import-try-ext module path))))
        (js-import-try-ext path)
        (js-import-try-ext (js-import-join-file path "index"))
        (when-let* ((package-json (js-import-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             '("main"))))
          (if (js-import-string-match-p js-import-enabled-extension-regexp
                                        module)
              (expand-file-name module path)
            (js-import-try-find-real-path (js-import-try-ext module path)))))))

(defun js-import-read-package-json-section (&optional package-json-path section)
  "Reads a SECTION from PACKAGE-JSON-PATH and returns its hash.
By default PACKAGE-JSON-PATH is a value of `js-import-find-package-json'.
Default section is `dependencies'"
  (unless section (setq section "dependencies"))
  (let ((path (or package-json-path (js-import-find-package-json)))
        (json-object-type 'hash-table))
    (when-let ((content (condition-case nil
                            (decode-coding-string
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (setq buffer-file-coding-system 'binary)
                               (insert-file-contents-literally path)
                               (buffer-substring-no-properties
                                (point-min)
                                (point-max)))
                             'utf-8)
                          (error nil)))
               (hash (condition-case nil
                         (gethash section (json-read-from-string content))
                       (error nil))))
      hash)))

(defun js-import-find-package-json ()
  "Return the path to package.json."
  (when-let ((root (js-import-find-project-root)))
    (js-import-join-file root "package.json")))

(defun js-import-try-json-sections(path sections)
  (let (section)
    (while sections
      (setq section (js-import-read-package-json-section path (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))

(defun js-import-join-when-exists(path filename)
  "Return joined PATH with FILENAME when exists."
  (let ((joined-path (and path filename (js-import-join-file path filename))))
    (when (file-exists-p joined-path)
      joined-path)))

(defclass js-import-source-imported-files(helm-source-in-buffer)
  ((header-name :initform (lambda(name)
                            (with-helm-current-buffer
                              (if buffer-file-name
                                  (let ((base
                                         (file-name-base
                                          buffer-file-name)))
                                    (format "%s %s" name base))
                                name))))
   (init :initform (lambda() (with-current-buffer
                            (helm-candidate-buffer 'global)
                          (let ((items (with-helm-current-buffer
                                         (js-import-find-imported-files))))
                            (mapc (lambda(it) (insert it) (newline-and-indent))
                                  items))
                          (goto-char (point-min)))))
   (action :initform 'js-import-file-actions)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (mode-line :initform (list "Imports"))
   (keymap :initform js-import-files-map)
   (get-line :initform #'buffer-substring)
   (group :initform 'js-import)))

(defun js-import-find-imported-files()
  "Return list of with imported imported paths in current buffer."
  (save-excursion
    (goto-char 0)
    (let (symbols)
      (with-syntax-table js-import-mode-syntax-table
        (while (re-search-forward js-import-regexp-import-keyword nil t 1)
          (backward-char)
          (unless (js-import-inside-comment-p)
            (when-let ((path (js-import-get-path-at-point)))
              (push path symbols)))
          (forward-line 1)))
      (cl-remove-duplicates (reverse symbols) :test 'string=))))

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."
  (setq candidate (js-import-path-to-real candidate default-directory))
  (when-let ((buf (get-buffer-create "*helm-js-import*"))
             (valid (and candidate (stringp candidate) (file-exists-p
                                                        candidate))))
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

(defun js-import-find-file-at-point()
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (let (path)
    (save-excursion
      (unless (js-import-inside-string-p)
        (beginning-of-line))
      (setq path (js-import-get-path-at-point)))
    (when path
      (js-import-find-file path))))

(defun js-import-find-file-and-exit(&optional _file)
  "Transform FILE to real and open it."
  (interactive)
  (helm-run-after-exit 'js-import-find-file (car (helm-marked-candidates))))

(defun js-import-find-file-other-window-and-exit()
  "Transform FILE to real and open it."
  (interactive)
  (helm-run-after-exit
   'js-import-find-file-other-window (car (helm-marked-candidates))))

;;;###autoload
(defun js-import-find-file(file)
  "Transform FILE to real and open it."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file path)
      (message "Could't find %s" file))))
(put 'js-import-find-file 'helm-only t)

;;;###autoload
(defun js-import-find-file-other-window(file)
  "Transform FILE to real and open it in other window."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file-other-window path)
      (message "Could't find %s" file))))
(put 'js-import-find-file-other-window 'helm-only t)

(defun js-import-marked-files(_c)
  "Call command `js-import-from-path' for marked candidates."
  (mapc 'js-import-from-path (helm-marked-candidates :all-sources t)))

;;;###autoload
(defun js-import-from-path(path)
  "Insert import statement with marked symbols, exported from PATH."
  (interactive)
  (with-current-buffer helm-current-buffer
    (when-let ((display-path (or (js-import-get-prop path 'display-path) path)))
      (setq js-import-current-export-path display-path)
      (setq js-import-last-export-path display-path)
      (setq js-import-current-export-real-path
            (js-import-path-to-real display-path default-directory))))
  (let (sources)
    (push (helm-make-source "Imported"
              'js-import-source-imported-symbols)
          sources)
    (push (helm-make-source "JS import exports"
              'js-import-source-symbols-in-path)
          sources)
    (helm :sources sources)))

(defclass js-import-source-imported-symbols(helm-source-sync)
  ((candidates :initform 'js-import-imported-candidates-in-buffer)
   (candidate-transformer
    :initform (lambda(candidates)
                (with-helm-current-buffer
                  (when js-import-current-export-path
                    (setq candidates
                          (js-import-filter-with-prop
                           'display-path
                           js-import-current-export-path
                           candidates))))
                candidates))
   (marked-with-props :initform 'withprop)
   (persistent-help :initform "Show symbol")
   (display-to-real :initform 'js-import-display-to-real-imports)
   (keymap :initform js-import-imported-symbols-map)
   (persistent-action :initform (lambda(c) (js-import-jump-to-item-in-buffer
                                       (js-import-display-to-real-imports c))))
   (action :initform 'js-import-symbol-actions)))

(defclass js-import-source-exported-symbols(helm-source-sync)
  ((candidates :initform 'js-import-exported-candidates-in-buffer)
   (marked-with-props :initform 'withprop)
   (persistent-help :initform "Show symbol")
   (display-to-real
    :initform (lambda(it)
                (with-helm-current-buffer
                  (seq-find (lambda(elt) (string= elt it))
                            js-import-cached-exports-in-buffer
                            it))))
   (persistent-action
    :initform (lambda(it)
                (with-helm-current-buffer
                  (setq it (seq-find (lambda(elt) (string= elt it))
                                     js-import-cached-exports-in-buffer
                                     it)))
                (js-import-jump-to-item-in-buffer it)))
   (action :initform 'js-import-jump-to-item-in-buffer)))

(defclass js-import-source-symbols-in-path(helm-source-sync)
  ((header-name :initform (lambda(name) (with-helm-current-buffer
                                     (if js-import-current-export-path
                                         (format "exports in %s"
                                                 js-import-current-export-path)
                                       "No exports"))))
   (candidates :initform 'js-import-init-exports-candidates)
   (candidate-transformer :initform 'js-import-exported-candidates-transformer)
   (marked-with-props :initform 'withprop)
   (cleanup :initform 'js-import-exports-cleanup)
   (volatile :initform t)
   (action :initform 'js-import-export-items-actions)
   (persistent-action
    :initform (lambda(c) (when-let ((props (js-import-display-to-real-exports c)))
                      (js-import-jump-to-item-persistent props))))))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Show imported symbols from current buffer.
  Available actions includes jumping to item in buffer, renaming, adding more
  imports from current paths and deleting a symbol or whole import."
  (interactive)
  (let (sources)
    (push (helm-make-source "Exports"
              'js-import-source-exported-symbols)
          sources)
    (push (helm-make-source "Imports"
              'js-import-source-imported-symbols)
          sources)
    (helm
     :preselect (js-import-preselect-symbol)
     :sources sources)))

;;;###autoload
(defun js-import-show-symbols()
  (interactive)
  (let* ((visible-defs (js-import-search-backward-identifiers))
         (sources (list (helm-make-source "JS Imports"
                            'js-import-source-imported-symbols)
                        (helm-make-source "JS Exports"
                            'js-import-source-exported-symbols)
                        (js-import-build-symbols-source "Visible definitions"
                                                        visible-defs))))
    (helm
     :preselect (js-import-preselect-symbol)
     :sources sources)))

(defun js-import-preselect()
  "Preselect function for file sources."
  (if  (and (> (point-max) (point))
            (stringp (or (js-import-get-path-at-point)
                         js-import-last-export-path
                         js-import-current-export-path)))
      (regexp-quote (or (js-import-get-path-at-point)
                        js-import-last-export-path
                        js-import-current-export-path))
    ""))

(defun js-import-preselect-symbol()
  "Preselect function for symbols."
  (or (when-let ((pos (> (point-max) (point)))
                 (symbol (js-import-which-word)))
        (unless (or (js-import-invalid-name-p symbol)
                    (js-import-reserved-word-p symbol))
          symbol))
      ""))

(defun js-import-reset-all-sources()
  "Reset all files sources."
  (interactive)
  (remhash (js-import-find-project-root) js-import-dependencies-cache)
  (setq js-import-buffer-files-source nil)
  (setq js-import-project-files-source nil)
  (setq js-import-node-modules-source nil))

(defun js-import-exports-cleanup()
  "Reset filter for imported candidates."
  (with-helm-current-buffer
    (setq js-import-current-export-path nil)
    (setq js-import-current-export-real-path nil)))

(defun js-import-display-to-real-exports(str)
  "Find STR in the variable `js-import-export-candidates-in-path'."
  (with-helm-current-buffer
    (seq-find (lambda(elt) (string= str elt))
              js-import-export-candidates-in-path)))

(defun js-import-display-to-real-imports(item)
  "Find ITEM in the variable `js-import-cached-imports-in-buffer.'."
  (with-helm-current-buffer
    (seq-find (lambda(elt) (string= elt item))
              js-import-cached-imports-in-buffer item)))

(defun js-import-imported-candidates-in-buffer(&optional buffer)
  "Returns imported symbols in BUFFER which are cached and stored
in a buffer local variable `js-import-cached-imports-in-buffer'.
   Cache are invalidated when `buffer-modified-tick' is changed."
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-import-cached-imports-in-buffer-tick tick)
          js-import-cached-imports-in-buffer
        (progn
          (setq js-import-cached-imports-in-buffer-tick tick)
          (setq js-import-cached-imports-in-buffer
                (seq-remove 'js-import-reserved-word-p
                            (js-import-extract-imports buffer-file-name)))
          js-import-cached-imports-in-buffer)))))

(defun js-import-extract-imports(&optional real-path)
  (with-temp-buffer
    (erase-buffer)
    (js-import-insert-buffer-or-file real-path)
    (save-excursion
      (goto-char 0)
      (let (symbols)
        (with-syntax-table js-import-mode-syntax-table
          (while (re-search-forward js-import-regexp-import-keyword nil t 1)
            (unless (or (js-import-inside-comment-p)
                        (js-import-inside-string-p))
              (let (display-path imports)
                (save-excursion
                  (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
                  (setq display-path (js-import-get-path-at-point)))
                (cond ((js-import-looking-at "*")
                       (let (m1 m2 renamed-name)
                         (setq m1 (point))
                         (forward-char)
                         (skip-chars-forward "\s\t")
                         (setq m2 (point))
                         (when (js-import-looking-at "as")
                           (skip-chars-forward "as")
                           (setq m2 (point))
                           (skip-chars-forward "\s\t")
                           (setq renamed-name (js-import-which-word))
                           (if (or (js-import-reserved-word-p renamed-name)
                                   (js-import-invalid-name-p renamed-name))
                               (setq renamed-name "")
                             (skip-chars-forward js-import-regexp-name)))
                         (setq m2 (point))
                         (push (js-import-make-index-item
                                (format "%s"
                                        (buffer-substring-no-properties m1 m2))
                                :type 16
                                :real-name renamed-name
                                :local-name renamed-name
                                :external-name "*"
                                :pos m1
                                :display-path display-path)
                               imports)
                         (skip-chars-forward
                          js-import-regexp-name-with-separators)))
                      ((looking-at-p js-import-regexp-name-set)
                       (push (js-import-make-index-item
                              (js-import-which-word)
                              :type 1
                              :real-name (js-import-which-word)
                              :external-name "default"
                              :local-name (js-import-which-word)
                              :pos (point)
                              :display-path display-path)
                             imports)
                       (skip-chars-forward
                        js-import-regexp-name-with-separators))
                      ((looking-at-p "[\"']")
                       (forward-char)
                       (when-let* ((display-path (js-import-get-path-at-point))
                                   (real-path (js-import-path-to-real
                                               display-path
                                               (js-import-dirname real-path)))
                                   (extracted-symbols (js-import-extract-imports
                                                       real-path)))
                         (setq symbols (append symbols extracted-symbols)))))
                (when (looking-at-p "{")
                  (setq imports (append imports
                                        (js-import-extract-imports-in-brackets
                                         display-path))))
                (setq symbols (append symbols imports))))
            (forward-line 1)))
        (cl-remove-duplicates symbols :test 'string=)))))

(defun js-import-extract-imports-in-brackets(display-path &optional real-path)
  (let (p1 p2 items real-name full-name)
    (setq p1 (1+ (point)))
    (save-excursion
      (re-search-forward "}" nil t 1)
      (setq p2 (- (point) 1)))
    (narrow-to-region p1 p2)
    (goto-char p1)
    (while (re-search-forward js-import-regexp-name-set nil t 1)
      (setq p1 (match-beginning 0))
      (goto-char p1)
      (setq real-name (js-import-which-word))
      (skip-chars-forward js-import-regexp-name)
      (setq p2 (point))
      (skip-chars-forward " \s\t\n")
      (when (looking-at-p "as[ \s\t\n]")
        (progn
          (re-search-forward "as[ \s\t\n]" nil t 1)
          (skip-chars-forward js-import-regexp-name)
          (setq p2 (point))))
      (setq full-name (string-trim (buffer-substring-no-properties p1 p2)))
      (push (js-import-make-index-item
             full-name
             :type 4
             :external-name real-name
             :local-name (or (nth 2 (split-string full-name)) real-name)
             :real-name real-name
             :display-path display-path
             :real-path real-path
             :pos p1)
            items))
    (widen)
    (reverse items)))

(defun js-import-exported-candidates-in-buffer(&optional buffer)
  "Returns imported symbols in BUFFER which are cached and stored
in a buffer local variable `js-import-cached-exports-in-buffer'.
   Cache are invalidated when `buffer-modified-tick' is changed."
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-import-cached-exports-in-buffer-tick tick)
          js-import-cached-exports-in-buffer
        (progn
          (setq js-import-cached-exports-in-buffer-tick tick)
          (setq js-import-cached-exports-in-buffer
                (seq-uniq (append (js-import-extract-esm-exports
                                   buffer-file-name)
                                  (js-import-extract-cjs-exports
                                   buffer-file-name))))
          js-import-cached-exports-in-buffer)))))

(defun js-import-exported-candidates-transformer(candidates)
  "Remove duplicates and imported members from from CANDIDATES plist."
  (with-current-buffer helm-current-buffer
    (let (imports exports)
      (setq imports (js-import-imported-candidates-in-buffer
                     helm-current-buffer))
      (setq imports (js-import-filter-with-prop 'display-path
                                                js-import-current-export-path
                                                imports))
      (setq exports (if imports (js-import-filter-exports candidates imports)
                      candidates))
      (setq exports (cl-remove-duplicates exports :test 'string=))
      (setq exports (mapcar (lambda(c) (js-import-propertize
                                   c 'display-path
                                   js-import-current-export-path))
                            exports)))))

(defun js-import-filter-with-prop(property value items)
  "Return filtered ITEMS with members whose PROPERTY equals VALUE."
  (seq-filter (lambda(str) (string= (js-import-get-prop str property) value))
              items))

(defun js-import-find-by-prop(property value list)
  "Find item in LIST whose PROPERTY equals VALUE."
  (seq-find (lambda(str) (equal (js-import-get-prop str property) value))
            list))

(defun js-import-filter-exports(exports imports)
  "Return EXPORTS plist with only those members that are not in IMPORTS plist.
   For named exports (with property `type' 4) the test for equality is done by
`real-name' and for default export by `type'."
  (let ((by-type (lambda(elt) (let ((type (js-import-get-prop elt 'type)))
                           (pcase type
                             (1
                              (js-import-find-by-prop 'type type imports))
                             (4
                              (js-import-find-by-prop 'real-name
                                                      (js-import-get-prop
                                                       elt
                                                       'real-name)
                                                      imports))
                             (16 (< 0 (length imports))))))))
    (seq-remove by-type exports)))

(defun js-import-delete-whole-import-persistent (&optional _cand)
  "Persistent action for quick delete CAND from import statement."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'js-import-delete-whole-import
                  '(js-import-delete-whole-import . never-split))
    (helm-execute-persistent-action 'js-import-delete-whole-import)
    (helm-refresh)))

(defun js-import-delete-persistent (&optional _cand)
  "Persistent action for quick delete CAND from import statement."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'js-import-delete-imported-item
                  '(js-import-delete-imported-item . never-split))
    (helm-execute-persistent-action 'js-import-delete-imported-item)
    (js-import-init-exports-candidates)
    (helm-refresh)))

(defun js-import-delete-imported-item(candidate)
  "Remove CANDIDATE from import statement in buffer."
  (let* ((display-path (js-import-get-prop candidate 'display-path))
         (type (js-import-get-prop candidate 'type))
         (other-imports (js-import-filter-with-prop
                         'display-path display-path
                         js-import-cached-imports-in-buffer))
         (whole-import-bounds (js-import-get-import-positions display-path))
         (beg (car whole-import-bounds))
         (end (cdr whole-import-bounds))
         (p1)
         (p2)
         (overlay))
    (setq other-imports (remove candidate other-imports))
    (remove-overlays (car whole-import-bounds) (cdr whole-import-bounds))
    (unwind-protect
        (if (or (= type 16) (not other-imports))
            (progn
              (setq overlay (make-overlay beg end))
              (overlay-put overlay 'face 'js-import-highlight-face)
              (when (yes-or-no-p "Delete whole import?")
                (remove-overlays beg end)
                (delete-region beg end)))
          (save-excursion
            (goto-char (js-import-get-prop candidate 'pos))
            (setq p1 (point))
            (re-search-forward candidate nil t 1)
            (setq p2 (point))
            (skip-chars-forward " ,\s\n\t")
            (setq p2 (point))
            (when (looking-at-p "}")
              (setq p2 (point))
              (goto-char p1)
              (skip-chars-backward " \s\t\n")
              (backward-char)
              (when (looking-at-p "{")
                (setq p2 (1+ p2))
                (setq p1 (point))
                (skip-chars-backward  " \s\t\n")
                (backward-char))
              (when (looking-at-p ",")
                (setq p1 (point))))
            (setq overlay (make-overlay p1 p2))
            (overlay-put overlay 'face 'js-import-highlight-face)
            (when (yes-or-no-p "Delete?")
              (remove-overlays p1 p2)
              (delete-region p1 p2))))
      (remove-overlays beg end))))

(defun js-import-rename-item()
  "Persistent quick action to rename CANDIDATE in buffer."
  (interactive)
  (helm-exit-and-execute-action 'js-import-rename-import))

(defun js-import-rename-import(candidate)
  "Rename imported CANDIDATE in buffer."
  (save-excursion
    (save-restriction
      (pcase (js-import-get-prop candidate 'type)
        (1 (js-import-rename-default-item candidate))
        (4 (js-import-rename-as candidate))
        (16 (js-import-rename-as candidate))))))

(defun js-import-rename-default-item(item)
  "Renames default imported ITEM."
  (let (real-name new-name overlay end beg)
    (setq real-name (or (js-import-get-prop item 'real-name)
                        (js-import-strip-text-props item)))
    (setq beg (js-import-get-prop item 'pos))
    (goto-char beg)
    (when (string= real-name (js-import-which-word))
      (setq end (+ (point) (length (js-import-which-word))))
      (unwind-protect
          (progn (setq overlay (make-overlay beg end))
                 (make-overlay beg end)
                 (overlay-put overlay 'face 'ag-match-face)
                 (setq new-name (read-string
                                 "Rename %s to" (concat "\s" real-name)))
                 (setq new-name (string-trim new-name))
                 (if (string-blank-p new-name)
                     (message "New name is blank")
                   (progn
                     (remove-overlays beg end)
                     (let ((case-fold-search nil)
                           (regexp (concat "\\_<" real-name "\\_>")))
                       (query-replace-regexp regexp new-name)))))
        (remove-overlays beg end)))))

(defun js-import-rename-as(item)
  "Rename named imports and module imports."
  (let* ((pos (js-import-get-prop item 'pos))
         (full-name (js-import-strip-text-props item))
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
               (string= real-name (js-import-which-word)))
      (skip-chars-forward real-name)
      (if as-word
          (progn
            (skip-chars-forward " \s\t\n")
            (skip-chars-forward "as")
            (skip-chars-forward " \s\t\n")
            (when (and renamed-name
                       (string= renamed-name (js-import-which-word)))
              (query-replace-regexp (concat "\\_<" renamed-name "\\_>")
                                    new-name)))
        (progn
          (insert (format " as %s" new-name))
          (query-replace-regexp (concat "\\_<" real-name "\\_>") new-name))))))

(defun js-import-insert-import(candidate)
  "Insert CANDIDATE into existing or new import statement."
  (save-excursion
    (let ((type (js-import-get-prop candidate 'type))
          (display-path (js-import-get-prop candidate 'display-path)))
      (pcase type
        (1 (js-import-insert-exports
            (js-import-propose-name candidate) nil display-path))
        (4 (js-import-insert-exports
            nil
            (js-import-strip-text-props candidate)
            display-path))
        (16 (js-import-insert-exports
             (js-import-propose-name candidate)
             nil display-path))))))

(defun js-import-insert-marked(&optional _candidate)
  "Call a function `js-import-insert-import' with marked candidates."
  (mapc 'js-import-insert-import (helm-marked-candidates)))

(defun js-import-insert-import-as(_item)
  "Insert and renames CANDIDATE into existing or new import statement."
  (let* ((candidate (helm-get-selection nil 'withprop))
         (type (js-import-get-prop candidate 'type))
         (normalized-path (js-import-get-prop candidate 'display-path))
         (real-name (or (js-import-get-prop candidate 'real-name) candidate))
         (renamed-name (car (split-string
                             (read-string
                              (format "import %s as " real-name)))))
         (full-name (concat real-name " as " renamed-name)))
    (pcase type
      (1 (js-import-insert-exports renamed-name nil normalized-path))
      (4 (js-import-insert-exports nil full-name normalized-path))
      (16 (js-import-insert-exports full-name nil normalized-path)))))

(defun js-import-insert-exports(default-name named-list path)
  (let ((names (if (stringp named-list)
                   named-list
                 (js-import-join-names named-list)))
        (imports (reverse (js-import-find-imported-files))))
    (save-excursion
      (js-import-goto-last-import)
      (if (member path imports)
          (progn
            (goto-char (cdr (js-import-get-import-positions path)))
            (js-import-add-to-current-imports default-name names))
        (progn
          (let (module project-root)
            (setq project-root (js-import-find-project-root))
            (cond ((js-import-relative-p path)
                   (let* ((dir (file-name-directory path))
                          (pred (lambda(it) (string= dir (file-name-directory it)))))
                     (setq module (seq-find pred imports))
                     (unless module
                       (setq module (seq-find 'js-import-relative-p imports)))))
                  ((js-import-dependency-p path)
                   (when-let ((dependencies (js-import-node-modules-candidates
                                             project-root)))
                     (setq module (seq-find (lambda(it) (member it dependencies))
                                            imports))
                     (unless module
                       (goto-char (point-min)))))
                  (t (let ((pred (lambda(it) (not (js-import-relative-p it)))))
                       (setq module (seq-find pred imports))
                       (unless module
                         (when-let* ((relative (seq-find 'js-import-relative-p
                                                         imports))
                                     (bounds (js-import-get-import-positions
                                              module)))
                           (goto-char (car bounds)))))))
            (when module
              (goto-char (cdr (js-import-get-import-positions module)))
              (forward-line)))
          (insert "import " (js-import-join-imports-names default-name names)
                  " from " js-import-quote path js-import-quote ";\n")))
      (js-import-goto-last-import)
      (unless (looking-at-p "\n")
        (newline-and-indent)))))

(defun js-import-add-to-current-imports (default-name names)
  (search-backward "import")
  (forward-word)
  (skip-chars-forward " \t\n")
  (when default-name
    (while (thing-at-point 'word)
      (js-import-kill-thing-at-point 'word))
    (insert default-name)
    (goto-char (- (point) (length default-name))))
  (when (word-at-point)
    (let* ((word (symbol-name (symbol-at-point)))
           (word-length (length word))
           (pos (point))
           (next-pos (+ pos word-length)))
      (goto-char next-pos)
      (skip-chars-forward " \t\n")
      (when (string= "," (string (char-after)))
        (delete-char 1))
      (insert ",")
      (skip-chars-forward ",")
      (skip-chars-forward " \t\n")))
  (when names
    (let ((brackets-exist (string="{" (string (char-after)))))
      (if brackets-exist
          (js-import-add-to-brackets names)
        (insert "{" names "}")))))

(defun js-import-add-to-brackets(names)
  (re-search-forward "\\({[^}]+\\([a-zA-Z0-9]*\\)\\)"  nil t)
  (skip-chars-backward " \t\n")
  (if (string= "," (string (char-before)))
      (insert " " names)
    (insert ", " names)))

(defun js-import-get-comments-bounds(&optional pos)
  (unless (setq pos (setq pos (point-min))))
  (save-excursion
    (save-restriction
      (with-syntax-table js-import-mode-syntax-table
        (let (comments)
          (goto-char pos)
          (while (re-search-forward "\\(/\\*\\)\\|\\(//\\)\\|[\"'`]" nil t 1)
            (if (save-excursion
                  (backward-char)
                  (looking-at "[\"'`]"))
                (js-import-skip-string)
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

(defun js-import-remove-comments()
  "Replaces comments in buffer with empty lines."
  (let ((comments (js-import-get-comments-bounds)))
    (dotimes (idx (length comments))
      (let* ((cell (nth idx comments))
             (p1 (car cell))
             (p2 (cdr cell))
             (replace (lambda()
                        (let* ((content (buffer-substring-no-properties
                                         (point-min)
                                         (point-max)))
                               (vect (make-vector (1+ (length content)) ""))
                               (replacement (append vect nil)))
                          (mapconcat 'identity replacement "\s")))))
        (replace-region-contents p1 p2 replace)))))

(defun js-import-insert-buffer-or-file(path &optional keep-comments)
  "A function inserts content either from buffer or file.
It depends whether buffer with the given PATH exists.
Without argument KEEP-COMMENTS content will inserted without comments."
  (when (and path (file-exists-p path))
    (if (get-file-buffer path)
        (progn
          (insert-buffer-substring-no-properties (get-file-buffer path)))
      (progn
        (let ((buffer-file-name path))
          (insert-file-contents path))))
    (unless keep-comments (js-import-remove-comments))))

(defun js-import-get-buffer-content-no-comments(&optional beg end)
  (js-import-buffer-substring-no-comments beg end))

(defun js-import-buffer-substring-no-comments(&optional beg end)
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (let ((content (buffer-substring-no-properties beg end)))
    (with-temp-buffer (insert content)
                      (js-import-remove-comments)
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max)))))

(defun js-import-init-exports-candidates()
  "Search exports in a file.
File is specified in the variable `js-import-current-export-path.'."
  (with-current-buffer helm-current-buffer
    (let ((namespace (js-import-make-index-item
                      "* as"
                      :type 16))
          (default (js-import-make-index-item
                    "default"
                    :type 1)))
      (if js-import-current-export-real-path
          (when-let* ((path js-import-current-export-real-path)
                      (str (stringp path)))
            (setq js-import-export-candidates-in-path
                  (or (js-import-extract-esm-exports path)
                      (js-import-extract-cjs-exports path)))
            (unless js-import-export-candidates-in-path
              (push namespace js-import-export-candidates-in-path)
              (push default js-import-export-candidates-in-path))
            js-import-export-candidates-in-path)
        (list namespace default)))))

(defun js-import-extract-esm-exports(&optional path)
  "Return exports in PATH defined with ES Module syntax."
  (with-temp-buffer
    (erase-buffer)
    (js-import-insert-buffer-or-file path)
    (save-excursion
      (goto-char (point-min))
      (let (symbols)
        (with-syntax-table js-import-mode-syntax-table
          (while (re-search-forward js-import-regexp-export-keyword nil t 1)
            (let (display-path exports)
              (unless (js-import-inside-string-p)
                (save-excursion
                  (if (looking-at-p js-import-delcaration-keywords--re)
                      (setq display-path path)
                    (progn (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]"
                                              nil t 1)
                           (setq display-path (js-import-get-path-at-point)))))
                (cond ((looking-at-p "*[ \s\t\n]+as[ \s\t\n]")
                       (when-let ((namespace-export
                                   (js-import-extract-esm-namespace-exports
                                    path
                                    display-path)))
                         (push namespace-export exports)))
                      ((looking-at-p "{")
                       (let ((from-path (save-excursion
                                          (forward-list)
                                          (skip-chars-forward "\s\t\n")
                                          (when (js-import-looking-at "from")
                                            (skip-chars-forward "from")
                                            (skip-chars-forward "\s\t\n")
                                            (forward-char)
                                            (js-import-path-to-real
                                             (js-import-get-path-at-point)
                                             (js-import-dirname path))))))
                         (save-excursion
                           (let ((items (js-import-extract-esm-braced-exports
                                         path
                                         (or from-path path))))
                             (when items (setq exports
                                               (append exports items))))))
                       (forward-list))
                      ((looking-at-p js-import-regexp-name-set)
                       (re-search-backward "export" nil t 1)
                       (when-let ((name (js-import-extract-var-export
                                         path
                                         display-path)))
                         (push name exports)))
                      ((looking-at-p "*[ \s\t]from")
                       (when-let* ((curr-dir (js-import-dirname path))
                                   (next-path (js-import-path-to-real
                                               display-path
                                               curr-dir)))
                         (setq symbols (append symbols
                                               (js-import-extract-esm-exports
                                                next-path))))))
                (setq symbols (append symbols exports))))
            (forward-line 1)))
        symbols))))

(defun js-import-extract-cjs-exports(&optional path)
  "Return exports in PATH defined with CommonJs syntax."
  (unless path (setq path buffer-file-name))
  (with-temp-buffer
    (erase-buffer)
    (js-import-insert-buffer-or-file path)
    (save-excursion
      (re-search-forward "['\"]use strict['\"]" nil t 1)
      (goto-char (point-min))
      (let (symbols)
        (with-syntax-table js-import-mode-syntax-table
          (while (re-search-forward
                  js-import-regexp-cjs-exports-keyword nil t 1)
            (skip-chars-forward "\s\t")
            (let (display-path exports)
              (unless (js-import-inside-comment-p)
                (cond ((looking-at-p "=\\([\s\t]+?\\)require[ \t('\"]")
                       (re-search-forward "require[ \t('\"]" nil t 1)
                       (skip-chars-forward "'\"")
                       (when-let* ((module-path (js-import-get-path-at-point))
                                   (curr-dir (js-import-dirname path))
                                   (next-path (js-import-path-to-real
                                               module-path
                                               curr-dir)))
                         (setq symbols (append symbols
                                               (js-import-extract-cjs-exports
                                                next-path)))))
                      ((looking-at-p "=\\([\s\t]+?\\){")
                       (re-search-forward "=\\([\s\t]+?\\)" nil t 1)
                       (when-let* ((items (js-import-parse-object-keys))
                                   (mapfunc (lambda(cell)
                                              (let ((name (car cell)) )
                                                (js-import-make-index-item
                                                 name
                                                 :pos (cdr cell)
                                                 :type 4
                                                 :external-name name
                                                 :external-path display-path
                                                 :real-path path
                                                 :display-path display-path
                                                 :real-name name)))))
                         (setq items (mapcar mapfunc items))
                         (setq exports (append exports items)))
                       (forward-list))
                      ((looking-at-p js-import-regexp-name-set)
                       (when-let ((name (js-import-extract-cjs-named-export
                                         path
                                         display-path)))
                         (push name exports)))
                      ((looking-at-p "=\\([\s\t]+?\\)")
                       (re-search-forward "=\\([\s\t]+?\\)")
                       (when-let ((item (js-import-extract-cjs-named-export
                                         path
                                         display-path)))
                         (push (js-import-propertize
                                item
                                'type 1)
                               exports))))
                (setq symbols (append symbols exports))))
            (forward-line 1)))
        symbols))))

(defun js-import-extract-esm-braced-exports(real-path &optional display-path)
  "Extracts exports beetween brackets."
  (save-excursion
    (save-restriction
      (let (p1 p2 item items real-name full-name external-name external-pos)
    (setq p1 (1+ (point)))
    (save-excursion (re-search-forward "}" nil t 1)
                    (setq p2 (- (point) 1)))
    (narrow-to-region p1 p2)
    (goto-char p1)
    (while (re-search-forward js-import-regexp-name-set nil t 1)
      (setq p1 (match-beginning 0))
      (goto-char p1)
      (setq external-pos (point))
      (setq external-name (js-import-which-word))
      (skip-chars-forward js-import-regexp-name)
      (setq p2 (point))
      (skip-chars-forward " \s\t\n")
      (when (looking-at-p "as[ \s\t\n]")
        (progn
          (re-search-forward "as[ \s\t\n]" nil t 1)
          (setq p1 (point))
          (setq real-name (js-import-which-word))
          (skip-chars-forward js-import-regexp-name)
          (setq p2 (point))))
      (setq full-name (string-trim (buffer-substring-no-properties p1 p2)))
      (setq item (js-import-make-index-item
                  full-name
                  :type 4
                  :real-name (or real-name external-name)
                  :external-pos external-pos
                  :external-name external-name
                  :external-path display-path
                  :display-path display-path
                  :real-path real-path
                  :pos p1))
      (push item items))
    (reverse items)))))

(defun js-import-parse-object-keys()
  "Return object keys at point."
  (save-excursion
    (let (start end children re)
      (setq re (concat js-import-regexp-name-set "[\s\t\n]*[:(,]"))
      (when (looking-at-p "{")
        (setq start (point))
        (forward-list)
        (setq end (point))
        (goto-char start)
        (forward-char))
      (unless (or (null start) (null end))
        (save-excursion
          (save-restriction
            (narrow-to-region (1+ start) (1- end))
            (while (re-search-forward re nil t 1)
              (let (prop delimiter)
                (setq delimiter (char-before))
                (save-excursion
                  (backward-char)
                  (skip-chars-backward "\s\t\n")
                  (setq prop (js-import-get-word-if-valid))
                  (when prop
                    (skip-chars-backward prop)
                    (push (cons prop (point)) children)))
                (cond ((char-equal delimiter ?:)
                       (skip-chars-forward "\s\t\n")
                       (when (looking-at-p js-import-regexp-name-set)
                         (skip-chars-forward js-import-regexp-name)
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
                               (skip-chars-forward js-import-regexp-name)))
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
                           (when-let ((word (js-import-get-word-if-valid))
                                      (pos (point)))
                             (skip-chars-forward word)
                             (skip-chars-forward "\s\t\n")
                             (push (cons word pos) children)))))))))))
      (when children (reverse (cl-remove 'null children))))))

(defun js-import-parse-destructive()
  "Return object values."
  (save-excursion
    (let (start end parent children)
      (when (looking-at-p "{")
        (setq start (point))
        (forward-list)
        (setq end (point))
        (when (looking-at-p "[\s\t\n]*=[\s\t\n]*")
          (save-excursion
            (re-search-forward "[\s\t\n]*=[\s\t\n]*" nil t 1)
            (setq parent (js-import-which-word)))))
      (save-excursion
        (save-restriction
          (narrow-to-region (1+ start) (1- end))
          (skip-chars-backward "},\s\t\n")
          (push (js-import-maybe-make-child-at-point parent) children)
          (while (re-search-backward "," nil t 1)
            (skip-chars-backward "},\s\t\n")
            (push (js-import-maybe-make-child-at-point parent) children))))
      (cl-remove 'null children))))

(defun js-import-extract-cjs-named-export(real-path &optional display-path)
  "Returns propertizied named or default export."
  (let* ((real-name (js-import-which-word)))
    (unless (js-import-invalid-name-p real-name)
      (js-import-make-index-item
       real-name
       :type (if (string= real-name "default")  1 4)
       :real-name real-name
       :external-name (if (string= real-name "default")  "default" real-name)
       :real-path real-path
       :pos (point)
       :display-path display-path))))

(defun js-import-extract-esm-namespace-exports(real-path &optional display-path)
  "Make export all as item."
  (re-search-forward "as[ \s\t\n]+\\([_$A-Za-z0-9]\\)" nil t 1)
  (let ((real-name (js-import-which-word)))
    (js-import-make-index-item
     real-name
     :type 4
     :real-name real-name
     :local-name "*"
     :external-name real-name
     :var-type "namespace"
     :pos (point)
     :real-path real-path
     :display-path display-path)))

(defun js-import-skip-reserved-words(&optional separators)
  (unless separators (setq separators "\s\t\".='*"))
  (let* ((stack)
         (stop)
         (func (lambda()
                 (let ((w (js-import-which-word))
                       (p (point)))
                   (when (rassoc p stack)
                     (setq stop t))
                   (push (cons w p) stack)
                   w))))
    (while (and (js-import-reserved-word-p (funcall func))
                (not stop))
      (skip-chars-forward js-import-regexp-name)
      (skip-chars-forward separators))
    stack))

(defun js-import-extract-var-export(&optional real-path display-path)
  "Returns propertizied named or default export."
  (let* ((stack (js-import-skip-reserved-words "\s\t*"))
         (default (assoc "default" stack))
         (last-cons (pop stack))
         (real-name (car last-cons))
         (var-type (car (pop stack))))
    (when (and default
               (or (not real-name)
                   (js-import-invalid-name-p real-name)
                   (js-import-reserved-word-p real-name)))
      (setq real-name var-type))
    (unless (js-import-invalid-name-p real-name)
      (js-import-make-index-item
       real-name
       :type (if default 1 4)
       :real-name real-name
       :external-name real-name
       :local-name real-name
       :real-path real-path
       :var-type (unless (or (null var-type) (string= var-type "default"))
                   var-type)
       :pos (point)
       :display-path display-path))))

(defun js-import-kill-thing-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun js-import-import-backward-exist-p(path)
  (re-search-backward (concat "from +['\"]" path "['\"]") nil t))

(defun js-import-join-names(symbols)
  (when (and (listp symbols) (<= 1 (length symbols)))
    (js-import-join ", " symbols)))

(defun js-import-join-imports-names(default-name names)
  (let (parts)
    (when (stringp names) (push (concat "{ " names" }") parts))
    (when (stringp default-name) (push default-name parts))
    (js-import-join ", " (seq-remove (lambda(it) (null it)) parts))))

(defun js-import-goto-last-import()
  (goto-char (point-min))
  (while (re-search-forward js-import-regexp-import-keyword nil t)
    (re-search-forward "['\"]" nil t 2)
    (forward-line 1))
  (point))

(defun js-import-get-import-positions(path)
  "Return a cons with bounds of import stament of PATH."
  (save-excursion
    (let ((pos1 (point-min))
          (pos2 (js-import-goto-last-import)))
      (when (js-import-import-backward-exist-p path)
        (re-search-forward "['\"]+;?" nil t 2)
        (setq pos2 (point))
        (re-search-backward js-import-regexp-import-keyword nil t)
        (setq pos1 (point)))
      (cons pos1 pos2))))

(defun js-import-delete-whole-import(candidate)
  "Remove whole import statement of CANDIDATE.
CANDIDATE should be propertizied with property `display-path'."
  (when-let* ((path (js-import-get-prop candidate 'display-path))
              (bounds (js-import-get-import-positions path)))
    (delete-region (car bounds) (cdr bounds))
    (join-line)))

(cl-defun js-import-make-index-item (candidate
                                     &key
                                     type
                                     parent
                                     display-path
                                     var-type
                                     real-path
                                     real-name
                                     local-name
                                     external-path
                                     external-pos
                                     external-name
                                     pos)
  "Utility function to propertize js symbol.
See also function `js-import-propertize'."
  (setq candidate (js-import-strip-text-props candidate))
  (js-import-propertize candidate
                        'real-name real-name
                        'display-path display-path
                        'real-path real-path
                        'type type
                        'var-type var-type
                        'pos pos
                        'local-name local-name
                        'parent parent
                        'external-name external-name
                        'external-pos external-pos
                        'external-path external-path))

(defun js-import-propertize (item &rest properties)
  "Stringify and `propertize' ITEM with PROPERTIES."
  (cl-loop for (k v) on properties by 'cddr
           if v append (list k v) into props
           finally return
           (apply 'propertize
                  (js-import-stringify item)
                  props)))

(defun js-import-get-prop (str property)
  "Return the value of zero position's PROPERTY in STR."
  (if (stringp str)
      (get-text-property 0 property str)))

(defun js-import-strip-text-props(item)
  "If ITEM is string, return it without text properties.
   If ITEM is symbol, return it is `symbol-name.'
   Otherwise return nil."
  (cond ((stringp item)
         (set-text-properties 0 (length item) nil item)
         item)
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun js-import-stringify (x)
  "Convert any object to string."
  (cl-typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun js-import-forward-name ()
  (skip-chars-forward js-import-regexp-name))

(defun js-import-backward-name ()
  (skip-chars-backward js-import-regexp-name))

(defun js-import-get-word-if-valid()
  "Return word at point if it is valid and not reservered, otherwise nil."
  (when-let* ((word (js-import-which-word))
              (valid (js-import-valid-identifier-p word)))
    word))

(defun js-import-which-word (&optional regexp)
  "Find closest to point whole word."
  (interactive)
  (unless regexp (setq regexp js-import-regexp-name))
  (unless (= (point) (point-max))
    (save-excursion
      (let (p1 p2 word)
        (save-excursion
          (skip-chars-backward regexp)
          (setq p1 (point))
          (right-char)
          (skip-chars-forward regexp)
          (setq p2 (point)))
        (when (< p1 (point))
          (goto-char p1))
        (setq word (buffer-substring-no-properties p1 p2))
        word)))
  )

(defun js-import-looking-at(str &optional regexp)
  (when-let ((word (js-import-which-word regexp)))
    (string= word str)))

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
    (when (js-import-inside-string-p)
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (let (p0 p1 p2 stops)
          (setq stops "^  \t\n\"`'‘’“”|[]{}〙·\\")
          (setq p0 (point))
          (skip-chars-backward stops)
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward stops)
          (setq p2 (point))
          (goto-char p0)
          (buffer-substring-no-properties p1 p2))))))

(defun js-import-string-match-p (regexp str &optional start)
  "Return t if STR matches REGEXP, otherwise return nil."
  (when (and (not (null str)) (stringp str))
    (not (null (string-match-p regexp str start)))))

(defun js-import-join (separator strings)
  "Join strings in STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

(defun js-import-string-contains-p (needle str &optional ignore-case)
  "Return t if STR contains NEEDLE, otherwise return nil.
If IGNORE-CASE is non-nil, the comparison will ignore case differences."
  (let ((case-fold-search ignore-case))
    (not (null (string-match-p (regexp-quote needle) str)))))

(defun js-import-string-blank-p (str)
  "Return t if STR is nil or empty, otherwise return nil."
  (or (null str) (string= "" str)))

(defun js-import-count-matches (regexp str &optional start end)
  "Count occurrences of REGEXP in STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))

(defun js-import-inside-string-p (&optional pos)
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (with-syntax-table js-import-mode-syntax-table
    (nth 3 (syntax-ppss (or pos (point))))))

(defun js-import-skip-string()
  (with-syntax-table js-import-mode-syntax-table
    (while (js-import-inside-string-p)
      (forward-char))))

(defun js-import-inside-comment-p ()
  "Return value of comment character in syntax table's or nil otherwise."
  (with-syntax-table js-import-mode-syntax-table
    (let ((comment-start "//")
          (comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
          (comment-use-syntax t)
          (result (nth 4 (syntax-ppss))))
      result)))

(defun js-import-invalid-name-p(str)
  "Return t when STR mathces any characters which are not allowed in js."
  (js-import-string-match-p (concat "[" "^" js-import-regexp-name "]") str))

(defun js-import-valid-identifier-p(str)
  "Return t if STR is a valid variable name, otherwise nil."
  (not (or (js-import-invalid-name-p str)
           (js-import-reserved-word-p str))))

(defun js-import-propose-name (candidate)
  (let* ((parts (split-string candidate))
         (type (js-import-get-prop candidate 'type))
         (current-name (car parts))
         (display-path (js-import-get-prop candidate 'display-path))
         (proposed-symbol (pcase type
                            (1 (if (js-import-valid-identifier-p candidate)
                                   candidate
                                 (js-import-generate-name-from-path
                                  display-path)))
                            (4 (js-import-generate-name-from-path
                                display-path))
                            (16 (js-import-generate-name-from-path
                                 display-path))))
         (prompt (format
                  (pcase type
                    (1 "Import default as (default: %s): ")
                    (4 "Import { (default: %s) }: ")
                    (16 "Import all exports as (default: %s): "))
                  proposed-symbol))
         (read-symbols
          (read-string
           prompt
           proposed-symbol
           nil nil proposed-symbol))
         (new-name (car (split-string (string-trim read-symbols))))
         (name (pcase type
                 (1 new-name)
                 (4 (format "%s as %s" current-name new-name))
                 (16 (format "* as %s" new-name)))))
    name))

(defun js-import-generate-name-from-path(path)
  "Generate name for default or module import from PATH."
  (let* ((split-path (lambda(str) (split-string str "[ \f\t\n\r\v-/.]")))
         (map-capitalize (lambda(p) (mapcar 'capitalize p)))
         (take-two (lambda(parts) (seq-take parts 2))))
    (js-import-compose-from path
                            'string-join
                            take-two
                            'reverse
                            map-capitalize
                            'seq-uniq
                            split-path
                            'js-import-maybe-remove-path-index
                            'js-import-remove-ext)))

(defun js-import-compose-from(arg &rest funcs)
  "Performs right-to-left unary function composition."
  (seq-reduce (lambda (xs fn)
                (funcall fn xs))
              (reverse funcs) arg))

(defun js-import-maybe-make-child-at-point(&optional parent)
  (when-let ((valid-id (js-import-get-word-if-valid)))
    (skip-chars-backward valid-id)
    (js-import-make-index-item valid-id
                               :pos (point)
                               :real-name valid-id
                               :parent parent
                               :local-name valid-id)))

(cl-defun js-import-highlight-word(&key pos limit buffer face secs jump)
  "Jumps to BEG and highlight word at point."
  (unless buffer (setq buffer (current-buffer)))
  (setq buffer (get-buffer-create buffer))
  (unless pos (setq pos (point)))
  (when (and jump (not (= pos (point))))
    (goto-char pos))
  (unless face (setq face 'js-import-highlight-face))
  (with-current-buffer buffer
    (let* ((buffer-name (if (bufferp buffer) (intern (buffer-name buffer))
                          (intern buffer)))
           (end (+ pos (or limit (length (js-import-which-word)))))
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
        (run-with-timer (or 1 secs) nil (lambda(o) (when (and (not (null o))
                                                         (overlayp o))
                                                (delete-overlay o)))
                        overlay)))))

(defun js-import-next-declaration-or-scope(&optional content pos)
  (interactive)
  (ignore-errors
    (unless pos (setq pos (point)))
    (unless content (setq content (js-import-get-buffer-content-no-comments)))
    (let ((opens-parens-re (regexp-opt (list "[" "{" "(")))
          declaration-start
          scope-start
          scope-end
          winner
          depth
          declaration-depth)
      (with-temp-buffer
        (insert content)
        (goto-char pos)
        (js-import-skip-string)
        (setq depth (nth 0 (syntax-ppss)))
        (save-excursion (when (re-search-forward opens-parens-re nil t 1)
                          (backward-char)
                          (setq scope-start (point))
                          (forward-list)
                          (setq scope-end (point))
                          (skip-chars-forward "\s\t\n,=>")
                          (while (looking-at
                                  (regexp-opt (list "[" "{" "(" "=>")))
                            (when (looking-at "=>")
                              (skip-chars-forward "\s\t\n=>"))
                            (forward-list)
                            (setq scope-end (point)))
                          (when (looking-at ";")
                            (skip-chars-forward ";")
                            (setq scope-end (point)))))
        (save-excursion
          (when (looking-at js-import-delcaration-keywords--re)
            (skip-chars-forward js-import-regexp-name))
          (when (re-search-forward js-import-delcaration-keywords--re nil t 1)
            (if (js-import-inside-string-p)
                (js-import-skip-string)
              (progn (setq declaration-depth (nth 0 (syntax-ppss)))
                     (skip-chars-backward js-import-regexp-name)
                     (setq declaration-start (point)))))))
      (if declaration-start
          (setq winner (if (and scope-start scope-end
                                (or (and (< scope-start declaration-start)
                                         (> scope-end declaration-start))
                                    (> declaration-depth depth)))
                           scope-end
                         declaration-start))
        (setq winner scope-end))
      (when winner (goto-char winner)))))

(defun js-import-previous-declaration-or-skope(&optional content pos)
  (interactive)
  (unless content (setq content (js-import-get-buffer-content-no-comments)))
  (unless pos (setq pos (point)))
  (let (declaration-start scope-start scope-end winner closed-parens-re)
    (setq closed-parens-re (regexp-opt (list "]" "}" ")")))
    (with-temp-buffer (insert content)
                      (goto-char pos)
                      (save-excursion
                        (when (re-search-backward closed-parens-re nil t 1)
                          (forward-char)
                          (setq scope-end (point))
                          (backward-list)
                          (setq scope-start (point))
                          (skip-chars-backward "\s\t\n,")
                          (while (looking-back closed-parens-re 1)
                            (backward-list)
                            (setq scope-start (point))
                            (skip-chars-backward "\s\t\n,"))))
                      (save-excursion
                        (when (and (re-search-backward
                                    js-import-delcaration-keywords--re nil t 1)
                                   (not (js-import-inside-string-p)))
                          (unless (js-import-inside-string-p)
                            (setq declaration-start (point))))))
    (if (and declaration-start scope-end scope-start)
        (setq winner (if (> scope-end declaration-start)
                         scope-start
                       declaration-start))
      (setq winner (or scope-start declaration-start)))
    (when winner (goto-char winner))))

(defun js-import-extract-parent-arguments(&optional parens-positions)
  (save-excursion
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
                  (re-search-backward js-import-delcaration-keywords--re nil t 1)
                  (when (looking-at js-import-delcaration-keywords--re)
                    (skip-chars-forward (js-import-which-word))
                    (skip-chars-forward "\s\t\n")
                    (setq parent (js-import-get-word-if-valid)))))
          (when (looking-back "=>" 1)
            (backward-char 2)
            (skip-chars-backward "\s\t\n")
            (when-let ((child (js-import-maybe-make-child-at-point parent)))
              (push child items)))
          (when-let ((end-pos (when (save-excursion (backward-char)
                                                    (looking-at ")"))
                                (point)))
                     (start (progn (backward-list) (point))))
            (forward-char)
            (skip-chars-forward "\s\t\n")
            (when-let ((params
                        (cond ((looking-at "{")
                               (js-import-parse-destructive))
                              ((looking-at js-import-regexp-name-set)
                               (goto-char (1- end-pos))
                               (skip-chars-backward ",\s\t\n")
                               (while (and (or (looking-at ",")
                                               (looking-at ")"))
                                           (not (looking-at "(")))
                                 (if (looking-at ",")
                                     (skip-chars-backward "\s\t\n,")
                                   (skip-chars-backward "\s\t\n"))
                                 (when-let ((child (js-import-maybe-make-child-at-point parent)))
                                   (push child items)
                                   (skip-chars-backward child)
                                   (skip-chars-backward "\s\t\n"))
                                 (unless (re-search-backward "," start t 1)
                                   (goto-char start)))))))
              (setq items (append items params))))
          (setq children (append children items))))
      children)))

(defun js-import-search-backward-identifiers()
  (let (init-p content ids parens)
    (setq init-p (point))
    (setq parens (nth 9 (syntax-ppss init-p)))
    (setq content (buffer-substring-no-properties (point-min) (point-max)))
    (setq ids (js-import-extract-parent-arguments parens))
    (with-temp-buffer
      (insert content)
      (js-import-remove-comments)
      (goto-char init-p)
      (setq content (buffer-substring-no-properties (point-min) (point-max)))
      (save-excursion
        (while (js-import-previous-declaration-or-skope content)
          (when-let ((token-p (looking-at js-import-delcaration-keywords--re))
                     (token (js-import-which-word)))
            (save-excursion
              (skip-chars-forward token)
              (skip-chars-forward "\s\t\n")
              (when-let* ((word (js-import-which-word))
                          (pos (point)))
                (if (js-import-valid-identifier-p word)
                    (push (js-import-make-index-item word
                                                     :pos pos
                                                     :var-type token
                                                     :real-name word
                                                     :local-name word)
                          ids)
                  (cond
                   ((string= word "{")
                    (let (parent children)
                      (when (looking-at-p "{")
                        (save-excursion
                          (forward-list)
                          (when (looking-at-p "[\s\t\n]*=[\s\t\n]*")
                            (re-search-forward "[\s\t\n]*=[\s\t\n]*" nil t 1)
                            (setq parent (js-import-which-word))))
                        (setq children (js-import-parse-destructive))
                        (setq children (mapcar
                                        (lambda(it)
                                          (js-import-propertize
                                           it 'var-type
                                           token 'parent parent))
                                        children)))
                      (setq ids (append ids children))))))))))))
    ids))

(defun js-import-jump-to-item-other-window(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'pos."
  (unless (js-import-get-prop item 'pos)
    (setq item (js-import-display-to-real-exports item)))
  (when-let ((pos (js-import-get-prop item 'pos))
             (item-path (or (js-import-get-prop item 'real-path)
                            (js-import-path-to-real (js-import-get-prop
                                                     item
                                                     'display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (js-import-jump-to-item-in-buffer item)
    item))

(defun js-import-jump-to-item-persistent(export-item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'pos."
  (let ((item (or (js-import-try-find-definition export-item) export-item))
        (js-buffer (get-buffer-create "*js-import*")))
    (if (and buffer-file-name (string= (js-import-get-prop item 'real-path)
                                       buffer-file-name))
        (js-import-jump-to-item-in-buffer item js-buffer)
      (cl-flet ((preview (item)
                         (when-let ((item-path (or (js-import-get-prop
                                                    item
                                                    'real-path)
                                                   (js-import-path-to-real
                                                    (js-import-get-prop
                                                     item
                                                     'display-path)
                                                    default-directory))))
                           (switch-to-buffer-other-window js-buffer)
                           (setq inhibit-read-only t)
                           (erase-buffer)
                           (js-import-insert-buffer-or-file item-path t)
                           (let ((buffer-file-name item-path))
                             (set-auto-mode))
                           (font-lock-ensure)
                           (setq inhibit-read-only nil)
                           (js-import-jump-to-item-in-buffer item js-buffer))))
        (if (and (helm-attr 'previewp)
                 (string= item (helm-attr 'current-candidate)))
            (progn
              (kill-buffer js-buffer)
              (helm-attrset 'previewp nil))
          (preview item)
          (helm-attrset 'previewp t))))
    item))

(defun js-import-jump-to-item-in-buffer(item &optional buffer)
  "Jumps to ITEM in buffer. ITEM must be propertized with a property `pos'."
  (when-let ((pos (js-import-get-prop item 'pos)))
    (js-import-highlight-word :pos pos :buffer buffer :jump t)
    (recenter-top-bottom)
    item))

(defun js-import-jump-to-definition(&optional real-name)
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (unless real-name (setq real-name (js-import-which-word)))
  (when (js-import-valid-identifier-p real-name)
    (let* ((buffer-items (js-import-search-backward-identifiers))
           (definition
             (js-import-find-by-prop 'real-name real-name buffer-items)))
      (if definition
          (progn
            (goto-char (js-import-get-prop definition 'pos))
            (js-import-highlight-word)))
      (progn
        (when-let* ((item (js-import-find-by-prop
                           'local-name real-name
                           (js-import-extract-imports (buffer-file-name))))
                    (display-path (js-import-get-prop item 'display-path))
                    (dir (or (js-import-dirname (buffer-file-name))
                             default-directory))
                    (real-path (js-import-path-to-real display-path dir))
                    (type (js-import-get-prop item 'type)))
          (setq item (js-import-propertize item 'real-path real-path))
          (if (= type 16)
              (find-file real-path)
            (when-let* ((found-item (js-import-try-find-definition item)))
              (find-file (js-import-get-prop found-item 'real-path))
              (goto-char (js-import-get-prop found-item 'pos))
              (js-import-highlight-word)
              (recenter-top-bottom))))))))

(defun js-import-try-find-definition(item)
  (let* (export-item import-item definition-item)
    (setq export-item
          (or
           (js-import-find-in-exports item (js-import-get-prop item 'real-path))
           item))
    (while (and export-item
                (not (null (js-import-get-prop export-item 'external-path)))
                (not (equal (js-import-get-prop export-item 'external-path)
                            (js-import-get-prop export-item 'real-path))))
      (when (and (js-import-get-prop export-item 'external-name)
                 (string= "default" (js-import-get-prop
                                     export-item 'external-name)))
        (setq export-item (js-import-propertize export-item 'type 1)))
      (when-let ((deep-export (js-import-find-in-exports
                               export-item
                               (js-import-get-prop
                                export-item 'external-path))))
        (setq export-item deep-export)))
    (when export-item
      (if (js-import-get-prop export-item 'var-type)
          export-item
        (let* ((path (or (js-import-get-prop export-item 'external-path)
                         (js-import-get-prop export-item 'real-path)))
               (pos (js-import-get-prop export-item 'pos))
               (name (or (js-import-get-prop export-item 'external-name)
                         (js-import-get-prop export-item 'real-name)))
               (items (with-temp-buffer
                        (save-excursion
                          (js-import-insert-buffer-or-file path)
                          (js-import-remove-comments)
                          (goto-char pos)
                          (js-import-search-backward-identifiers)))))
          (setq definition-item (js-import-find-by-prop 'local-name name items))
          (when definition-item (setq definition-item (js-import-propertize
                                                       definition-item
                                                       'real-path path)))
          (if definition-item
              definition-item
            (progn (let ((imports (js-import-extract-imports path)))
                     (setq import-item (js-import-find-by-prop
                                        'local-name name imports))
                     (when import-item
                       (when-let ((display-path (js-import-get-prop
                                                 import-item 'display-path))
                                  (real-path (js-import-path-to-real
                                              display-path
                                              (js-import-dirname path))))
                         (setq import-item
                               (js-import-propertize
                                import-item 'real-path real-path))
                         (setq definition-item
                               (js-import-try-find-definition import-item))
                         definition-item)))))
          (or definition-item export-item))))))

(defun js-import-find-in-exports(item path &optional default)
  (let* ((exports (or (js-import-extract-esm-exports path)
                      (js-import-extract-cjs-exports path)))
         (type (js-import-get-prop item 'type))
         (external-name (js-import-get-prop item 'external-name))
         (external-default (when external-name (string= external-name
                                                        "default")))
         (id-prop (pcase type
                    (1 'type)
                    (4 (if external-default 'external-name 'real-name))))
         (id-value (pcase type
                     (1 type)
                     (4 (or (js-import-get-prop item 'external-name)
                            (js-import-get-prop item 'real-name))))))
    (if (and id-prop id-value)
        (or (js-import-find-by-prop id-prop id-value exports) default)
      (car exports))))

(defun js-import-build-symbols-source(name items)
  (helm-build-sync-source name
    :candidates items
    :action (lambda(it)
              (when-let* ((item (car (member it items)))
                          (pos (or (js-import-get-prop item 'pos))))
                (goto-char pos)
                (js-import-highlight-word)))))

(defun js-import--print-item(item &optional label)
  (unless (null item)
    (if label
        (message (concat label "\s" (js-import--format-item-props item)))
      (message (js-import--format-item-props item)))))

(defun js-import--format-item-props(item)
  (when item (let ((props '(real-name
                            var-type
                            display-path
                            local-name
                            real-path
                            type
                            pos
                            external-name
                            external-pos
                            external-path))
                   (print-list))
               (mapc (lambda(prop)
                       (when-let ((value (js-import-get-prop item prop)))
                         (push (format "%s: %s" prop value) print-list)))
                     props)
               (setq print-list (reverse print-list))
               (push (propertize item 'face 'font-lock-variable-name-face)
                     print-list)
               (setq print-list (mapconcat 'identity print-list "\n\s"))
               print-list)))

(defun js-import--debug-display-file ()
  (interactive)
  (let* ((files (js-import-find-project-files))
         (dependencies (js-import-node-modules-candidates))
         (choices (append (list buffer-file-name) files dependencies))
         (choice (completing-read "Select a file: " choices)))
    (setq choice (js-import-path-to-real choice))
    (let ((buffer-file-name choice))
      (js-import--display-help-buffer
       (js-import-get-file-content-no-comments choice) choice))))

(defun js-import-get-file-content-no-comments(path)
  "A function inserts content either from buffer or file.
It depends whether buffer with the given PATH exists.
Without argument KEEP-COMMENTS content will inserted without comments."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (if (get-file-buffer path)
          (progn
            (insert-buffer-substring-no-properties (get-file-buffer path)))
        (progn
          (let ((buffer-file-name path))
            (insert-file-contents path))))
      (js-import-remove-comments)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun js-import--display-help-buffer (body &optional feature)
  (let ((buffer (with-current-buffer
                    (get-buffer-create
                     (concat "*js-import" (or feature "") "*"))
                  (setq buffer-read-only t)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (when body
                      (save-excursion
                        (insert body))))
                  (js-import-mode)
                  (local-set-key (kbd "C-c C-p") 'js-import-previous-declaration-or-skope)
                  (local-set-key (kbd "C-c C-n") 'js-import-next-declaration-or-scope)
                  (local-set-key (kbd "q") #'quit-window)
                  (current-buffer))))
    (display-buffer buffer t)
    (if help-window-select
        (progn
          (pop-to-buffer buffer)
          (message "Type \"q\" to restore previous buffer"))
      (message (concat "Type \"q\" in the " feature " buffer to close it")))))

(provide 'js-import)
;;; js-import.el ends here
