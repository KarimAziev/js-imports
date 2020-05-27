;;; js-import.el --- Import for JavaScript files easily -*- lexical-binding: t -*-

;; Copyright (C) 2020 Karim Aziiev <karim.aziev@gmail.com>

;; Author: Karim Aziiev <karim.aziev@gmail.com>
;; URL: https://github.com/KarimAziev/js-import
;; Keywords: conveniences
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

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
(require 'f)
(require 's)
(require 'json)
(require 'js-import-regexp)
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

(defcustom js-import-dependencies-number-limit 200
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

(defconst js-import-enabled-extension-regexp "\\.[jt]s\\(x\\)?$")

(defcustom js-import-preffered-extensions '("d.ts" "ts" "js" "tsx" "jsx")
  "Sorted by priority suffixes for selecting files with the same name but different extension."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-buffer "*helm js import*"
  "Name of `js-import' buffer."
  :group 'js-import
  :type 'string)

(defcustom js-import-ignored-files-regexp '("__tests__"
                                            "[a-zZ-A]+\\.test[s]"
                                            "[a-zZ-A0-9]*\\.#[a-zZ-A0-9/]")
  "Regexp for excluding files."
  :group 'js-import
  :type '(repeat string))


(defcustom js-import-symbols-faces
  '(("^\\(type\\|interface\\)$" . font-lock-type-face)
    ("^\\(function\\|function*\\)$" . font-lock-function-name-face)
    ("^\\(export\\|default\\)$" . font-lock-builtin-face)
    ("^\\(import\\|const\\|let\\|var\\|class\\)$" . font-lock-variable-name-face))
  "Faces in a list of cons cells for showing symbols types in js-symbols-menu."
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

(defface js-import-deletion-face '((t (:background "#e52b50" :foreground "white")))
  "Face used to highlight deleletion"
  :group 'js-import)

(defvar js-import-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-import)
    (define-key map (kbd "C-c C-.") 'js-import-edit-buffer-imports)
    (define-key map (kbd "C-c C-d") 'js-import-dependency)
    (define-key map (kbd "C-c C-a") 'js-import-alias)
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
    (define-key map (kbd "C-c o") 'js-import-find-file-other-window-and-exit)
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
   (js-import-with-files-map "Import from file(s)"    'js-import-marked-files)
   'js-import-marked-files
   (js-import-with-files-map "Find file"              'js-import-find-file-and-exit)
   'js-import-find-file-and-exit
   (js-import-with-files-map "Find file other window" 'js-import-find-file-other-window-and-exit)
   'js-import-find-file-other-window-and-exit
   (js-import-with-files-map "Next alias"             'js-import-switch-to-next-alias)
   'js-import-switch-to-next-alias
   (js-import-with-files-map "Prev alias"             'js-import-switch-to-prev-alias)
   'js-import-switch-to-prev-alias
   (js-import-with-files-map "Relative"               'js-import-switch-to-relative)
   'js-import-switch-to-relative)
  "Default actions for files."
  :group 'js-import
  :type '(alist :key-type string :value-type function))

(defun js-import-with-symbols-map(name command)
  (substitute-command-keys
   (concat name "\s\\<js-import-imported-symbols-map>`\\" "[" (symbol-name command) "]")))

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
   (js-import-with-symbols-map "Jump"                 'js-import-jump-to-item-in-buffer)
   'js-import-jump-to-item-in-buffer
   (js-import-with-symbols-map "Rename"               'js-import-rename-import)
   'js-import-rename-import
   (js-import-with-symbols-map "Add imports"          'js-import-from-path)
   'js-import-from-path
   (js-import-with-symbols-map "Quick delete"         'js-import-delete-persistent)
   'js-import-delete-imported-item
   (js-import-with-symbols-map "Delete whole import"  'js-import-delete-whole-import-persistent)
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

(defvar js-import-dependencies-cache (make-hash-table :test 'equal))
(defvar js-import-dependencies-cache-tick nil)
(defvar js-import-current-alias nil)
(defvar js-import-dependency-source-name "node modules")
(defvar js-import-buffer-source-name "imported in")
(defvar js-import-files-source-name "js-import-files")
(defvar js-import-aliases nil)
(make-variable-buffer-local 'js-import-aliases)
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

(defvar js-import-node-modules-source nil
  "Variable keeps source files from node_modules.")
(defvar js-import-project-files-source nil
  "Variable for source of relative and aliased files without dependencies.")
(defvar js-import-buffer-files-source nil
  "Buffer local variable for source of all imported files in a buffer.")
(make-variable-buffer-local 'js-import-buffer-files-source)

(defcustom js-import-files-source '(js-import-buffer-files-source
                                    js-import-project-files-source
                                    js-import-node-modules-source)
  "Preferred sources for command `js-import'"
  :type '(repeat (choice symbol))
  :group 'js-import)

;;;###autoload
(defun js-import ()
  "Preconfigured helm for selecting files.
Run all sources defined in option `js-import-files-source'."
  (interactive)
  (unless js-import-buffer-files-source
    (setq js-import-buffer-files-source
          (helm-make-source
              js-import-buffer-source-name
              'js-import-source-imported-files)))
  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source
              js-import-files-source-name
              'js-import-source-project-files)))
  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source
              js-import-dependency-source-name
              'js-import-source-node-modules)))
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
   (init :initform 'js-import-init-project-files)
   (candidates :initform 'js-import-find-project-files)
   (candidate-number-limit :initform js-import-files-number-limit)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (filtered-candidate-transformer :initform 'js-import-project-files-transformer)
   (mode-line :initform (list "File(s)"))
   (keymap :initform js-import-files-map)
   (action :initform 'js-import-file-actions)
   (group :initform 'js-import)))

(defun js-import-find-project-root (&optional dir)
  (unless dir (setq dir default-directory))
  (let ((parent (expand-file-name ".." dir)))
    (unless (or (equal parent dir)
                (equal dir "/"))
      (if (file-exists-p (expand-file-name "package.json" dir))
          dir
        (js-import-find-project-root parent)))))

(defun js-import-directory-files(dir &optional recursive)
  "Return files in DIR which match pattern from variable `js-import-enabled-extension-regexp'.
Optional argument RECURSIVE non-nil means to search recursive."
  (if recursive
      (directory-files-recursively dir js-import-enabled-extension-regexp nil)
    (directory-files dir t js-import-enabled-extension-regexp t)))


(defun js-import-files-header-name(_name)
  "A function for display header name for project files."
  (with-helm-current-buffer
    (when-let ((dir (or (js-import-find-project-root) default-directory)))
      (if js-import-current-alias
          (progn
            (concat
             (propertize js-import-current-alias 'face 'font-lock-function-name-face)
             "\s" (f-short dir) "/"
             (plist-get js-import-alias-map js-import-current-alias)))
        (if buffer-file-name
            (format "relative to %s" (replace-regexp-in-string (js-import-slash dir) "" buffer-file-name))
          "relative files")))))

(defun js-import-init-project-files()
  "Init project files."
  (with-current-buffer helm-current-buffer
    (setq js-import-aliases (js-import-get-aliases))
    (when (and js-import-current-alias
               (not (member js-import-current-alias js-import-aliases)))
      (setq js-import-current-alias nil))))

(defun js-import-find-project-files()
  "Return project files without dependencies."
  (let* ((root (js-import-find-project-root))
         (alias-path (and js-import-current-alias
                          (js-import-get-alias-path js-import-current-alias)))
         (priority-dir (if (and alias-path (not (f-parent-of? root alias-path)))
                           alias-path
                         default-directory))
         (dirs (f-directories (or alias-path root) (lambda(it) (not (or (s-contains? "node_modules" it)
                                                                   (s-matches? "/\\.[a-zZ-A]" it))))))
         (files (js-import-directory-files priority-dir t)))
    (mapc (lambda(dir) (setq files (append files (js-import-directory-files dir t))))
          (reverse dirs))
    files))


(defun js-import-get-alias-path(alias &optional project-root)
  (when alias
    (let ((root (or project-root (js-import-find-project-root))))
      (f-join root (plist-get js-import-alias-map alias)))))

(defun js-import-project-files-transformer(files &optional _source)
  "Filter FILES by extension and one of the aliases, if present."
  (with-current-buffer helm-current-buffer
    (let ((alias-path (when js-import-current-alias
                        (f-slash (f-join (js-import-find-project-root)
                                         (plist-get js-import-alias-map js-import-current-alias))))))
      (setq files (seq-uniq files))
      (when alias-path
        (setq files (seq-filter
                     (lambda(filename) (if (f-absolute? filename)
                                      (s-matches-p alias-path filename)
                                    filename))
                     files)))
      (setq files (seq-remove (lambda(filename) (equal buffer-file-name filename)) files))
      (if alias-path (mapcar (lambda(path)
                               (js-import-normalize-path
                                (replace-regexp-in-string alias-path (js-import-slash js-import-current-alias)
                                                          path)))
                             files)
        (mapcar (lambda(path) (js-import-normalize-path (js-import-path-to-relative path))) files)))))


(defun js-import-path-to-relative(path &optional dir)
  "Transform PATH into relative to the DIR (default: ‘default-directory’).
If PATH is a relative file, it will be returned without changes."
  (if (js-import-is-relative? path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-import-is-relative? relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-import-slash(str)
  "Append slash to non-empty STR unless one already."
  (if (or (null str) (s-matches? "/$" str) (s-blank? str))
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
  (if (js-import-is-index-trimmable? path)
      (replace-regexp-in-string js-import-file-index-regexp "" path)
    path))

(defun js-import-remove-ext(path)
  (replace-regexp-in-string js-import-file-ext-regexp "" path))

(defun js-import-is-index-file?(path)
  (s-matches? js-import-file-index-regexp path))

(defun js-import-is-relative?(path)
  (s-matches? "^\\.+/" path))

(defun js-import-is-index-trimmable?(path)
  "Check if PATH index can be trimmed"
  (if (js-import-is-relative? path)
      (and (js-import-is-index-file? path)
           (< 1 (s-count-matches "/" path)))
    (js-import-is-index-file? path)))

(defun js-import-get-aliases ()
  "Get list of aliases."
  (let ((root (js-import-find-project-root))
        (pl js-import-alias-map)
        (vals))
    (while pl
      (when-let* ((alias (car pl))
                  (path (plist-get js-import-alias-map alias))
                  (exists (f-exists? (f-join root path))))
        (push alias vals))
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun js-import-path-to-real(path &optional dir)
  (when (stringp path)
    (setq path (js-import-strip-text-props path))
    (cond ((and (f-ext? path) (f-exists? path)
                (not (js-import-is-relative? path)))
           path)
          ((js-import-is-relative? path)
           (js-import-relative-to-real path dir))
          ((js-import-is-dependency? path (js-import-find-project-root))
           (js-import-maybe-expand-dependency path))
          (t (js-import-alias-path-to-real path)))))

(defun js-import-relative-to-real(path &optional dir)
  (unless dir (setq dir default-directory))
  (or (js-import-try-ext path dir)
      (js-import-try-ext (f-join path "index") dir)))

(defun js-import-alias-path-to-real(path)
  (let (aliases alias real-path)
    (setq aliases (js-import-get-aliases))
    (while aliases
      (setq alias (pop aliases))
      (let* ((alias-regexp (if (s-blank? alias)
                               (concat "^" alias)
                             (concat "^" alias "\\(/\\|$\\)" )))
             (alias-path (js-import-get-alias-path alias))
             (joined-path (f-join alias-path (replace-regexp-in-string alias-regexp "" path)))
             (found-path (if (and (f-ext? joined-path)
                                  (f-exists? joined-path))
                             joined-path
                           (or (js-import-try-ext joined-path)
                               (js-import-try-ext (f-join joined-path "index"))))))
        (when (and found-path (f-exists? found-path))
          (setq real-path found-path)
          (setq aliases nil))))
    real-path))

(defun js-import-try-ext(path &optional dir extensions)
  "A function tries to join into PATH every element from EXTENSIONS
from left to right until first existing file will be found or nil otherwise.

If optional argument DIR is passed, PATH will be firstly expanded as relative to DIR."
  (unless extensions (setq extensions js-import-preffered-extensions))
  (let (ext real-path)
    (while extensions
      (setq ext (pop extensions))
      (setq real-path (if dir
                          (f-expand (f-swap-ext path ext) dir)
                        (f-swap-ext path ext)))
      (if (f-exists? real-path)
          (setq extensions nil)
        (setq real-path nil)))
    real-path))

(defun js-import-switch-to-relative(&optional _cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias nil)
      (setq js-import-current-alias (car js-import-aliases)))
    (helm-refresh)))

(defun js-import-switch-to-next-alias(&optional _cand)
  "Switch to next alias in `js-import-aliases' list."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias (car (cdr (member js-import-current-alias js-import-aliases))))
      (setq js-import-current-alias (car js-import-aliases)))
    (helm-refresh)))

(defun js-import-switch-to-prev-alias(&optional _cand)
  "Switch to previous alias in `js-import-aliases' list."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias (car (cdr (member js-import-current-alias (reverse js-import-aliases)))))
      (setq js-import-current-alias (car (reverse js-import-aliases))))
    (helm-refresh)))


(defclass js-import-source-node-modules (helm-source-sync)
  ((candidates :initform 'js-import-node-modules-candidates)
   (candidate-number-limit :initform js-import-dependencies-number-limit)
   (action :initform 'js-import-file-actions)
   (mode-line :initform (list "Dependencies"))
   (keymap :initform js-import-files-map)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (group :initform 'js-import)))

(defun js-import-node-modules-candidates(&optional project-root)
  "Return dependencies for PROJECT-ROOT from package json."
  (unless project-root (setq project-root (js-import-find-project-root)))
  (unless js-import-dependencies-cache
    (setq js-import-dependencies-cache (make-hash-table :test 'equal)))
  (let* ((package-json-path (f-join project-root "package.json"))
         (tick (file-attribute-modification-time (file-attributes package-json-path 'string)))
         (project-cache (gethash project-root js-import-dependencies-cache)))
    (when (or (not (equal js-import-dependencies-cache-tick tick))
              (not project-cache))
      (let (submodules modules)
        (remhash project-root js-import-dependencies-cache)
        (mapc (lambda(section)
                (when-let ((hash (js-import-read-package-json-section package-json-path section)))
                  (setq modules (append modules (hash-table-keys hash)))))
              js-import-package-json-sections)
        (let* ((max (length modules))
               (progress-reporter
                (make-progress-reporter "Scaning node modules" 0  max)))
          (dotimes (k max)
            (let ((elt (nth k modules)))
              (sit-for 0.01)
              (unless (s-contains? "/" elt)
                (setq submodules (append submodules (js-import-find-interfaces elt))))
              (progress-reporter-update progress-reporter k)))
          (setq modules (append modules submodules))
          (puthash project-root modules js-import-dependencies-cache)
          (setq js-import-dependencies-cache-tick tick)
          (progress-reporter-done progress-reporter))))
    (gethash project-root js-import-dependencies-cache)))

(defun js-import-find-interfaces(display-path)
  (when-let* ((real-path (js-import-expand-node-modules display-path))
              (exists (f-exists-p real-path))
              (files (f-files real-path (lambda(path)
                                          (and (s-matches? ".d.ts$" path)
                                               (not (js-import-is-index-file? path)))))))
    (mapcar (lambda(it) (f-join display-path (f-filename (js-import-remove-ext it)))) files)))

(defun js-import-is-dependency? (display-path &optional project-root)
  "Check if path is dependency"
  (let ((dependencies (js-import-node-modules-candidates project-root))
        (dirname (car (split-string display-path "/"))))
    (or (member dirname dependencies)
        (f-exists? (js-import-expand-node-modules dirname project-root)))))

(defun js-import-get-node-modules-path (&optional project-dir)
  "Return the path to node-modules."
  (if (f-absolute? js-import-node-modules-dir)
      js-import-node-modules-dir
    (f-join (or project-dir
                (js-import-find-project-root))
            js-import-node-modules-dir)))

(defun js-import-expand-node-modules(module &optional project-dir)
  (f-join (js-import-get-node-modules-path project-dir) module))

(defun js-import-maybe-expand-dependency(display-path &optional $real-path)
  (let ((real-path (or $real-path (js-import-expand-node-modules display-path))))
    (unless (f-ext real-path)
      (setq real-path (js-import-try-find-real-path real-path))
      real-path)))

(defun js-import-try-find-real-path(path)
  (if (and (f-ext? path) (f-exists? path))
      path
    (or (when-let* ((package-json (js-import-join-when-exists path "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             js-import-node-modules-priority-section-to-read)))
          (if (f-ext? module)
              (f-expand module path)
            (js-import-try-find-real-path (js-import-try-ext module path))))
        (js-import-try-ext path)
        (js-import-try-ext (f-join path "index"))
        (when-let* ((dir (f-join path "src"))
                    (exists (f-exists? dir))
                    (files (js-import-directory-files dir)))
          (if (= 1 (length files))
              (car files)
            (seq-find (lambda(it) (s-matches? js-import-file-index-regexp it))
                      files))))))

(defun js-import-read-package-json-section (&optional package-json-path section)
  "Reads a SECTION from PACKAGE-JSON-PATH and returns its hash.

Default value for PACKAGE-JSON-PATH is a result of calling `js-import-find-package-json'
and default section is `dependencies'"
  (unless section (setq section "dependencies"))
  (let ((path (or package-json-path (js-import-find-package-json)))
        (json-object-type 'hash-table))
    (when-let ((content (condition-case nil
                            (f-read-text path 'utf-8)
                          (error nil)))
               (hash (condition-case nil
                         (gethash section (json-read-from-string content))
                       (error nil))))
      hash)))

(defun js-import-find-package-json ()
  "Return the path to package.json."
  (f-join (js-import-find-project-root) "package.json"))

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


(defclass js-import-source-imported-files(helm-source-in-buffer)
  ((header-name :initform (lambda(name)
                            (with-helm-current-buffer
                              (if buffer-file-name
                                  (format "%s %s" name (file-name-base buffer-file-name))
                                name))))
   (init :initform 'js-import-find-imported-files-in-buffer)
   (action :initform 'js-import-file-actions)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (mode-line :initform (list "Imports"))
   (keymap :initform js-import-files-map)
   (get-line :initform #'buffer-substring)))

(defun js-import-find-imported-files-in-buffer()
  "Pluck paths from import statements of current buffer and inserts them one by one divided by new line."
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((items (with-helm-current-buffer (buffer-substring-no-properties
                                            (point-min)
                                            (js-import-goto-last-import)))))
      (setq items (s-match-strings-all js-import-import-regexp items))
      (setq items (mapcar (lambda(name) (car (last name))) items))
      (mapc (lambda(name) (insert name) (newline-and-indent)) items))
    (goto-char (point-min))))

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."
  (setq candidate (js-import-path-to-real candidate default-directory))
  (when-let ((buf (get-buffer-create "*helm-js-import*"))
             (valid (and candidate (stringp candidate) (f-exists? candidate))))
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
      (unless (js-import-inside-string-q)
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
  (helm-run-after-exit 'js-import-find-file-other-window (car (helm-marked-candidates))))

;;;###autoload
(defun js-import-find-file(file)
  "Transform FILE to real and open it."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (f-exists? path))
        (find-file path)
      (message "Could't find %s" file))))
(put 'js-import-find-file 'helm-only t)

;;;###autoload
(defun js-import-find-file-other-window(file)
  "Transform FILE to real and open it in other window."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (f-exists? path))
        (find-file-other-window path)
      (message "Could't find %s" file))))
(put 'js-import-find-file-other-window 'helm-only t)

(defun js-import-marked-files(_c)
  "Call command `js-import-from-path' for marked candidates."
  (mapc 'js-import-from-path (helm-marked-candidates)))


;;;###autoload
(defun js-import-from-path(path)
  "Propose to make import from PATH."
  (interactive)
  (with-current-buffer helm-current-buffer
    (when-let ((display-path (or (js-import-get-prop path 'display-path) path)))
      (setq js-import-current-export-path display-path)
      (setq js-import-last-export-path display-path)
      (setq js-import-current-export-real-path
            (js-import-path-to-real display-path default-directory))))
  (let (sources)
    (push (helm-make-source "Imported" 'js-import-source-imported-symbols)
          sources)
    (push (helm-make-source "Exports" 'js-import-source-symbols-in-path)
          sources)
    (helm :sources sources)))


(defclass js-import-source-imported-symbols(helm-source-sync)
  ((candidates :initform 'js-import-imported-candidates-in-buffer)
   (candidate-transformer :initform (lambda(candidates)
                                      (with-helm-current-buffer
                                        (if js-import-current-export-path
                                            (js-import-filter-with-prop 'display-path js-import-current-export-path candidates)
                                          candidates))))
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
   (display-to-real :initform (lambda(it) (with-helm-current-buffer
                                       (seq-find (lambda(elt) (string= elt it))
                                                 js-import-cached-exports-in-buffer it))))
   (persistent-action :initform (lambda(it) (with-helm-current-buffer
                                         (js-import-jump-to-item-in-buffer
                                          (seq-find (lambda(elt) (string= elt it))
                                                    js-import-cached-exports-in-buffer it)))))
   (action :initform 'js-import-jump-to-item-in-buffer)))

(defclass js-import-source-symbols-in-path(helm-source-sync)
  ((header-name :initform (lambda(name) (with-helm-current-buffer
                                     (if js-import-current-export-path
                                         (format "exports in %s" js-import-current-export-path)
                                       "No exports"))))
   (candidates :initform 'js-import-init-exports-candidates)
   (candidate-transformer :initform 'js-import-exported-candidates-transformer)
   (marked-with-props :initform 'withprop)
   (cleanup :initform 'js-import-exports-cleanup)
   (volatile :initform t)
   (action :initform 'js-import-export-items-actions)
   (persistent-action :initform (lambda(c)
                                  (when-let ((props (js-import-display-to-real-exports c)))
                                    (js-import-jump-to-item-persistent props))))))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Show imported symbols from current buffer.

  Available actions includes jumping to item in buffer, renaming, adding more
  imports from current paths and deleting a symbol or whole import."
  (interactive)
  (let (sources)
    (push (helm-make-source "Exports" 'js-import-source-exported-symbols) sources)
    (push (helm-make-source "Imports" 'js-import-source-imported-symbols) sources)
    (helm
     :preselect (or (js-import-which-word) "")
     :sources sources)))


(defun js-import-preselect()
  "Preselect function for file sources."
  (if  (and (> (point-max) (point))
            (stringp (or (js-import-get-path-at-point)
                         js-import-last-export-path)))
      (or (js-import-get-path-at-point)
          js-import-last-export-path) ""))

(defun js-import-reset-all-sources()
  "Reset all files sources."
  (interactive)
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

(defun js-import-jump-to-item-in-buffer(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker."
  (when-let ((m (js-import-get-prop item 'marker)))
    (goto-char m)
    (recenter-top-bottom)
    (helm-highlight-current-line)
    item))

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
          (setq js-import-cached-imports-in-buffer (js-import-extract-imports))
          js-import-cached-imports-in-buffer)))))

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
          (setq js-import-cached-exports-in-buffer (js-import-extract-exports buffer-file-name))
          js-import-cached-exports-in-buffer)))))

(defun js-import-exported-candidates-transformer(candidates)
  "Remove duplicates and imported members from from CANDIDATES plist."
  (with-current-buffer helm-current-buffer
    (let (imports exports)
      (setq imports (js-import-imported-candidates-in-buffer helm-current-buffer))
      (setq imports (js-import-filter-with-prop 'display-path js-import-current-export-path
                                                imports))

      (setq exports (if imports (js-import-filter-exports candidates imports) candidates))
      (setq exports (js-import-strip-duplicates exports))
      (setq exports (mapcar (lambda(c) (js-import-propertize c 'display-path js-import-current-export-path))
                            exports)))))

(defun js-import-filter-with-prop(property value items)
  "Return filtered ITEMS with members whose PROPERTY equals VALUE."
  (seq-filter (lambda(str) (string= (js-import-get-prop str property) value))
              items))

(defun js-import-filter-exports(exports imports)
  "Returns filtered EXPORTS plist with only those members that are not in IMPORTS plist.

   For named exports (with property `type' 4) the test for equality is done by `real-name' and for default export by `type'."

  (seq-remove (lambda(elt) (pcase (js-import-get-prop elt 'type)
                        (1 (seq-find (lambda(imp) (eq 1 (and (js-import-get-prop imp 'type)))) imports))
                        (4 (seq-find (lambda(imp) (string= (js-import-get-prop elt 'real-name)
                                                      (js-import-get-prop imp 'real-name)))
                                     imports))
                        (16 (< 0 (length imports)))))
              exports))

(defun js-import-jump-to-item-persistent(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker."
  (when-let ((m (js-import-get-prop item 'marker))
             (js-buffer "*js-import persistent")
             (item-path (or (js-import-get-prop item 'real-path)
                            (js-import-path-to-real (js-import-get-prop item 'display-path)
                                                    default-directory))))
    (if (and m buffer-file-name (string= item-path buffer-file-name))
        (progn
          (goto-char m)
          (recenter-top-bottom)
          (helm-highlight-current-line))
      (cl-flet ((preview (item-path)
                         (switch-to-buffer-other-window js-buffer)
                         (setq inhibit-read-only t)
                         (erase-buffer)
                         (insert-file-contents item-path)
                         (let ((buffer-file-name item-path))
                           (set-auto-mode))
                         (font-lock-ensure)
                         (setq inhibit-read-only nil)))
        (if (and (helm-attr 'previewp)
                 (string= item-path (helm-attr 'current-candidate)))
            (progn
              (kill-buffer js-buffer)
              (helm-attrset 'previewp nil))
          (preview item-path)
          (helm-attrset 'previewp t)
          (goto-char m)
          (recenter-top-bottom)
          (helm-highlight-current-line))))
    item))

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
         (other-imports (js-import-filter-with-prop 'display-path display-path
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
              (overlay-put overlay 'face 'js-import-deletion-face)
              (when (yes-or-no-p "Delete whole import?")
                (remove-overlays beg end)
                (delete-region beg end)))
          (save-excursion
            (goto-char (js-import-get-prop candidate 'marker))
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
            (overlay-put overlay 'face 'js-import-deletion-face)
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

(defun js-import-insert-import-as(candidate)
  "Insert and renames CANDIDATE into existing or new import statement."
  (let* ((type (js-import-get-prop candidate 'type))
         (normalized-path (js-import-get-prop candidate 'display-path))
         (real-name (js-import-get-prop candidate 'real-name))
         (renamed-name (s-trim (read-string
                                (format "import %s as " real-name))))
         (full-name (concat real-name " as " renamed-name)))
    (pcase type
      (1 (js-import-insert-exports renamed-name nil normalized-path))
      (4 (js-import-insert-exports nil full-name normalized-path))
      (16 (js-import-insert-exports full-name nil normalized-path)))))

(defun js-import-jump-to-item-other-window(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker."
  (when-let ((m (js-import-get-prop item 'marker))
             (item-path (or (js-import-get-prop item 'real-path)
                            (js-import-path-to-real (js-import-get-prop item 'display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (progn
      (goto-char m)
      (recenter-top-bottom)
      (helm-highlight-current-line))
    item))


(defun js-import-insert-buffer-or-file(path)
  "A function inserts content either from buffer or file
depending whether buffer with the given PATH exists.

In both cases the content will be copied without properties"
  (when (and path (f-exists-p path))
    (if (get-file-buffer path)
        (insert-buffer-substring-no-properties (get-file-buffer path))
      (progn
        (let ((buffer-file-name path))
          (insert-file-contents path)
          (set-auto-mode))))))

(defun js-import-insert-exports(default-name named-list path)
  (let ((names (if (stringp named-list)
                   named-list
                 (js-import-join-names named-list))))
    (save-excursion
      (js-import-goto-last-import)
      (cond
       ((js-import-import-backward-exist? path)
        (js-import-add-to-current-imports default-name names))
       (t (insert "import " (js-import-join-imports-names default-name names)
                  " from " js-import-quote path js-import-quote ";\n"))))))

(defun js-import-add-to-brackets(names)
  (re-search-forward "\\({[^}]+\\([a-zA-Z0-9]*\\)\\)"  nil t)
  (skip-chars-backward " \t\n")
  (if (string= "," (string (char-before)))
      (insert " " names)
    (insert ", " names)))

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

(cl-defun js-import-make-index-item (candidate
                                     &key
                                     type
                                     display-path
                                     var-type
                                     real-path
                                     real-name
                                     marker)
  "Utility function to propertize js symbol.
See also function `js-import-propertize'."
  (setq candidate (js-import-strip-text-props candidate))
  (js-import-propertize candidate
                        'real-name real-name
                        'display-path display-path
                        'real-path real-path
                        'type type
                        'var-type var-type
                        'marker marker))

(defun js-import-init-exports-candidates()
  "Extract exports from file specified in a variable `js-import-current-export-path.'"
  (with-current-buffer helm-current-buffer
    (let ((default-candidates (list (js-import-make-index-item
                                     "* as"
                                     :type 16))))
      (if js-import-current-export-real-path
          (when-let* ((path js-import-current-export-real-path)
                      (str (stringp path)))
            (setq js-import-export-candidates-in-path (append default-candidates
                                                              (or (js-import-extract-exports path)
                                                                  (list (js-import-make-index-item
                                                                         "default"
                                                                         :type 1))))))
        (append default-candidates (list (js-import-make-index-item
                                          "default"
                                          :type 1)))))))


(defun js-import-extract-exports(&optional path)
  "Return list of exported symbols in PATH or nil otherwise."
  (with-temp-buffer
    (erase-buffer)
    (js-import-insert-buffer-or-file path)
    (save-excursion
      (goto-char (point-min))
      (let (symbols)
        (with-syntax-table js-import-mode-syntax-table
          (while (re-search-forward js-import-regexp-export-keyword nil t 1)
            (let (display-path exports)
              (unless (js-import-inside-comment?)
                (save-excursion
                  (if (looking-at-p js-import-js-vars)
                      (setq display-path path)
                    (progn (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
                           (setq display-path (js-import-get-path-at-point)))))
                (cond ((looking-at-p "*[ \s\t\n]+as[ \s\t\n]")
                       (when-let ((export-all (js-import-extract-all-as-exports path display-path)))
                         (push export-all exports)))
                      ((looking-at-p "{")
                       (when-let ((items (js-import-extract-exports-in-brackets path display-path)))
                         (setq exports (append exports items)))
                       (re-search-forward "}"))
                      ((looking-at-p js-import-regexp-name-set)
                       (when-let ((name (js-import-extract-named-or-default-export path display-path)))
                         (push name exports)))
                      ((looking-at-p "*[ \s\t]from")
                       (when-let* ((curr-dir (f-dirname path))
                                   (next-path (js-import-path-to-real display-path curr-dir)))
                         (setq symbols (append symbols (js-import-extract-exports next-path))))))
                (setq symbols (append symbols exports))))
            (forward-line 1)))
        symbols))))

(defun js-import-extract-all-as-exports(real-path &optional display-path)
  "Make export all as item."
  (re-search-forward "as[ \s\t\n]+\\([_$A-Za-z0-9]\\)" nil t 1)
  (let ((real-name (js-import-which-word)))
    (js-import-make-index-item
     real-name
     :type 4
     :real-name real-name
     :var-type "export"
     :marker (point)
     :real-path real-path
     :display-path display-path)))

(defun js-import-extract-named-or-default-export(real-path &optional display-path)
  "Returns propertizied named or default export."
  (let (var-type real-name stop default?)
    (setq var-type (js-import-which-word))
    (when (string= var-type "default")
      (setq default? t))
    (while (and (js-import-word-reserved? (js-import-which-word))
                (not stop))
      (skip-chars-forward js-import-regexp-name)
      (skip-chars-forward "\s\t*")
      (if (js-import-invalid-name? (js-import-which-word))
          (setq stop t)
        (setq real-name (js-import-which-word))))
    (when (and default?
               (or (not real-name)
                   (js-import-invalid-name? real-name)
                   (js-import-word-reserved? real-name)))
      (setq real-name var-type))
    (when real-name
      (js-import-make-index-item
       real-name
       :type (if default? 1 4)
       :real-name real-name
       :real-path real-path
       :var-type var-type
       :marker (point)
       :display-path display-path))))

(defun js-import-extract-exports-in-brackets(real-path &optional display-path)
  "Extracts exports beetween brackets."
  (let (p1 p2 item items real-name full-name)
    (setq p1 (1+ (point)))
    (save-excursion (re-search-forward "}" nil t 1)
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
          (setq p1 (point))
          (setq real-name (js-import-which-word))
          (skip-chars-forward js-import-regexp-name)
          (setq p2 (point))))
      (setq full-name (string-trim (buffer-substring-no-properties p1 p2)))
      (setq item (js-import-make-index-item
                  full-name
                  :type 4
                  :real-name real-name
                  :display-path display-path
                  :real-path real-path
                  :marker p1))
      (push item items))
    (widen)
    (reverse items)))

(defun js-import-extract-imports()
  (save-excursion
    (goto-char 0)
    (let (symbols)
      (with-syntax-table js-import-mode-syntax-table
        (while (re-search-forward js-import-regexp-import-keyword nil t 1)
          (unless (js-import-inside-comment?)
            (let (path imports)
              (save-excursion
                (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
                (setq path (js-import-get-path-at-point)))
              (cond ((looking-at-p "*")
                     (let (m1 m2 renamed-name)
                       (setq m1 (point))
                       (forward-char)
                       (skip-chars-forward "\s\t")
                       (setq m2 (point))
                       (when (string= "as" (js-import-which-word))
                         (skip-chars-forward "as")
                         (setq m2 (point))
                         (skip-chars-forward "\s\t")
                         (setq renamed-name (js-import-which-word))
                         (if (or (js-import-word-reserved? renamed-name)
                                 (js-import-invalid-name? renamed-name))
                             (setq renamed-name "")
                           (skip-chars-forward js-import-regexp-name)))
                       (setq m2 (point))
                       (push (js-import-make-index-item
                              (format "%s" (buffer-substring-no-properties m1 m2))
                              :type 16
                              :real-name renamed-name
                              :marker m1
                              :display-path path)
                             imports)
                       (skip-chars-forward js-import-regexp-name-with-separators)))
                    ((looking-at-p js-import-regexp-name-set)
                     (push (js-import-make-index-item
                            (js-import-which-word)
                            :type 1
                            :real-name (js-import-which-word)
                            :marker (point)
                            :display-path path)
                           imports)
                     (skip-chars-forward js-import-regexp-name-with-separators)))
              (when (looking-at-p "{")
                (setq imports (append imports (js-import-extract-imports-in-brackets path))))
              (setq symbols (append symbols imports))))
          (forward-line 1)))
      symbols)))


(defun js-import-extract-imports-in-brackets(display-path &optional real-path)
  "Extracts items beetween symbols."
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
             :real-name real-name
             :display-path display-path
             :real-path real-path
             :marker p1)
            items))
    (widen)
    (reverse items)))

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
    (s-join ", " (seq-remove (lambda(it) (null it)) parts))))

(defun js-import-goto-last-import()
  (goto-char (point-min))
  (while (re-search-forward js-import-regexp-import-keyword nil t)
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
        (re-search-backward js-import-regexp-import-keyword nil t)
        (setq pos1 (point)))
      (cons pos1 pos2))))

(defun js-import-delete-whole-import(candidate)
  "Remove CANDIDATE's import statement.
CANDIDATE must be properizied with prop `display-path'"
  (when-let* ((path (js-import-get-prop candidate 'display-path))
              (bounds (js-import-get-import-positions path)))
    (delete-region (car bounds) (cdr bounds))
    (join-line)))


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

(defun js-import-which-word (&optional regexp)
  "Find closest to point whole word."
  (interactive)
  (unless regexp (setq regexp js-import-regexp-name))
  (save-excursion
    (let (p1 p2)
      (if (use-region-p)
          (progn
            (setq p1 (region-beginning))
            (setq p2 (region-end)))
        (save-excursion
          (skip-chars-backward regexp)
          (setq p1 (point))
          (right-char)
          (skip-chars-forward regexp)
          (setq p2 (point))))
      (setq mark-active nil)
      (when (< p1 (point))
        (goto-char p1))
      (buffer-substring-no-properties p1 p2))))

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
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (let (p0 p1 p2 stops)
          (setq stops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\")
          (setq p0 (point))
          (skip-chars-backward stops)
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward stops)
          (setq p2 (point))
          (goto-char p0)
          (buffer-substring-no-properties p1 p2))))))

(defun js-import-inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (nth 3 (syntax-ppss)))

(defun js-import-inside-comment? ()
  "Returns value of comment character in syntax table's or nil otherwise"
  (interactive)
  (nth 4 (syntax-ppss)))

(defun js-import-strip-duplicates (list)
  (let (new-list)
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

(defun js-import-rename-default-item(item)
  "Renames default imported item "
  (let (real-name new-name overlay end beg)
    (setq real-name (or (js-import-get-prop item 'real-name)
                        (js-import-strip-text-props item)))
    (setq beg (js-import-get-prop item 'marker))
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
  (let* ((marker (js-import-get-prop item 'marker))
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
              (query-replace-regexp (concat "\\_<" renamed-name "\\_>") new-name)))
        (progn
          (insert (format " as %s" new-name))
          (query-replace-regexp (concat "\\_<" real-name "\\_>") new-name))))))

(defun js-import-invalid-name?(str)
  "Validates STR by matching any characters which are not allowed for variable name."
  (s-matches? (concat "[" "^" js-import-regexp-name "]") str))

(defun js-import-generate-name-from-path(path)
  "Generate name for default or module import from PATH"
  (let ((parts (js-import-compose-from
                path
                'reverse
                's-split-words
                'js-import-maybe-remove-path-index
                'js-import-remove-ext)))

    (setq parts (seq-take parts 2))
    (mapconcat 'capitalize parts "")))

(defun js-import-propose-name (candidate)
(let* ((parts (split-string candidate))
       (type (js-import-get-prop candidate 'type))
       (current-name (car parts))
       (display-path (js-import-get-prop candidate 'display-path))
       (proposed-symbol (pcase type
                          (1 (if (or (js-import-word-reserved? candidate)
                                     (js-import-invalid-name? candidate))
                                 (js-import-generate-name-from-path display-path)
                               candidate))
                          (4 (js-import-generate-name-from-path display-path))
                          (16 (js-import-generate-name-from-path display-path))))
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

(defun js-import-compose-from(arg &rest funcs)
  "Performs right-to-left unary function composition."
  (seq-reduce (lambda (xs fn)
                (funcall fn xs))
              (reverse funcs) arg))

(provide 'js-import)
;;; js-import.el ends here
