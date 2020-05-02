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
(require 'subr-x)
(require 'cl-lib)
(require 'f)
(require 'json)
(require 's)
(require 'js-import-regexp)


(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import
  :group 'languages)

(defcustom js-import-alias-map '("" "src")
  "List of pairs (alias and path)"
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-quote "'"
  "Quote type."
  :group 'js-import
  :type '(choice (const :tag "Double" "\"")
                 (const :tag "Single" "\\'")))

(defcustom js-import-files-number-limit 50
  "The limit for number of project files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

(defcustom js-import-dependencies-number-limit 200
  "The limit for number of dependencies files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

(defcustom js-import-preffered-extensions '("d.ts" "ts" "js" "tsx" "jsx")
  "Sorted by priority list of stringed suffixes for selecting files with the same name but diffent extension"
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-symbols-faces
  '(("^\\(type\\|interface\\)$" . font-lock-type-face)
    ("^\\(function\\|function*\\)$" . font-lock-function-name-face)
    ("^\\(export\\|default\\)$" . font-lock-builtin-face)
    ("^\\(import\\|const\\|let\\|var\\|class\\)$" . font-lock-variable-name-face))
  "Faces in a list of cons cells for showing symbols types in js-symbols-menu"
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))


(defcustom js-import-node-modules-dir "node_modules"
  "Node modules path"
  :group 'js-import
  :type 'string)

(defcustom js-import-buffer "*js import*"
  "Name of js-import buffer"
  :group 'js-import
  :type 'string)


(defvar js-import-command-map
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
  "Keymap for Js-import commands")

;;;###autoload
(define-minor-mode js-import-mode
  "js-import-mode is a minor mode for importing.
\\{js-import-mode-map}"
  :lighter " js-import"
  :group 'js-import
  :global nil
  :keymap js-import-command-map)

(defmacro js-import-exit-and-run (func)
  "Exit and run FUNC"
  (declare (indent 2) (debug t))
  `(lambda ()
     (interactive)
     (helm-exit-and-execute-action ,func)))

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

(defvar js-import-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    (define-key map (kbd "<C-return>") 'js-import-find-file-and-exit)
    (define-key map (kbd "C-c C-o") 'js-import-find-file-other-window-and-exit)
    map)
  "Keymap for files sources in Helm.")


(defvar js-import-imported-symbols-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-d") 'js-import-delete-persistent)
    map)

  "Keymap for symdol sources.")

(put 'js-import-imported-symbols-keymap 'helm-only t)


(defvar js-import-dependencies-cache (make-hash-table :test 'equal))
(defvar js-import-dependencies-cache-tick nil)

(defvar js-import-current-alias nil)

(defvar js-import-aliases nil)
(make-variable-buffer-local 'js-import-aliases)
(defvar js-import-relative-transformer nil)
(make-variable-buffer-local 'js-import-relative-transformer)

(defvar js-import-dependencies-cache-plist nil)

(defvar js-import-dependency-source-name "node modules")
(defvar js-import-buffer-source-name "imported in buffer")

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

(defvar js-import-last-export-path nil)
(make-variable-buffer-local 'js-import-last-export-path)
(defvar js-import-node-modules-source nil)
(defvar js-import-project-files-source nil)
(defvar js-import-buffer-files-source nil)

(make-variable-buffer-local 'js-import-buffer-files-source)

(defvar js-import-files-actions
  (helm-make-actions "Import from file" (js-import-with-marked-candidates 'js-import-from-path)
                     (substitute-command-keys "Find file \\<js-import-files-map>`\\[js-import-find-file-and-exit]") 'js-import-find-file-and-exit
                     (substitute-command-keys "Open file \\<js-import-files-map>`\\[js-import-find-file-other-window-and-exit]'") 'js-import-find-file-other-window-and-exit
                     (substitute-command-keys "Next alias \\<js-import-files-map>`\\[js-import-switch-to-next-alias]'") 'js-import-switch-to-next-alias
                     (substitute-command-keys "Prev alias \\<js-import-files-map>`\\[js-import-switch-to-prev-alias]'") 'js-import-switch-to-prev-alias
                     (substitute-command-keys "Relative \\<js-import-files-map>`\\[js-import-switch-to-relative]'") 'js-import-switch-to-relative)

  "File actions")

(defvar js-import-imported-items-actions
  (helm-make-actions
   "Go" 'js-import-jump-to-item-in-buffer
   "Rename" (js-import-with-marked-candidates 'js-import-rename-import)
   "Add more imports" (js-import-call-with-marked-candidates-prop 'js-import-from-path 'display-path)
   "Delete" 'js-import-delete-imported-item
   "Delete whole import" (js-import-call-with-marked-candidates-prop 'js-import-delete-whole-import 'display-path))
  "Actions for editing imported symbols in buffer")

(defvar js-import-export-items-actions
  (helm-make-actions
   "Import" (js-import-with-marked-candidates 'js-import-insert-import)
   "Import as " (js-import-with-marked-candidates 'js-import-insert-import-as)
   "Go" 'js-import-jump-to-item-other-window)
  "Actions for generating and inserting imports statements")


(defun js-import-preselect()
  "Preselect function"
  (if  (and (> (point-max) (point))
            (stringp (or (js-import-get-path-at-point)
                         js-import-last-export-path)))
      (or (js-import-get-path-at-point)
          js-import-last-export-path)
    ""))



;;;###autoload
(defun js-import ()
  "Init imports from your current project"
  (interactive)

  (unless js-import-buffer-files-source
    (setq js-import-buffer-files-source
          (helm-make-source js-import-buffer-source-name 'js-import-imported-files-source)))

  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source "js files" 'js-import-alias-source)))

  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))

  (save-excursion
    (helm
     :sources '(js-import-buffer-files-source
                js-import-project-files-source
                js-import-node-modules-source)
     :buffer js-import-buffer
     :preselect (js-import-preselect)
     :prompt "Select a file: ")))


;;;###autoload
(defun js-import-alias ()
  "Import from project files without dependencies."
  (interactive)

  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source "js files" 'js-import-alias-source)))

  (save-excursion
    (helm
     :sources 'js-import-project-files-source
     :buffer js-import-buffer
     :preselect (js-import-preselect)
     :prompt "Select a file: ")))


;;;###autoload
(defun js-import-dependency (&optional dependency)
  "Import from node modules"
  (interactive)
  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))

  (save-excursion
    (when-let ((module (or dependency
                           (helm :sources js-import-node-modules-source
                                 :preselect (js-import-preselect)))))
      (js-import-from-path module))))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Show imported symbols from current buffer.

  Available actions includes jumping to item in buffer, renaming, adding more
  imports from current paths and deleting a symbol or whole import."
  (interactive)
  (helm :sources (helm-make-source "*js symbols*" 'js-import-buffer-imports-source)))


(defun js-import-from-path(path)
  "Extract and propose to make import from PATH "
  (interactive)
  (with-current-buffer helm-current-buffer
    (setq js-import-current-export-path path)
    (setq js-import-last-export-path path)
    (setq js-import-current-export-real-path (js-import-path-to-real path default-directory)))

  (helm :sources (append (list (helm-make-source "js-exports " 'js-import-exports-source)
                               (helm-make-source "imported" 'js-import-buffer-imports-source)))))



(defclass js-import-imported-files-source(helm-source-in-buffer)
  ((init :initform 'js-import-find-imported-files-in-buffer)
   (action :initform 'js-import-files-actions)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (mode-line :initform (list "Imports"))
   (keymap :initform js-import-files-map)
   (get-line :initform #'buffer-substring)))


(defclass js-import-alias-source (helm-source-sync)
  ((header-name :initform 'js-import-files-header-name)
   (init :initform 'js-import-alias-init)
   (candidates :initform 'projectile-current-project-files)
   (candidate-number-limit :initform js-import-files-number-limit)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (candidate-transformer :initform (lambda(files)
                                      (with-current-buffer helm-current-buffer
                                        (let ((alias-path (plist-get js-import-alias-map js-import-current-alias)))
                                          (js-import-filter-files files alias-path)))))
   (filter-one-by-one :initform (lambda(path)
                                  (with-current-buffer helm-current-buffer
                                    (js-import-transform-path-one-by-one path js-import-current-alias))))
   (mode-line :initform (list "File(s)"))
   (keymap :initform js-import-files-map)
   (action :initform 'js-import-files-actions)
   (group :initform 'js-import)))


(defun js-import-node-modules-candidates(&optional $project-root)
  "Returns list of dependencies"
  (unless js-import-dependencies-cache (setq js-import-dependencies-cache (make-hash-table :test 'equal)))
  (let* ((project-root (or $project-root (projectile-project-root)))
         (package-json-path (f-join project-root "package.json"))
         (tick (file-attribute-modification-time (file-attributes package-json-path 'string)))
         (project-cache (gethash project-root js-import-dependencies-cache))
         (sections '("dependencies" "devDependencies")))

    (when (or (not (equal js-import-dependencies-cache-tick tick))
              (not project-cache))
      (let (submodules modules)
        (remhash project-root js-import-dependencies-cache)
        (mapc (lambda(section) (when-let ((hash (js-import-read-package-json-section package-json-path section)))
                            (setq modules (append modules (hash-table-keys hash)))))
              sections)

        (let* ((max (length modules))
               (progress-reporter
                (make-progress-reporter "Scaning node modules" 0  max)))
          (dotimes (k max)
            (let ((elt (nth k modules)))
              (sit-for 0.01)
              (unless (s-contains? "/" elt) (setq submodules (append submodules (js-import-find-interfaces elt))))
              (progress-reporter-update progress-reporter k)))

          (setq modules (append modules submodules))

          (puthash project-root modules js-import-dependencies-cache)
          (setq js-import-dependencies-cache-tick tick)
          (progress-reporter-done progress-reporter))))

    (gethash project-root js-import-dependencies-cache)))

(defclass js-import-dependency-source (helm-source-sync)
  ((candidates :initform 'js-import-node-modules-candidates)
   (candidate-number-limit :initform js-import-dependencies-number-limit)
   (action :initform 'js-import-files-actions)
   (mode-line :initform (list "Dependencies"))
   (keymap :initform js-import-files-map)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (group :initform 'js-import)))


(defclass js-import-buffer-imports-source(helm-source-sync)
  ((candidates :initform 'js-import-imported-candidates-in-buffer)
   (candidate-transformer :initform (lambda(candidates) (with-helm-current-buffer
                                                     (if js-import-current-export-path
                                                         (js-import-filter-plist 'display-path
                                                                                 (equal js-import-current-export-path it)
                                                                                 candidates)
                                                       candidates))))
   (marked-with-props :initform 'withprop)
   (persistent-help :initform "Show symbol")
   (display-to-real :initform 'js-import-display-to-real-imports)
   (keymap :initform js-import-imported-symbols-keymap)
   (persistent-action :initform (lambda(c) (js-import-jump-to-item-in-buffer
                                       (js-import-display-to-real-imports c))))
   (action :initform 'js-import-imported-items-actions)))


(defclass js-import-exports-source(helm-source-sync)
  ((candidates :initform 'js-import-init-exports-candidates)
   (cleanup :initform 'js-import-exports-cleanup)
   (volatile :initform t)
   (header-name :initform (lambda(name) (with-helm-current-buffer
                                     (if js-import-current-export-path
                                         (format "exports in %s" js-import-current-export-path)
                                       "No exports"))))
   (marked-with-props :initform 'withprop)
   (persistent-action :initform (lambda(c) (js-import-jump-to-item-persistent
                                       (js-import-display-to-real-exports c))))
   (action :initform 'js-import-export-items-actions)
   (candidate-transformer :initform 'js-import-exported-candidates-transformer)))


(defun js-import-exports-cleanup()
  "Reset filter for imported candidates"
  (with-helm-current-buffer
    (setq js-import-current-export-path nil)
    (setq js-import-current-export-real-path nil)))

(defun js-import-display-to-real-exports(it)
  "Search for export item"
  (with-helm-current-buffer
    (seq-find (lambda(elt) (string= (js-import-get-prop elt 'real-name) it)) js-import-export-candidates-in-path)))

(defun js-import-display-to-real-imports(it)
  "Search for texport item"
  (with-helm-current-buffer
    (let (import-item)
      (setq import-item (seq-find (lambda(elt) (string= elt it)) js-import-cached-imports-in-buffer it))
      import-item)))

(defun js-import-jump-to-item-in-buffer(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker"
  (when-let ((m (js-import-get-prop item 'marker)))
    (goto-char m)
    (recenter-top-bottom)
    (helm-highlight-current-line)
    item))

(defun js-import-imported-candidates-in-buffer(&optional buffer)
  "Returns imported symbols in BUFFER. Symbols are cached and are stored in a buffer local variable `js-import-cached-imports-in-buffer'.
   Cache are invalidated when `buffer-modified-tick' changes."
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-import-cached-imports-in-buffer-tick tick)
          js-import-cached-imports-in-buffer
        (progn
          (setq js-import-cached-imports-in-buffer-tick tick)
          (setq js-import-cached-imports-in-buffer (save-excursion
                                                     (with-syntax-table (syntax-table)
                                                       (js-import-extracts-imports))))
          js-import-cached-imports-in-buffer)))))


(defun js-import-exported-candidates-transformer(candidates)
  "Removes duplicates and imported members from from CANDIDATES plist."
  (with-current-buffer helm-current-buffer
    (let (imports exports)
      (setq imports (js-import-imported-candidates-in-buffer helm-current-buffer))
      (setq imports (js-import-filter-plist 'display-path (string= js-import-current-export-path it) imports))

      (setq exports (if imports (js-import-filter-exports candidates imports) candidates))
      (setq exports (js-import-strip-duplicates exports))
      (setq exports (mapcar (lambda(c) (js-import-propertize c 'display-path js-import-current-export-path)) exports)))))



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


(defun js-import-find-imported-files-in-buffer()
  "Plucks paths from import statements of current buffer and inserts them one by one divided by new line."
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((items (with-helm-current-buffer (buffer-substring-no-properties
                                            (point-min)
                                            (js-import-goto-last-import)))))
      (setq items (mapcar (lambda(name) (car (last name))) (s-match-strings-all js-import-import-regexp items)))
      (mapc (lambda(name) (insert name) (newline-and-indent)) items))
    (goto-char (point-min))))

(defun js-import-alias-init()
  "Init project files."
  (with-current-buffer helm-current-buffer
    (setq js-import-aliases (js-import-get-aliases))
    (when (and js-import-current-alias (not (member js-import-current-alias js-import-aliases)))
      (setq js-import-current-alias nil))
    (unless js-import-relative-transformer
      (setq js-import-relative-transformer 'js-import-relative-one-by-one))))

(defun js-import-relative-one-by-one(path)
  "Transform relative to `projectile-project-root' PATH into relative to the current `buffer-file-name'"
  (if (s-matches? "^\\.+/" path)
      path
    (let ((relative-path (f-relative (f-join (projectile-project-root) path) default-directory)))
      (unless (s-matches? "^\\.+/" relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))


(defun js-import-slash(str)
  "Append slash to non-empty STR unless one already."
  (if (or (s-matches? "/$" str) (s-blank? str))
      str
    (concat str "/")))

(defun js-import-transform-to-alias(filepath alias)
  "Project"
  (let ((alias-path (plist-get js-import-alias-map js-import-current-alias)))
    (s-replace-regexp (concat "^" alias-path "/") (js-import-slash alias) filepath)))

(defun js-import-transform-path-one-by-one(path &optional alias)
  "Transform relative to the project root PATH into aliased one or relative to the current `buffer-file-name'"
  (if alias
      (js-import-normalize-path (js-import-transform-to-alias path alias))
    (js-import-normalize-path (js-import-relative-one-by-one path))))


(defun js-import-switch-to-relative(&optional _cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias nil)
      (setq js-import-current-alias (car js-import-aliases)))
    (helm-refresh)))


(defun js-import-switch-to-next-alias(&optional _cand)
  "Switch to next alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias (car (cdr (member js-import-current-alias js-import-aliases))))
      (setq js-import-current-alias (car js-import-aliases)))
    (helm-refresh)))


(defun js-import-switch-to-prev-alias(&optional _cand)
  "Switch to previous alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-current-alias
        (setq js-import-current-alias (car (cdr (member js-import-current-alias (reverse js-import-aliases)))))
      (setq js-import-current-alias (car (reverse js-import-aliases))))
    (helm-refresh)))

(defun js-import-alias-pattern-transformer(pattern)
  "Expand PATTERN if it starts with relative prefix"
  (with-current-buffer helm-current-buffer
    (cond ((s-matches? "^\\.+/" pattern)
           (s-replace-regexp (projectile-project-root) "" (f-expand pattern default-directory)))
          (t (or pattern "")))))

(defun js-import-make-alias-pattern(alias)
  "Transform alias to path"
  (when-let ((path (lax-plist-get js-import-alias-map alias)))
    (unless (and (s-matches? "/$" path) )
      (setq path (concat "" path "/"))
      (concat path))))


(defun js-import-files-header-name(_name)
  "A function for display header name for project files. Concatenates `js-import-current-alias' and PROJECT-NAME"
  (with-helm-current-buffer
    (if js-import-current-alias
        (progn
          (concat
           (propertize js-import-current-alias 'face 'font-lock-function-name-face)
           "\s" (projectile-project-name) "/"
           (plist-get js-import-alias-map js-import-current-alias)))
      (format "Relative files"))))


(defun js-import-find-file-at-point()
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (when-let ((path (js-import-get-path-at-point)))
    (js-import-find-file path)))

(defun js-import-find-file-and-exit(&optional _file)
  "Transform FILE to real and open it"
  (interactive)
  (helm-run-after-exit 'js-import-find-file (car (helm-marked-candidates))))

(defun js-import-find-file-other-window-and-exit()
  "Transform FILE to real and open it"
  (interactive)
  (helm-run-after-exit 'js-import-find-file-other-window (car (helm-marked-candidates))))

;;;###autoload
(defun js-import-find-file(file)
  "Transform FILE to real and open it"
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (f-exists? path))
        (find-file path)
      (message "Could't find %s" file))))
(put 'js-import-find-file 'helm-only t)

;;;###autoload
(defun js-import-find-file-other-window(file)
  "Transform FILE to real and open it in other window"
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (f-exists? path))
        (find-file-other-window path)
      (message "Could't find %s" file))))
(put 'js-import-find-file-other-window 'helm-only t)

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."
  (setq candidate (js-import-path-to-real candidate default-directory))
  (when-let ((buf (get-buffer-create " *js-import persistent*"))
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


(defun js-import-jump-to-item-persistent(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker"
  (when-let ((m (js-import-get-prop item 'marker))
             (js-buffer "*js-import persistent")
             (item-path (or (js-import-get-prop item 'real-path)
                            (js-import-path-to-real (js-import-get-prop item 'display-path) default-directory))))
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



(defun js-import-delete-persistent (&optional _cand)
  "Persistent action for quick delete CAND from import statement"
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'js-import-delete-imported-item '(js-import-delete-imported-item . never-split))
    (helm-execute-persistent-action 'js-import-delete-imported-item)
    (js-import-init-exports-candidates)
    (helm-refresh)))

(defun js-import-delete-imported-item(candidate)
  "Remove CANDIDATE from import statement in buffer."
  (let ((type (js-import-get-prop candidate 'type))
        (fullname (js-import-get-prop candidate 'display-name))
        (display-path (js-import-get-prop candidate 'display-path)))
    (if (equal type 16)
        (js-import-delete-whole-import display-path)
      (js-import-delete-imported-name fullname display-path))))


(defun js-import-rename-import(candidate)
  "Rename imported CANDIDATE in buffer."
  (interactive)
  (print candidate)
  (save-excursion
    (save-restriction
      (pcase (js-import-get-prop candidate 'type)
        (1 (js-import-rename-default-item candidate))
        (4 (js-import-rename-as candidate))
        (16 (js-import-rename-as candidate))))))

(defun js-import-insert-import(candidate)
  "Inserts CANDIDATE into existing or new import statement."
  (save-excursion
    (let ((type (js-import-get-prop candidate 'type))
          (name (js-import-get-prop candidate 'display-name))
          (renamed-name (js-import-get-prop candidate 'display-name))
          (normalized-path (js-import-get-prop candidate 'display-path)))
      (pcase type
        (1
         (js-import-insert-exports
          (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))
        (4
         (js-import-insert-exports nil name normalized-path))
        (16
         (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))))))

(defun js-import-insert-import-as(candidate)
  "Inserts and renames CANDIDATE into existing or new import statement."
  (let* ((type (js-import-get-prop candidate 'type))
         (normalized-path (js-import-get-prop candidate 'display-path))
         (real-name (js-import-get-prop candidate 'real-name))
         (renamed-name (s-trim (read-string
                                (format "import %s as " real-name)
                                nil nil)))
         (full-name (concat real-name " as " renamed-name)))
    (pcase type
      (1 (js-import-insert-exports renamed-name nil normalized-path))
      (4 (js-import-insert-exports nil full-name normalized-path))
      (16 (js-import-insert-exports full-name nil normalized-path)))))

(defun js-import-jump-to-item-other-window(item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop 'marker"
  (when-let ((m (js-import-get-prop item 'marker))
             (item-path (or (js-import-get-prop item 'real-path) (js-import-path-to-real (js-import-get-prop item 'display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (progn
      (goto-char m)
      (recenter-top-bottom)
      (helm-highlight-current-line))
    item))


(defun js-import-init-exports-candidates()
  "Extracts all exports from file specified in buffer local variable 'js-import-current-export-path"
  (with-current-buffer helm-current-buffer
    (let ((default-candidates (list (js-import-make-index-item "export all" :type 16))))
      (if js-import-current-export-real-path
          (progn (when-let ((path js-import-current-export-real-path)
                            (syntax (syntax-table))
                            (str (stringp path)))

                   (setq js-import-export-candidates-in-path (with-temp-buffer
                                                               (erase-buffer)
                                                               (with-syntax-table syntax
                                                                 (js-import-insert-buffer-or-file path)
                                                                 (append default-candidates (js-import-extracts-exports path)))))))
        default-candidates))))


(defun js-import-insert-buffer-or-file(path)
  "A function inserts content either from buffer or file depending whether buffer with the given PATH exists. In both cases the content will be copied without properties"
  (if (get-file-buffer path)
      (insert-buffer-substring-no-properties (get-file-buffer path))
    (insert-file-contents path)))


(defun js-import-get-aliases ()
  "Get list of aliases"
  (let ((pl js-import-alias-map)
        (vals  ()))
    (while pl
      (push (car pl) vals)
      (setq pl  (cddr pl)))
    (nreverse vals)))

(defun js-import-path-to-real(path &optional dir)
  (when (stringp path)
    (setq path (js-import-strip-text-props path))
    (cond ((and (f-ext? path) (f-exists? path)
                (not (js-import-is-relative? path)))
           path)
          ((js-import-is-relative? path)
           (js-import-path-to-relative path dir))
          ((js-import-is-dependency? path (projectile-project-root))
           (js-import-maybe-expand-dependency path))
          (t (js-import-alias-path-to-real path)))))


(defun js-import-alias-path-to-real(path)
  (let ((aliases (js-import-get-aliases))
        (alias)
        (real-path))
    (while aliases
      (setq alias (pop aliases))
      (let* ((alias-regexp (if (s-blank? alias) (concat "^" alias) (concat "^" alias "\\(/\\|$\\)" )))
             (alias-path (js-import-get-alias-path alias))
             (joined-path (f-join alias-path (s-replace-regexp alias-regexp "" path)))
             (found-path (cond
                          ((and (f-ext? joined-path) (f-exists? joined-path))
                           (setq real-path joined-path))
                          ((f-exists? (f-swap-ext joined-path "ts") )
                           (setq real-path (f-swap-ext joined-path "ts")))
                          ((f-exists? (f-swap-ext joined-path "js") )
                           (setq real-path (f-swap-ext joined-path "js")))
                          ((and (not (f-ext? joined-path)) (f-exists? (f-join joined-path "index.ts")) )
                           (setq real-path (f-join joined-path "index.ts")))
                          ((and (not (f-ext? joined-path)) (f-exists? (f-join joined-path "index.js")) )
                           (setq real-path (f-join joined-path "index.js"))))))
        (when (and found-path (f-exists? found-path))
          (setq real-path found-path)
          (setq aliases nil))))
    real-path))


(defun js-import-real-path-to-alias(real-path alias)
  (let ((alias-path (js-import-get-alias-path alias)))
    (f-join alias (replace-regexp-in-string (concat "^" alias-path) "" real-path))))

(defun js-import-get-alias-path(alias)
  (f-slash (f-join (projectile-project-root) (plist-get js-import-alias-map alias))))


(defun js-import-is-dependency? (display-path &optional project-root)
  "Check if path is dependency"
  (let ((dependencies (js-import-node-modules-candidates project-root))
        (dirname (car (split-string display-path "/"))))
    (or (member dirname dependencies)
        (f-exists? (js-import-expand-node-modules dirname project-root)))))

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
                                     display-path
                                     full-name
                                     p1
                                     p2
                                     display-name
                                     display-part
                                     type
                                     export-type
                                     import-type
                                     var-type
                                     real-path
                                     renamed-name
                                     real-name
                                     marker)
  "Utility function to propertize js symbol. See also
`js-import-propertize'."

  (setq candidate (js-import-strip-text-props candidate))
  (js-import-propertize candidate
                        'real-name real-name
                        'full-name full-name
                        'p1 p1
                        'p2 p2
                        'renamed-name renamed-name
                        'display-name (or display-name candidate)
                        'display-path display-path
                        'real-path real-path
                        'type type
                        'display-part display-part
                        'export-type export-type
                        'import-type import-type
                        'var-type var-type
                        'marker marker))

(defun js-import-extracts-exports(&optional real-path)
  "Extracts all available exports. REAL-PATH used for resolving relative nested exports - `export * from ...`'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (symbols)
      (while (re-search-forward "\\(^\\| +\\)export[ \t\n]+" nil t 1)
        (let (path exports)
          (unless (js-import-inside-comment?)
            (save-excursion
              (if (looking-at-p "\\(const\\|let\\|var\\|function[*]?\\|interface\\|type\\|class\\|default\\)")
                  (setq path real-path)
                (progn (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
                       (setq path (js-import-get-path-at-point)))))

            (cond ((looking-at-p "*[ \s\t\n]+as[ \s\t\n]")
                   (re-search-forward "as[ \s\t\n]+\\([_$A-Za-z0-9]\\)" nil t 1)

                   (let ((real-name (js-import-which-word)))
                     (push (js-import-make-index-item (format "* as %s" real-name)
                                                      :type 16
                                                      :export-type 16
                                                      :real-name real-name
                                                      :var-type "export"
                                                      :display-part "*"
                                                      :marker (point)
                                                      :real-path real-path
                                                      :display-path path)
                           exports)))
                  ((looking-at-p "{")
                   (let (p1 p2)
                     (setq p1 (point))
                     (re-search-forward "}")
                     (setq p2 (point))
                     (mapc (lambda(it) (let* ((parts (split-string it))
                                         (real-name (car (last parts)))
                                         (type (if (string= "default" real-name) 1 4)))
                                    (push (js-import-make-index-item real-name
                                                                     :type type
                                                                     :export-type type
                                                                     :display-part it
                                                                     :var-type "export"
                                                                     :real-name real-name
                                                                     :real-path real-path
                                                                     :marker (point)
                                                                     :display-path path)
                                          exports)))
                           (js-import-cut-names
                            (buffer-substring-no-properties p1 p2)
                            ",\\|}\\|{"))))
                  ((looking-at-p "default")
                   (forward-word)
                   (skip-chars-forward "\s\t")

                   (when (js-import-word-reserved? (js-import-which-word))
                     (forward-word)
                     (skip-chars-forward "\s\t"))

                   (push (js-import-make-index-item (js-import-which-word)
                                                    :type 1
                                                    :real-name (js-import-which-word)
                                                    :export-type 1
                                                    :display-part "default"
                                                    :real-path real-path
                                                    :var-type "export default"
                                                    :marker (point)
                                                    :display-path path)
                         exports))
                  ((looking-at-p "[_$A-Za-z0-9]")
                   (let ((var-type (js-import-which-word))
                         (real-name))

                     (forward-word)
                     (skip-chars-forward "\s\t*")
                     (setq real-name (js-import-which-word))
                     (push (js-import-make-index-item real-name
                                                      :type 4
                                                      :real-name real-name
                                                      :export-type 4
                                                      :real-path real-path
                                                      :var-type var-type
                                                      :marker (point)
                                                      :real-path real-path
                                                      :display-path real-path)
                           exports)))
                  ((looking-at-p "*[ \s\t]from")
                   (let* ((curr-dir (f-dirname real-path))
                          (table (syntax-table))
                          (next-path (js-import-path-to-real path curr-dir)))
                     (with-temp-buffer
                       (setq path next-path)
                       (with-syntax-table table
                         (erase-buffer)
                         (js-import-insert-buffer-or-file next-path)
                         (setq symbols (append symbols (js-import-extracts-exports next-path))))))))
            (setq symbols (append symbols exports))))
        (forward-line 1))
      symbols)))

(defun js-import-extracts-imports()
  (save-excursion
    (goto-char 0)
    (let (symbols)
      (while (re-search-forward "\\(^\\| +\\)import[ \t\n]+" nil t 1)
        (unless (js-import-inside-comment?)
          (let (path named-imports default-import module-import)
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
                               (s-matches? "[^_$A-Za-z0-9]" renamed-name))
                           (setq renamed-name "")
                         (skip-chars-forward "_$A-Za-z0-9")))
                     (setq m2 (point))
                     (setq module-import (js-import-make-index-item (format "%s" (buffer-substring-no-properties m1 m2))
                                                                    :type 16
                                                                    :import-type 16
                                                                    :var-type "import"
                                                                    :marker m1
                                                                    :display-path path))

                     (push module-import named-imports)
                     (skip-chars-forward "_$A-Za-z0-9,\s\n\t")
                     ))
                  ((looking-at-p "[_$A-Za-z0-9]")
                   (setq default-import (js-import-make-index-item (js-import-which-word)
                                                                   :type 1
                                                                   :import-type 1
                                                                   :real-name (js-import-which-word)
                                                                   :display-part "default"
                                                                   :var-type "import"
                                                                   :marker (point)
                                                                   :display-path path))
                   (push default-import named-imports)
                   (skip-chars-forward "_$A-Za-z0-9,\s\n\t")))
            (when (looking-at-p "{")
              (let (p1 p2 item items real-name renamed-name full-name)
                (setq p1 (+ 1 (point)))

                (save-excursion (re-search-forward "}" nil t 1)
                                (setq p2 (- (point) 1)))
                (narrow-to-region p1 p2)
                (goto-char p1)
                (while (re-search-forward "[_$A-Za-z0-9]" nil t 1)
                  (setq p1 (match-beginning 0))
                  (goto-char p1)
                  (setq real-name (js-import-which-word))
                  (skip-chars-forward "_$A-Za-z0-9")
                  (setq p2 (point))
                  (skip-chars-forward " \s\t\n")
                  (when (looking-at-p "as[ \s\t\n]")
                    (progn
                      (re-search-forward "as[ \s\t\n]" nil t 1)
                      (setq renamed-name (js-import-which-word))
                      (skip-chars-forward "_$A-Za-z0-9")
                      (setq p2 (point))))

                  (setq full-name (string-trim (buffer-substring-no-properties p1 p2)))
                  (setq item (js-import-make-index-item full-name
                                                        :type 4
                                                        :display-part full-name
                                                        :full-name full-name
                                                        :renamed-name renamed-name
                                                        :display-path path
                                                        :import-type 1
                                                        :real-name real-name
                                                        :marker p1))

                  (push item items))
                (widen)
                (setq named-imports (append named-imports (reverse items)))))

            (setq symbols (append symbols named-imports))))
        (forward-line 1))
      symbols)))


(defun js-import-cut-names(str reg)
  (when (stringp str) (seq-remove 's-blank? (mapcar 's-trim (split-string str reg t)))))

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



(defun js-import-filter-pred(filename &optional alias)
  (and (not (string-equal (s-replace (projectile-project-root) "" buffer-file-name) filename))
       (js-import-is-ext-enabled? filename)
       (not (s-matches? js-import-unsaved-file-regexp filename))
       (not (s-matches? js-import-test-file-regexp filename))
       (if alias (s-matches? (concat "^" alias) filename) t)))

(defun js-import-filter-files(files &optional alias)
  "Filter FILES by extension and one of the aliases, if present."
  (seq-filter (lambda(it) (js-import-filter-pred it alias)) files))


(defun js-import-compose-from(arg &rest funcs)
  "Performs right-to-left unary function composition."
  (seq-reduce (lambda (xs fn)
                (funcall fn xs))
              (reverse funcs) arg))

(defun js-import-generate-name-from-path(path)
  "Generate name for default export from PATH"
  (js-import-compose-from path
                          '(lambda(words) (mapconcat 'capitalize words ""))
                          '(lambda(it) (seq-take it 2))
                          'reverse
                          's-split-words
                          '(lambda(str) (replace-regexp-in-string js-import-file-index-regexp "" str))
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
  (js-import-compose-from path
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
  (f-join (or project-dir (projectile-project-root)) js-import-node-modules-dir))

(defun js-import-expand-node-modules(module &optional project-dir)
  (let ((node-modules-path (f-join (js-import-get-node-modules-path project-dir) module)))
    node-modules-path))


(defun js-import-is-dir-and-exist(path)
  (and (f-exists? path) (not (f-ext? path))))

(defun js-import-is-package-json(path)
  (string="patckage.json" (f-filename path)))

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
  (unless extensions (setq extensions js-import-preffered-extensions))
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


(defun js-import-strip-duplicates (list)
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

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



(provide 'js-import)
;;; js-import.el ends here
