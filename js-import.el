
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
(require 'dash)
(require 'json)
(require 's)
(require 'js-import-regexp)
(require 'js-import-utils)


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

(defcustom js-import-files-number-limit 20
  "The limit for number of project files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

(defcustom js-import-dependencies-number-limit 200
  "The limit for number of dependencies files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

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


(defvar js-import-files-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "Keymap for `js-import-alias-source' source.")


(defmacro js-import-exit-and-run (func)
  "Exit and run FUNC"
  (declare (indent 2) (debug t))
  `(lambda ()
     (interactive)
     (helm-exit-and-execute-action ,func)))


(defvar js-import-imported-symbols-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-d") 'js-import-delete-persistent)
    map)

  "Keymap for symdol sources.")

(put 'js-import-imported-symbols-keymap 'helm-only t)


(defvar js-import-current-alias nil)

(make-variable-buffer-local 'js-import-current-alias)
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

(defvar js-import-symbols-in-buffer nil)
(make-variable-buffer-local 'js-import-symbols-in-buffer)
(defvar js-import-symbols-in-buffer-tick nil)
(make-variable-buffer-local 'js-import-symbols-in-buffer-tick)

(defvar js-import-last-export-path nil)
(make-variable-buffer-local 'js-import-last-export-path)


(defvar js-import-files-actions
  (helm-make-actions "Import from file" (js-import-with-marked-candidates 'js-import-from-path)
                     "Find file" 'js-import-find-file
                     "Find file other window" 'js-import-find-file-other-window))

(defvar js-import-dependency-action
  (helm-make-actions "Import from file" (js-import-with-marked-candidates 'js-import-from-path)
                     "Find nested imports" 'js-import-select-subdir
                     "Find file" 'js-import-find-file
                     "Find file other window" 'js-import-find-file-other-window)
  "File actions for dependencies. Dependencies should be specified into package json and installed into node_modules")


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

(defvar js-import-node-modules-source nil)
(defvar js-import-project-files-source nil)
(defvar js-import-buffer-files-source nil)

(make-variable-buffer-local 'js-import-buffer-files-source)

;;;###autoload
(defun js-import ()
  "Init imports from your current project"
  (interactive)

  (unless js-import-buffer-files-source (setq js-import-buffer-files-source
                                              (helm-make-source js-import-buffer-source-name 'js-import-imported-files-source)))

  (if js-import-project-files-source
      (helm-attrset 'candidate-number-limit js-import-files-number-limit js-import-project-files-source)
    (setq js-import-project-files-source
          (helm-make-source "js files" 'js-import-alias-source)))

  (unless js-import-node-modules-source (setq js-import-node-modules-source
                                              (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))

  (save-excursion
    (helm
     :sources '(js-import-buffer-files-source
                js-import-project-files-source
                js-import-node-modules-source)
     :buffer js-import-buffer
     :preselect (or (js-import-get-path-at-point)
                    js-import-last-export-path
                    "")
     :prompt "Select a file: ")))


;;;###autoload
(defun js-import-alias ()
  "Import from project files without dependencies."
  (interactive)
  (if js-import-project-files-source
      (helm-attrset 'candidate-number-limit 150 js-import-project-files-source)
    (setq js-import-project-files-source
          (helm-make-source "js files" 'js-import-alias-source)))


  (save-excursion
    (helm
     :sources 'js-import-project-files-source
     :buffer js-import-buffer
     :preselect (or (js-import-get-path-at-point)
                    js-import-last-export-path
                    "")
     :prompt "Select a file: ")))


;;;###autoload
(defun js-import-dependency (&optional dependency)
  "Import from node modules"
  (interactive)
  (unless js-import-node-modules-source (setq js-import-node-modules-source
                                              (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))
  (save-excursion
    (when-let ((module (or dependency
                           (helm :sources js-import-node-modules-source))))
      (js-import-from-path module))))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Show imported symbols from current buffer. Available actions includes jumping to item in buffer, renaming, adding more imports from current paths and deleting a symbol or whole import."
  (interactive)
  (helm :sources (helm-make-source "*js symbols*" 'js-import-buffer-imports-source)))


(defun js-import-from-path(path)
  "Extract and propose to make import from PATH "
  (interactive)
  (with-current-buffer helm-current-buffer
    (setq js-import-current-export-path path)
    (setq js-import-last-export-path path)
    (setq js-import-current-export-real-path (js-import-path-to-real path default-directory)))

  (helm :sources (append (list (helm-make-source "exports in" 'js-import-exports-source)
                               (helm-make-source "imported" 'js-import-buffer-imports-source)))))



(defclass js-import-imported-files-source(helm-source-in-buffer)
  ((init :initform 'js-import-find-imported-files-in-buffer)
   (action :initform 'js-import-files-actions)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (mode-line :initform (list "Imports"))
   (keymap :initform js-import-files-keymap)
   (get-line :initform #'buffer-substring)))


(defclass js-import-alias-source (helm-source-sync)
  ((header-name :initform 'js-import-alias-header-name)
   (init :initform 'js-import-alias-init)
   (candidates :initform 'projectile-current-project-files)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (candidate-transformer :initform 'js-import-project-files-transformer)
   (filter-one-by-one :initform 'js-import-project-files-filter-one-by-one)
   (mode-line :initform (list "File(s)"))
   (keymap :initform js-import-files-keymap)
   (action :initform 'js-import-files-actions)
   (group :initform 'js-import)))

(defclass js-import-dependency-source (helm-source-sync)
  ((candidates :initform 'js-import-get-all-dependencies)
   (candidate-number-limit :initform js-import-dependencies-number-limit)
   (action :initform 'js-import-dependency-action)
   (mode-line :initform (list "Dependencies"))
   (keymap :initform js-import-files-keymap)
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
    (let (export-item)
      (setq export-item (seq-find (lambda(elt) (string= (js-import-get-prop elt 'real-name) it)) js-import-export-candidates-in-path)))))

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
          (setq js-import-cached-imports-in-buffer (save-excursion (with-syntax-table (syntax-table) (js-import-extracts-imports))))
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
  "Returns filtered EXPORTS plist with only those members that are not in IMPORTS plist. For named exports (with property `type' 4) the test for equality is done by `real-name' and for default export by `type'."
  (seq-remove (lambda(elt) (pcase (js-import-get-prop elt 'type)
                             (1 (seq-find (lambda(imp) (eq 1 (and (js-import-get-prop imp 'type)))) imports))
                             (4 (seq-find (lambda(imp) (string= (js-import-get-prop elt 'real-name)
                                                                (js-import-get-prop imp 'real-name)))
                                          imports))))
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
    (unless (and js-import-current-alias (not js-import-relative-transformer))
      (setq js-import-current-alias (car js-import-aliases)))))

(defun js-import-relative-one-by-one(path)
  "Transform relative to `projectile-project-root' PATH into relative to the current `buffer-file-name'"
  (if (s-matches? "^\\.+/" path)
      path
    (with-current-buffer helm-current-buffer
      (let* ((path (f-join (projectile-project-root) path))
             (relative-path (f-relative path (f-dirname buffer-file-name))))
        (unless (s-matches? "^\\.+/" relative-path)
          (setq relative-path (concat "./" relative-path)))
        relative-path))))


(defun js-import-project-files-transformer(files &optional _source)
  "Filter FILES by extension and one of the aliases, if present."
  (with-current-buffer helm-current-buffer
    (let ((alias-filter (when js-import-current-alias (concat "^" (lax-plist-get js-import-alias-map js-import-current-alias)))))
      (if (and alias-filter (not js-import-relative-transformer))
          (--filter (and (js-import-filter-pred it) (s-matches? alias-filter it)) files)
        (--filter (js-import-filter-pred it) files)))))


(defun js-import-slash(str)
  "Append slash to non-empty STR unless one already."
  (if (or (s-matches? "/$" str) (s-blank? str))
      str
    (concat str "/")))

(defun js-import-project-files-filter-one-by-one(path)
  "Transform relative to the project root PATH into aliased one or relative to the current `buffer-file-name'"
  (with-current-buffer helm-current-buffer
    (if (and js-import-current-alias (not js-import-relative-transformer))
        (progn
          (let ((regexp (concat "^" (lax-plist-get js-import-alias-map js-import-current-alias) "/")))
            (js-import-normalize-path (s-replace-regexp regexp (js-import-slash js-import-current-alias) path))))

      (js-import-normalize-path (js-import-relative-one-by-one path)))))


(defun js-import-switch-to-relative(&optional _cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (with-current-buffer helm-current-buffer
    (if js-import-relative-transformer
        (progn
          (setq js-import-relative-transformer nil)
          (when (and (not js-import-current-alias) (car js-import-aliases))
            (setq js-import-current-alias (car js-import-aliases)))
          (helm-refresh))
      (progn
        (setq js-import-relative-transformer 'js-import-relative-one-by-one)
        (helm-refresh)))))


(defun js-import-switch-to-next-alias(&optional _cand)
  "Switch to next alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (let ((default-alias (car js-import-aliases))
          (next-alias (car (cdr (member js-import-current-alias js-import-aliases)))))
      (setq js-import-current-alias (or next-alias default-alias))
      (setq js-import-relative-transformer nil)
      (helm-refresh))))


(defun js-import-switch-to-prev-alias(&optional _cand)
  "Switch to previous alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (let ((default-alias (car (reverse js-import-aliases)))
          (next-alias (car (cdr (member js-import-current-alias (reverse js-import-aliases))))))
      (setq js-import-current-alias (or next-alias default-alias))
      (setq js-import-relative-transformer nil)
      (helm-refresh))))

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


(defun js-import-alias-header-name(_name)
  "A function for display header name for project files. Concatenates `js-import-current-alias' and PROJECT-NAME"
  (with-helm-current-buffer
    (cond
     (js-import-current-alias
      (concat
       (propertize js-import-current-alias 'face 'font-lock-function-name-face)
       "\s" (projectile-project-name) "/"
       (lax-plist-get js-import-alias-map js-import-current-alias)))
     (js-import-relative-transformer
      (format "%s" (projectile-project-name)))
     (t (projectile-project-name)))))


(defun js-import-find-file-at-point()
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (when-let ((path (js-import-get-path-at-point)))
    (js-import-find-file path)))

(defun js-import-find-file(file)
  "Transform FILE to real and open it"
  (find-file (js-import-path-to-real file)))

(defun js-import-find-file-other-window(file)
  "Transform FILE to real and open it in other window"
  (find-file-other-window (js-import-path-to-real file)))

(defun js-import-select-subdir(dependency)
  "Action which checks if DEPENDENCY has nested dirs with exports and propose to select it"
  (when-let ((subfiles (js-import-find-interfaces dependency)))
    (push dependency subfiles)
    (let ((module (completing-read "Select: " subfiles nil t dependency)))
      (js-import-from-path module))))

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."
  (setq candidate (js-import-path-to-real candidate))
  (when-let ((str (stringp candidate))
             (buf (get-buffer-create " *js-import persistent*")))
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
  (let ((m (js-import-get-prop item 'marker))
        (js-buffer "*js-import persistent")
        (item-path (or (js-import-get-prop item 'real-path) (js-import-path-to-real (js-import-get-prop item 'display-path) default-directory))))
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



(defun js-import-delete-persistent (&optional cand)
  "Bind a new persistent action 'foo-action to foo function.
foo function is an action function called with one arg candidate."
  (interactive)
  (with-helm-alive-p
    ;; never split means to not split the helm window when executing
    ;; this persistent action. If your source is using full frame you
    ;; will want your helm buffer to split to display a buffer for the
    ;; persistent action (if it needs one to display something).
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
  (let* ((real-name (js-import-get-prop candidate 'real-name))
         (renamed-name (js-import-get-prop candidate 'renamed-name))
         (display-path (js-import-get-prop candidate 'display-path))
         (prompt (if renamed-name (format "Rename %s as (%s) " real-name renamed-name)
                   (format "Rename %s as " real-name)))
         (new-name (s-trim (read-string
                            prompt
                            (or renamed-name real-name)
                            nil
                            renamed-name))))

    (when (and new-name
               (<= 1 (length new-name))
               (not (string= new-name real-name)))
      (save-excursion
        (save-restriction
          (js-import-narrow-to-import display-path)
          (let ((case-fold-search nil))
            (if renamed-name
                (progn (re-search-forward (concat real-name "[_\s\n]+as[_\s\n]") nil t 1)
                       (re-search-forward renamed-name nil t 1)
                       (replace-match new-name)
                       (widen)
                       (query-replace-regexp (concat "\\_<" renamed-name "\\_>") new-name))
              (progn (re-search-forward (concat real-name "[_\s\n,]+") nil t 1)
                     (skip-chars-backward "[_\s\n,]")
                     (insert (concat " as " new-name))
                     (widen))))
          )))))

(defun js-import-insert-import(candidate)
  "Inserts CANDIDATE into existing or new import statement."
  (save-excursion
    (let ((type (js-import-get-prop candidate 'type))
          (name (js-import-get-prop candidate 'display-name))
          (renamed-name (js-import-get-prop candidate 'display-name))
          (normalized-path (js-import-get-prop candidate 'display-path)))
      (pcase type
        (1 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))
        (4 (js-import-insert-exports nil name normalized-path))
        (16 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))))))

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
    (when-let* ((path js-import-current-export-real-path)
                (syntax (syntax-table))
                (exist (f-exists? path)))

      (setq js-import-export-candidates-in-path (with-temp-buffer
                                                  (erase-buffer)
                                                  (with-syntax-table syntax
                                                    (js-import-insert-buffer-or-file path)
                                                    (js-import-extracts-exports path))))
      js-import-export-candidates-in-path)))


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
          ((js-import-is-dependency? path)
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
             (exists (f-exists? joined-path))
             (is-match (s-matches? alias-regexp path))
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
  (f-slash (f-join (projectile-project-root) (lax-plist-get js-import-alias-map alias))))


(defun js-import-is-dependency? (display-path &optional project-root)
  "Check if path is dependency"
  (let ((dependencies (or (plist-get js-import-dependencies-cache-plist (or project-root (projectile-project-root))) (js-import-get-all-dependencies)))
        (dirname (car (split-string display-path "/"))))


    (or (f-exists? (js-import-expand-node-modules dirname project-root))
        (-contains? dependencies dirname))))

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
    (let (paths symbols)
      (while (re-search-forward "\\(^\\| +\\)export[ \t\n]+" nil t 1)
        (let (path exports)
          (unless (js-import-inside-comment?)
            (save-excursion
              (if (looking-at-p "\\(const\\|let\\|var\\|function[*]?\\|interface\\|type\\|class\\|default\\)")
                  (setq path real-path)
                (progn (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
                       (setq path (js-import-get-path-at-point)))))

            (cond ((looking-at-p "*[ \s\t\n]+as[ \s\t\n]")
                   (re-search-forward "as[ \s\t\n]+\\([[:word:]]\\)" nil t 1)

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
                  ((looking-at-p "[[:word:]]")
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
    (goto-char (point-min))
    (let (paths symbols)
      (while (re-search-forward "\\(^\\| +\\)import[ \t\n]+" nil t)
        (unless (js-import-inside-comment?)
          (let (path named-imports default-import module-import)
            (save-excursion
              (re-search-forward "[ \s\t\n]from[ \s\t]+['\"]" nil t 1)
              (setq path (js-import-get-path-at-point)))

            (cond ((looking-at-p "*[ \s\t\n]+as[ \s\t\n]")
                   (re-search-forward "as[ \s\t\n]+\\([[:word:]]\\)" nil t 1)

                   (let ((real-name (js-import-which-word)))
                     (setq module-import (js-import-make-index-item (format "* as %s" real-name)
                                                                    :type 16
                                                                    :import-type 16
                                                                    :real-name real-name
                                                                    :var-type "import"
                                                                    :display-part "*"
                                                                    :marker (point)
                                                                    :display-path path)))

                   (push module-import named-imports))
                  ((looking-at-p "[[:word:]]")
                   (setq default-import (js-import-make-index-item (js-import-which-word)
                                                                   :type 1
                                                                   :import-type 1
                                                                   :real-name (js-import-which-word)
                                                                   :display-part "default"
                                                                   :var-type "import"
                                                                   :marker (point)
                                                                   :display-path path))
                   (push default-import named-imports)
                   (skip-chars-forward "_A-Za-z0-9$,\s\n\t")))
            (when (looking-at-p "{")
              (let (p1 p2 item items real-name renamed-name full-name)
                (setq p1 (+ 1 (point)))

                (save-excursion (re-search-forward "}" nil t 1)
                                (setq p2 (- (point) 1)))
                (narrow-to-region p1 p2)
                (goto-char p1)
                (while (re-search-forward "[[:word:]]" nil t 1)
                  (setq p1 (match-beginning 0))
                  (goto-char p1)
                  (setq real-name (js-import-which-word))

                  (re-search-forward real-name nil t 1)
                  (setq p2 (point))
                  (skip-chars-forward " \s\t\n")
                  (when (looking-at-p "as[ \s\t\n]")
                    (progn
                      (re-search-forward "as[ \s\t\n]" nil t 1)
                      (setq renamed-name (js-import-which-word))
                      (re-search-forward renamed-name nil t 1)
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
                                                        :p1 p1
                                                        :p2 p2
                                                        :marker p1))

                  (push item items))
                (widen)
                (setq named-imports (append named-imports (reverse items)))))

            (setq symbols (append symbols named-imports)))
          )
        (forward-line 1))
      symbols)))


(provide 'js-import)
;;; js-import.el ends here
