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

(defcustom js-import-files-number-limit 50
  "The limit for number of project files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

(defcustom js-import-dependencies-number-limit 50
  "The limit for number of dependencies files displayed. Override `helm-candidate-number-limit'"
  :group 'js-import
  :type 'number)

(defcustom js-import-type-faces
  '(("^\\(as\\)$" . font-lock-type-face)
    ("^\\(*\\)$" . font-lock-type-face)
    ("^\\(default\\)$" . font-lock-variable-name-face)
    ("^\\(Function\\|Functions\\|Defuns\\)$" . font-lock-function-name-face)
    ("^\\(Types\\|Provides\\|Requires\\|Classes\\|Class\\|Includes\\|Imports\\|Misc\\)$" . font-lock-type-face))
  "Faces for showing type in js-import-menu.
This is a list of cons cells.  The cdr of each cell is a face to be used,
and it can also just be like \\='(:foreground \"yellow\").
Each car is a regexp match pattern of the imenu type string."
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

(defvar js-import-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-import)
    (define-key map (kbd "C-c C-.") 'js-import-edit-buffer-imports)
    (define-key map (kbd "C-c C-d") 'js-import-dependency)
    (define-key map (kbd "C-c C-a") 'js-import-alias)
    (easy-menu-define js-import-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-import]
        ["Edit current buffer imports" js-import-edit-buffer-imports]
        ["Import alias" js-import-alias]
        ["Import depenency" js-import-dependency]))
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


(defvar js-import-alias-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
    (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
    (define-key map (kbd "C-r") 'js-import-switch-to-relative)
    map)
  "Keymap for `js-import-alias-source' source.")


(defvar js-import-current-alias nil)
(make-variable-buffer-local 'js-import-current-alias)
(defvar js-import-aliases nil)
(make-variable-buffer-local 'js-import-aliases)
(defvar js-import-relative-transformer nil)
(make-variable-buffer-local 'js-import-relative-transformer)

(defvar js-import-dependencies-cache-plist '())

(defvar js-import-dependency-source-name "node modules")
(defvar js-import-buffer-source-name "imports in buffer")

(defvar js-import-imported-items-actions
  (helm-make-actions
   "Go" 'js-import-action--goto-export
   "Rename" 'js-import-action--rename-import
   "Add more imports" 'js-import-action--add-to-import
   "Delete" 'js-import-action--delete-import
   "Delete whole import" 'js-import-action--delete-whole-import))

(defvar js-import-export-items-actions
  (helm-make-actions
   "Import" 'js-import-action--import-candidate
   "Import as " 'js-import-action--import-as
   "Go" 'js-import-action--goto-export))

;;;###autoload
(defun js-import ()
  "Init imports from your current project"
  (interactive)
  (save-excursion
    (helm
     :sources (append (list
                       (helm-make-source js-import-buffer-source-name 'js-import-buffer-source)
                       (helm-make-source (or (projectile-project-name) (projectile-project-root)) 'js-import-alias-source)
                       (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))
     :buffer "js import"
     :prompt "Select file: ")))


;;;###autoload
(defun js-import-alias ()
  "Import from your current project with alias prefix"
  (interactive)
  (save-excursion
    (helm
     :sources (helm-make-source (or (projectile-project-name) (projectile-project-root)) 'js-import-alias-source))))


;;;###autoload
(defun js-import-dependency (&optional dependency)
  "Import from node modules"
  (interactive)
  (save-excursion
    (when-let ((module (or dependency (helm
                                       :sources (helm-make-source js-import-dependency-source-name 'js-import-dependency-source)))))
      (js-import-from-path module))))


(defun js-import-from-path(normalized-path &optional path)
  (let* ((display-path (js-import-normalize-path normalized-path))
         (import-alist (js-import-find-current-imports display-path))
         (all-exports-alist (js-import-find-all-exports normalized-path path))
         (import-reals (--map (funcall (-compose 's-trim 'car 'split-string 'car) it)
                              import-alist))
         (import-source (helm-build-sync-source (format "imported from %s" display-path)
                          :display-to-real (lambda(candidate)
                                             (js-import-make-item candidate
                                                                  :display-path display-path))
                          :candidate-transformer 'js-import-imports-transformer
                          :candidates import-alist
                          :persistent-action 'js-import-action--goto-persistent
                          :action 'js-import-imported-items-actions))
         (export-source (helm-build-sync-source (format "Exports from %s" normalized-path)
                          :candidates (-filter (lambda(it)
                                                 (pcase (cdr it)
                                                   (1 (not (rassoc 1 import-alist)))
                                                   (4 (not (-contains? import-reals (car it))))
                                                   (16 (not (rassoc 16 import-alist)))))
                                               all-exports-alist)
                          :candidate-transformer 'js-import-imports-transformer
                          :persistent-action 'js-import-action--goto-persistent
                          :display-to-real (lambda(candidate)
                                             (js-import-make-item candidate
                                                                  :real-path path
                                                                  :cell (assoc candidate all-exports-alist)
                                                                  :display-path display-path))

                          :action 'js-import-export-items-actions)))

    (helm :sources (list export-source import-source))))

;;;###autoload
(defun js-import-edit-buffer-imports()
  "Find all imported symbols in current buffer and propose to jump or edit them"
  (interactive)
  (save-excursion
    (helm
     :preselect (js-import-get-unreserved-word-at-point)
     :sources (append
               (mapcar (lambda(sublist) (let ((path (car sublist))
                                         (items (cdr sublist)))
                                     (helm-build-sync-source (format "imported from %s" path)
                                       :display-to-real (lambda(candidate) (js-import-make-item (js-import-strip-text-props candidate)
                                                                                           :real-path (js-import-path-to-real path)
                                                                                           :display-path path))
                                       :candidate-transformer 'js-import-imports-transformer
                                       :candidates items
                                       :persistent-action 'js-import-action--goto-persistent
                                       :action 'js-import-imported-items-actions)))
                       (js-import-find-all-buffer-imports))))))



(defclass js-import-buffer-source(helm-source-in-buffer)
  ((init :initform (lambda() (with-current-buffer (helm-candidate-buffer 'global)
                          (let ((items (with-helm-current-buffer (buffer-substring-no-properties (point-min) (js-import-goto-last-import)))))
                            (setq items (mapcar (lambda(name) (car (last name))) (s-match-strings-all js-import-import-regexp items)))
                            (mapc (lambda(name) (insert name) (newline-and-indent)) items))
                          (goto-char (point-min)))))
   (action :initform 'js-import-select-file-action)
   (header-name :initform (lambda(name) (with-helm-current-buffer
                                     (concat "imports in " (file-name-nondirectory (buffer-file-name))))))
   (persistent-action :initform 'js-import-ff-persistent-action)
   (get-line :initform #'buffer-substring)))

(defclass js-import-dependency-source (helm-source-sync)
  ((candidates :initform 'js-import-get-all-dependencies)
   (candidate-number-limit :initform js-import-dependencies-number-limit)
   (nomark :initform nil)
   (action :initform '(("Show exported symbols" . js-import-select-file-action)
                       ("Select from subdirectory" . js-import-select-subdir)))
   (persistent-action :initform 'js-import-ff-persistent-action)
   (group :initform 'js-import)))

(defun js-import-alias-init()
  "Init project files."
  (with-current-buffer helm-current-buffer
    (setq js-import-aliases (js-import-get-aliases))
    (unless (and js-import-current-alias (not js-import-relative-transformer))
      (setq js-import-current-alias (car js-import-aliases)))))

(defclass js-import-alias-source (helm-source-sync)
  ((header-name :initform 'js-import-alias-header-name)
   (init :initform 'js-import-alias-init)
   (candidates :initform 'projectile-current-project-files)
   (candidate-number-limit :initform js-import-files-number-limit)
   (persistent-action :initform 'js-import-ff-persistent-action)
   (candidate-transformer :initform 'js-import-project-files-transformer)
   (filter-one-by-one :initform 'js-import-project-files-filter-one-by-one)
   (nomark :initform nil)
   (keymap :initform js-import-alias-keymap)
   (action :initform `(("Show exported symbols" . js-import-select-file-action)
                       (,(substitute-command-keys "Switch to next alias \\<js-import-alias-keymap>`\\[js-import-switch-to-next-alias]'")
                        . js-import-switch-to-next-alias)
                       (,(substitute-command-keys "Switch to previous alias \\<js-import-alias-keymap>`\\[js-import-switch-to-prev-alias]'")
                        . js-import-switch-to-prev-alias)
                       (,(substitute-command-keys "Switch to relative \\<js-import-alias-keymap>`\\[js-import-switch-to-relative]'")
                        . js-import-switch-to-relative)))
   (group :initform 'js-import)))



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
            (s-replace-regexp regexp (js-import-slash js-import-current-alias) path)))

      (js-import-relative-one-by-one path))))


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
    (let ((current-alias js-import-current-alias)
          (default-alias (car js-import-aliases))
          (next-alias (car (cdr (member js-import-current-alias js-import-aliases)))))
      (setq js-import-current-alias (or next-alias default-alias))
      (setq js-import-relative-transformer nil)
      (helm-refresh))))


(defun js-import-switch-to-prev-alias(&optional _cand)
  "Switch to previous alias in `js-import-aliases' list"
  (interactive)
  (with-current-buffer helm-current-buffer
    (let ((current-alias js-import-current-alias)
          (default-alias (car (reverse js-import-aliases)))
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


(defun js-import-select-file-action(&optional _file)
  (with-current-buffer helm-current-buffer
    (mapc
     'js-import-from-path
     (helm-marked-candidates))))

(defun js-import-select-subdir(dependency)
  "Action which checks if DEPENDENCY has nested dirs with exports and propose to select it"
  (with-helm-quittable
    (when-let ((subfiles (js-import-find-interfaces dependency)))
      (push dependency subfiles)
      (let ((module (completing-read "Select: " subfiles nil t dependency)))
        (js-import-from-path module)))))


(defun js-import-alias-header-name(project-name)
  "A function for display header name. Concatenates `js-import-current-alias' and PROJECT-NAME"
  (with-helm-current-buffer helm-current-buffer
                            (cond
                             (js-import-current-alias
                              (concat (propertize js-import-current-alias 'face 'font-lock-function-name-face) "\s" project-name "/" (lax-plist-get js-import-alias-map js-import-current-alias)))
                             (js-import-relative-transformer
                              (format "%s" project-name))
                             (t project-name))))

(defun js-import-ff-persistent-action (candidate)
  "Preview the contents of a file in a temporary buffer."

  (print candidate)
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



(defun js-import-action--goto-persistent(candidate)
  (with-helm-quittable
    (let ((real-path (js-import-get-prop candidate 'real-path))
          (display-path (js-import-get-prop candidate 'display-path)))
      (when (and (not real-path) display-path)
        (setq real-path (js-import-path-to-real candidate)))
      (when (and real-path (f-exists? real-path))

        (js-import-ff-persistent-action real-path)))))

(defun js-import-action--goto-export(candidate)
  (with-helm-quittable
    (let ((real-name-regexp (concat "\\([\s\t\n,]" (js-import-get-prop candidate 'real-name) "[\s\t\n,]\\)"))
          (real-path (js-import-get-prop candidate 'real-path))
          (display-path (js-import-get-prop candidate 'display-path))
          (export-type  (js-import-get-prop candidate 'type)))
      (when (and (not real-path) display-path)
        (setq real-path (js-import-path-to-real candidate)))
      (when (and real-path (f-exists? real-path))
        (find-file-read-only-other-window real-path)
        (goto-char 0)
        (re-search-forward (pcase export-type
                             (1 (concat "export[ \s\t\n]+default"))
                             (4 (concat "export[ \s\t\n]+.*+" real-name-regexp))
                             (16 "export[\s\t\n]")))
        (helm-highlight-current-line)))))

(defun js-import-action--delete-import(_cand)
  (mapc
   (lambda(candidate)
     (let ((type (js-import-get-prop candidate 'type))
           (fullname (js-import-get-prop candidate 'display-name))
           (display-path (js-import-get-prop candidate 'display-path)))
       (if (equal type 16)
           (js-import-delete-whole-import display-path)
         (js-import-delete-imported-name fullname display-path))))
   (helm-marked-candidates)))

(defun js-import-action--delete-whole-import(_cand)
  (mapc
   (lambda(candidate)
     (let ((display-path (js-import-get-prop candidate 'display-path)))
       (js-import-delete-whole-import display-path)))
   (helm-marked-candidates)))

(defun js-import-action--add-to-import(_cand)
  (mapc
   (lambda(candidate)
     (let ((display-path (js-import-get-prop candidate 'display-path))
           (real-path (js-import-get-prop candidate 'real-path)))
       (js-import-from-path display-path real-path)))
   (helm-marked-candidates)))

(defun js-import-action--rename-import(_cand)
  (mapc
   (lambda(candidate)
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
   (helm-marked-candidates)))

(defun js-import-action--import-candidate(_candidate)
  (mapc (lambda(c)
          (save-excursion
            (let ((type (js-import-get-prop c 'type))
                  (name (js-import-get-prop c 'display-name))
                  (renamed-name (js-import-get-prop c 'display-name))
                  (normalized-path (js-import-get-prop c 'display-path)))
              (pcase type
                (1 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))
                (4 (js-import-insert-exports nil name normalized-path))
                (16 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))))))
        (helm-marked-candidates)))

(defun js-import-action--import-as(_candidate)
  (mapc (lambda(c) (let* ((type (js-import-get-prop c 'type))
                     (normalized-path (js-import-get-prop c 'display-path))
                     (real-name (js-import-get-prop c 'real-name))
                     (renamed-name (s-trim (read-string
                                            (format "import %s as " real-name)
                                            nil nil)))
                     (full-name (concat real-name " as " renamed-name)))
                (pcase type
                  (1 (js-import-insert-exports renamed-name nil normalized-path))
                  (4 (js-import-insert-exports nil full-name normalized-path))
                  (16 (js-import-insert-exports full-name nil normalized-path)))))
        (helm-marked-candidates)))


(defun js-import-get-aliases ()
  "Get list of aliases"
  (let ((pl js-import-alias-map)
        (vals  ()))
    (while pl
      (push (car pl) vals)
      (setq pl  (cddr pl)))
    (nreverse vals)))


(defun js-import-alias-path-to-real(path)
  (let* ((aliases (js-import-get-aliases))
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


(defun js-import-path-to-real(path &optional dir)
  (when (stringp path)
    (setq path (js-import-strip-text-props path))
    (cond ((and (f-ext? path) (f-exists? path)) path)
          ((js-import-is-relative? path)
           (js-import-path-to-relative path dir))
          ((js-import-is-dependency? path)
           (js-import-maybe-expand-dependency path))
          (t (js-import-alias-path-to-real path)))))

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


(cl-defun js-import-make-item (candidate
                               &key
                               display-path
                               type
                               real-path
                               cell)
  "Utility function to make js-import item. See also

`js-import-propertize'."
  (setq candidate (js-import-strip-text-props candidate))
  (let* ((splitted-name (split-string candidate "[ \t\s]+as[ \t\s]+"))
         (result (js-import-propertize candidate
                                       'real-name (nth 0 splitted-name)
                                       'display-name candidate
                                       'display-path display-path
                                       'renamed-name (nth 1 splitted-name)
                                       'real-path (or real-path (js-import-path-to-real display-path))
                                       'type (or type (cdr cell)))))

    result))

(provide 'js-import)
;;; js-import.el ends here
