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

(require 'cl-lib)
(require 'json)

(eval-when-compile
  (require 'subr-x))

(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import
  :group 'languages)

(defcustom js-import-project-aliases '()
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

(defcustom js-import-package-json-sections
  '("dependencies" "devDependencies")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-node-modules-priority-section-to-read
  '("jsnext:main" "module" "types" "typings")
  "Package-json sections to retrieve candidates from node_modules."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-node-modules-dir "node_modules"
  "Relative to project root or absolute path to node_modules directory."
  :group 'js-import
  :type 'string)

(defcustom js-import-preffered-extensions
  '("d.ts" "ts" "tsx" "jsx" "mjs" "js" "cjs")
  "Preffered suffixes for files with different extension."
  :group 'js-import
  :type '(repeat string))

(defcustom js-import-buffer "*helm js import*"
  "Name of `js-import' buffer."
  :group 'js-import
  :type 'string)

(defface js-import-highlight-face
  '((t (:background "#daa520" :foreground "white")))
  "Face used to highlight symbol."
  :group 'js-import)

(defvar js-import-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") 'js-import)
    (define-key map (kbd "C-c C-.") 'js-import-symbols-menu)
    (define-key map (kbd "C-c C-j") 'js-import-find-symbol-at-point)
    (define-key map (kbd "C-.") 'js-import-find-file-at-point)
    (easy-menu-define js-import-mode-menu map
      "Menu for Js import"
      '("Js import"
        ["Import from all sources" js-import]
        ["Jump to symbol in buffer" js-import-symbols-menu]
        ["Jump to definition" js-import-find-symbol-at-point]
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

(defvar js-import-files-map nil
  "Keymap for files sources.")

(defvar js-import-imported-symbols-map nil
  "Keymap for symdol sources.")

(defvar js-import-export-symbols-map nil
  "Keymap for symdol sources.")
(defvar js-import-file-persistent-action nil)

(defcustom js-import-file-actions
  '(("Import" . js-import-from-path)
    ("Find file" . js-import-find-file)
    ("Find file other window" . js-import-find-file-other-window)
    ("Next alias" . js-import-switch-to-next-alias)
    ("Prev alias" . js-import-switch-to-prev-alias)
    ("Relative" . js-import-switch-to-relative))
  "Default actions for files."
  :group 'js-import
  :type '(alist :key-type string :value-type function))

(defun js-import-make-opt-symbol-regexp (words)
  "Return regexp from `regexp-opt'"
  (concat "\\_<" (regexp-opt (if (listp words)
                                 words
                               (list words)) t) "\\_>"))

(defconst js-import-file-ext-regexp
  (concat "[.]" (regexp-opt js-import-preffered-extensions) "$")
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-import-file-index-regexp
  (concat "\\(/\\|^\\)index" "\\($\\|" js-import-file-ext-regexp "\\)"))

(defconst js-import-expression-keywords
  '("const" "var" "let" "interface" "type" "class"))

(defconst js-import-expression-keywords--re
  (js-import-make-opt-symbol-regexp js-import-expression-keywords))

(defconst js-import-from-keyword--re
  (js-import-make-opt-symbol-regexp "from"))

(defconst js-import-delcaration-keywords
  (append '("function" "function*") js-import-expression-keywords))

(defconst js-import-delcaration-keywords--re
  (concat "\\_<" (regexp-opt js-import-delcaration-keywords t) "\\_>"))

(defconst js-import-regexp-name
  "_$A-Za-z0-9"
  "Regexp matching the start of a js identifier.")

(defconst js-import-name-as--re
  "\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)\\([\s\t\n]+as[\s\t\n]+\\([_$A-Za-z0-9]+\\)\\)?")

(defconst js-import-regexp-name-set
  "[_$A-Za-z0-9]"
  "Regexp set matching the start of a js identifier.")

(defconst js-import-regexp-import-keyword
  (js-import-make-opt-symbol-regexp "import")
  "Regexp matching keyword import.")

(defconst js-import-esm-export-keyword--re
  (concat "\\_<" (regexp-opt (list "export") t) "\\_>")
  "Regexp matching keyword export.")

(defconst js-import-cjs-export-keyword--re
  (concat "\\_<" (regexp-opt (list "exports") t) "\\_>")
  "Regexp matching keyword for exports.")

(defconst js-import-node-modules-regexp
  "\\(/\\|^\\)node_modules\\(/\\|$\\)"
  "Regexp matching path with node_modules.")

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

(defun js-import-reserved-word-p (str &optional reserved-list)
  "Check if STR is js reserved word."
  (unless reserved-list (setq reserved-list js-import-reserved-js-words))
  (when (stringp str)
    (member str reserved-list)))

(defvar js-import-open-paren-re "[^=(]{")
(defvar js-import-closed-paren-re (regexp-opt '("}") t))

(defvar js-import-dependencies-cache (make-hash-table :test 'equal))
(defvar js-import-dependencies-cache-tick nil)
(defvar js-import-current-alias nil)
(defvar js-import-current-project-root nil)
(defvar js-import-aliases nil)
(defvar js-import-current-buffer nil)

(defvar-local js-import-buffer-tick nil
  "Buffer modified tick.")
(defvar-local js-import-current-export-path nil)
(defvar-local js-import-last-export-path nil)
(defvar-local js-import-export-candidates-in-path nil)
(defvar-local js-import-cached-imports-in-buffer nil)

(defvar js-import-node-modules-source nil
  "Variable keeps source files from node_modules.")
(defvar js-import-project-files-source nil
  "Variable for source of relative and aliased files without dependencies.")
(defvar js-import-buffer-files-source nil
  "Variable for source of imported files in the current buffer.")
(defvar js-import-imported-symbols-source nil)
(defvar js-import-exports-source nil)
(defvar js-import-definitions-source nil)

(defcustom js-import-completion-system
  'default
  "Which completion system to use."
  :group 'js-import
  :type '(choice (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)
                 (const :tag "Default" default)))

(defcustom js-import-files-source '(js-import-buffer-files-source
                                    js-import-project-files-source
                                    js-import-node-modules-source)
  "Helm sources for files for command `js-import'."
  :type '(repeat (choice symbol))
  :group 'js-import)

(defcustom js-import-symbol-sources '(js-import-imported-symbols-source
                                      js-import-exports-source
                                      js-import-definitions-source)
  "Helm sources for symbols for command `js-import-symbols-menu'."
  :type '(repeat (choice symbol))
  :group 'js-import)

(defmacro js-import-with-buffer-or-file-content (filename &rest body)
  "Execute BODY in temp buffer with file or buffer content of FILENAME.
 Bind FILENAME to variables `buffer-file-name' and `current-path''.
 It is also bind `default-directory' into FILENAME's directory."
  (declare (indent 2) (debug t))
  `(when-let ((current-path ,filename))
     (when (and current-path (file-exists-p current-path))
       (with-temp-buffer
         (erase-buffer)
         (if (get-file-buffer current-path)
             (progn
               (insert-buffer-substring-no-properties
                (get-file-buffer current-path)))
           (progn
             (let ((buffer-file-name current-path))
               (insert-file-contents current-path))))
         (with-syntax-table js-import-mode-syntax-table
           (let* ((buffer-file-name current-path)
                  (default-directory (js-import-dirname buffer-file-name)))
             (progn ,@body)))))))

(defmacro js-import-completion-clause (completion-symb bound-form)
  (declare (indent 2) (debug t))
  `(when-let ((completion-system ,completion-symb))
     (if (eq js-import-completion-system completion-system)
         (if (and ,bound-form)
             t
           (user-error (concat "Cannot find "
                               (symbol-name completion-system)))))))

;;;###autoload
(defun js-import ()
  "Read a filename to extract exported symbols and add selected ones in buffer."
  (interactive)
  (js-import-init-project)
  (let ((prompt "Module:\s"))
    (cond
     ((js-import-completion-clause 'helm (fboundp 'helm))
      (js-import-helm-read-file-name prompt js-import-file-actions))
     ((js-import-completion-clause 'ivy (fboundp 'ivy-read))
      (ivy-read prompt
                (js-import-get-all-modules)
                :preselect (js-import-preselect-file)
                :require-match t
                :action 'js-import-from-path
                :caller 'js-import))
     (t (let ((module (funcall completing-read-function prompt
                               (js-import-get-all-modules))))
          (js-import-from-path module))))))

;;;###autoload
(defun js-import-symbols-menu ()
  "Jump or refactor to exported, imported and definitions in current buffer."
  (interactive)
  (js-import-init-project)
  (setq js-import-current-export-path nil)
  (cond
   ((js-import-completion-clause 'helm (fboundp 'helm))
    (js-import-helm-build-symbols-sources)
    (helm
     :preselect (js-import-preselect-symbol)
     :sources js-import-symbol-sources))
   ((js-import-completion-clause 'ivy (fboundp 'ivy-read))
    (let* ((choices (append (js-import-extract-all-exports buffer-file-name)
                            (js-import-extract-imports buffer-file-name)
                            (js-import-search-backward-identifiers
                             buffer-file-name (point-max)))))
      (ivy-read "Jump to\s"
                choices
                :preselect (js-import-preselect-symbol)
                :caller 'js-import-symbol-sources
                :action (lambda(it)
                          (when-let ((item (js-import-find-definition it)))
                            (let ((cands (js-import-get-prop item :stack))
                                  (preselect))
                              (when cands
                                (push item cands)
                                (setq cands (reverse cands))
                                (setq cands (js-import-map-stack cands))
                                (setq preselect
                                      (seq-find
                                       (lambda(d) (js-import-get-prop d
                                                                 :var-type))
                                       cands))
                                (setq item (ivy-read "Jump:\s" cands
                                                     :preselect (or preselect
                                                                    it)))))
                            (when-let ((pos (and item (js-import-get-prop
                                                       item :pos))))
                              (find-file (js-import-get-prop item :real-path))
                              (goto-char pos)
                              (js-import-highlight-word)))))))
   (t (when-let* ((choices (append (js-import-extract-all-exports
                                    buffer-file-name)
                                   (js-import-extract-imports buffer-file-name)
                                   (js-import-search-backward-identifiers
                                    buffer-file-name (point-max))))
                  (symbol (completing-read "Select symbol"
                                           choices nil t
                                           (js-import-preselect-symbol))))
        (setq symbol (mapcar (lambda(it) (car (member it choices)))
                             symbol))
        (when-let ((item (js-import-find-definition symbol)))
          (when (js-import-get-prop item :var-type)
            (find-file (js-import-get-prop item :real-path))
            (progn (goto-char (js-import-get-prop item :pos))
                   (js-import-highlight-word))))))))

;;;###autoload
(defun js-import-find-symbol-at-point ()
  "Deep jump to a definition of symbol at point through renaming, re-exports."
  (interactive)
  (js-import-init-project)
  (when-let ((name (js-import-get-word-if-valid)))
    (let (real-name as-name)
      (save-excursion (skip-chars-backward name)
                      (if (save-excursion
                            (js-import-skip-whitespace-backward)
                            (js-import-looking-at "as"))
                          (progn
                            (setq as-name name)
                            (js-import-re-search-backward "as" nil t 1)
                            (js-import-skip-whitespace-backward)
                            (setq real-name (js-import-get-word-if-valid)))
                        (progn
                          (setq real-name name)
                          (js-import-re-search-forward real-name)
                          (js-import-skip-whitespace-forward)
                          (if (not (js-import-looking-at "as"))
                              (setq as-name real-name)
                            (js-import-re-search-forward "as" nil t 1)
                            (js-import-skip-whitespace-forward)
                            (setq as-name (js-import-get-word-if-valid))))))
      (when-let ((item (and (or real-name as-name)
                            (or (js-import-find-by-prop
                                 :real-name real-name
                                 (js-import-search-backward-identifiers
                                  buffer-file-name (point)))
                                (js-import-find-by-prop
                                 :as-name as-name
                                 (js-import-extract-imports
                                  buffer-file-name))
                                (js-import-find-by-prop
                                 :real-name real-name
                                 (js-import-extract-all-exports
                                  buffer-file-name))))))
        (unless (js-import-get-prop item :var-type)
          (setq item (js-import-find-definition item)))
        (when item
          (find-file (js-import-get-prop item :real-path))
          (progn (goto-char (js-import-get-prop item :pos))
                 (js-import-highlight-word)))))))

;;;###autoload
(defun js-import-find-file (&optional file)
  "Transform FILE to real and open it."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file path)
      (message "Could't find %s" file))))

;;;###autoload
(defun js-import-find-file-other-window (&optional file)
  "Transform FILE to real and open it in other window."
  (interactive)
  (let ((path (js-import-path-to-real file)))
    (if (and path (file-exists-p path))
        (find-file-other-window path)
      (message "Could't find %s" file))))

(defun js-import-from-path (path)
  "Insert import statement with marked symbols, exported from PATH."
  (with-current-buffer js-import-current-buffer
    (when-let ((display-path (or (js-import-get-prop path :display-path)
                                 path)))
      (setq js-import-current-export-path display-path)
      (setq js-import-last-export-path display-path)))
  (js-import-init-exports-candidates)
  (cond
   ((and (eq js-import-completion-system 'helm)
         (fboundp 'helm))
    (js-import-helm-build-symbols-sources)
    (helm
     :preselect (js-import-preselect-symbol)
     :sources '(js-import-exports-source
                js-import-imported-symbols-source)))
   ((and (eq js-import-completion-system 'ivy)
         (fboundp 'ivy-read))
    (let ((choices (js-import-export-filtered-candidate-transformer
                    (js-import-exported-candidates-transformer
                     js-import-export-candidates-in-path))))
      (if (null choices)
          (message "No exports found")
        (ivy-read
         "Symbols\s" choices
         :require-match t
         :caller 'js-import-from-path
         :preselect (js-import-preselect-symbol)
         :action (lambda(it) (js-import-insert-import
                         (js-import-display-to-real-exports it)))))))
   (t (let ((choices (js-import-export-filtered-candidate-transformer
                      (js-import-exported-candidates-transformer
                       js-import-export-candidates-in-path))))
        (if (null choices)
            (message "No exports found")
          (js-import-completing-read
           "Symbols\s" choices
           :require-match t
           :action (lambda(it) (js-import-insert-import
                           (js-import-display-to-real-exports
                            it)))))))))

(defun js-import-init-project ()
  "Initialize project by setting buffer, finding root and aliases."
  (setq js-import-current-buffer (current-buffer))
  (setq js-import-current-project-root (js-import-find-project-root))
  (setq js-import-aliases (js-import-get-aliases
                           js-import-current-project-root))
  (when (and js-import-current-alias
             (not (member js-import-current-alias js-import-aliases)))
    (setq js-import-current-alias nil)))

(defun js-import-build-helm-file-sources (&optional action)
  "Make and setup file sources for `helm.'"
  (unless js-import-files-map
    (when-let ((h-map (and (boundp 'helm-map) helm-map))
               (map (make-sparse-keymap)))
      (set-keymap-parent map h-map)
      (define-key map (kbd "C-c o")
        (lambda() (interactive)
          (helm-run-after-exit
           'js-import-find-file
           (helm-get-selection))))
      (define-key map (kbd "C-c C-o")
        (lambda() (interactive)
          (helm-run-after-exit
           'js-import-find-file-other-window
           (helm-get-selection))))
      (define-key map (kbd "C->") 'js-import-switch-to-next-alias)
      (define-key map (kbd "C-<") 'js-import-switch-to-prev-alias)
      (define-key map (kbd "C-c C-.") 'js-import-switch-to-relative)
      (setq js-import-files-map map)
      (put 'js-import-files-map 'helm-only t)))
  (setq js-import-file-persistent-action
        '(lambda(candidate)
           (setq candidate (js-import-path-to-real candidate
                                                   default-directory))
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
             (helm-attrset 'current-candidate candidate))))
  (unless js-import-buffer-files-source
    (setq js-import-buffer-files-source
          (helm-make-source "Imported files" 'helm-source-in-buffer
            :init (lambda() (with-current-buffer (helm-candidate-buffer 'global)
                         (let ((items (with-current-buffer
                                          js-import-current-buffer
                                        (js-import-find-imported-files))))
                           (mapc (lambda(it) (insert it)
                                   (newline-and-indent))
                                 items))
                         (goto-char (point-min))))
            :get-line #'buffer-substring
            :action action
            :keymap js-import-files-map
            :group 'js-import
            :persistent-action (lambda(it) (funcall js-import-file-persistent-action
                                               it))
            :mode-line (list "Imports"))))
  (unless js-import-node-modules-source
    (setq js-import-node-modules-source
          (helm-make-source "Node Modules" 'helm-source-sync
            :candidates #'js-import-node-modules-candidates
            :candidate-number-limit js-import-dependencies-number-limit
            :action action
            :mode-line (list "Dependencies")
            :keymap js-import-files-map
            :persistent-action (lambda(it) (funcall js-import-file-persistent-action
                                               it))
            :group 'js-import))
    (helm-attr 'candidates js-import-node-modules-source t))
  (unless js-import-project-files-source
    (setq js-import-project-files-source
          (helm-make-source "Project files" 'helm-source-sync
            :group 'js-import
            :mode-line (list "File(s)")
            :candidate-number-limit js-import-files-number-limit
            :persistent-action (lambda(it) (funcall js-import-file-persistent-action
                                               it))
            :action action
            :keymap js-import-files-map
            :candidates #'js-import-find-project-files
            :filtered-candidate-transformer
            #'js-import-project-files-transformer))))

(defun js-import-helm-read-file-name (&optional prompt action)
  "Preconfigured helm for selecting files.
Run sources defined in option `js-import-files-source'."
  (interactive)
  (js-import-build-helm-file-sources action)
  (helm
   :sources js-import-files-source
   :buffer js-import-buffer
   :preselect (js-import-preselect-file)
   :prompt (or prompt "Select a file: ")))

(defun js-import-find-project-files (&optional project-root)
  "Return project files without dependencies."
  (let* ((root (js-import-slash (or project-root
                                    (js-import-find-project-root))))
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
    (mapc (lambda(dir) (setq files (append files
                                      (js-import-directory-files dir t))))
          dirs)
    files))

(defun js-import-get-all-modules (&optional project-root)
  (let* ((project-files (js-import-find-project-files project-root))
         (node-modules (js-import-node-modules-candidates project-root))
         (aliases (js-import-get-aliases project-root))
         (relative-files (js-import-transform-files-to-relative
                          default-directory project-files))
         (transformed-files (mapcan (lambda(a) (js-import-transform-files-to-alias
                                           a project-files))
                                    aliases)))
    (append transformed-files node-modules relative-files)))

(defun js-import-project-files-transformer (files &optional _source)
  "Filter and transform FILES to aliased or relative."
  (let* ((current-file (buffer-file-name js-import-current-buffer))
         (current-dir (js-import-dirname current-file)))
    (setq files (seq-remove (lambda(filename) (string= filename current-file))
                            files))
    (if js-import-current-alias
        (js-import-transform-files-to-alias js-import-current-alias files)
      (js-import-transform-files-to-relative current-dir files))))

(defun js-import-transform-files-to-alias (alias files)
  (when-let* ((alias-path (js-import-compose-from alias
                                                  'js-import-slash
                                                  'js-import-get-alias-path))
              (slashed-alias (js-import-slash alias))
              (remove-pred (lambda(filename)
                             (and (file-name-absolute-p filename)
                                  (not (js-import-string-match-p alias-path
                                                                 filename)))))
              (transformer (lambda(path) (js-import-normalize-path
                                     (replace-regexp-in-string alias-path
                                                               slashed-alias
                                                               path)))))
    (mapcar transformer (seq-remove remove-pred files))))

(defun js-import-transform-files-to-relative (dir files)
  (mapcar (lambda(path) (js-import-normalize-path
                    (js-import-path-to-relative
                     path
                     dir)))
          files))

(defun js-import-get-alias-path (alias &optional project-root)
  (when-let ((alias-path (plist-get js-import-project-aliases alias)))
    (if (file-exists-p alias-path)
        alias-path
      (js-import-join-file (or project-root
                               (js-import-find-project-root))
                           alias-path))))

(defun js-import-find-project-root (&optional dir)
  (unless dir (setq dir default-directory))
  (let ((parent (expand-file-name ".." dir)))
    (unless (or (string= parent dir)
                (string= dir "/"))
      (if (file-exists-p (expand-file-name "package.json" dir))
          dir
        (js-import-slash (js-import-find-project-root parent))))))

(defun js-import-directory-files (dir &optional recursive re)
  "Return files in DIR that matches value of the variable
`js-import-file-ext-regexp'.
Optional argument RECURSIVE non-nil means to search recursive."
  (unless re (setq re js-import-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir re nil)
    (directory-files dir t re t)))

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

(defun js-import-path-to-relative (path &optional dir)
  "Transform PATH into relative to the DIR (default: ‘default-directory’).
If PATH is a relative file, it will be returned without changes."
  (if (js-import-relative-p path)
      path
    (let ((relative-path (file-relative-name path (or dir default-directory))))
      (unless (js-import-relative-p relative-path)
        (setq relative-path (concat "./" relative-path)))
      relative-path)))

(defun js-import-slash (str)
  "Append slash to non-empty STR unless one already."
  (if (or (null str)
          (js-import-string-match-p "/$" str)
          (js-import-string-blank-p str))
      str
    (concat str "/")))

(defun js-import-normalize-path (path)
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

(defun js-import-index-trimmable-p (path)
  "Check if PATH index can be trimmed."
  (if (js-import-relative-p path)
      (and (js-import-is-index-file-p path)
           (< 1 (js-import-count-matches "/" path)))
    (js-import-is-index-file-p path)))

(defun js-import-remove-ext (path)
  (replace-regexp-in-string js-import-file-ext-regexp "" path))

(defun js-import-is-index-file-p (path)
  (js-import-string-match-p js-import-file-index-regexp path))

(defun js-import-relative-p (path)
  (js-import-string-match-p "^\\.+/" path))

(defun js-import-get-aliases (&optional project-root)
  "Get list of aliases of PROJECT-ROOT without real paths."
  (let ((root (or project-root (js-import-find-project-root)))
        (pl js-import-project-aliases)
        (vals))
    (while pl
      (when-let* ((alias (car pl))
                  (path (plist-get js-import-project-aliases alias))
                  (exists (file-exists-p (js-import-join-file root path))))
        (push alias vals))
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun js-import-path-to-real (path &optional dir)
  (when (and path (stringp path))
    (setq path (js-import-strip-text-props path))
    (cond ((and (js-import-string-match-p
                 js-import-file-ext-regexp path)
                (file-exists-p path)
                (not (js-import-relative-p path)))
           path)
          ((js-import-relative-p path)
           (js-import-relative-to-real path dir))
          ((js-import-dependency-p path (js-import-find-project-root))
           (js-import-node-module-to-real path))
          (t (js-import-alias-path-to-real path)))))

(defun js-import-relative-to-real (path &optional dir)
  (unless dir (setq dir default-directory))
  (or (js-import-try-ext path dir)
      (js-import-try-ext (js-import-join-file path "index") dir)))

(defun js-import-alias-path-to-real (path)
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
                                   js-import-file-ext-regexp
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

(defun js-import-add-ext (path ext)
  (if (js-import-string-match-p js-import-file-ext-regexp path)
      path
    (concat path "." ext)))

(defun js-import-try-ext (path &optional dir extensions)
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

(defun js-import-switch-to-relative (&optional _cand)
  "Toggle displaying aliased files to relative."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias nil)
    (setq js-import-current-alias (car js-import-aliases)))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

;;;###autoload
(defun js-import-switch-to-next-alias (&optional _cand)
  "Switch to next alias in `js-import-aliases' list."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias (car (cdr
                                          (member js-import-current-alias
                                                  js-import-aliases))))
    (setq js-import-current-alias (car js-import-aliases)))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

;;;###autoload
(defun js-import-switch-to-prev-alias (&optional _cand)
  "Switch to previous alias in `js-import-aliases' list."
  (interactive)
  (if js-import-current-alias
      (setq js-import-current-alias (car (cdr (member
                                               js-import-current-alias
                                               (reverse js-import-aliases)))))
    (setq js-import-current-alias (car (reverse js-import-aliases))))
  (when (fboundp 'helm-refresh)
    (helm-refresh)))

(defun js-import-node-modules-candidates (&optional project-root)
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

(defun js-import-find-interfaces (display-path dir)
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

(defun js-import-find-node-modules (&optional project-dir)
  "Return the path to node-modules."
  (if (file-name-absolute-p js-import-node-modules-dir)
      js-import-node-modules-dir
    (when-let ((root (or project-dir (js-import-find-project-root))))
      (setq root (car (split-string root js-import-node-modules-regexp)))
      (js-import-join-when-exists root js-import-node-modules-dir))))

(defun js-import-node-module-to-real (module &optional project-root)
  (when-let* ((node-modules (or (js-import-find-node-modules project-root)
                                (js-import-find-node-modules)))
              (real-path (js-import-join-file node-modules module)))
    (unless (js-import-string-match-p
             js-import-file-ext-regexp real-path)
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

(defun js-import-try-find-real-path (path)
  (if (or (null path) (and (js-import-string-match-p
                            js-import-file-ext-regexp path)
                           (file-exists-p path)))
      path
    (or (when-let* ((package-json (js-import-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             js-import-node-modules-priority-section-to-read)))
          (if (js-import-string-match-p js-import-file-ext-regexp
                                        module)
              (expand-file-name module path)
            (js-import-try-find-real-path (js-import-try-ext module path))))
        (when-let* ((dir (js-import-join-file path "src"))
                    (exists (file-exists-p dir))
                    (files (js-import-directory-files dir)))
          (if (= 1 (length files))
              (car files)
            (seq-find (lambda(it) (js-import-string-match-p
                              js-import-file-index-regexp
                              it))
                      files)))
        (js-import-try-ext path)
        (js-import-try-ext (js-import-join-file path "index"))
        (when-let* ((package-json (js-import-join-when-exists
                                   path
                                   "package.json"))
                    (module (js-import-try-json-sections
                             package-json
                             '("main"))))
          (if (js-import-string-match-p js-import-file-ext-regexp
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
    (when-let ((content
                (condition-case nil
                    (decode-coding-string
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (setq buffer-file-coding-system 'binary)
                       (insert-file-contents-literally path)
                       (buffer-substring-no-properties (point-min) (point-max)))
                     'utf-8)
                  (error nil))))
      (condition-case nil
          (gethash section (json-read-from-string content))
        (error nil)))))

(defun js-import-find-package-json ()
  "Return the path to package.json."
  (when-let ((root (js-import-find-project-root)))
    (js-import-join-file root "package.json")))

(defun js-import-try-json-sections (path sections)
  (let (section)
    (while sections
      (setq section (js-import-read-package-json-section
                     path
                     (pop sections)))
      (if section
          (setq sections nil)
        (setq section nil)))
    section))

(defun js-import-join-when-exists (&rest args)
  "Return joined ARGS when exists."
  (let ((joined-path (apply 'js-import-join-file args)))
    (when (file-exists-p joined-path)
      joined-path)))

(defun js-import-find-imported-files ()
  "Return list of with imported imported paths in current buffer."
  (save-excursion
    (goto-char 0)
    (let (symbols)
      (with-syntax-table js-import-mode-syntax-table
        (while (js-import-re-search-forward
                js-import-regexp-import-keyword nil t 1)
          (when-let ((path (js-import-get-path-at-point)))
            (push path symbols))))
      (cl-remove-duplicates (reverse symbols) :test 'string=))))

(defun js-import-find-file-at-point ()
  "Find a file when cursor are placed under stringified path."
  (interactive)
  (let (path)
    (save-excursion
      (unless (js-import-inside-string-p)
        (beginning-of-line))
      (setq path (js-import-get-path-at-point)))
    (when path
      (js-import-find-file path))))

(cl-defun js-import-completing-read (prompt
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

(defun js-import-helm-build-symbols-sources ()
  "Build helm sources for symbols menu."
  (when (and (boundp 'helm-map)
             (fboundp 'helm-make-source))
    (unless js-import-export-symbols-map
      (setq js-import-export-symbols-map
            (js-import-build-helm-exports-keymap))
      (put 'js-import-export-symbols-map 'helm-only t))
    (unless js-import-imported-symbols-map
      (setq js-import-imported-symbols-map
            (js-import-build-helm-imported-keymap))
      (put 'js-import-imported-symbols-map 'helm-only t))
    (unless js-import-imported-symbols-source
      (setq js-import-imported-symbols-source
            (helm-make-source "Imported" 'helm-source-sync
              :candidates 'js-import-imported-candidates-in-buffer
              :candidate-transformer
              (lambda(candidates)
                (with-current-buffer js-import-current-buffer
                  (when js-import-current-export-path
                    (setq candidates
                          (js-import-filter-with-prop
                           :display-path
                           js-import-current-export-path
                           candidates))))
                candidates)
              :action '(("Jump" . js-import-jump-to-item-in-buffer)
                        ("Rename" . js-import-rename-import)
                        ("Quick delete" . js-import-delete-import-item)
                        ("Delete whole import" .
                         js-import-delete-import-statetement))
              :persistent-action (lambda(it)
                                   (js-import-jump-to-item-in-buffer
                                    (js-import-display-to-real-imports it)))
              :keymap js-import-imported-symbols-map
              :volatile t
              :display-to-real 'js-import-display-to-real-imports
              :persistent-help "Show symbol"
              :marked-with-props 'withprop)))
    (unless js-import-exports-source
      (setq js-import-exports-source
            (helm-make-source "Exports" 'helm-source-sync
              :header-name (lambda (_name)
                             (with-current-buffer js-import-current-buffer
                               (format "Exports in %s"
                                       (or js-import-current-export-path
                                           buffer-file-name))))
              :candidates 'js-import-init-exports-candidates
              :candidate-transformer 'js-import-exported-candidates-transformer
              :filtered-candidate-transformer
              'js-import-export-filtered-candidate-transformer
              :marked-with-props 'withprop
              :volatile t
              :keymap 'js-import-export-symbols-map
              :action '()
              :action-transformer
              (lambda(_candidate _actions)
                (if (with-current-buffer js-import-current-buffer
                      js-import-current-export-path)
                    '(("Import" . (lambda(_it)
                                    (mapc
                                     'js-import-insert-import
                                     (mapcar 'js-import-display-to-real-exports
                                             (helm-marked-candidates)))))
                      ("Import as " . (lambda(_it)
                                        (js-import-insert-import-as
                                         (helm-get-selection nil 'with-prop))))
                      ("Jump to export" . js-import-jump-to-item-other-window)
                      ("Jump to definition" . js-import-find-export-definition))
                  '(("Jump" . (lambda(it)
                                (js-import-jump-to-item-in-buffer
                                 (js-import-display-to-real-exports it)))))))
              :persistent-action
              (lambda(c)
                (when-let ((item (js-import-display-to-real-exports c)))
                  (when (js-import-get-prop item :pos)
                    (let ((js-buffer (get-buffer-create "*helm-js-import*")))
                      (cl-flet ((preview
                                 (item)
                                 (setq item (or (js-import-find-definition item)
                                                item))
                                 (when-let ((item-path (or
                                                        (js-import-get-prop
                                                         item
                                                         :real-path)))
                                            (pos (js-import-get-prop
                                                  item
                                                  :pos)))
                                   (switch-to-buffer-other-window
                                    js-buffer)
                                   (setq inhibit-read-only t)
                                   (erase-buffer)
                                   (js-import-insert-buffer-or-file
                                    item-path)
                                   (let ((buffer-file-name item-path))
                                     (set-auto-mode))
                                   (goto-char pos)
                                   (js-import-highlight-word)
                                   (font-lock-ensure)
                                   (setq inhibit-read-only nil))))
                        (if (and (helm-attr 'previewp)
                                 (equal item (helm-attr 'current-candidate)))
                            (progn
                              (kill-buffer js-buffer)
                              (helm-attrset 'previewp nil))
                          (preview item)
                          (helm-attrset 'previewp t)))
                      (helm-attrset 'current-candidate item)))))))
      (unless js-import-definitions-source
        (setq js-import-definitions-source
              (helm-make-source "Definitions" 'helm-source-sync
                :candidates (lambda() (with-current-buffer js-import-current-buffer
                                   (js-import-search-backward-identifiers
                                    buffer-file-name (point))))
                :marked-with-props 'withprop
                :volatile t
                :action '(("Jump" .
                           (lambda(_it)
                             (js-import-jump-to-item-in-buffer
                              (helm-get-selection nil 'withprop)))))))))))

(defun js-import-build-helm-exports-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map)
                         (fboundp 'helm-run-after-exit)
                         helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key
      map (kbd "C-c o")
      (lambda() (interactive)
        (helm-run-after-exit 'js-import-jump-to-item-other-window
                             (helm-get-selection nil 'withprop))))
    (define-key
      map
      (kbd "C-c C-j")
      (lambda() (interactive)
        (helm-run-after-exit 'js-import-find-export-definition
                             (helm-get-selection nil 'withprop))))
    map))

(defun js-import-build-helm-imported-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map) helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key map
      (kbd "M-d") (lambda()
                    (interactive)
                    (when (and (boundp 'helm-alive-p) helm-alive-p)
                      (helm-attrset
                       'js-import-delete-import-item
                       '(js-import-delete-import-item . never-split))
                      (helm-execute-persistent-action
                       'js-import-delete-import-item)
                      (js-import-init-exports-candidates)
                      (helm-refresh))))
    (define-key map
      (kbd "M-D") (lambda()
                    (interactive)
                    (when (and (boundp 'helm-alive-p) helm-alive-p)
                      (helm-attrset
                       'js-import-delete-import-statetement
                       '(js-import-delete-import-statetement . never-split))
                      (helm-execute-persistent-action
                       'js-import-delete-import-statetement)
                      (helm-refresh))))
    (define-key map
      (kbd "M-r") (lambda()
                    (interactive)
                    (helm-exit-and-execute-action 'js-import-rename-import)))
    map))

(defun js-import-preselect-file ()
  "Preselect function for file sources."
  (if-let ((path (with-current-buffer js-import-current-buffer
                   (or js-import-last-export-path
                       js-import-current-export-path
                       (when (> (point-max) (point))
                         (js-import-get-path-at-point))))))
      (regexp-quote path)
    ""))

(defun js-import-preselect-symbol ()
  "Preselect function for symbols."
  (or (when-let ((pos (> (point-max) (point)))
                 (symbol (js-import-which-word)))
        (unless (or (js-import-invalid-name-p symbol)
                    (js-import-reserved-word-p symbol))
          symbol))
      ""))

(defun js-import-reset-all-sources ()
  "Reset file and symbol sources. Also remove cache."
  (interactive)
  (remhash (js-import-find-project-root) js-import-dependencies-cache)
  (setq js-import-buffer-files-source nil)
  (setq js-import-project-files-source nil)
  (setq js-import-node-modules-source nil)
  (setq js-import-definitions-source nil)
  (setq js-import-exports-source nil)
  (setq js-import-imported-symbols-source nil))

(defun js-import-display-to-real-exports (str)
  "Find STR in the variable `js-import-export-candidates-in-path'."
  (with-current-buffer js-import-current-buffer
    (seq-find (lambda(elt) (string= str (js-import-get-prop elt :as-name)))
              js-import-export-candidates-in-path)))

(defun js-import-display-to-real-imports (item)
  "Find ITEM in the variable `js-import-cached-imports-in-buffer.'."
  (with-current-buffer js-import-current-buffer
    (seq-find (lambda(elt) (string= elt item))
              js-import-cached-imports-in-buffer item)))

(defun js-import-imported-candidates-in-buffer (&optional buffer)
  "Returns imported symbols in BUFFER which are cached and stored
in a buffer local variable `js-import-cached-imports-in-buffer'.
   Cache are invalidated when `buffer-modified-tick' is changed."
  (with-current-buffer (or buffer js-import-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq js-import-buffer-tick tick)
          js-import-cached-imports-in-buffer
        (progn
          (setq js-import-buffer-tick tick)
          (setq js-import-cached-imports-in-buffer
                (seq-remove 'js-import-reserved-word-p
                            (js-import-extract-imports buffer-file-name)))
          js-import-cached-imports-in-buffer)))))

(defun js-import-exported-candidates-transformer (candidates)
  "Remove duplicates and imported members from CANDIDATES plist."
  (with-current-buffer js-import-current-buffer
    (let (imports exports)
      (setq imports (js-import-imported-candidates-in-buffer
                     js-import-current-buffer))
      (setq imports (js-import-filter-with-prop
                     :display-path
                     js-import-current-export-path
                     imports))
      (setq exports (if imports (js-import-filter-exports candidates imports)
                      candidates))
      (setq exports (cl-remove-duplicates exports))
      (setq exports (mapcar (lambda(c) (js-import-propertize
                                   c :display-path
                                   js-import-current-export-path))
                            exports))
      exports)))

(defun js-import-export-filtered-candidate-transformer (candidates &optional
                                                                  _source)
  (mapcar (lambda(c) (js-import-get-prop c :as-name))
          candidates))

(defun js-import-filter-with-prop (property value items)
  "Return filtered ITEMS with members whose PROPERTY equals VALUE."
  (seq-filter (lambda(str) (string= (js-import-get-prop str property) value))
              items))

(defun js-import-find-by-prop (property value list)
  "Find item in LIST whose PROPERTY equals VALUE."
  (seq-find (lambda(str) (equal (js-import-get-prop str property) value))
            list))

(defun js-import-filter-exports (exports imports)
  "Return EXPORTS plist with only those members that are not in IMPORTS plist."
  (let* ((find-in-imports (lambda(key value) (js-import-find-by-prop key value
                                                                imports)))
         (by-type (lambda(elt) (let ((type (js-import-get-prop elt :type))
                                (name (js-import-get-prop elt :as-name)))
                            (pcase type
                              (1
                               (funcall find-in-imports :type type))
                              (4
                               (funcall find-in-imports :as-name name))
                              (16 (< 0 (length imports))))))))
    (seq-remove by-type exports)))

(defun js-import-extract-imports (real-path)
  (js-import-with-buffer-or-file-content real-path
      (goto-char 0)
    (let (symbols)
      (while (js-import-re-search-forward
              js-import-regexp-import-keyword nil t 1)
        (js-import-skip-whitespace-forward)
        (let (display-path imports)
          (cond ((js-import-looking-at "*")
                 (let (beg end renamed-name)
                   (setq beg (point))
                   (forward-char)
                   (skip-chars-forward "\s\t")
                   (setq end (point))
                   (when (js-import-looking-at "as")
                     (skip-chars-forward "as")
                     (setq end (point))
                     (skip-chars-forward "\s\t")
                     (setq renamed-name (js-import-which-word))
                     (if (or (js-import-reserved-word-p renamed-name)
                             (js-import-invalid-name-p renamed-name))
                         (setq renamed-name "")
                       (skip-chars-forward js-import-regexp-name)))
                   (setq end (point))
                   (push (js-import-make-item
                          (format "%s" (buffer-substring-no-properties beg end))
                          :type 16
                          :real-name "*"
                          :as-name renamed-name
                          :pos beg)
                         imports)))
                ((js-import-get-word-if-valid)
                 (when-let ((name (js-import-get-word-if-valid)))
                   (push (js-import-make-item
                          name
                          :type 1
                          :real-name name
                          :as-name name
                          :pos (point))
                         imports)
                   (skip-chars-forward name)
                   (js-import-skip-whitespace-forward)
                   (skip-chars-forward ",")
                   (js-import-skip-whitespace-forward))))
          (when (looking-at-p "{")
            (let ((map-func (lambda(item) (js-import-propertize
                                      item
                                      :type 4
                                      :pos
                                      (js-import-get-prop item :start))))
                  (named-items (js-import-extract-esm-braced-symbols
                                js-import-name-as--re)))
              (setq named-items (mapcar map-func named-items))
              (setq imports (append imports named-items))))
          (unless (looking-at-p "[\"']")
            (js-import-re-search-forward js-import-from-keyword--re nil t 1)
            (js-import-skip-whitespace-forward)
            (when (looking-at-p "[\"']")
              (save-excursion
                (forward-char 1)
                (setq display-path (js-import-get-path-at-point)))
              (setq imports (mapcar (lambda(it)
                                      (js-import-propertize it
                                                            :display-path
                                                            display-path
                                                            :real-path
                                                            real-path
                                                            :import t))
                                    imports))
              (setq symbols (append symbols imports))))))
      symbols)))

(defun js-import-extract-esm-braced-symbols (&optional re)
  (save-excursion
    (when-let* ((brace-start (when (looking-at-p "{") (point)))
                (brace-end (save-excursion (forward-list) (point))))
      (let (items)
        (save-restriction
          (narrow-to-region brace-start brace-end)
          (js-import-remove-comments)
          (while (js-import-re-search-forward re nil t 1)
            (when-let* ((start (match-beginning 0))
                        (end (match-end 0))
                        (parts (split-string (buffer-substring-no-properties
                                              start end)))
                        (full-name (mapconcat 'identity parts "\s")))
              (push (js-import-make-item full-name
                                         :real-name (car parts)
                                         :as-name (car (reverse parts))
                                         :start start
                                         :end end)
                    items))))
        items))))

(defun js-import-init-exports-candidates ()
  "Search exports in a file.
File is specified in the variable `js-import-current-export-path.'."
  (with-current-buffer js-import-current-buffer
    (setq js-import-export-candidates-in-path
          (js-import-extract-all-exports (or js-import-current-export-path
                                             buffer-file-name)))
    js-import-export-candidates-in-path))

(defun js-import-extract-all-exports (&optional init-path)
  "Return exports in PATH defined with CommonJs syntax."
  (let* ((symbols)
         (external-paths)
         (path)
         (processed-paths)
         (map-result (lambda(result)
                       (cond ((listp result)
                              (setq result (seq-remove 'null result))
                              (setq symbols (append result symbols)))
                             ((= 16 (js-import-get-prop result :type))
                              (when-let ((external-path (js-import-path-to-real
                                                         (js-import-get-prop
                                                          result
                                                          :display-path))))
                                (unless (or (member external-path
                                                    processed-paths)
                                            (member external-path
                                                    external-paths))
                                  (push external-path external-paths))))
                             ((not (null result))
                              (push result symbols))))))
    (push (js-import-path-to-real init-path) external-paths)
    (while external-paths
      (setq path (pop external-paths))
      (push path processed-paths)
      (when path
        (js-import-with-buffer-or-file-content path
            (goto-char (point-min))
          (while
              (js-import-re-search-forward
               js-import-esm-export-keyword--re nil t 1)
            (unless (or (js-import-inside-comment-p)
                        (js-import-inside-string-p (point))
                        (not (looking-at "[\s\t\n]\\|[/][*/]"))
                        (save-excursion
                          (skip-chars-backward "export")
                          (unless (<= (point) (point-min))
                            (backward-char 1)
                            (not (looking-at "[\s\t\n;/*]")))))
              (js-import-skip-whitespace-forward)
              (when-let ((result (js-import-make-esm-export-at-point
                                  buffer-file-name)))
                (funcall map-result result))))
          (unless symbols
            (goto-char (point-min))
            (while (js-import-re-search-forward
                    js-import-cjs-export-keyword--re nil t 1)
              (js-import-skip-whitespace-forward)
              (when-let ((result (js-import-extract-cjs-exports
                                  buffer-file-name)))
                (funcall map-result result)))))))
    (mapcar (lambda(it) (js-import-propertize it :export t))
            (reverse symbols))))

(defun js-import-make-esm-export-at-point (&optional path)
  "Return exports in PATH defined with ES Module syntax."
  (cond ((looking-at-p "\\*")
         (let ((start (point))
               (as-name)
               (full-name)
               (end))
           (forward-char 1)
           (js-import-skip-whitespace-forward)
           (pcase (js-import-which-word)
             ("as" (progn (re-search-forward "as" nil t 1)
                          (js-import-skip-whitespace-forward)
                          (setq as-name (js-import-get-word-if-valid))
                          (skip-chars-forward as-name)
                          (setq end (point))
                          (setq full-name (concat "* as" as-name))
                          (js-import-make-item full-name
                                               :type 4
                                               :as-name as-name
                                               :real-name "*"
                                               :real-path path
                                               :end end
                                               :pos start)))
             ("from" (when-let ((from (js-import-get-path-at-point)))
                       (js-import-make-item "*"
                                            :type 16
                                            :as-name "*"
                                            :real-name "*"
                                            :display-path from
                                            :real-path path
                                            :pos start))))))
        ((looking-at-p "{")
         (let ((symbols (js-import-extract-esm-braced-symbols
                         js-import-name-as--re))
               (map-func (lambda(it) (let ((type
                                       (if (equal "default"
                                                  (js-import-get-prop it
                                                                      :as-name))
                                           1 4))
                                      (pos (js-import-get-prop it :start)))
                                  (js-import-propertize it
                                                        :type type
                                                        :real-path path
                                                        :pos pos))))
               (from-path (progn
                            (forward-list)
                            (js-import-skip-whitespace-forward)
                            (when (js-import-looking-at "from")
                              (skip-chars-forward "from")
                              (js-import-skip-whitespace-forward)
                              (forward-char 1)
                              (js-import-get-path-at-point)))))
           (setq symbols (if from-path
                             (mapcar (lambda(it) (js-import-propertize (funcall
                                                                   map-func it)
                                                                  :display-path
                                                                  from-path))
                                     symbols)
                           (mapcar map-func symbols)))
           symbols))
        ((looking-at js-import-regexp-name-set)
         (let* ((stack (js-import-skip-reserved-words "\s\t*"))
                (default (assoc "default" stack))
                (real-name (car (seq-find (lambda(it) (js-import-valid-identifier-p
                                                  (car it)))
                                          stack)))
                (var-type (car
                           (seq-find (lambda(it)
                                       (member
                                        (car it)
                                        js-import-delcaration-keywords))
                                     stack)))
                (as-name real-name))
           (js-import-make-item
            (or real-name as-name)
            :type (if default 1 4)
            :real-name real-name
            :as-name as-name
            :real-path path
            :var-type var-type
            :pos (point))))))

(defun js-import-extract-cjs-exports (path)
  (cond ((looking-at-p "=\\([\s\t\n]+?\\)require[ \t('\"]")
         (re-search-forward "require[ \t('\"]" nil t 1)
         (skip-chars-forward "'\"")
         (when-let ((from (js-import-get-path-at-point)))
           (js-import-make-item "*"
                                :type 16
                                :as-name "*"
                                :real-name "*"
                                :display-path from
                                :real-path path
                                :pos (point))))
        ((looking-at-p "=\\([\s\t]+?\\){")
         (js-import-re-search-forward "=" nil t 1)
         (js-import-skip-whitespace-forward)
         (when-let* ((items (js-import-parse-object-keys)))
           (when (looking-at-p "{")
             (forward-list))
           (mapcar (lambda(cell)
                     (let ((name (car cell)) )
                       (js-import-make-item
                        name
                        :pos (cdr cell)
                        :as-name name
                        :type 4
                        :real-path path
                        :real-name name)))
                   items)))
        ((looking-at-p "=[^=]")
         (forward-char 1)
         (js-import-skip-whitespace-forward)
         (when-let ((real-name (js-import-get-word-if-valid)))
           (js-import-make-item
            real-name
            :type 1
            :real-name real-name
            :as-name real-name
            :real-path path
            :pos (point))))
        ((looking-at-p "[.]")
         (forward-char 1)
         (when-let ((as-name (js-import-which-word)))
           (js-import-make-item
            as-name
            :type (if (string= as-name "default")  1 4)
            :as-name as-name
            :real-name (progn
                         (js-import-re-search-forward "=" nil t 1)
                         (js-import-skip-whitespace-forward)
                         (or (js-import-get-word-if-valid)
                             as-name))
            :real-path path
            :pos (point))))))

(defun js-import-parse-object-keys ()
  "Return object keys at point."
  (when-let ((start (when (looking-at-p "{")
                      (1+ (point))))
             (end (save-excursion (forward-list) (1- (point))))
             (re (concat js-import-regexp-name-set "[\s\t\n]*[:(,]")))
    (let (children)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (js-import-remove-comments)
          (while (js-import-re-search-forward re nil t 1)
            (let (prop delimiter)
              (setq delimiter (char-before))
              (save-excursion
                (backward-char)
                (skip-chars-backward "\s\t\n")
                (setq prop (js-import-which-word))
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
                         (when-let ((word (js-import-which-word))
                                    (pos (point)))
                           (skip-chars-forward word)
                           (skip-chars-forward "\s\t\n")
                           (push (cons word pos) children))))))))))
      (when children
        (seq-uniq (cl-remove 'null children))))))

(defun js-import-parse-destructive ()
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
              (setq parent (js-import-which-word))))
          (narrow-to-region (1+ start) (1- end))
          (js-import-remove-comments)
          (skip-chars-backward "},\s\t\n")
          (push (js-import-maybe-make-child-at-point parent) children)
          (while (re-search-backward "," nil t 1)
            (skip-chars-backward "},\s\t\n")
            (push (js-import-maybe-make-child-at-point parent) children))
          (seq-remove 'null children))))))

(defun js-import-skip-reserved-words (&optional separators)
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

(defun js-import-kill-thing-at-point (&optional $thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let* ((thing (or $thing 'sexp))
         (bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun js-import-import-backward-exist-p (path)
  (re-search-backward (concat "from +['\"]" path "['\"]") nil t))

(defun js-import-join-names (symbols)
  (when (and (listp symbols) (<= 1 (length symbols)))
    (js-import-join ", " symbols)))

(defun js-import-join-imports-names (default-name names)
  (let (parts)
    (when (stringp names) (push (concat "{ " names" }") parts))
    (when (stringp default-name) (push default-name parts))
    (js-import-join ", " (seq-remove 'null parts))))

(defun js-import-goto-last-import ()
  (goto-char (point-min))
  (while (re-search-forward js-import-regexp-import-keyword nil t)
    (re-search-forward "['\"]" nil t 2)
    (forward-line 1))
  (point))

(defun js-import-get-import-positions (path)
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

(defun js-import-make-item (candidate &rest plist)
  (apply 'js-import-propertize candidate plist))

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

(defun js-import-strip-text-props (item)
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

(defun js-import-skip-whitespace-forward ()
  (let ((pos (point))
        (curr-pos))
    (while (looking-at "[\n\s\t]\\|\\(//[^\n]*\\)\\|\\(/[*]\\)")
      (setq curr-pos (point))
      (js-import-re-search-forward "[\s\t\n]+" nil t 1))
    (unless (null curr-pos)
      (js-import-re-search-backward "[^\s\t\n]+" pos t 1)
      (unless (looking-at "/")
        (skip-chars-backward (js-import-which-word))
        (when (< pos (point))
          (setq curr-pos (point))))
      (goto-char curr-pos))))

(defun js-import-skip-whitespace-backward ()
  (while (save-excursion
           (backward-char 1)
           (looking-at "[\n\s\t]\\|\\(//[^\n]\\)\\|\\([*]\\\)"))
    (js-import-re-search-backward "[\s\t\n]+" nil t 1)))

(defun js-import-get-word-if-valid ()
  "Return word at point if it is valid and not reservered, otherwise nil."
  (when-let* ((word (js-import-which-word))
              (valid (js-import-valid-identifier-p word)))
    word))

(defun js-import-looking-at (str)
  (when-let ((word (js-import-which-word)))
    (string= word str)))

(defun js-import-which-word (&optional re)
  "Find closest to point whole word."
  (unless re (setq re js-import-regexp-name))
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

(defun js-import-get-path-at-point ()
  (save-excursion
    (when-let* ((word (js-import-which-word))
                (meta-word (or (string= "import" word)
                               (string= "export" word)
                               (string= "from" word))))
      (if (string= word "from")
          (search-forward-regexp "['\"]" nil t 1)
        (search-forward-regexp "[\s\t\n]+from[\s\t\n]+['\"]" nil t 1)))
    (when (js-import-inside-string-p)
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
  (with-syntax-table js-import-mode-syntax-table
    (nth 3 (syntax-ppss (or pos (point))))))

(defun js-import-skip-string ()
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

(defun js-import-invalid-name-p (str)
  "Return t when STR mathces any characters which are not allowed in js."
  (js-import-string-match-p (concat "[" "^" js-import-regexp-name "]") str))

(defun js-import-valid-identifier-p (str)
  "Return t if STR is a valid variable name, otherwise nil."
  (not (or (js-import-invalid-name-p str)
           (js-import-reserved-word-p str))))

(defun js-import-propose-name (candidate)
  (let* ((parts (split-string candidate))
         (type (js-import-get-prop candidate :type))
         (current-name (car parts))
         (display-path (js-import-get-prop candidate :display-path))
         (proposed-symbol (pcase type
                            (1 (if (js-import-valid-identifier-p candidate)
                                   candidate
                                 (js-import-generate-name-from-path
                                  display-path)))
                            (4 (js-import-generate-name-from-path
                                display-path))
                            (16 (js-import-generate-name-from-path
                                 display-path))))
         (prompt (format "%s %s"
                         (pcase type
                           (1 "Import (default: %s): ")
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

(defun js-import-generate-name-from-path (path)
  "Generate name for default or module import from PATH."
  (let* ((split-path (lambda(str) (split-string str "[ \f\t\n\r\v/.-]")))
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

(defun js-import-compose-from (arg &rest funcs)
  "Performs right-to-left unary function composition."
  (seq-reduce (lambda (xs fn)
                (funcall fn xs))
              (reverse funcs) arg))

(defun js-import-maybe-make-child-at-point (&optional parent)
  (when-let ((valid-id (js-import-get-word-if-valid)))
    (skip-chars-backward valid-id)
    (js-import-make-item valid-id
                         :pos (point)
                         :real-name valid-id
                         :parent parent
                         :as-name valid-id)))

(cl-defun js-import-highlight-word (&key pos limit buffer face secs jump)
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

(defun js-import-extract-parent-arguments (&optional parens-positions)
  (save-excursion
    (with-syntax-table js-import-mode-syntax-table
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
                    (js-import-re-search-backward
                     js-import-delcaration-keywords--re nil t 1)
                    (when (looking-at js-import-delcaration-keywords--re)
                      (skip-chars-forward (js-import-which-word))
                      (skip-chars-forward "\s\t\n*")
                      (setq parent (js-import-get-word-if-valid)))))
            (when parent
              (when (looking-back "=>" 1)
                (backward-char 2)
                (skip-chars-backward "\s\t\n")
                (when-let ((child
                            (js-import-maybe-make-child-at-point parent)))
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
                                    (js-import-parse-destructive))))
                      ((looking-at js-import-regexp-name-set)
                       (goto-char (1- end-pos))
                       (skip-chars-backward ",\s\t\n")
                       (while (and (or (looking-at ",")
                                       (looking-at ")"))
                                   (not (looking-at "(")))
                         (if (looking-at ",")
                             (skip-chars-backward "\s\t\n,")
                           (skip-chars-backward "\s\t\n"))
                         (let ((child
                                (js-import-maybe-make-child-at-point
                                 parent)))
                           (push child items)
                           (skip-chars-backward child)
                           (skip-chars-backward "\s\t\n"))
                         (unless
                             (js-import-re-search-backward
                              "," start t 1)
                           (goto-char start))))))
              (setq children (append children items)))))
        children))))

(defun js-import-next-declaration-or-scope (&optional pos)
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
      (with-syntax-table js-import-mode-syntax-table
        (setq init-depth (nth 0 (syntax-ppss (point))))
        (when-let ((found
                    (js-import-re-search-forward
                     js-import-open-paren-re nil t 1)))
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
                 (js-import-skip-whitespace-backward)
                 (and (char-equal (char-before (point)) ?>)
                      (char-equal (char-before (1- (point))) ?=)
                      (js-import-re-search-backward
                       js-import-expression-keywords--re nil t 1))
                 (forward-word)
                 (js-import-skip-whitespace-forward)
                 (when (js-import-valid-identifier-p (js-import-which-word))
                   (setq scope-start (point)))))))
    (save-excursion
      (goto-char pos)
      (with-syntax-table js-import-mode-syntax-table
        (save-excursion
          (when (looking-at js-import-delcaration-keywords--re)
            (skip-chars-forward js-import-regexp-name))
          (when (js-import-re-search-forward
                 js-import-delcaration-keywords--re nil t 1)
            (if (js-import-inside-string-p)
                (js-import-skip-string)
              (progn (setq declaration-depth (nth 0 (syntax-ppss)))
                     (skip-chars-backward js-import-regexp-name)
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

(defun js-import-previous-declaration-or-skope (&optional pos)
  (unless pos (setq pos (point)))
  (let (declaration-start scope-start scope-end winner)
    (with-syntax-table js-import-mode-syntax-table
      (goto-char pos)
      (when (js-import-re-search-backward js-import-closed-paren-re nil t 1)
        (setq scope-end (1+ (point)))
        (when (nth 1 (syntax-ppss (point)))
          (goto-char (nth 1 (syntax-ppss (point))))
          (setq scope-start (point))
          (skip-chars-backward "\s\t\n,"))))
    (with-syntax-table js-import-mode-syntax-table
      (goto-char pos)
      (when (and (js-import-re-search-backward
                  js-import-delcaration-keywords--re nil t 1)
                 (not (js-import-inside-string-p)))
        (unless (js-import-inside-string-p)
          (setq declaration-start (point)))))
    (goto-char pos)
    (if (and declaration-start scope-end scope-start)
        (setq winner (if (> scope-end declaration-start)
                         scope-start
                       declaration-start))
      (setq winner (or scope-start declaration-start)))
    (when winner (goto-char winner))))

(defun js-import-previous-declaration (&optional pos)
  (let ((init-pos (or pos (point)))
        (curr-pos))
    (goto-char init-pos)
    (while (and (not curr-pos)
                (js-import-previous-declaration-or-skope))
      (when (js-import-declaration-at-point)
        (setq curr-pos (point))))
    (when (null curr-pos)
      (goto-char init-pos))
    (unless (equal curr-pos init-pos)
      curr-pos)))

(defun js-import-looking-at-function-expression ()
  (and (js-import-looking-at "function")
       (> (point-min) (point))
       (save-excursion
         (js-import-skip-whitespace-backward)
         (let ((c (char-before (point))))
           (or (char-equal c ?=)
               (char-equal c ?|)
               (char-equal c ??)
               (char-equal c ?:))))))

(defun js-import-declaration-at-point ()
  (when-let ((token-p (and (looking-at js-import-delcaration-keywords--re)
                           (not (js-import-looking-at-function-expression))))
             (var-type (js-import-which-word)))
    (save-excursion
      (js-import-re-search-forward var-type nil t 1)
      (when (looking-at "\\*")
        (forward-char 1))
      (js-import-skip-whitespace-forward)
      (when (looking-at "\\*")
        (forward-char 1)
        (js-import-skip-whitespace-forward))
      (when-let* ((word (js-import-which-word))
                  (pos (point)))
        (if (js-import-valid-identifier-p word)
            (js-import-make-item word
                                 :pos pos
                                 :var-type var-type
                                 :real-name word
                                 :as-name word)
          (cond
           ((string= word "{")
            (let (parent children)
              (when (looking-at-p "{")
                (save-excursion
                  (forward-list)
                  (when (looking-at-p "[\s\t\n]*=[\s\t\n]*")
                    (js-import-re-search-forward "[\s\t\n]*=[\s\t\n]*" nil t 1)
                    (setq parent (js-import-which-word))))
                (setq children (js-import-parse-destructive))
                (setq children (mapcar
                                (lambda(it)
                                  (js-import-propertize
                                   it :var-type
                                   var-type :parent parent))
                                children)))
              children))))))))

(defun js-import-search-backward-identifiers (&optional path position)
  "Extract visible on POSITION declarations in PATH.
By default PATH is taken from a variable `buffer-file-name'.
Default value for POSITION also current point position."
  (unless path (setq path buffer-file-name))
  (unless position (setq position (if (string= path buffer-file-name)
                                      (point)
                                    (point-max))))
  (let (ids depth depth-position)
    (js-import-with-buffer-or-file-content path
        (goto-char position)
      (setq ids (ignore-errors (js-import-extract-parent-arguments)))
      (skip-chars-forward js-import-regexp-name)
      (while (js-import-previous-declaration)
        (unless depth
          (setq depth-position (point))
          (setq depth (nth 0 (syntax-ppss depth-position))))
        (when-let ((decl (js-import-declaration-at-point)))
          (if (listp decl)
              (setq ids (append ids decl))
            (push decl ids))))
      (unless (or (null depth) (null depth-position))
        (save-excursion
          (goto-char depth-position)
          (while (ignore-errors (js-import-next-declaration-or-scope))
            (when-let ((decl (js-import-declaration-at-point)))
              (if (listp decl)
                  (setq ids (append ids decl))
                (push decl ids)))))))
    (mapcar (lambda(it) (js-import-propertize it :real-path path))
            ids)))

(defun js-import-find-definition (item)
  (let ((stack)
        (current-item item))
    (while (and current-item
                (not (js-import-get-prop current-item :var-type)))
      (let ((item-path (js-import-get-prop current-item :real-path)))
        (push current-item stack)
        (setq current-item
              (cond ((js-import-get-prop current-item :import)
                     (when-let* ((from-path (js-import-get-prop current-item
                                                                :display-path))
                                 (dir (js-import-dirname item-path))
                                 (path (js-import-path-to-real from-path dir))
                                 (exports (js-import-extract-all-exports path))
                                 (item-type (js-import-get-prop
                                             current-item :type))
                                 (export-name (js-import-get-prop
                                               current-item :real-name)))
                       (pcase item-type
                         (1 (js-import-find-by-prop :type item-type exports))
                         (4 (js-import-find-by-prop :as-name export-name
                                                    exports)))))
                    ((and (js-import-get-prop current-item :display-path)
                          (js-import-get-prop current-item :export))
                     (when-let* ((from-path (js-import-get-prop current-item
                                                                :display-path))
                                 (dir (js-import-dirname item-path))
                                 (path (js-import-path-to-real from-path dir))
                                 (exports (js-import-extract-all-exports path))
                                 (export-name (js-import-get-prop
                                               current-item :real-name))
                                 (item-type (if (equal export-name "default") 1
                                              (js-import-get-prop
                                               current-item :type))))
                       (pcase item-type
                         (1 (js-import-find-by-prop :type item-type exports))
                         (4 (js-import-find-by-prop :as-name export-name
                                                    exports)))))
                    ((js-import-get-prop current-item :export)
                     (let* ((path (js-import-get-prop current-item :real-path))
                            (pos (js-import-get-prop current-item :pos))
                            (definitions (js-import-search-backward-identifiers
                                          path pos))
                            (real-name (js-import-get-prop
                                        current-item :real-name)))
                       (or (js-import-find-by-prop
                            :real-name real-name definitions)
                           (js-import-find-by-prop
                            :as-name real-name
                            (js-import-extract-imports path))))))))
      current-item)
    (setq current-item (or current-item (pop stack)))
    (if (and current-item stack)
        (js-import-propertize current-item :stack stack)
      current-item)))

(defun js-import-jump-to-item-in-buffer (item &optional buffer)
  "Jumps to ITEM in buffer. ITEM must be propertized with a keyword `pos'."
  (when-let ((pos (js-import-get-prop item :pos)))
    (js-import-highlight-word :pos pos :buffer buffer :jump t)
    item))

(defun js-import-jump-to-item-other-window (item)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop :pos."
  (unless (js-import-get-prop item :pos)
    (setq item (js-import-display-to-real-exports item)))
  (when-let ((pos (js-import-get-prop item :pos))
             (item-path (or (js-import-get-prop item :real-path)
                            (js-import-path-to-real (js-import-get-prop
                                                     item
                                                     :display-path)))))
    (unless (and buffer-file-name (string= item-path buffer-file-name))
      (find-file-other-window item-path))
    (js-import-jump-to-item-in-buffer item)
    item))

(defun js-import-map-stack (cands)
  (seq-map-indexed
   (lambda(c i) (let* ((real-path (js-import-get-prop c :real-path))
                  (short-path (if (equal (with-current-buffer
                                             js-import-current-buffer
                                           buffer-file-name)
                                         real-path)
                                  "current buffer"
                                (replace-regexp-in-string
                                 (concat "^" (js-import-slash
                                              js-import-current-project-root))
                                 ""
                                 real-path )))
                  (display-path (js-import-get-prop c :display-path))
                  (export (and (js-import-get-prop c :export)
                               (if display-path
                                   (format "reexport from %s" display-path)
                                 "export")))
                  (import (and (js-import-get-prop c :import)
                               (format "import from %s" display-path)))
                  (var (js-import-get-prop c :var-type))
                  (indents (unless (= i 0) (make-string i ?\s)))
                  (parts (seq-remove 'null
                                     (list indents c "/" export import var
                                           (and display-path
                                                "from")
                                           display-path "in" short-path))))
             (js-import-propertize (mapconcat 'identity parts "\s")
                                   :pos (js-import-get-prop c :pos)
                                   :var-type (js-import-get-prop c :var-type)
                                   :real-path (js-import-get-prop
                                               c :real-path))))
   cands))

(defun js-import-find-export-definition (export-symbol)
  "Jumps to ITEM in buffer. ITEM must be propertized with prop :pos."
  (unless (js-import-get-prop export-symbol :pos)
    (setq export-symbol (js-import-display-to-real-exports export-symbol)))
  (when-let* ((definition (js-import-find-definition export-symbol))
              (pos (js-import-get-prop definition :pos))
              (item-path (or (js-import-get-prop definition :real-path)
                             (js-import-path-to-real (js-import-get-prop
                                                      definition
                                                      :display-path)))))
    (find-file-other-window item-path)
    (js-import-jump-to-item-in-buffer definition)
    definition))

(defun js-import-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js-import-re-search-forward'."
  (let ((parse)
        str-terminator)
    (while (> count 0)
      (with-syntax-table js-import-mode-syntax-table
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

(defun js-import-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring comments and strings."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js-import-re-search-backward-inner)
               ((> count 0) #'js-import-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun js-import-re-search-backward-inner (regexp &optional bound count)
  "Helper for `js-import-re-search-backward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table js-import-mode-syntax-table
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

(defun js-import-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring strings and comments."
  (js-import-re-search-forward regexp bound noerror (if count (- count) -1)))

(defun js-import-remove-comments (&optional start end)
  "Replaces comments in buffer beetween START and END with empty lines."
  (let ((comments (js-import-get-comments-bounds start end)))
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

(defun js-import-get-comments-bounds (&optional start end)
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (save-restriction
      (with-syntax-table js-import-mode-syntax-table
        (let (comments)
          (goto-char start)
          (while (re-search-forward "\\(/\\*\\)\\|\\(//\\)\\|[\"'`]" nil t 1)
            (if (save-excursion
                  (backward-char 1)
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

(defun js-import-insert-buffer-or-file (path)
  "A function inserts content either from buffer or file.
It depends whether buffer with the given PATH exists.
Without argument KEEP-COMMENTS content will inserted without comments."
  (when (and path (file-exists-p path))
    (if (get-file-buffer path)
        (progn
          (insert-buffer-substring-no-properties (get-file-buffer path)))
      (progn
        (let ((buffer-file-name path))
          (insert-file-contents path))))))

(defun js-import-rename-import (candidate)
  "Rename imported CANDIDATE in buffer."
  (save-excursion
    (save-restriction
      (pcase (js-import-get-prop candidate :type)
        (1 (js-import-rename-default-item candidate))
        (4 (js-import-rename-as candidate))
        (16 (js-import-rename-as candidate))))))

(defun js-import-rename-default-item (item)
  "Renames default imported ITEM."
  (let (real-name new-name overlay end beg)
    (setq real-name (or (js-import-get-prop item :real-name)
                        (js-import-strip-text-props item)))
    (setq beg (js-import-get-prop item :pos))
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
                     (progn
                       (remove-overlays beg end)
                       (let ((case-fold-search nil)
                             (regexp (concat "\\_<" real-name "\\_>")))
                         (query-replace-regexp regexp new-name)))))
        (remove-overlays beg end)))))

(defun js-import-rename-as (item)
  "Rename named imports and module imports."
  (let* ((pos (js-import-get-prop item :pos))
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
            (skip-chars-forward "\s\t\n")
            (skip-chars-forward "as")
            (skip-chars-forward "\s\t\n")
            (when (and renamed-name
                       (string= renamed-name (js-import-which-word)))
              (query-replace-regexp (concat "\\_<" renamed-name "\\_>")
                                    new-name)))
        (progn
          (insert (format " as %s" new-name))
          (query-replace-regexp (concat "\\_<" real-name "\\_>") new-name))))))

(defun js-import-insert-import (candidate)
  "Insert CANDIDATE into existing or new import statement."
  (save-excursion
    (let ((as-name (js-import-get-prop candidate :as-name))
          (type (js-import-get-prop candidate :type))
          (display-path js-import-last-export-path))
      (pcase type
        (1 (js-import-insert-exports
            (js-import-propose-name candidate) nil display-path))
        (4 (js-import-insert-exports
            nil
            (js-import-strip-text-props as-name)
            display-path))
        (16 (js-import-insert-exports
             (js-import-propose-name candidate)
             nil display-path))))))

(defun js-import-insert-import-as (candidate)
  "Insert and renames CANDIDATE into existing or new import statement."
  (let* ((type (js-import-get-prop candidate :type))
         (normalized-path (js-import-get-prop candidate :display-path))
         (real-name (or (js-import-get-prop candidate :as-name) candidate))
         (renamed-name (car (split-string
                             (read-string
                              (format "import %s as " real-name)))))
         (full-name (concat real-name " as " renamed-name)))
    (pcase type
      (1 (js-import-insert-exports renamed-name nil normalized-path))
      (4 (js-import-insert-exports nil full-name normalized-path))
      (16 (js-import-insert-exports full-name nil normalized-path)))))

(defun js-import-insert-exports (default-name named-list path)
  (let ((names (if (stringp named-list)
                   named-list
                 (js-import-join-names named-list)))
        (imports (reverse (js-import-find-imported-files))))
    (save-excursion
      (js-import-goto-last-import)
      (if (member path imports)
          (js-import-add-to-current-imports path default-name names)
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
            (forward-line))
          (insert "import " (js-import-join-imports-names
                             default-name names)
                  " from " js-import-quote path js-import-quote ";\n")))
      (js-import-goto-last-import)
      (unless (looking-at-p "\n")
        (newline-and-indent)))))

(defun js-import-add-to-current-imports (path default-name &optional names)
  (when-let* ((bounds (js-import-get-import-positions path))
              (start (car bounds))
              (end (cdr bounds)))
    (save-excursion
      (save-restriction
        (goto-char start)
        (narrow-to-region start end)
        (forward-word)
        (js-import-skip-whitespace-forward)
        (if (looking-at-p "\\*")
            (progn
              (goto-char end)
              (skip-chars-forward ";")
              (newline-and-indent)
              (insert "import " (js-import-join-imports-names
                                 default-name names)
                      " from " js-import-quote path js-import-quote ";"))
          (when default-name
            (when (looking-at-p js-import-regexp-name-set)
              (js-import-kill-thing-at-point 'sexp))
            (insert default-name))
          (when (or (looking-at-p js-import-regexp-name-set)
                    default-name)
            (skip-chars-forward js-import-regexp-name-set)
            (js-import-skip-whitespace-forward)
            (unless (looking-at-p ",")
              (js-import-re-search-backward js-import-regexp-name-set nil t 1)
              (forward-char 1)
              (insert ", "))
            (skip-chars-forward ",")
            (js-import-skip-whitespace-forward))
          (when names
            (if (looking-at-p "{")
                (progn (js-import-re-search-forward "}" nil t 1)
                       (backward-char 1)
                       (js-import-skip-whitespace-backward)
                       (let ((separator (if (save-excursion (backward-char 1)
                                                            (looking-at-p ","))
                                            " "
                                          ", ")))
                         (insert separator names)))
              (insert "{" names "}\s"))))))))

(defun js-import-delete-import-statetement (candidate)
  "Remove whole import statement of CANDIDATE.
CANDIDATE should be propertizied with property `display-path'."
  (when-let* ((path (js-import-get-prop candidate :display-path))
              (bounds (js-import-get-import-positions path)))
    (delete-region (car bounds) (cdr bounds))
    (join-line)))

(defun js-import-delete-import-item (candidate)
  "Remove CANDIDATE from import statement in buffer."
  (let* ((display-path (js-import-get-prop candidate :display-path))
         (type (js-import-get-prop candidate :type))
         (other-imports (js-import-filter-with-prop
                         :display-path display-path
                         js-import-cached-imports-in-buffer))
         (whole-import-bounds (js-import-get-import-positions display-path))
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
                (overlay-put overlay 'face 'js-import-highlight-face)
                (when (yes-or-no-p "Delete whole import?")
                  (remove-overlays beg end)
                  (delete-region beg end)))
            (save-excursion
              (goto-char (js-import-get-prop candidate :pos))
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
              (overlay-put overlay 'face 'js-import-highlight-face)
              (when (yes-or-no-p "Delete?")
                (remove-overlays p1 p2)
                (delete-region p1 p2)))))
      (remove-overlays beg end))))

(provide 'js-import)
;;; js-import.el ends here
