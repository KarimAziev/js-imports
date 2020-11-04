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
(eval-and-compile
  (require 'cc-mode))

(eval-when-compile
  (require 'subr-x))

(defgroup js-import nil
  "Minor mode providing JavaScript import."
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/js-import")
  :prefix 'js-import
  :group 'languages)

(defvar js-import-next-alias-action nil)
(defvar js-import-prev-alias-action nil)
(defvar js-import-current-alias nil)
(defvar js-import-aliases nil)
(defvar js-import-switch-alias-post-command nil)
(defvar js-import-files-map nil
  "Keymap for files sources.")

(defvar js-import-imported-symbols-map nil
  "Keymap for symdol sources.")

(defvar js-import-export-symbols-map nil
  "Keymap for symdol sources.")

(defvar js-import-project-aliases '()
  "List of pairs (alias and path).")

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

(defcustom js-import-type-faces
  '(("^var\\|let\\|const\\|type\\|interface\\|private\\|public\\|readonly\\)$" .
     font-lock-variable-name-face)
    ("^\\(function[*]?\\|class\\)$" . font-lock-function-name-face)
    ("^\\(import\\|require\\)$" . font-lock-type-face)
    ("\\(namespace\\|module\\|export\\|exports\\|default\\|module\\)$" .
     font-lock-type-face))
  "Faces for showing type in `js-import-symbols-menu'
This is a list of cons cells.  The cdr of each cell is a face to
be used, and it can also just be like \\='(:foreground
\"yellow\").  Each car is a regexp match pattern of the imenu type
string."
  :group 'js-import
  :type '(repeat
          (cons
           (regexp :tag "Js import type regexp pattern")
           (sexp :tag "Face"))))

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

(defcustom js-import-file-actions
  '(("Import" . js-import-from-path)
    ("Find file" . js-import-find-file)
    ("Find file other window" . js-import-find-file-other-window))
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
  (concat "\\<" (regexp-opt (list "exports")) "\\>")
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
(defvar js-import-current-project-root nil)
(defvar js-import-current-buffer nil)
(defvar js-import-project-files nil)

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

(defun js-import-setup-ivy ()
  (require 'ivy)
  (setq js-import-files-map (make-sparse-keymap))
  (setq js-import-switch-alias-post-command
        (lambda()
          (let ((input (when (boundp 'ivy-text)
                         ivy-text)))
            (progn
              (put 'quit 'error-message "")
              (run-at-time nil nil
                           (lambda ()
                             (put 'quit 'error-message "Quit")
                             (with-demoted-errors "Error: %S"
                               (js-import-ivy-read-file-name
                                (when (and (boundp 'ivy-last)
                                           (fboundp 'ivy-state-current))
                                  (ivy-state-current ivy-last))
                                input))))
              (abort-recursive-edit)))))
  (setq js-import-next-alias-action
        (lambda() (interactive)
          (funcall 'js-import-next-or-prev-alias 1)))
  (setq js-import-prev-alias-action
        (lambda() (interactive)
          (funcall 'js-import-next-or-prev-alias -1)))
  (define-key js-import-files-map (kbd "C->")
    js-import-next-alias-action)
  (define-key js-import-files-map (kbd "C-<")
    js-import-prev-alias-action))

(defun js-import-view-file (candidate)
  (if-let ((file (js-import-path-to-real
                  candidate)))
      (view-file file)
    (message "Couldn't find %s" candidate)))

(defun js-import-setup-helm ()
  (require 'helm)
  (setq js-import-files-map (make-sparse-keymap))
  (js-import-reset-all-sources)
  (when (boundp 'helm-map)
    (setq js-import-switch-alias-post-command
          (when (and (fboundp 'helm-refresh))
            'helm-refresh))
    (set-keymap-parent js-import-files-map helm-map)
    (define-key js-import-files-map (kbd "C-c o")
      (lambda() (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-import-find-file
           (helm-get-selection)))))
    (define-key js-import-files-map (kbd "C->")
      (lambda()
        (interactive)
        (funcall 'js-import-next-or-prev-alias 1)))
    (define-key js-import-files-map (kbd "C-<")
      (lambda() (interactive)
        (funcall 'js-import-next-or-prev-alias -1)))
    (define-key js-import-files-map (kbd "C-c C-o")
      (lambda() (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-import-find-file-other-window
           (helm-get-selection)))))
    (setq js-import-export-symbols-map
          (js-import-build-helm-exports-keymap))
    (setq js-import-imported-symbols-map
          (js-import-build-helm-imported-keymap))
    (put 'js-import-files-map 'helm-only t)
    (put 'js-import-export-symbols-map 'helm-only t)
    (put 'js-import-imported-symbols-map 'helm-only t))
  (when (fboundp 'helm-make-source)
    (when (fboundp 'helm-candidate-buffer)
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
              :get-line #'buffer-substring-no-properties
              :action 'js-import-file-actions
              :keymap js-import-files-map
              :group 'js-import
              :persistent-action #'js-import-view-file
              :mode-line (list "Imports"))))
    (setq js-import-project-files-source
          (helm-make-source "Project files" 'helm-source-sync
            :group 'js-import
            :mode-line (list "File(s)")
            :candidate-number-limit js-import-files-number-limit
            :action 'js-import-file-actions
            :persistent-action #'js-import-view-file
            :keymap js-import-files-map
            :candidates #'js-import-find-project-files
            :filtered-candidate-transformer
            #'js-import-project-files-transformer))
    (setq js-import-node-modules-source
          (helm-make-source "Node Modules" 'helm-source-sync
            :candidates #'js-import-node-modules-candidates
            :candidate-number-limit js-import-dependencies-number-limit
            :action 'js-import-file-actions
            :mode-line (list "Dependencies")
            :keymap js-import-files-map
            :persistent-action #'js-import-view-file
            :group 'js-import)))
  (when (and (boundp 'helm-map)
             (fboundp 'helm-make-source))
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
            :display-to-real #'js-import-display-to-real-imports
            :persistent-help "Show symbol"
            :marked-with-props 'withprop))
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
            #'js-import-export-filtered-candidate-transformer
            :marked-with-props 'withprop
            :volatile t
            :keymap 'js-import-export-symbols-map
            :action '()
            :action-transformer
            (lambda(_candidate _actions)
              (if (with-current-buffer js-import-current-buffer
                    js-import-current-export-path)
                  '(("Import" . (lambda(_it)
                                  (let ((marked (helm-marked-candidates)))
                                    (dotimes (i (length marked))
                                      (let ((item (nth i marked)))
                                        (js-import-insert-import item))))))
                    ("Jump to export" . js-import-jump-to-item-other-window)
                    ("Jump to definition" . js-import-find-export-definition))
                '(("Jump" . (lambda(it)
                              (js-import-jump-to-item-in-buffer
                               (js-import-display-to-real-exports it)))))))
            :persistent-action
            (lambda(c)
              (when-let ((item (js-import-display-to-real-exports c)))
                (setq item (js-import-find-definition item))
                (when (and (js-import-get-prop item :pos))
                  (view-file (js-import-get-prop
                              item
                              :real-path))
                  (goto-char (js-import-get-prop item :pos))
                  (js-import-highlight-word))))))
    (setq js-import-definitions-source
          (helm-make-source "Definitions" 'helm-source-sync
            :candidates (lambda() (with-current-buffer js-import-current-buffer
                               (js-import-search-backward-identifiers
                                buffer-file-name (point))))
            :marked-with-props 'withprop
            :volatile t
            :action '(("Jump" .
                       (lambda(_it)
                         (when (fboundp 'helm-get-selection)
                           (js-import-jump-to-item-in-buffer
                            (helm-get-selection nil
                                                'withprop))))))))))

(defun js-import-set-completion (var value &optional &rest _ignored)
  "Set VAR to VALUE."
  (pcase value
    ('ivy (funcall 'js-import-setup-ivy))
    ('helm (funcall 'js-import-setup-helm))
    ('default (message "default")))
  (set var value))

(defcustom js-import-completion-system
  (pcase completing-read-function
    ('ivy-completing-read 'ivy)
    ('helm-comp-read 'helm)
    (_ 'default))
  "Which completion system to use."
  :group 'js-import
  :set 'js-import-set-completion
  :type '(choice (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)
                 (const :tag "Default" default)))

(add-variable-watcher 'js-import-completion-system
                      'js-import-set-completion)

(defun js-import-customize-completion ()
  (interactive)
  (let* ((choices '(helm ivy default))
         (result (completing-read (format "Completion:\s (Current: %s)"
                                          js-import-completion-system)
                                  choices)))
    (js-import-set-completion 'js-import-completion-system
                              (intern-soft result))))

(defmacro js-import-with-buffer-or-file-content (filename &rest body)
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
           (with-syntax-table js-import-mode-syntax-table
             (let* ((buffer-file-name current-path)
                    (default-directory (js-import-dirname buffer-file-name)))
               (delay-mode-hooks
                 (set-auto-mode)
                 (progn ,@body)))))))))

(defun js-import-make-files-prompt ()
  (when js-import-current-project-root
    (let ((project-name (car
                         (reverse (split-string
                                   (directory-file-name
                                    js-import-current-project-root) "/")))))
      (concat project-name "\s" "files" "\s" (or js-import-current-alias
                                                 "./")))))

(defun js-import-ivy-read-file-name (&optional preselect input)
  (require 'ivy)
  (when (fboundp 'ivy-read)
    (ivy-read
     (js-import-make-files-prompt)
     (append (js-import-project-files-transformer
              (or js-import-project-files
                  (js-import-find-project-files))))
     :preselect (or preselect (js-import-preselect-file))
     :require-match t
     :caller 'js-import-ivy-read-file-name
     :initial-input input
     :keymap js-import-files-map
     :action (lambda(it) (if (and (boundp 'ivy-exit)
                             ivy-exit)
                        (js-import-from-path it)
                      (js-import-find-file it))))))

;;;###autoload
(defun js-import ()
  "Read a filename to extract exported symbols and add selected ones in buffer."
  (interactive)
  (js-import-init-project)
  (pcase js-import-completion-system
    ('helm
     (require 'helm)
     (when (and (fboundp 'helm)
                (fboundp 'helm-attr))
       (unless js-import-node-modules-source
         (js-import-setup-helm))
       (helm
        :sources js-import-files-source
        :buffer js-import-buffer
        :preselect (js-import-preselect-file)
        :prompt (js-import-make-files-prompt))))
    ('ivy (js-import-ivy-read-file-name))
    (_ (let ((module (funcall completing-read-function
                              (js-import-make-files-prompt)
                              (js-import-get-all-modules))))
         (js-import-from-path module)))))

;;;###autoload
(defun js-import-symbols-menu ()
  "Jump or refactor to exported, imported and definitions in current buffer."
  (interactive)
  (js-import-init-project)
  (setq js-import-current-export-path nil)
  (pcase js-import-completion-system
    ('helm (when (and (fboundp 'helm))
             (helm
              :preselect (js-import-preselect-symbol)
              :sources js-import-symbol-sources)))
    ('ivy (when (fboundp 'ivy-read)
            (let ((choices (append
                            (js-import-extract-imports buffer-file-name)
                            (js-import-extract-all-exports buffer-file-name)
                            (js-import-search-backward-identifiers
                             buffer-file-name (point-max)))))
              (ivy-read "Jump to\s"
                        choices
                        :preselect (js-import-preselect-symbol)
                        :caller 'js-import-symbols-menu
                        :action (lambda(it)
                                  (if (and (boundp 'ivy-exit)
                                           ivy-exit)
                                      (js-import-jump-to-symbol-action it)
                                    (js-import-jump-to-item-in-buffer
                                     it
                                     js-import-current-buffer)))))))
    (_ (when-let* ((choices (append (js-import-extract-all-exports
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

(defun js-import-jump-to-symbol-action (it)
  (when-let ((item (js-import-find-definition it)))
    (let ((stack (js-import-get-prop item :stack)))
      (when stack
        (push item stack)
        (setq stack (js-import-compose-from stack
                                            'js-import-map-stack
                                            'reverse))
        (setq item
              (completing-read "Jump:\s" stack nil t))))
    (when-let ((pos (and item (js-import-get-prop
                               item :pos))))
      (find-file
       (js-import-get-prop item :real-path))
      (goto-char pos)
      (js-import-highlight-word))))

;;;###autoload
(defun js-import-find-symbol-at-point ()
  "Deep jump to a definition of symbol at point through renaming, re-exports."
  (interactive)
  (js-import-init-project)
  (if-let ((name (and
                  (not (js-import-inside-string-p))
                  (js-import-get-word-if-valid))))
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
                   (js-import-highlight-word)))))
    (js-import-find-file-at-point)))

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

;;;###autoload
(defun js-import-from-path (&optional path)
  "Insert import statement with marked symbols, exported from PATH."
  (interactive)
  (unless path
    (js-import-init-project)
    (setq path (read-file-name "File:\s")))
  (with-current-buffer js-import-current-buffer
    (when (file-name-absolute-p path)
      (setq path (js-import-propertize
                  path :display-path
                  (funcall
                   completing-read-function
                   "Transform to\s"
                   (js-import-get-file-variants path
                                                default-directory)))))
    (when-let ((display-path (or (js-import-get-prop path :display-path)
                                 path)))
      (setq js-import-current-export-path display-path)
      (setq js-import-last-export-path display-path)))
  (js-import-init-exports-candidates)
  (cond
   ((and (eq js-import-completion-system 'helm)
         (fboundp 'helm))
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
         :require-match nil
         :caller 'js-import-from-path
         :preselect (js-import-preselect-symbol)
         :multi-action (lambda(marked)
                         (dotimes (i (length marked))
                           (let ((it (nth i marked)))
                             (js-import-insert-import it))))
         :action 'js-import-ivy-insert-or-view-export))))
   (t (let ((choices (js-import-export-filtered-candidate-transformer
                      (js-import-exported-candidates-transformer
                       js-import-export-candidates-in-path))))
        (if (null choices)
            (message "No exports found")
          (js-import-completing-read
           "Symbols\s" choices
           :require-match t
           :action 'js-import-insert-import))))))

(defun js-import-init-project ()
  "Initialize project by setting buffer, finding root and aliases."
  (setq js-import-current-buffer (current-buffer))
  (setq js-import-current-project-root (js-import-find-project-root))
  (setq js-import-aliases (js-import-get-aliases
                           js-import-current-project-root))
  (when js-import-current-alias
    (unless (member js-import-current-alias js-import-aliases)
      (setq js-import-current-alias nil)))
  (setq js-import-project-files (js-import-find-project-files
                                 js-import-current-project-root)))

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

(defun js-import-get-file-variants (&optional path dir)
  (let ((relative (js-import-path-to-relative path (or dir default-directory)))
        (aliased (mapcar (apply-partially 'js-import-transform-file-to-alias
                                          path)
                         (js-import-get-aliases))))
    (push relative aliased)))

(defun js-import-transform-file-to-alias (filename alias)
  (when-let* ((absolute-p (file-name-absolute-p filename))
              (alias-path (js-import-compose-from alias
                                                  'js-import-slash
                                                  'js-import-get-alias-path))
              (match-alias-path (js-import-string-match-p alias-path filename)))
    (js-import-normalize-path (replace-regexp-in-string
                               alias-path
                               (js-import-slash alias)
                               filename))))

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
  (when-let ((alias-path (plist-get (with-current-buffer
                                        js-import-current-buffer
                                      js-import-project-aliases)
                                    alias)))
    (if (file-exists-p alias-path)
        alias-path
      (js-import-join-when-exists (or project-root
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

(defun js-import-directory-files (dir &optional recursive re include-dirs pred)
  "Return files in DIR that matches value of the variable
`js-import-file-ext-regexp'.
Optional argument RECURSIVE non-nil means to search recursive."
  (unless re (setq re js-import-file-ext-regexp))
  (if recursive
      (directory-files-recursively dir re include-dirs pred)
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

(defun js-import-get-aliases (&optional project-root aliases-plist)
  "Extract keys from ALIASES-PLIST of PROJECT-ROOT."
  (unless aliases-plist (setq aliases-plist
                              (if js-import-current-buffer
                                  (with-current-buffer js-import-current-buffer
                                    js-import-project-aliases)
                                js-import-project-aliases)))
  (let ((root (or project-root (js-import-find-project-root)))
        (pl aliases-plist)
        (vals))
    (while pl
      (when-let* ((alias (car pl))
                  (path (with-current-buffer js-import-current-buffer
                          (plist-get aliases-plist alias)))
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

(defun js-import-get-ext (str)
  (when-let ((ext-pos (string-match
                       "\\.\\([a-zZ-A0-9]+\\)$"
                       str)))
    (substring str (1+ ext-pos))))

(defun js-import-sort-by-exts (files &optional extensions)
  (setq extensions (or extensions js-import-preffered-extensions))
  (seq-sort-by (lambda (a)
                 (if-let ((ext (js-import-get-ext a)))
                     (or (seq-position extensions ext 'string=) -1)
                   -1))
               #'>
               files))

(defun js-import-relative-to-real (path &optional dir)
  (when (or (string= path ".")
            (string= path ".."))
    (setq path (js-import-slash path)))
  (let* ((base (file-name-base path))
         (ext (js-import-get-ext path))
         (up (if (string-empty-p base)
                 path
               (replace-regexp-in-string
                (format "/%s\\(\\.[a-zZ-A0-9]+\\)*$" base) "" path)))
         (parent (expand-file-name up (or dir default-directory)))
         (files (cond ((and ext)
                       (directory-files parent nil
                                        (format "^%s\\.%s$" base ext)))
                      ((string-empty-p base)
                       (js-import-sort-by-exts
                        (directory-files parent nil
                                         "^index\\(\\.[a-zZ-A0-9]+\\)$")))
                      (t (js-import-sort-by-exts
                          (directory-files
                           parent nil
                           (format
                            "^%s\\(\\.[a-zZ-A0-9]+\\)?$" base))))))
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

(defun js-import-next-or-prev-alias (&optional direction)
  "Set value for variable `js-import-current-alias'."
  (unless direction (setq direction 1))
  (let* ((aliases (if (> direction 0)
                      js-import-aliases
                    (reverse js-import-aliases))))
    (setq js-import-current-alias (if js-import-current-alias
                                      (car (cdr
                                            (member js-import-current-alias
                                                    aliases)))
                                    (car aliases))))
  (when js-import-switch-alias-post-command
    (funcall js-import-switch-alias-post-command)))

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

(defun js-import-build-helm-exports-keymap ()
  "Make keymap for helm symbols type."
  (when-let ((h-map (and (boundp 'helm-map)
                         helm-map))
             (map (make-sparse-keymap)))
    (set-keymap-parent map h-map)
    (define-key
      map (kbd "C-c o")
      (lambda() (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-import-jump-to-item-other-window
           (helm-get-selection nil 'withprop)))))
    (define-key
      map
      (kbd "C-c C-j")
      (lambda() (interactive)
        (when (and (fboundp 'helm-run-after-exit)
                   (fboundp 'helm-get-selection))
          (helm-run-after-exit
           'js-import-find-export-definition
           (helm-get-selection nil 'withprop)))))
    map))

(defun js-import-build-helm-imported-keymap ()
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
      (kbd "M-d") (lambda()
                    (interactive)
                    (when (and (boundp 'helm-alive-p)
                               helm-alive-p
                               (fboundp 'helm-refresh)
                               (fboundp 'helm-execute-persistent-action)
                               (fboundp 'helm-attrset))
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
                    (when (and (boundp 'helm-alive-p)
                               helm-alive-p
                               (fboundp 'helm-refresh)
                               (fboundp 'helm-execute-persistent-action)
                               (fboundp 'helm-attrset))
                      (helm-attrset
                       'js-import-delete-import-statetement
                       '(js-import-delete-import-statetement . never-split))
                      (helm-execute-persistent-action
                       'js-import-delete-import-statetement)
                      (helm-refresh))))
    (define-key map
      (kbd "M-r") (lambda()
                    (interactive)
                    (when (fboundp 'helm-exit-and-execute-action)
                      (helm-exit-and-execute-action 'js-import-rename-import))))
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
    (seq-find (lambda(elt) (equal elt item))
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

(defun js-import-export-filtered-candidate-transformer (candidates
                                                        &optional
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
  (let* ((esm-exports)
         (cjs-exports)
         (external-paths)
         (path)
         (processed-paths)
         (map-result (lambda(result)
                       (cond ((listp result)
                              (setq result (seq-remove 'null result))
                              (setq esm-exports (append result esm-exports)))
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
                              (push result cjs-exports))))))
    (push (js-import-path-to-real init-path) external-paths)
    (while (setq path (pop external-paths))
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
          (unless esm-exports
            (let ((export-depth)
                  (cjs-locals))
              (while (js-import-re-search-forward
                      js-import-cjs-export-keyword--re nil t 1)
                (let ((depth (nth 0 (syntax-ppss (point)))))
                  (when (or (not export-depth)
                            (> export-depth depth))
                    (setq export-depth depth)
                    (setq cjs-locals nil))
                  (js-import-skip-whitespace-forward)
                  (when-let ((result (js-import-extract-cjs-exports
                                      buffer-file-name)))
                    (cond ((listp result)
                           (setq result (seq-remove 'null result))
                           (setq cjs-locals (append result cjs-locals)))
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
                           (push result cjs-locals))))))
              (when cjs-locals
                (setq cjs-exports (seq-uniq
                                   (append cjs-exports cjs-locals)))))))))
    (mapcar (lambda(it) (js-import-propertize it :export t))
            (reverse (seq-remove 'null (append esm-exports cjs-exports))))))

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
         (let* ((stack (js-import-skip-reserved-words "\s\t\\*"))
                (default (car (assoc "default" stack)))
                (real-name (or (car (seq-find (lambda(it)
                                                (let ((name (car it)))
                                                  (js-import-valid-identifier-p
                                                   name)))
                                              stack))
                               default))
                (var-type (car
                           (seq-find (lambda(it)
                                       (member
                                        (car it)
                                        js-import-delcaration-keywords))
                                     stack)))
                (as-name (or real-name default)))
           (js-import-make-item
            (or as-name real-name)
            :type (if default 1 4)
            :real-name (or real-name default as-name)
            :real-path path
            :as-name as-name
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
  (unless separators (setq separators "\s\t\\*"))
  (let* ((stack)
         (prev)
         (word))
    (while (and
            (not (equal prev (point)))
            (js-import-reserved-word-p (setq word (js-import-which-word))))
      (setq prev (point))
      (skip-chars-forward word)
      (js-import-skip-whitespace-forward)
      (skip-chars-forward separators)
      (push (cons word prev) stack))
    (when-let ((id (js-import-which-word)))
      (push (cons id (point)) stack))
    (seq-remove 'null stack)))

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
    (string-join symbols ", ")))

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
        (skip-chars-backward js-import-regexp-name)
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
  (not (or
        (null str)
        (js-import-invalid-name-p str)
        (js-import-reserved-word-p str))))

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
       (< (point-min) (point))
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

(defun js-import-transform-export-symbol (it)
  (when-let ((item (js-import-display-to-real-exports
                    it)))
    (js-import-transform-symbol item)))

(defun js-import-transform-symbol (c &optional margin)
  (let* ((real-path (js-import-get-prop c :real-path))
         (short-path (if (equal (with-current-buffer
                                    js-import-current-buffer
                                  buffer-file-name)
                                real-path)
                         (format "%s" js-import-current-buffer)
                       (replace-regexp-in-string
                        (concat "^" (js-import-slash
                                     js-import-current-project-root))
                        ""
                        real-path)))
         (display-path (js-import-get-prop c :display-path))
         (export (and (js-import-get-prop c :export)
                      (if display-path
                          "Reexport"
                        "Export")))
         (import (and (js-import-get-prop c :import)
                      "Import"))
         (var (js-import-get-prop c :var-type))
         (default (and (equal
                        (js-import-get-prop c :type)
                        1)
                       "Default"))
         (parts (string-trim (mapconcat
                              (lambda(it) (propertize (capitalize (string-trim it))
                                                 'face
                                                 'font-lock-function-name-face))
                              (seq-remove 'null (list
                                                 export
                                                 import
                                                 default
                                                 var))
                              "\s")))
         (indents (make-string (or margin 1) ?\s))
         (name (replace-regexp-in-string " default$" ""
                                         (or (js-import-get-prop c :as-name)
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
(defun js-import-map-stack (cands)
  (seq-map-indexed
   'js-import-transform-symbol
   (seq-remove 'null cands)))

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

(defun js-import-items-from-string (&optional str)
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
               (not (js-import-valid-identifier-p default-as-name)))
      (setq default-as-name nil))
    (when (and default-as-name
               (= default-type 16))
      (setq default (format "* as %s" default-as-name)))
    (list default named)))

(defun js-import-ivy-insert-or-view-export (item)
  "Add ITEM into existing or new import statement."
  (if-let* ((real (js-import-display-to-real-exports item))
            (definition (js-import-find-definition real))
            (exit (and (boundp 'ivy-exit)
                       (not ivy-exit))))
      (progn (view-file-other-window
              (js-import-get-prop definition
                                  :real-path))
             (goto-char (js-import-get-prop definition :pos))
             (js-import-highlight-word))
    (js-import-insert-import item)))

(defun js-import-insert-import (candidate)
  "Insert CANDIDATE into existing or new import statement."
  (save-excursion
    (if-let* ((real (js-import-display-to-real-exports candidate))
              (type (or (js-import-get-prop real :type)))
              (display-path js-import-last-export-path)
              (as-name (js-import-get-prop real :as-name))
              (default-name (if (or (= type 4)
                                    (js-import-valid-identifier-p as-name))
                                as-name
                              (js-import-generate-name-from-path
                               display-path)))
              (prompt (pcase type
                        (1 (format "Import default as (default %s)\s"
                                   default-name))
                        (4 (format "Import (default %s)\s"
                                   as-name))
                        (16 (format "Import * as (default %s)\s"
                                    default-name))))
              (confirmed-name (read-string
                               prompt default-name nil default-name))
              (full-name (pcase type
                           (1 confirmed-name)
                           (4 (if (string= default-name confirmed-name)
                                  default-name
                                (format "%s as %s" as-name confirmed-name)))
                           (16 (if (string= default-name confirmed-name)
                                   default-name
                                 (format "* as %s" confirmed-name))))))
        (pcase type
          (4 (js-import-insert-exports nil full-name display-path))
          (_ (js-import-insert-exports full-name nil display-path)))
      (let ((items (js-import-items-from-string (format "%s" candidate))))
        (js-import-insert-exports (pop items)
                                  (pop items)
                                  js-import-last-export-path)))))

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
                                          imports)))
                 (unless module
                   (goto-char (point-min))))
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

(with-eval-after-load 'ivy
  (when (fboundp 'ivy-set-actions)
    (ivy-set-actions
     'js-import
     '(("f" js-import-find-file
        "find file")
       ("j"
        js-import-find-file-other-window
        "other window"))))
  (when (fboundp 'ivy-set-display-transformer)
    (ivy-set-display-transformer
     'js-import-symbols-menu
     'js-import-transform-symbol)
    (ivy-set-display-transformer
     'js-import-from-path
     'js-import-transform-export-symbol))
  (when (fboundp 'ivy-set-sources)
    (ivy-set-sources
     'js-import-ivy-read-file-name
     '((original-source)
       (js-import-node-modules-candidates)))))

;;;###autoload
(define-minor-mode js-import-mode
  "js-import-mode is a minor mode for importing.
\\{js-import-mode-map}"
  :lighter " js-import"
  :group 'js-import
  :keymap js-import-mode-map)

(provide 'js-import)
;;; js-import.el ends here
