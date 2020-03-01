;;; -*- lexical-binding: t -*-
;;; js-import-path.el --- This is an Emacs Lisp file with Emacs Lisp code.

;; Copyright (C) 2020 KarimAziev

;; Author: KarimAziev <karim.aziev@gmail.com>

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

(require 'f)
(require 'json)
(require 'subr-x)
(require 's)
(require 'projectile)

(defcustom js-import-alias-map '("" "src")
  "List of pairs (alias and path)"
  :group 'js-import
  :type '(repeat string))

(defun js-import-get-aliases ()
  "Get list of aliases"
  (let ((pl js-import-alias-map)
        (vals  ()))
    (while pl
      (push (car pl) vals)
      (setq pl  (cddr pl)))
    (nreverse vals)))

(defun js-import-normalize-path(path)
  (funcall (-compose
            'js-import-remove-path-index
            'f-no-ext
            'js-import-remove-double-slashes)
           path))

(defun js-import-maybe-slash-alias(alias)
  (if (or (s-blank? alias) (s-matches? ".*\\/$" alias)) alias (concat alias "/")))

(defun js-import-real-path-to-alias(real-path alias)
  (let ((alias-path (js-import-get-alias-path alias))
        (slashed-alias (js-import-maybe-slash-alias alias)))
    (replace-regexp-in-string (concat "^" alias-path) slashed-alias real-path)))

(defun js-import-get-alias-path(alias)
  (f-slash (f-join (projectile-project-root) (lax-plist-get js-import-alias-map alias))))

(defun js-import-normalize-relative-path (path)
  (funcall (-compose (lambda (str)
                       (cond ((s-blank? str) "./index")
                             ((not (s-matches? "\\.+/" str))
                              (concat "./" str))
                             (t str)))
                     'js-import-normalize-path)
           path))

(defun js-import-remove-path-index (path)
  (let ((cutted-path (replace-regexp-in-string "[/]*index\\(.jsx?$\\|.tsx?$\\)" "" path)))
    (if (s-blank? cutted-path)
        path
      cutted-path)))

(defun js-import-remove-double-slashes (path)
  (replace-regexp-in-string "//"  "/" path))

(defun js-import-expand-path(candidate)
  (f-short (f-expand candidate (projectile-project-root))))

(defun js-import-expand-dependency-path(candidate)
  (f-expand candidate (projectile-project-root)))

(defun js-import-get-node-modules-path ()
  "Return the path to node-modules."
  (f-join (projectile-project-root) "node_modules"))

(defun js-import-expand-node-modules(module)
  (let ((node-modules-path (f-join (js-import-get-node-modules-path) module)))
    node-modules-path))

(defun js-import-js-file? (filename)
  "Check if FILENAME ends with either .js or .jsx."
  (or (string-suffix-p ".js" filename t)
      (string-suffix-p ".jsx" filename t)
      (string-suffix-p ".ts" filename t)
      (string-suffix-p ".tsx" filename t)))

(defun js-import-get-package-json-path ()
  "Return the path to package.json."
  (f-join (projectile-project-root) "package.json"))

(defun js-import-find-node-module-index-path(module)
  (let ((path (js-import-expand-node-modules module)))
    (cond ((f-exists? (f-join path "es" "index.js"))
           (setq path (f-join path "es/index.js")))
          ((f-exists? (f-join path "src/index.js"))
           (setq path (f-join path "src/index.js")))
          ((f-exists? (f-join path "lib/index.js"))
           (setq path (f-join path "lib/index.js")))
          ((f-exists? (f-join path "es" "index.ts"))
           (setq path (f-join path "es/index.ts")))
          ((f-exists? (f-join path "src/index.ts"))
           (setq path (f-join path "src/index.ts")))
          ((f-exists? (f-join path "lib/index.d.ts"))
           (setq path (f-join path "lib/index.d.ts")))
          ((f-exists? (f-join path "lib/index.d.ts"))
           (setq path (f-join path "lib/index.d.ts")))
          ((f-exists? (f-join path "index.ts"))
           (setq path (f-join path "index.ts")))
          ((f-exists? (f-join path "index.d.ts"))
           (setq path (f-join path "index.d.ts")))
          (t (setq path (f-join path "index.js"))))))

(defun js-import-dependencies-hash (&optional $package-json-path $section)
  "Return a dependency hash fetched from package-json-path in section.  If file not found, return nil."
  (let ((package-json-path (or $package-json-path (js-import-get-package-json-path)))
        (section (or $section "dependencies"))
        (json-object-type 'hash-table))
    (when-let ((package-json-content (condition-case nil
                                         (f-read-text package-json-path 'utf-8) (error nil)))
               (dependencies-hash (condition-case nil
                                      (gethash section (json-read-from-string package-json-content)) (error nil))))
      dependencies-hash)))

(defun js-import-is-dependency? (display-path)
  "Check if path is dependency"
  (let ((dependencies-hash (js-import-dependencies-hash))
        (path (car (split-string display-path "/"))))
    (message "path\n %s display-path\n %s"  path display-path)
    (if dependencies-hash
        (gethash path dependencies-hash)
      nil)))


(defun js-import-path-to-real(path)
  (message "path %s isDEPENDENC %s" path (js-import-is-dependency? path))
  (let ((result (cond
                 ((js-import-is-dependency? path)
                  (js-import-find-node-module-index-path path))

                 ((s-matches? "^\\." path)
                  (let ((filepath (f-short (f-expand path))))
                    (cond
                     ((f-exists? (concat filepath ".js"))
                      (setq filepath (concat filepath ".js"))
                      filepath)
                     ((f-exists? (concat filepath "/index.js"))
                      (message "filepath %s exist %s" filepath (f-exists? (concat filepath ".js")))
                      (setq filepath (concat filepath "/index.js")))
                     (t filepath))
                    filepath)
                  ))))
    (message "js-import-path-to-real path\n %s and real-path %s"  path result)
    result))

(provide 'js-import-path)
;;; js-import-path.el ends here
