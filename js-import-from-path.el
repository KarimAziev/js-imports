;;; -*- lexical-binding: t -*-
;;; js-import-from-path.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

(require 'f)
(require 'json)
(require 'subr-x)
(require 'js-import-utils)

(defun js-import-from-path(path &optional normalized-path)
  (let* ((import-alist (js-import-find-current-imports normalized-path))
         (all-exports-alist (js-import-find-all-exports normalized-path path))
         (import-reals (--map (funcall (-compose 's-trim 'car 'split-string 'car) it)
                              import-alist))
         (import-source (js-import-build-imported-source import-alist normalized-path))
         (export-source (helm-build-sync-source (format "Exports from %s" normalized-path)
                          :candidates (-filter (lambda(it)
                                                 (let ((name (car it))
                                                       (type (cdr it)))
                                                   (pcase type
                                                     (1 (not (rassoc 1 import-alist)))
                                                     (4 (not (-contains? import-reals (car it))))
                                                     (16 (not (rassoc 16 import-alist))))
                                                   ))
                                               all-exports-alist)
                          :candidate-transformer 'js-import-imports-transformer
                          :persistent-action 'js-import-action--goto-export
                          :display-to-real (lambda(candidate)
                                             (js-import-make-item candidate
                                                                  :real-path path
                                                                  :cell (assoc candidate all-exports-alist)
                                                                  :display-path normalized-path))

                          :action '(("Import" . js-import-action--import-candidate)
                                    ("Import as " . js-import-action--import-as)
                                    ("Go" . js-import-action--goto-export)))))

    (helm :sources (list export-source import-source))))

(defun js-import-build-imported-source(candidates display-path &optional real-path)
  (helm-build-sync-source (format "imported from %s" display-path)
    :display-to-real (lambda(candidate)
                       (js-import-make-item candidate
                                            :cell (assoc candidate candidates)
                                            :real-path (or real-path (js-import-alias-path-to-real display-path))
                                            :display-path display-path))
    :candidate-transformer 'js-import-imports-transformer
    :candidates candidates
    :persistent-action 'js-import-action--goto-export
    :action '(("Go" . js-import-action--goto-export)
              ("Rename" . js-import-action--rename-import)
              ("Add more imports" . js-import-action--add-to-import)
              ("Delete" . js-import-action--delete-import)
              ("Delete whole import" . js-import-action--delete-whole-import))))

(defun js-import-build-exported-source(candidates display-path &optional real-path)
  (helm-build-sync-source (format "import from %s" display-path)
    :display-to-real (lambda(candidate)
                       (js-import-make-item candidate
                                            :cell (assoc candidate candidates)
                                            :real-path real-path
                                            :display-path display-path))
    :candidate-transformer 'js-import-imports-transformer
    :candidates candidates
    :persistent-action 'js-import-action--goto-export
    :action '(("Go" . js-import-action--goto-export)
              ("Rename" . js-import-action--rename-import)
              ("Delete" . js-import-action--delete-import)
              ("Delete whole import" . js-import-action--delete-whole-import)
              )))

(defun js-import-action--goto-export(candidate)
  (with-helm-quittable
    (let ((real-name-regexp (concat "\\([\s\t\n,]" (js-import-get-prop candidate 'real-name) "[\s\t\n,]\\)"))
          (real-path (js-import-get-prop candidate 'real-path))
          (export-type  (js-import-get-prop candidate 'type)))
      (when (f-exists? real-path)
        (find-file-other-window real-path)
        (goto-char 0)
        (re-search-forward (pcase export-type
                             (1 (concat "export[\t\s\n]+default"))
                             (4 (concat "export[\t\s\n]+.*+" real-name-regexp))
                             (16 "export[\t\s\n]")))
        (helm-highlight-current-line)))))

(defun js-import-action--delete-import(cand)
  (mapc
   (lambda(candidate)
     (let ((type (js-import-get-prop candidate 'type))
           (fullname (js-import-get-prop candidate 'display-name))
           (display-path (js-import-get-prop candidate 'display-path)))

       (pcase type
         (16 (js-import-delete-whole-import display-path))
         (t (js-import-delete-imported-name fullname display-path)))))

   (helm-marked-candidates)))

(defun js-import-action--delete-whole-import(cand)
  (mapc
   (lambda(candidate)
     (let ((display-path (js-import-get-prop candidate 'display-path)))
       (js-import-delete-whole-import display-path)))
   (helm-marked-candidates)))

(defun js-import-action--add-to-import(cand)
  (mapc
   (lambda(candidate)
     (let ((display-path (js-import-get-prop candidate 'display-path))
           (real-path (js-import-get-prop candidate 'real-path)))
       (js-import-from-path real-path display-path)))
   (helm-marked-candidates)))

(defun js-import-action--rename-import(cand)
  (mapc
   (lambda(candidate)
     (let* ((type (js-import-get-prop candidate 'type))
            (fullname (js-import-get-prop candidate 'display-name))
            (real-name (js-import-get-prop candidate 'real-name))
            (renamed-name (js-import-get-prop candidate 'renamed-name))
            (display-path (js-import-get-prop candidate 'display-path))
            (new-name (s-trim (read-string
                               (format "Rename %s as (%s) " real-name renamed-name)
                               nil nil renamed-name))))
       (when new-name (save-excursion
                        (save-restriction
                          (js-import-narrow-to-import display-path)
                          (let ((case-fold-search nil))
                            (if renamed-name
                                (progn (re-search-forward (concat real-name "[_\s\n]+as[_\s\n]") nil t 1)
                                       (re-search-forward renamed-name nil t 1)
                                       (replace-match new-name))
                              (progn (re-search-forward (concat real-name "[_\s\n,]+") nil t 1)
                                     (skip-chars-backward "[_\s\n,]")
                                     (insert (concat " as " new-name)))))
                          (widen))))))
   (helm-marked-candidates)))

(defun js-import-action--import-candidate(candidate)
  (mapc (lambda(c)
          (save-excursion
            (let ((type (js-import-get-prop c 'type))
                  (real-name (js-import-get-prop c 'real-name))
                  (name (js-import-get-prop c 'display-name))
                  (renamed-name (js-import-get-prop c 'display-name))
                  (normalized-path (js-import-get-prop c 'display-path)))
              (pcase type
                (1 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))
                (4 (js-import-insert-exports nil name normalized-path))
                (16 (js-import-insert-exports (js-propose-import-name normalized-path (cons renamed-name type)) nil normalized-path))))))
        (helm-marked-candidates)))

(defun js-import-action--import-as(candidate)
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

(defun js-import-make-imports-sources()
  (save-excursion
    (--map (js-import-build-imported-source (cdr it) (car it))
           (js-import-find-all-buffer-imports))))

(provide 'js-import-from-path)
;;; js-import-from-path.el ends here
