;;; js-imports-ast.el --- Parser for JavaScript files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-imports
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

(require 'js-imports)

(defvar js-imports--regexp-js-regexp
  "\\([!]?+\\)?/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)"
  "Regexp matching javascript regular expression.")

(defun js-imports-get-obj-key (&optional with-props &rest args)
  "Forward symbol at point and return string.
If optional argument WITH-PROPS is non-nil, return string propertied with ARGS
and props :id, :start and :end."
  (let ((re "*_~$A-Za-z0-9.")
        (start)
        (end)
        (key))
    (if (looking-at "[\\|['\"`]")
        (progn
          (setq start (point))
          (forward-sexp 1)
          (setq end (point)))
      (when (looking-at (concat "[" re "]"))
        (setq start (point))
        (when (> (skip-chars-forward (concat re "?")) 0)
          (setq end (point)))
        (when (looking-at "\\[")
          (forward-sexp 1)
          (setq end (point)))))
    (setq key (and start end (buffer-substring-no-properties start end)))
    (if (and key with-props)
        (apply 'js-imports-propertize
               (append (list key)
                       (list :id key
                             :start start
                             :end end)
                       args))
      key)))

(defvar js-imports-angles-syntax-table
  (let ((table (copy-syntax-table
                js-imports-mode-syntax-table)))
    (modify-syntax-entry ?< "(^" table)
    (modify-syntax-entry ?> ")$" table)
    table))

(defun js-imports-backward-angles ()
  "Backward angles at point and return string."
  (when (looking-back ">" 0)
    (let ((a)
          (b (point))
          (res))
      (with-syntax-table js-imports-angles-syntax-table
        (ignore-errors (forward-sexp -1))
        (setq a (point))
        (when (= b a)
          (forward-char -1))
        (setq res (buffer-substring-no-properties a b))
        res))))

(defun js-imports-forward-angles ()
  "Forward angles at point and return string."
  (when-let ((count (when (looking-at "<")
                      (forward-char 1)
                      1))
             (beg (1- (point))))
    (while (and (> count 0)
                (js-imports-re-search-forward "[[{(<]\\|>" nil t 1))
      (let ((chars (split-string (js-imports-get-prev-char 2) "" t)))
        (pcase (nth 1 chars)
          ((or "{" "(" "[")
           (forward-char -1)
           (forward-sexp 1))
          ("<" (setq count (1+ count)))
          (_ (unless (equal (car chars) "=")
               (setq count (1- count)))))))
    (buffer-substring-no-properties beg (point))))

(defun js-imports-skip-to-char-same-scope (&optional stop-chars)
  (unless stop-chars (setq stop-chars "[;]"))
  (let ((open-chars-re "[^]({[;}]+")
        (search-point)
        (scope-start)
        (scope-end)
        (stop))
    (when (looking-at "[[({[]")
      (forward-sexp 1))
    (while (and
            (null stop)
            (not (looking-at stop-chars))
            (not (looking-at "[]})]"))
            (progn
              (setq search-point
                    (save-excursion
                      (js-imports-re-search-forward stop-chars nil t 1)))
              (save-excursion
                (setq scope-start
                      (js-imports-re-search-forward open-chars-re nil t 1))
                (forward-char -1)
                (setq scope-end
                      (when (looking-at "[[({[]")
                        (forward-sexp 1)
                        (setq scope-end (point)))))
              (cond
               ((and scope-start search-point
                     (> search-point scope-start))
                (if scope-end
                    (goto-char scope-end)
                  (goto-char scope-start)))
               ((and search-point)
                (goto-char search-point)
                (skip-chars-backward stop-chars)
                (setq stop t)))))
      (if (looking-at "[[({[]")
          (forward-sexp 1)
        (setq stop t)))
    (and (looking-at stop-chars)
         (point))))

(defun js-imports-normalize-value (value)
	"Trim VALUE and if maybe convert it to number."
  (setq value
        (string-trim value))
  (if (string-match-p "^[0-9]+$" value)
      (setq value (string-to-number value))
    value))

(defun js-imports-ensure-semicolon ()
	"If text after point is semicolon forward it and return point position.
Whitespace and comments are ignored."
  (let ((pos))
    (setq pos (if (looking-at ";")
                  (1+ (point))
                (save-excursion
                  (and (> (js-imports-forward-whitespace) 0)
                       (looking-at ";")
                       (1+ (point))))))
    (when pos
      (goto-char pos)
      pos)))

(defun js-imports-skip-arrow ()
	"Forward arrow at point and whitespace after.
Return poisiton of arrow's end."
  (when-let ((next (js-imports-get-next-char 2)))
    (when (string= next "=>")
      (forward-char 2)
      (prog1
          (point)
        (js-imports-forward-whitespace)))))

(defvar js-imports-assignment-operators
  '("=" "+=" "-=" "*=" "/=" "%=")
  "List of javascript assignment operators.")

(defun js-imports-get-operator-at-point ()
	"Forward and return operator at point as string."
  (let ((start (point)))
    (when (> (skip-chars-forward "!@#%^&*=><~|\\-+/?:") 0)
      (buffer-substring-no-properties start (point)))))

(defun js-imports-get-assigmnent-operator ()
	"Get assigmnent operator at point or nil."
  (when-let ((operator (js-imports-get-operator-at-point)))
    (when (member operator js-imports-assignment-operators)
      operator)))

(defun js-imports-parse-funcall-params ()
	"Parse arguments of non-recoursively."
  (js-imports-parse-arguments nil 'js-imports-parse-object))

(defun js-imports-parse-funcall ()
  "Parse function call at point and return propertized string."
  (let ((name)
        (end)
        (start (point))
        (args)
        (results))
    (when (and (js-imports-get-word-if-valid)
               (save-excursion
                 (setq name (js-imports-get-obj-key))
                 (js-imports-forward-whitespace "\s\t")
                 (setq args (js-imports-parse-funcall-params))
                 (setq end (point))
                 (when (and args
                            end)
                   (js-imports-forward-whitespace "\s\t")
                   (null (looking-at "{")))))
      (goto-char end)
      (while (or (looking-at "\\.")
                 (save-excursion
                   (js-imports-forward-whitespace "\s\t")
                   (looking-at "[({]")))
        (if-let ((calls (progn
                          (js-imports-forward-whitespace "\s\t")
                          (js-imports-parse-funcall-params))))
            (push calls results)
          (push (list "." (js-imports-get-obj-key)) results))
        (setq end (point)))
      (js-imports-make-item name
                            :args args
                            :start start
                            :end (point)
                            :children results
                            :var-type "Funcall"))))

(defun js-imports-parse-assignment ()
  "Parse assignment at point and return propertized string."
  (let ((start (point))
        (nodes))
    (while (when-let* ((id (or
                            (js-imports-parse-funcall)
                            (js-imports-get-obj-key t)))
                       (operator (progn
                                   (js-imports-forward-whitespace)
                                   (js-imports-get-assigmnent-operator))))
             (push id nodes))
      (js-imports-forward-whitespace))
    (unless nodes (goto-char start))
    (when nodes
      (let ((value (or (js-imports-parse-value)
                       (pop nodes))))
        (or (mapcar (lambda (it) (js-imports-make-item it
                                                  :var-type "Assignment"
                                                  :value value))
                    (reverse nodes))
            (js-imports-make-item value :var-type "Assignment"))))))

(defun js-imports-get-return-type ()
  "Return string with typescript value if char at point is colon."
  (when (looking-at ":")
    (forward-char 1)
    (js-imports-forward-whitespace)
    (if (looking-at "{")
        (js-imports-parse-object-recoursive)
      (let ((type)
            (types))
        (while (setq type (js-imports-get-obj-key t))
          (js-imports-forward-whitespace)
          (push type types))
        (string-join (reverse types) "\s")))))

(defun js-imports-get-word-or-char ()
  "Get word or char at point."
  (or (js-imports-get-word)
      (js-imports-get-next-char)))

(defun js-imports-get-prev-token-ignore-whitespace ()
	"Return string with previous token ignoring comments and whitespace."
  (save-excursion
    (js-imports-backward-whitespace)
    (when-let* ((end (point))
                (start
                 (when (or (< (skip-chars-backward "*_~$A-Za-z0-9") 0)
                           (< (skip-chars-backward "!@#%^&*=><~|\\-+/?:") 0)
                           (when (>= (1- (point))
                                     (point-min))
                             (forward-char -1)
                             t))
                   (point))))
      (cons (buffer-substring-no-properties start end) start))))

(defun js-imports-parse-function-declaration (&optional deep)
  "Parse function declaration at point.
Depending of optional argument DEEP return propertized string or list.
If value of DEEP is not nil, return list."
  (let ((parent-start))
    (when (looking-at "\\_<\\(async\\)\\_>")
      (setq parent-start (point))
      (skip-chars-forward js-imports-regexp-name)
      (js-imports-forward-whitespace))
    (pcase (js-imports-get-word-or-char)
      ((or "function" "function*")
       (let ((node-start (or parent-start (point)))
             (node-end (+ (point)
                          (skip-chars-forward "*_~$A-Za-z0-9")))
             (id-pos)
             (body-start)
             (body-end)
             (func-type)
             (result)
             (id)
             (children))
         (setq func-type
               (buffer-substring-no-properties node-start node-end))
         (js-imports-forward-whitespace)
         (when (looking-at "*")
           (setq func-type (concat func-type "*"))
           (forward-char 1))
         (setq id-pos (point))
         (setq id
               (or (and
                    (> (skip-chars-forward "_~$A-Za-z0-9") 0)
                    (buffer-substring-no-properties
                     id-pos (point)))
                   "Anonymus"))
         (js-imports-forward-whitespace)
         (when (js-imports-forward-angles)
           (js-imports-forward-whitespace))
         (when-let ((args (js-imports-parse-arguments deep)))
           (js-imports-forward-whitespace)
           (when (looking-at ":")
             (js-imports-forward-whitespace)
             (js-imports-parse-value))
           (setq body-start (point))
           (setq children
                 (and (looking-at "{")
                      (if deep
                          (js-imports-parse-scope-inner body-start)
                        (forward-sexp 1))))
           (setq body-end (point))
           (js-imports-forward-angles)
           (setq result
                 (js-imports-make-item
                  id
                  :var-type func-type
                  :start node-start
                  :end node-end
                  :id id
                  :value-start body-start
                  :value-end body-end
                  :children children
                  :args args))
           (if deep
               (append (list result) children)
             result))))
      (_ (js-imports-parse-arrow-function)))))

(defun js-imports-parse-object-method (&optional id)
  "Parse object method at point and return propertized string.
Optional argument ID is will b."
  (if-let* ((pos (point))
            (args (progn
                    (js-imports-get-obj-key)
                    (js-imports-forward-whitespace)
                    (when (js-imports-forward-angles)
                      (js-imports-forward-whitespace))
                    (js-imports-forward-whitespace)
                    (js-imports-parse-arguments)))
            (body-bounds (progn
                           (js-imports-forward-whitespace)
                           (js-imports-get-return-type)
                           (or (js-imports-forward-scope)
                               (cons
                                (point) (point))))))
      (js-imports-make-item
       (or id "method")
       :args args
       :var-type "Method"
       :value-start (car body-bounds)
       :value-end (cdr body-bounds))
    (goto-char pos)
    nil))

(defun js-imports-parse-arrow-function ()
  (if-let* ((parent-start (point))
            (args
             (progn
               (js-imports-forward-whitespace)
               (when (js-imports-forward-angles)
                 (js-imports-forward-whitespace))
               (or (js-imports-parse-arguments)
                   (js-imports-get-obj-key))))
            (arrow (progn
                     (js-imports-forward-whitespace)
                     (let ((returns (js-imports-get-return-type)))
                       (when returns
                         (js-imports-forward-whitespace)))
                     (js-imports-skip-arrow))))
      (let ((children (progn
                        (js-imports-forward-whitespace)
                        (js-imports-parse-scope-inner)))
            (value))
        (unless children (setq value (js-imports-parse-value)))
        (js-imports-make-item
         "Anonymus"
         :args args
         :value value
         :children children
         :return-type (or value)
         :var-type "function"
         :parent-start parent-start
         :parent-end (point)))
    (goto-char parent-start)
    nil))

(defun js-imports-trim-length (str &optional max)
  (setq str (string-join (split-string str) "\s"))
  (unless max (setq max 100))
  (let ((total (length str))
        (step (/ max 2)))
    (if (> total max)
        (concat (substring-no-properties str 0 step)
                "..."
                (substring-no-properties
                 str (- total step) total))
      str)))

(defun js-imports-parse-es-import-braces ()
  (when-let ((obj (js-imports-parse-object t)))
    (mapcar (lambda (cell) (let ((real-name
                             (car cell))
                            (as-name
                             (cdr cell)))
                        (js-imports-make-item
                         (or as-name real-name)
                         :as-name (or as-name real-name)
                         :real-name (or real-name as-name)
                         :start (js-imports-get-prop
                                 real-name :start)
                         :end (js-imports-get-prop
                               (or as-name real-name) :end)
                         :type 4
                         :var-type "import")))
            obj)))

(defun js-imports-parse-es-import ()
  (when (looking-at "\\_<\\(import\\)\\_>")
    (let ((start (point))
          (end)
          (named-imports)
          (dynamic-import)
          (namespace-import)
          (default-import)
          (display-path)
          (children))
      (skip-chars-forward js-imports-regexp-name)
      (js-imports-forward-whitespace)
      (while (pcase (js-imports-get-next-char-or-word)
               ("from"
                (skip-chars-forward js-imports-regexp-name)
                (js-imports-forward-whitespace))
               ("*"
                (let ((beg (point))
                      (as-name))
                  (forward-char 1)
                  (js-imports-forward-whitespace)
                  (js-imports-get-obj-key)
                  (js-imports-forward-whitespace)
                  (setq as-name (js-imports-get-obj-key t))
                  (js-imports-forward-whitespace)
                  (setq namespace-import
                        (js-imports-make-item as-name
                                              :real-name "*"
                                              :as-name as-name
                                              :type 16
                                              :var-type "import"
                                              :start beg
                                              :end (point)))))
               ("type" (skip-chars-forward js-imports-regexp-name)
                (js-imports-forward-whitespace)
                (setq named-imports
                      (js-imports-parse-es-import-braces)))
               ("("
                (js-imports-get-obj-key)
                (setq dynamic-import (js-imports-parse-value)))
               ("{"
                (setq named-imports
                      (js-imports-parse-es-import-braces)))
               ("\"" (setq display-path (js-imports-path-at-point)))
               ("'" (setq display-path (js-imports-path-at-point)))
               ("=" (progn
                      (forward-char 1)
                      (js-imports-forward-whitespace)
                      (js-imports-parse-value)
                      nil))
               (_ (when-let ((default (js-imports-get-word-if-valid))
                             (beg (point)))
                    (skip-chars-forward js-imports-regexp-name-set)
                    (setq default-import
                          (js-imports-make-item default
                                                :real-name default
                                                :type 1
                                                :parent-end end
                                                :parent-start start
                                                :var-type "import"
                                                :start beg
                                                :end (point))))))
        (js-imports-forward-whitespace)
        (skip-chars-forward ",")
        (js-imports-forward-whitespace))
      (js-imports-backward-whitespace)
      (js-imports-ensure-semicolon)
      (setq end (point))
      (setq children
            (mapcar
             (lambda (it)
               (js-imports-make-item
                it
                :display-path display-path
                :import t
                :type (or (js-imports-get-prop it :type)
                          4)
                :var-type "import"
                :parent-end end
                :parent-start start))
             (delete nil
                     (append
                      `(,default-import ,namespace-import)
                      named-imports))))
      (or children
          dynamic-import
          (and display-path (js-imports-make-item
                             display-path
                             :var-type "import"
                             :parent-start start
                             :parent-end end))))))

(defun js-imports-parse-regexp ()
  "Forward regexp at point and return string."
  (when-let ((re (and (looking-at js-imports--regexp-js-regexp)
                      (js-imports-re-search-forward
                       js-imports--regexp-js-regexp nil t 1)
                      (match-string-no-properties 0))))
    (let ((pos (point)))
      (if (> (skip-chars-forward "gimsuy") 0)
          (concat re (buffer-substring-no-properties pos (point)))
        re))))

(defun js-imports-parse-string ()
  (when-let ((start (and (looking-at "[`'\"]")
                         (point))))
    (forward-sexp 1)
    (buffer-substring-no-properties start (point))))

(defun js-imports-path-at-point ()
  (when-let ((start (and (looking-at "[`'\"]")
                         (point))))
    (forward-sexp 1)
    (buffer-substring-no-properties (1+ start) (1- (point)))))

(defun js-imports-parse-call ()
  (when-let ((key (save-excursion (js-imports-get-obj-key))))
    (unless (js-imports-reserved-word-p key)
      (js-imports-get-obj-key)
      (let ((parts `(,key))
            (item))
        (while (setq item
                     (pcase (js-imports-get-next-char)
                       ("("
                        (let ((a (point)))
                          (forward-sexp 1)
                          (buffer-substring-no-properties a (point))))
                       ("\\."
                        (forward-char 1)
                        (concat "\\." (js-imports-get-obj-key)))))
          (push item parts)
          (js-imports-forward-whitespace))
        (string-join (reverse parts) "\s")))))

(defun js-imports-get-require-path ()
  (let ((case-fold-search nil))
    (when-let* ((bounds
                 (when (looking-at "\\_<\\(require\\)\\_>")
                   (save-excursion
                     (js-imports-get-obj-key)
                     (js-imports-forward-whitespace)
                     (when (equal (js-imports-get-next-char) "(")
                       (js-imports-forward-scope)))))
                (path (save-excursion (goto-char (1+ (car bounds)))
                                      (js-imports-forward-whitespace)
                                      (js-imports-parse-string))))
      (goto-char (cdr bounds))
      (js-imports-strip-quotes path))))

(defun js-imports-parse-value ()
  (let ((start)
        (end)
        (value)
        (count)
        (obj)
        (nodes))
    (setq start (point))
    (while
        (progn
          (and (not obj)
               (setq count (or (and count (1+ count))
                               0))
               (setq value
                     (or
                      (when-let ((v (or (js-imports-parse-funcall)
                                        (js-imports-parse-regexp))))
                        (if (equal (js-imports-get-next-char 1) ".")
                            (setq v (progn (forward-char 1) (concat v ".")))
                          v))
                      (if-let ((operator
                                (save-excursion
                                  (js-imports-get-operator-at-point))))
                          (or
                           (js-imports-forward-angles)
                           (progn
                             (skip-chars-forward operator)
                             (js-imports-forward-whitespace)
                             operator))
                        (pcase (js-imports-get-next-char-or-word)
                          ((or "export" "const" "module.exports"
                               "class" "let" "var" "enum" "interface")
                           nil)
                          ((or "async" "function" "function*")
                           (js-imports-parse-function-declaration))
                          ("as"
                           (js-imports-get-obj-key)
                           (js-imports-forward-whitespace)
                           (js-imports-get-obj-key))
                          ("{"
                           (let ((a (point)))
                             (forward-sexp 1)
                             (if (= count 0)
                                 (js-imports-make-item
                                  (buffer-substring-no-properties a (point))
                                  :value-type "object")
                               (buffer-substring-no-properties a (point)))))
                          ("["
                           (let ((a (point)))
                             (forward-sexp 1)
                             (if (= count 0)
                                 (js-imports-make-item
                                  (buffer-substring-no-properties a (point))
                                  :value-type "array")
                               (buffer-substring-no-properties a (point)))))
                          ("<"
                           (js-imports-forward-angles))
                          ("("
                           (let ((a (point)))
                             (forward-sexp 1)
                             (if (> count 1)
                                 (js-imports-make-item
                                  (buffer-substring-no-properties a (point))
                                  :value-type "call"
                                  :args
                                  (buffer-substring-no-properties a (point)))
                               (buffer-substring-no-properties a (point)))))
                          ((or "\"" "'" "`")
                           (js-imports-make-item
                            (js-imports-parse-string)
                            :value-type "string"))
                          (_ (let ((key (js-imports-get-obj-key)))
                               (if (looking-at ":")
                                   (progn (forward-char 1)
                                          (format "%s:" key))
                                 key)))))))))
      (push value nodes)
      (js-imports-forward-whitespace "\s\t")
      (when-let ((func (and (looking-at "{")
                            (equal (and value
                                        (js-imports-get-prop value :value-type))
                                   "call"))))
        (let ((children (js-imports-parse-scope-inner)))
          (js-imports-forward-whitespace "\s\t")
          (js-imports-make-item
           func :children children
           :value-end (point) :value-type "function"))
        (js-imports-forward-whitespace "\s\t"))
      (when (looking-at "\\(\n\\|\r\\)[\s\t\f]?+[?:|><,]")
        (js-imports-forward-whitespace))
      (setq end (point))
      (when (looking-at "\\.")
        (forward-char 1)
        (js-imports-get-obj-key)
        (setq end (point))
        (js-imports-forward-whitespace "\s\t")))
    (when nodes
      (if (> (length nodes) 1)
          (js-imports-make-item
           (string-join (reverse nodes) "\s")
           :start start :end end)
        (let ((val (pop nodes)))
          (if (listp val)
              (js-imports-make-item
               (mapconcat 'js-imports-stringify val "")
               :start start :end end)
            (js-imports-make-item
             (format "%s" val) :start start :end end)))))))

(defun js-imports-backward-up-list (&optional arg)
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((pos (point)))
    (when-let ((end (ignore-errors
                      (backward-up-list arg)
                      (point))))
      (unless (= end pos)
        end))))

(defun js-imports-backward-list ()
  (when (looking-back "[)]" 0)
    (forward-sexp -1)
    (point)))

(defun js-imports--looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.
Case is not ignored."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun js-imports-parse-variable (&optional node)
  (when (js-imports--looking-at
         "\\_<\\(const\\|let\\|enum\\|var\\|type\\|class\\|interface\\|declare\\|namespace\\)\\_>")
    (let ((start (or (and
                      node
                      (js-imports-get-prop
                       node :parent-start))
                     (point)))
          (node-type)
          (nodes)
          (count))
      (pcase (setq node-type (js-imports-get-obj-key))
        ("declare"
         (js-imports-forward-whitespace)
         (if (not (looking-at "\\_<\\(module\\)\\_>"))
             (when-let ((item (or (js-imports-parse-function-declaration)
                                  (js-imports-parse-variable))))
               (if (listp item)
                   (mapcar (lambda (it)
                             (js-imports-make-item it
                                                   :parent-start start
                                                   :var-type
                                                   (if-let
                                                       ((var-type
                                                         (js-imports-get-prop
                                                          it :var-type)))
                                                       (concat
                                                        "declare " var-type)
                                                     "declare")))
                           item)
                 (js-imports-make-item item
                                       :parent-start start
                                       :var-type
                                       (if-let ((var-type
                                                 (js-imports-get-prop
                                                  item :var-type)))
                                           (concat "declare " var-type)
                                         "declare"))))
           (js-imports-get-obj-key)
           (js-imports-forward-whitespace)
           (js-imports-get-obj-key)
           (js-imports-forward-whitespace)
           (js-imports-parse-scope-inner)))
        ("namespace"
         (js-imports-forward-whitespace)
         (js-imports-get-obj-key)
         (js-imports-forward-whitespace)
         (js-imports-parse-scope-inner))
        ("type"
         (js-imports-forward-whitespace)
         (when-let ((id (js-imports-get-obj-key)))
           (let ((args (js-imports-forward-angles)))
             (js-imports-forward-whitespace)
             (js-imports-make-item
              id
              :args (if (stringp args) (list args) args)
              :var-type (or node "type")
              :value
              (when (equal (js-imports-get-operator-at-point) "=")
                (forward-char 1)
                (js-imports-forward-whitespace)
                (js-imports-parse-value))))))
        ((or "const" "let" "var")
         (js-imports-forward-whitespace)
         (while (or (null count)
                    (when-let ((pos (save-excursion
                                      (js-imports-forward-whitespace)
                                      (when
                                          (or
                                           (looking-at ",")
                                           (member
                                            (js-imports-get-operator-at-point)
                                            js-imports-assignment-operators))
                                        (point)))))
                      (goto-char pos)
                      t))
           (if (null count)
               (setq count 0)
             (skip-chars-forward ",")
             (setq count (1+ count))
             (js-imports-forward-whitespace))
           (let ((token (js-imports-parse-token-at-point))
                 (value)
                 (ts-type)
                 (import-path))
             (js-imports-forward-whitespace)
             (if (equal (js-imports-strip-text-props token) "enum")
                 (when-let ((id (js-imports-get-obj-key)))
                   (js-imports-forward-whitespace)
                   (push (js-imports-make-item
                          id
                          :var-type (string-join `(,node-type "enum") "\s")
                          :value (js-imports-parse-value))
                         nodes))
               (when (equal (js-imports-get-next-char) ":")
                 (forward-char 1)
                 (js-imports-forward-whitespace)
                 (setq ts-type (js-imports-parse-value))
                 (js-imports-forward-whitespace))
               (setq value
                     (or (when (looking-at "=[^=]")
                           (forward-char 1)
                           (js-imports-forward-whitespace)
                           (setq import-path (save-excursion
                                               (js-imports-get-require-path)))
                           (or
                            (js-imports-parse-arrow-function)
                            (js-imports-parse-value)))
                         ts-type))
               (if (not (listp token))
                   (push (js-imports-make-item
                          token
                          :import (when import-path t)
                          :display-path import-path
                          :type (when import-path 1)
                          :var-type
                          (string-join
                           (delete nil
                                   `(,(and
                                       node
                                       (js-imports-strip-text-props
                                        node))
                                     ,node-type)) "\s")
                          :value value)
                         nodes)
                 (dolist (subitem token)
                   (let ((it
                          (js-imports-make-item
                           subitem
                           :import (when import-path t)
                           :type (when import-path 4)
                           :display-path import-path
                           :var-type
                           (string-join
                            (delete nil `(,(and
                                            node
                                            (js-imports-strip-text-props node))
                                          ,node-type)) "\s")
                           :value value)))
                     (push it nodes)))))))
         (js-imports-ensure-semicolon)
         (setq nodes (mapcar (lambda (it)
                               (js-imports-propertize it
                                                      :parent-start start
                                                      :parent-end
                                                      (point)))
                             nodes)))
        ((or "class" "interface" "enum")
         (js-imports-get-obj-key)
         (js-imports-forward-whitespace)
         (let ((id (js-imports-get-obj-key)))
           (js-imports-forward-whitespace)
           (js-imports-forward-angles)
           (js-imports-forward-whitespace)
           (while (js-imports-get-obj-key)
             (js-imports-forward-angles)
             (when (> (js-imports-forward-whitespace "\s\t\n\r\f\v,") 0)
               (js-imports-forward-angles)))
           (js-imports-make-item id
                                 :var-type (format "%s" (or node node-type))
                                 :value (js-imports-parse-object))))))))

(defun js-imports-parse-path ()
  (when-let ((str (js-imports-parse-string)))
    (substring str 1 (1- (length str)))))

(defun js-imports-apply-on-one-many (func items)
  (if (listp items)
      (mapcar (lambda (it) (js-imports-apply-on-one-many func it))
              items)
    (and items (funcall func items))))

(defun js-imports-parse-amd-export ()
  (when-let* ((define-keyword (and (looking-at "\\_<\\(define\\)\\_>")
                                   (js-imports-get-obj-key)))
              (args (progn
                      (js-imports-forward-whitespace)
                      (js-imports-parse-arguments t)))
              (func (seq-find (lambda (it) (js-imports-get-prop
                                       it :args))
                              args))
              (found
               (seq-find (lambda (it) (equal
                                  (js-imports-get-prop
                                   it :var-type) "return"))
                         (js-imports-get-prop func :children))))
    (js-imports-ensure-semicolon)
    (js-imports-make-item (js-imports-get-prop found :value)
                          :type 1
                          :export t
                          :parent-start
                          (js-imports-get-prop define-keyword :start)
                          :parent-end (point))))

(defun js-imports-parse-es-export-braces ()
  (let ((body (js-imports-parse-object t))
        (parent-end)
        (display-path))
    (setq parent-end
          (if
              (null (save-excursion
                      (js-imports-forward-whitespace)
                      (equal (js-imports-get-word)
                             "from")))
              (point)
            (js-imports-re-search-forward "from")
            (js-imports-forward-whitespace)
            (setq display-path (js-imports-path-at-point))
            (js-imports-ensure-semicolon)
            (point)))
    (mapcar (lambda (cell) (let ((real-name
                             (car cell))
                            (as-name
                             (cdr cell)))
                        (js-imports-make-item
                         (or as-name real-name)
                         :start (js-imports-get-prop
                                 real-name :start)
                         :end (js-imports-get-prop
                               (or as-name real-name) :end)
                         :type (if
                                   (and (equal
                                         (js-imports-strip-text-props
                                          real-name)
                                         "default")
                                        (or
                                         (null as-name)
                                         (equal
                                          (js-imports-strip-text-props as-name)
                                          "default")))
                                   1 4)
                         :var-type "export"
                         :parent-end parent-end
                         :display-path display-path)))
            body)))

(defun js-imports-parse-export ()
  (let ((parent-start (point))
        (result
         (cond
          ((looking-at "\\_<\\(export\\)\\_>")
           (let ((next))
             (js-imports-get-obj-key)
             (js-imports-forward-whitespace)
             (setq next (js-imports-get-next-char-or-word))
             (pcase next
               ("*"
                (forward-char 1)
                (js-imports-forward-whitespace)
                (let ((as-name-or-from (js-imports-get-obj-key))
                      (item))
                  (js-imports-forward-whitespace)
                  (setq item
                        (cond
                         ((equal as-name-or-from "from")
                          (js-imports-make-item "*"
                                                :display-path
                                                (js-imports-parse-path)
                                                :var-type "export"
                                                :type 16
                                                :export t))
                         ((equal "as" as-name-or-from)
                          (js-imports-forward-whitespace)
                          (let ((token (js-imports-get-obj-key t)))
                            (js-imports-forward-whitespace)
                            (js-imports-make-item
                             token
                             :display-path
                             :real-name (js-imports-strip-text-props token)
                             :as-name (js-imports-strip-text-props token)
                             (and
                              (equal
                               (js-imports-get-obj-key)
                               "from")
                              (js-imports-forward-whitespace)
                              (js-imports-parse-path))
                             :type 4
                             :export t)))))
                  (js-imports-ensure-semicolon)
                  (setq item
                        (js-imports-make-item item :parent-end (point)))))
               ("{"
                (js-imports-parse-es-export-braces))
               ("type"
                (if-let ((var (js-imports-parse-variable)))
                    var
                  (js-imports-get-obj-key)
                  (js-imports-forward-whitespace)
                  (js-imports-parse-es-export-braces)))
               ((or "const" "var" "let" "class" "interface" "enum")
                (js-imports-parse-variable))
               ("="
                (forward-char 1)
                (js-imports-forward-whitespace)
                (when-let ((value (js-imports-parse-value)))
                  (js-imports-forward-whitespace)
                  (when (js-imports-forward-list)
                    (js-imports-get-obj-key))
                  (js-imports-make-item value
                                        :type 1
                                        :export t
                                        :value value
                                        :var-type "export")))
               ("default"
                (skip-chars-forward next)
                (js-imports-forward-whitespace)
                (js-imports-make-item
                 (or (js-imports-parse-function-declaration)
                     (js-imports-get-obj-key))
                 :var-type "export default"
                 :exports
                 (or (js-imports-parse-object t)
                     (js-imports-parse-function-declaration)
                     (js-imports-parse-value))
                 :export t
                 :type 1
                 :end (point)))
               ((or "async" "function" "function*")
                (when-let ((func (js-imports-parse-function-declaration)))
                  (js-imports-make-item
                   func
                   :export t
                   :value func
                   :type 4
                   :var-type
                   (concat
                    "export "
                    (js-imports-get-prop
                     func :var-type))))))))
          ((looking-at "\\_<\\(\\(module\\.\\)?exports\\)\\_>")
           (let ((start (point))
                 (node (match-string-no-properties 0))
                 (id)
                 (value))
             (goto-char (+ start (length node)))
             (if (member (js-imports-get-next-char 1) '("." "["))
                 (progn
                   (forward-char 1)
                   (setq id (js-imports-get-obj-key t))
                   (js-imports-forward-whitespace)
                   (when (js-imports-get-assigmnent-operator)
                     (js-imports-forward-whitespace)
                     (setq value (js-imports-parse-value))
                     (js-imports-make-item
                      id
                      :type 4
                      :var-type "module.exports"
                      :export t
                      :value value
                      :parent-end (point))))
               (js-imports-forward-whitespace)
               (and
                (js-imports-get-assigmnent-operator)
                (if-let ((named (progn
                                  (js-imports-forward-whitespace)
                                  (js-imports-parse-object t)))
                         (obj-end (point)))
                    (mapcar (lambda (it)
                              (let ((real-name)
                                    (as-name))
                                (when (consp it)
                                  (setq real-name (car it))
                                  (setq as-name (if (stringp (cdr it))
                                                    (cdr it)
                                                  it)))
                                (js-imports-make-item
                                 (or real-name it)
                                 :type 4
                                 :as-name (or as-name it)
                                 :export t
                                 :var-type "module.exports"
                                 :parent-end obj-end)))
                            named)
                  (if-let ((path
                            (progn (when (looking-at "\\_<\\(require\\)\\_>")
                                     (skip-chars-forward
                                      js-imports-regexp-name)
                                     (js-imports-forward-whitespace))
                                   (when (equal
                                          (js-imports-get-next-char) "(")
                                     (forward-char 1)
                                     (js-imports-forward-whitespace)
                                     (js-imports-path-at-point)))))
                      (js-imports-make-item (format "require(%s)" path)
                                            :type 16
                                            :display-path path
                                            :export t
                                            :var-type "module.exports"
                                            :parent-end (point))
                    (setq value (js-imports-parse-value))
                    (js-imports-make-item value
                                          :type 1
                                          :value value
                                          :export t
                                          :var-type "module.exports"
                                          :parent-end (point))))))))
          (t (js-imports-parse-amd-export))))
        (export-node))
    (setq export-node
          (js-imports-apply-on-one-many (lambda (it)
                                          (js-imports-make-item
                                           it
                                           :export t
                                           :as-name
                                           (or (js-imports-get-prop it :as-name)
                                               (js-imports-strip-text-props it))
                                           :real-name
                                           (or
                                            (js-imports-get-prop it :real-name)
                                            (js-imports-strip-text-props it))
                                           :type
                                           (or (js-imports-get-prop it :type)
                                               4)
                                           :parent-start parent-start))
                                        result))
    export-node))

(defun js-imports-parse-statement ()
  (when-let ((item (and (js-imports-reserved-word-p (js-imports-which-word))
                        (js-imports-get-obj-key))))
    (js-imports-forward-whitespace)
    (let ((parts))
      (while (looking-at "[\\[]")
        (let ((prop (js-imports-get-obj-key)))
          (push prop parts)))
      (when parts
        (push item parts)
        (setq item (string-join parts ""))
        (js-imports-forward-whitespace)))
    (pcase item
      ("try" (let ((items (js-imports-parse-scope-inner))
                   (next))
               (js-imports-forward-whitespace)
               (setq next (js-imports-get-obj-key))
               (when (equal next "catch")
                 (js-imports-forward-whitespace)
                 (setq items (append items (js-imports-parse-arguments)))
                 (js-imports-forward-list)
                 (js-imports-forward-whitespace)
                 (setq items (append items (js-imports-parse-scope-inner))))
               (when (equal (save-excursion
                              (js-imports-forward-whitespace)
                              (js-imports-get-obj-key)) "finally")
                 (js-imports-re-search-forward "finally" nil t 1)
                 (js-imports-forward-whitespace)
                 (setq items (append items (js-imports-parse-scope-inner))))
               items))
      ("do" (let ((items (js-imports-parse-scope-inner)))
              (js-imports-re-search-forward "\\_<\\(while\\)\\_>" nil t 1)
              (js-imports-forward-whitespace)
              (js-imports-forward-list)
              items))
      ("while"
       (js-imports-forward-whitespace)
       (js-imports-forward-list))
      ((or "if" "for" "switch")
       (js-imports-forward-list)
       (js-imports-forward-whitespace)
       (if (not (looking-at "{"))
           (js-imports-get-obj-key)
         (js-imports-parse-scope-inner)))
      ("else"
       (pcase (js-imports-get-next-char-or-word)
         ("{" (js-imports-re-search-forward "{" nil t 1)
          (forward-char -1)
          (js-imports-parse-scope-inner))
         ("if" (js-imports-re-search-forward "if" nil t 1)
          (js-imports-forward-whitespace)
          (if (js-imports-get-obj-key)
              (js-imports-parse-value)
            (when (js-imports-forward-list)
              (js-imports-forward-whitespace)
              (js-imports-parse-scope-inner))))))
      ("return"
       (let ((value (js-imports-parse-value)))
         (js-imports-make-item value
                               :var-type "return"
                               :value value)))
      ((or "yield" "yield*")
       (js-imports-forward-whitespace)
       (let ((value (js-imports-parse-value)))
         (js-imports-make-item value
                               :value value
                               :start (js-imports-get-prop item :start)
                               :end (js-imports-get-prop item :end)
                               :var-type item))))))

(defun js-imports-parse-iife ()
  (when-let ((children
              (pcase (js-imports-get-next-char)
                ((or "!" ";")
                 (forward-char 1)
                 (js-imports-forward-whitespace)
                 (when (looking-at "[(]")
                   (forward-char 1)
                   (js-imports-forward-whitespace))
                 (js-imports-parse-function-declaration t))
                ("("
                 (forward-char 1)
                 (js-imports-forward-whitespace)
                 (js-imports-parse-function-declaration t)))))
    (js-imports-forward-whitespace)
    (when (looking-at "[)]")
      (forward-char 1)
      (js-imports-forward-whitespace))
    (when (looking-at "[(]")
      (setq children (append (if (listp children)
                                 children
                               (list children))
                             (js-imports-parse-arguments t))))
    children))

(defun js-imports-parse-node-at-point (&optional deep)
  (let ((node-start)
        (node-end)
        (node))
    (js-imports-forward-whitespace)
    (when (js-imports-ensure-semicolon)
      (js-imports-forward-whitespace))
    (setq node-start (point))
    (setq node (or
                (js-imports-parse-iife)
                (js-imports-parse-export)
                (js-imports-parse-es-import)
                (js-imports-parse-function-declaration deep)
                (js-imports-parse-variable)
                (js-imports-parse-statement)
                (js-imports-parse-assignment)
                ;; (js-imports-parse-funcall)
                (js-imports-parse-value)))
    (unless node
      (setq node-start (point)))
    (when (and (listp node)
               (= (length node) 1))
      (setq node (pop node)))
    (js-imports-ensure-semicolon)
    (setq node-end (point))
    (if (listp node)
        node
      (js-imports-make-item node :start node-start :end node-end))))

(defun js-imports-parse-scope (&optional start end deep callback)
  (when start (goto-char start))
  (let ((node)
        (prev-pos)
        (nodes))
    (js-imports-forward-whitespace)
    (while (or (not (equal prev-pos (point))))
      (setq prev-pos (point))
      (setq node (if end
                     (and (>= end (point))
                          (js-imports-parse-node-at-point deep))
                   (js-imports-parse-node-at-point deep)))
      (when callback
        (funcall callback node))
      (if (listp node)
          (setq nodes (append node nodes))
        (push node nodes))
      (js-imports-forward-whitespace "\s\t\n\r\f\v,=?:+#-"))
    (reverse nodes)))

(defun js-imports-traverse-node ()
  (interactive)
  (when-let ((node (progn (js-imports-forward-whitespace)
                          (js-imports-parse-node-at-point))))
    (prog1 node
      (js-imports-forward-whitespace))))

(defun js-imports-traverse-all ()
  (let ((nodes)
        (node))
    (while (setq node (js-imports-traverse-node))
      (push node nodes))
    nodes))

(defun js-imports-skip-node-at-point ()
  (interactive)
  (js-imports-forward-whitespace)
  (js-imports-parse-node-at-point))

(defun js-imports-get-node-props (node &rest props)
  "Return list of values "
  (let ((result))
    (dolist (prop props)
      (when-let ((value (js-imports-get-prop node prop)))
        (push value result)))
    (reverse result)))

(defun js-imports-get-node-positions (node &rest props)
  "Return list of values "
  (let ((result))
    (dolist (prop props)
      (when-let ((value (js-imports-get-prop node prop)))
        (push value result)))
    (reverse result)))

(defun js-imports-find-by-node-pos (pos node)
  (cond ((consp node)
         (mapcar (apply-partially 'js-imports-find-by-node-pos pos) node))
        ((listp node)
         (seq-find (apply-partially 'js-imports-find-by-node-pos pos) node))
        ((stringp node)
         (let ((value (js-imports-get-prop node :value)))
           (when-let
               ((node-start
                 (car
                  (seq-sort
                   '< (or
                       (delete
                        nil (append
                             (list
                              (js-imports-get-prop
                               value :start)
                              (js-imports-get-prop
                               value :parent-start)
                              (js-imports-get-prop
                               value :value-start))
                             (list
                              (js-imports-get-prop
                               node
                               :parent-start)
                              (js-imports-get-prop
                               node :start)
                              (js-imports-get-prop
                               node
                               :value-start))))))))
                (node-end
                 (car (seq-sort
                       '> (delete nil (append (list
                                               (js-imports-get-prop
                                                value
                                                :end)
                                               (js-imports-get-prop
                                                value
                                                :parent-end)
                                               (js-imports-get-prop
                                                value
                                                :value-end))
                                              (list
                                               (js-imports-get-prop node
                                                                    :end)
                                               (js-imports-get-prop
                                                node
                                                :parent-end)
                                               (js-imports-get-prop
                                                node
                                                :value-end))))))))
             (and (<= node-start pos)
                  (> node-end pos)))))))

(defun js-imports-parse-context (&optional deep)
  (let ((top-scope)
        (scopes)
        (pos (point))
        (parent-node)
        (children))
    (setq top-scope (save-excursion
                      (js-imports-parse-scope (point-min) (point-max) deep)))
    (setq parent-node
          (seq-find
           (apply-partially 'js-imports-find-by-node-pos pos) top-scope))
    (while (progn (setq children
                        (or
                         (delete nil
                                 (append (js-imports-get-prop parent-node
                                                              :args)
                                         (js-imports-get-prop parent-node
                                                              :children)))
                         (delete nil
                                 (append
                                  (js-imports-get-prop
                                   (js-imports-get-prop parent-node :value)
                                   :args)
                                  (js-imports-get-prop
                                   (js-imports-get-prop parent-node :value)
                                   :children))))))
      (setq scopes (append scopes children))
      (setq parent-node
            (seq-find
             (apply-partially 'js-imports-find-by-node-pos pos) children)))
    (js-imports-sort-by-prop :start (if scopes
                                        (append scopes top-scope)
                                      top-scope))))

(defmacro js-imports-ast-in-temp-buffer (&rest body)
  `(let ((left--content (buffer-substring-no-properties
                         (point-min)
                         (point)))
         (right--content (buffer-substring-no-properties
                          (point)
                          (point-max))))
     (js-imports-with-temp-buffer
      (insert left--content)
      (save-excursion (insert right--content))
      ,@body)))

(defun js-imports-parse-context-from-current-buffer (&optional start end)
  (let ((result)
        (content (buffer-substring-no-properties
                  (or start (point-min))
                  (or end (point-max))))
        (p (point)))
    (js-imports-with-temp-buffer
     (if start
         (insert (make-string (1- start) ?\s) content)
       (insert content))
     (goto-char p)
     (setq result (js-imports-parse-current-scope))
     (setq result (append result (js-imports-parse-context)))
     result)))

(defun js-imports-find-parent-node ()
  (let ((top-scope (save-excursion
                     (js-imports-parse-scope (point-min) (point-max)))))
    (seq-find
     (apply-partially 'js-imports-find-by-node-pos (point))
     top-scope)))

(defun js-imports-parse-scope-inner (&optional start)
  (when start (goto-char start))
  (when-let ((bounds (js-imports-forward-scope)))
    (save-excursion (js-imports-parse-scope (1+ (car bounds)) (cdr bounds)))))

(defun js-imports-extract-parent-args (parent)
  (let ((result (and parent
                     (or (js-imports-get-prop parent :args)
                         (js-imports-get-deep-prop parent :value :args)))))
    result))

(defun js-imports-parse-current-scope ()
  (let ((items))
    (save-excursion
      (while (js-imports-backward-up-list)
        (let ((prev-token-cell (js-imports-get-prev-token-ignore-whitespace))
              (looking-at-brackets (looking-at "{"))
              (curr-pos (point))
              (prev-token)
              (prev-token-pos))
          (setq prev-token (car prev-token-cell))
          (setq prev-token-pos (cdr prev-token-cell))
          (when-let ((args
                      (cond ((equal prev-token "=>")
                             (goto-char prev-token-pos)
                             (js-imports-backward-whitespace)
                             (js-imports-backward-list)
                             (skip-chars-backward js-imports-regexp-name-set)
                             (message "1")
                             (let ((args
                                    (or
                                     (js-imports-parse-arguments)
                                     (js-imports-make-item
                                      (js-imports-get-obj-key)
                                      :var-type "Argument")))
                                   (children))
                               (goto-char curr-pos)
                               (setq children (js-imports-parse-scope-inner))
                               (append args children)))
                            ((and (equal prev-token "(")
                                  looking-at-brackets)
                             (message "2")
                             (goto-char prev-token-pos)
                             (save-excursion (js-imports-parse-arguments)))
                            ((and (equal prev-token ")")
                                  looking-at-brackets)
                             (message "3")
                             (let* ((body (save-excursion
                                            (js-imports-parse-scope-inner)))
                                    (args (progn (goto-char (1+ prev-token-pos))
                                                 (forward-sexp -1)
                                                 (js-imports-parse-arguments))))
                               (append args body)))
                            ((and looking-at-brackets
                                  (equal prev-token "="))
                             (message "4")
                             (save-excursion (js-imports-parse-object))))))
            (if (listp args)
                (setq items (append items args))
              (push args items))))))
    items))

;; (when (and (looking-at "{")
;;                      (save-excursion
;;                        (js-imports-backward-whitespace)
;;                        (when (looking-back ")" 0)
;;                          (backward-sexp 1)
;;                          (js-imports-backward-whitespace)
;;                          (equal (js-imports-get-word) "if"))))
;;             (setq items (append items (save-excursion
;;                                         (js-imports-parse-scope-inner))))
;;             (js-imports-backward-whitespace)
;;             (when-let ((args
;;                         (cond ((looking-back "=>" 0)
;;                                (forward-char -2)
;;                                (js-imports-backward-whitespace)
;;                                (when-let
;;                                    ((arg
;;                                      (or (js-imports-get-word)
;;                                          (progn (forward-sexp -1)
;;                                                 (js-imports-parse-arguments)))))
;;                                  arg))
;;                               ((looking-back "[)]" 0)
;;                                (forward-sexp -1)
;;                                (js-imports-parse-arguments))
;;                               ((equal "return" (js-imports-which-word))
;;                                (js-imports-parse-object t))
;;                               ((and
;;                                 (looking-at "{")
;;                                 (looking-back "[(]" 0))
;;                                (prog1 (js-imports-parse-object t)
;;                                  (js-imports-backward-up-list))))))
;;               (if (listp args)
;;                   (setq items (append items args))
;;                 (push args items))))

(defun js-imports-show-node-at-point ()
  (interactive)
  (if (looking-at "\n")
      (js-imports-backward-whitespace)
    (js-imports-forward-whitespace))
  (if-let ((node (save-excursion
                   (skip-chars-backward js-imports-regexp-name)
                   (js-imports-parse-node-at-point))))
      (let ((value)
            (type))
        (js-imports-print (js-imports-to-string node)
                          'emacs-lisp-mode)
        (when (listp node)
          (setq node (if (= 1 (length node))
                         (car node)
                       (js-imports-read-items node))))
        (setq value (js-imports-get-prop node :value))
        (setq type  (and value (js-imports-get-prop value :type)))
        (pcase type
          ("object"
           (js-imports-highlight-node node)
           (save-excursion (goto-char (js-imports-get-prop value :start))
                           (js-imports-read-object)))
          (_ (js-imports-highlight-node node)))
        (js-imports-highlight-node node)
        node)
    (js-imports-read-items)))

(defun js-imports-extract-preselect (pos nodes)
  (let ((found)
        (it)
        (items (seq-copy nodes))
        (prevs))
    (while (and
            (null found)
            (setq it (pop items)))
      (let ((start (or
                    (js-imports-get-prop
                     it :start)
                    (js-imports-get-prop
                     it :parent-start))))
        (when (and start (>= start pos))
          (setq found (or (car prevs) it))))
      (push it prevs))
    (if found
        (seq-position nodes found)
      (length nodes))))

(defun js-imports-flattenize-pp (it &optional indent)
  (if (and (listp it)
           (not (null it)))
      (mapconcat (lambda (l) (js-imports-flattenize-pp l (1+ (or indent 0))))
                 it (concat "\n" (if indent (make-string indent ?\s))))
    it))

(defun js-imports-get-item-keywords (item)
  (if (and item (listp item))
      (mapcar 'js-imports-get-item-keywords item)
    (when-let ((props (and item (stringp item)
                           (text-properties-at 0 item))))
      (cons (js-imports-strip-text-props item)
            (mapcar (lambda (it) (or
                             (js-imports-get-item-keywords it)
                             it))
                    props)))))

(defun js-imports-to-string (object)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-escape-newlines t)
          (print-quoted t))
      (prin1 object (current-buffer)))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((ignore-errors (down-list 1) t)
        (save-excursion
          (backward-char 1)
          (skip-chars-backward "'`#^")
          (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
            (delete-region
             (point)
             (progn (skip-chars-backward " \t\n") (point)))
            (insert "\n"))))
       ((ignore-errors (up-list 1) t)
        (skip-syntax-forward ")")
        (delete-region
         (point)
         (progn (skip-chars-forward " \t\n") (point)))
        (insert ?\n))
       (t (goto-char (point-max)))))
    (goto-char (point-min))
    (indent-sexp)
    (buffer-string)))

(defun js-imports-fontify (content &optional mode-fn &rest args)
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (and (eq major-mode 'emacs-lisp-mode)
                       (not (stringp content)))
                  (js-imports-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))
(defvar js-imports-pp-key nil
  "A sequence of keystrokes, a string or vector, read by `js-imports-print'")

(defun js-imports-print (content &optional mode-fn &rest args)
  (let ((buffer (get-buffer-create "*js-imports-pp*"))
        (content (if mode-fn
                     (apply 'js-imports-fontify
                            (append
                             (list content mode-fn)
                             args))
                   content)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-in-side-window
                '((window-height . fit-window-to-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (unwind-protect
                  (setq js-imports-pp-key
                        (read-key-sequence ""))
                (quit-restore-window window 'kill)
                (if-let ((wind (active-minibuffer-window)))
                    (with-selected-window wind
                      (let ((key-descr
                             (key-description js-imports-pp-key)))
                        (when-let ((command (prog1
                                                (key-binding js-imports-pp-key)
                                              (setq js-imports-pp-key nil))))
                          (if (and (commandp command)
                                   (equal 0
                                          (car (func-arity command))))
                              (funcall command)
                            (and key-descr (execute-kbd-macro
                                            key-descr))))))))))
        (setq cursor-type nil)
        (insert content)))))

(defun js-imports-sort-by-prop (prop items)
  (seq-sort-by (lambda (it)
                 (or (js-imports-get-prop it prop) -1))
               #'< (if (vectorp items)
                       (append items nil)
                     items)))

(defun js-imports-annotate-return (return-value)
  (and return-value (string-join (split-string (format "%s" return-value))
                                 "\s")))

(defun js-imports-node-display-fn (item)
  (if-let ((var-type
            (js-imports-get-prop item :var-type)))
      (let ((return (js-imports-get-prop item :return))
            (value (js-imports-get-prop item :value))
            (args (js-imports-get-prop item :args))
            (type (js-imports-get-deep-prop item :value :value-type))
            (parts)
            (name))
        (unless args (setq args (when-let ((value-args
                                            (and value
                                                 (js-imports-get-prop value
                                                                      :args))))
                                  (format "%s%s" (or value "") value-args))))
        (setq parts (list
                     (and args (format "%s: " args))
                     (and type (numberp type)
                          (format "%s" type))
                     (if (and type
                              (member type '("object" "array")))
                         type
                       (and return
                            (js-imports-trim-length
                             return)))))
        (setq parts (mapconcat (lambda (it)
                                 (js-imports-propertize
                                  (format "%s" it)
                                  'face 'font-lock-doc-face))
                               (delete nil parts)
                               "\s"))
        (setq name
              (concat (propertize var-type
                                  'face 'font-lock-builtin-face)
                      ": "
                      (js-imports-trim-length
                       (pcase var-type
                         ("Property" item)
                         (_ item))
                       120)
                      " "
                      (and parts (js-imports-trim-length parts))))
        name)
    (concat item (if-let ((val (js-imports-get-prop item :value)))
                     (concat " " (js-imports-trim-length val 120))
                   ""))))

(defun js-imports-make-overlay (start end buffer face &optional priority)
  (let ((overlay (make-overlay start end buffer)))
    (when priority
      (overlay-put overlay 'priority priority))
    (overlay-put overlay 'face face)
    overlay))

(defvar js-imports-overlays nil)

(defun js-imports-remove-overlays ()
  (remove-hook 'post-command-hook 'js-imports-remove-overlays t)
  (mapc 'delete-overlay
        (seq-filter
         'overlayp
         js-imports-overlays))
  (setq js-imports-overlays nil))

(defun js-imports-highlight-node (item &optional buffer)
  "Jumps to POS and highlight word at point."
  (when-let ((start (or (js-imports-get-prop item :start)
                        (js-imports-get-prop item :parent-start)))
             (end (or (js-imports-get-prop item :end)
                      (js-imports-get-prop item :parent-end))))
    (let ((parent-start (js-imports-get-prop item :parent-start))
          (parent-end (js-imports-get-prop item :parent-end))
          (value-start (or (js-imports-get-deep-prop item :value :start)
                           (js-imports-get-prop item :value-start)))
          (value-end (or
                      (js-imports-get-deep-prop item :value :end)
                      (js-imports-get-prop item :value-end))))
      (mapc 'delete-overlay (seq-filter 'overlayp js-imports-overlays))
      (setq js-imports-overlays nil)
      (when (and parent-start parent-end)
        (goto-char parent-start)
        (push (js-imports-make-overlay
               parent-start parent-end buffer 'hl-line)
              js-imports-overlays))
      (when (and (numberp value-start) (numberp value-end))
        (push (js-imports-make-overlay
               value-start value-end buffer
               'widget-field 2)
              js-imports-overlays))
      (when (and start end)
        (goto-char start)
        (let ((overlay (js-imports-make-overlay start end buffer
                                                'highlight 2)))
          (push overlay js-imports-overlays)))
      (add-hook 'post-command-hook 'js-imports-remove-overlays nil t))))

(defun js-imports-parse-from-string (&optional content fn &rest args)
  (js-imports-with-temp-buffer
   (if-let ((start (js-imports-get-prop content :start)))
       (progn (insert (make-string (1- start) ?\s))
              (save-excursion (insert content)))
     (insert content)
     (goto-char (point-min)))
   (if fn
       (apply fn args)
       (js-imports-parse-context t))))

(defun js-imports-get-deep-prop (item &rest args)
  (let ((value))
    (while (and item args (setq item (js-imports-get-prop item (pop args))))
      (setq value item))
    (if args
        nil
      value)))

(defun js-imports-get-token-at-point ()
  (or
   (js-imports-parse-regexp)
   (js-imports-get-operator-at-point)
   (js-imports-get-obj-key)
   (when-let ((c (js-imports-get-next-char)))
     (forward-char 1)
     c)))

(defun js-imports-traverse-nodes ()
  (let ((nodes)
        (token)
        (brackets-count 0))
    (while (setq token (progn
                         (js-imports-forward-whitespace)
                         (js-imports-get-token-at-point)))
      (pcase token
        ("{"
         (setq brackets-count (1+ brackets-count)))
        ("}"
         (setq brackets-count (1- brackets-count))))
      (unless (or token
                  (string-empty-p (string-trim token)))
        (push token nodes)))
    (reverse nodes)))

(defun js-imports-merge-item-props (item &rest props)
  (let ((item-props (if (stringp item)
                        (text-properties-at 0 item)
                      item)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let ((val (plist-get props (nth idx props))))
          (plist-put item-props (nth idx props) val))))
    (apply 'js-imports-make-item item item-props)))

(defun js-imports-merge-plists (item props)
  (let ((item-props (if (stringp item)
                        (text-properties-at 0 item)
                      item)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let ((val (plist-get props (nth idx props))))
          (plist-put item-props (nth idx props) val))))
    item-props))

(defun js-imports-parse-object-recoursive (&optional with-props)
  (when (looking-at-p "{")
    (forward-char 1))
  (let ((alist)
        (key)
        (divider))
    (js-imports-forward-whitespace)
    (while (setq key
                 (progn
                   (js-imports-get-obj-key with-props :var-type "Property")))
      (let ((value)
            (value-start)
            (value-end))
        (js-imports-forward-whitespace)
        (setq divider (js-imports-get-next-char-or-word))
        (when (and (stringp divider)
                   (not (member divider '("(" "{" "<" "}" ")"))))
          (forward-char (length divider))
          (js-imports-forward-whitespace))
        (pcase divider
          ("as" (js-imports-forward-whitespace)
           (setq value-start (point))
           (setq value (js-imports-get-obj-key))
           (setq value-end (point)))
          ;; end key
          ((or "," ";")
           (setq value-start nil)
           (setq value-end nil))
          ;; object method
          ("("
           (setq value-start (point))
           (if-let ((method (js-imports-parse-object-method key)))
               (setq value method)
             (goto-char value-start)
             (forward-sexp 1)
             (setq value-end (point))
             (js-imports-forward-whitespace)
             (when (looking-at "{")
               (forward-sexp 1)
               (setq value-end (point))
               (js-imports-forward-whitespace))))
          ;; object value
          (":"
           (setq value-start (point))
           (cond
            ((looking-at "(")
             (if-let ((arrow (js-imports-parse-arrow-function)))
                 (setq value arrow)
               (js-imports-skip-to-char-same-scope "[,;]")
               (setq value-end (point))))
            ((looking-at "function[*\s\t\n]")
             (setq value (js-imports-parse-function-declaration)))
            ((looking-at "{")
             (setq key (js-imports-make-item key :value-start (point)))
             (setq value (js-imports-parse-object-recoursive with-props))
             (when (looking-at "}")
               (forward-char 1)
               (setq key (js-imports-make-item key :value-end (point)))
               (js-imports-forward-whitespace)
               (when (looking-at "|" )
                 (forward-char 1)
                 (js-imports-forward-whitespace)
                 (js-imports-skip-to-char-same-scope ";")))
             (js-imports-forward-whitespace))
            ((looking-at "[[]")
             (let ((arr-bounds (save-excursion (js-imports-forward-scope))))
               (setq value (js-imports-parse-array with-props))
               (goto-char (cdr arr-bounds))))
            (t (js-imports-parse-value)))
           (setq value-end (point)))
          (_ (js-imports-skip-to-char-same-scope "[,;}]")
             (setq value-end (point))))
        (setq value-end (point))
        (js-imports-forward-whitespace)
        (when (looking-at "[,;]")
          (forward-char 1)
          (js-imports-forward-whitespace))
        (when (and with-props key value-start value-end)
          (setq key (js-imports-make-item
                     key
                     :value-start
                     value-start
                     :value-end value-end))
          (when (and (or (stringp value)
                         (null value))
                     value-start value-end)
            (setq value (js-imports-make-item
                         (js-imports-normalize-value
                          (buffer-substring-no-properties
                           value-start
                           value-end))
                         :start value-start
                         :end value-end))))
        (cond ((and value)
               (push (cons key value) alist))
              ((and value-start value-end)
               (setq value
                     (js-imports-normalize-value
                      (buffer-substring-no-properties
                       value-start
                       value-end)))
               (push (cons key value) alist))
              (t
               (push (cons key nil) alist)))))
    (setq alist (reverse alist))))

(defun js-imports-parse-array (&optional with-props)
  (when (looking-at "[[]")
    (let ((arr-items)
          (value))
      (save-excursion
        (forward-char 1)
        (js-imports-forward-whitespace)
        (while (setq value (pcase (js-imports-get-next-char 1)
                             ("{" (js-imports-parse-object with-props))
                             ("["
                              (let ((arr-bounds
                                     (save-excursion
                                       (js-imports-forward-scope))))
                                (prog1 (js-imports-parse-array with-props)
                                  (goto-char (cdr arr-bounds)))))
                             (_ (js-imports-parse-value))))
          (push value arr-items)
          (js-imports-forward-whitespace)
          (when (looking-at ",")
            (forward-char 1)
            (js-imports-forward-whitespace))))
      (apply 'vector (reverse arr-items)))))

(defun js-imports-parse-object (&optional with-props)
  (with-syntax-table js-imports-mode-syntax-table
    (when-let ((bounds (save-excursion
                         (js-imports-forward-scope))))
      (let ((obj (or
                  (js-imports-parse-array with-props)
                  (js-imports-parse-object-recoursive with-props))))
        (goto-char (cdr bounds))
        (if with-props
            obj
          (js-imports-strip-object-props obj))))))

(defun js-imports-parse-arguments (&optional deep parse-object-fn)
	"If text after point is open bracket parse arguments at point.
With optional argument DEEP functions will be parsed recoursively.
PARSE-OBJECT-FN specifies how to parse objects."
  (unless parse-object-fn (setq parse-object-fn
                                'js-imports-parse-token-at-point))
  (when-let ((end (save-excursion
                    (when (looking-at "[(]")
                      (forward-sexp 1)
                      (point)))))
    (forward-char 1)
    (js-imports-forward-whitespace)
    (let ((args)
          (count))
      (while (or (null count)
                 (looking-at ","))
        (if (null count)
            (setq count 0)
          (skip-chars-forward ",")
          (setq count (1+ count))
          (js-imports-forward-whitespace))
        (if-let ((children (js-imports-parse-function-declaration deep)))
            (setq args (append args
                               (if (stringp children) (list children)
                                 children)))
          (let ((token (if (looking-at "[{]")
                           (funcall parse-object-fn)
                         (js-imports-parse-value)))
                (value)
                (item))
            (js-imports-forward-whitespace)
            (setq value
                  (when (looking-at "=[^=]")
                    (forward-char 1)
                    (js-imports-forward-whitespace)
                    (js-imports-parse-value)))
            (js-imports-forward-whitespace)
            (if (stringp token)
                (progn
                  (let ((ts-type (string-match-p ":" token)))
                    (setq item (js-imports-make-item
                                (if ts-type
                                    (substring token 0 ts-type)
                                  token)
                                :var-type
                                (or (js-imports-get-prop token :var-type)
                                    "Argument")
                                :value
                                (or value (and ts-type
                                               (string-trim
                                                (substring token ts-type)))))))
                  (push item args))
              (setq args (append args
                                 (mapcar
                                  (lambda (param)
                                    (js-imports-make-item
                                     param
                                     :var-type (or (js-imports-get-prop
                                                    param
                                                    :var-type)
                                                   "Argument")
                                     :value value))
                                  token)))))))
      (goto-char end)
      (or (reverse args) '("")))))

(defun js-imports-forward-scope ()
  "Move forward across one balanced expression and return it's bounds."
  (when (looking-at "[{[]")
    (let ((scope-start (point))
          (scope-end))
      (forward-sexp 1)
      (setq scope-end (point))
      (cons scope-start scope-end))))

(defun js-imports-forward-list ()
  (let ((count)
        (pos (point))
        (end))
    (while (looking-at "(")
      (forward-sexp 1)
      (setq count (1+ (or count 0)))
      (setq end (point))
      (js-imports-forward-whitespace))
    (if (null count)
        (progn
          (goto-char pos)
          nil)
      (goto-char end))))

(defun js-imports-parse-object-from-string (&optional content with-props)
  (js-imports-with-temp-buffer
   (insert content)
   (goto-char (point-min))
   (js-imports-parse-object with-props)))

(defun js-imports-add-parent-key (parent-key &optional key)
  (cond ((or (null key)
             (string-empty-p parent-key))
         key)
        ((string= "[" (substring-no-properties key 0 1))
         (concat parent-key key))
        ((string-match-p "^['\"]" key)
         (concat parent-key "[" key "]"))
        (t (concat parent-key "." key))))

(defun js-imports-sort-object-props-by-pos (items)
  (let ((max (point-max)))
    (seq-sort-by (lambda (it)
                   (or (js-imports-get-prop it :start)
                       (js-imports-get-prop it :parent-start)
                       max))
                 #'< items)))

(defun js-imports-transpile-alist (item &optional indent)
  (unless indent (setq indent 0))
  (let ((margin (if indent (make-string indent ?\s)
                  (make-string indent ?\s))))
    (concat margin (cond ((and item (consp item)
                               (stringp (car item)))
                          (let ((key (js-imports-strip-text-props (car item)))
                                (value (and item (listp item) (cdr item))))
                            (when (and (string-match-p "[-/]" key)
                                       (not (string-match-p "['\"]" key)))
                              (setq key (concat "'" key "'")))
                            (format (concat "%s: %s") key
                                    (string-trim
                                     (js-imports-transpile-alist
                                      value indent)))))
                         ((vectorp item)
                          (format "[ %s ]"
                                  (mapconcat 'js-imports-transpile-alist
                                             (append item nil) ", ")))
                         ((listp item)
                          (format (concat "{\n" margin
                                          "%s" "\n" margin "}")
                                  (mapconcat (lambda (it)
                                               (concat
                                                margin
                                                (js-imports-transpile-alist
                                                 it (1+ indent))))
                                             item (concat ",\n" margin))))
                         ((numberp item)
                          (format "%s" item))
                         ((eq item t)
                          "true")
                         ((stringp item)
                          (prin1-to-string item))))))

(defun js-imports-maybe-strip-quotes (it)
  (if (and (string-match-p "^[\"']" it)
           (string-match-p "[\"']$" it)
           (not (string-match-p "[-/]" it)))
      (substring it 1 (1- (length it)))
    it))

(defun js-imports-strip-quotes (it)
  (if (and (string-match-p "^[\"']" it)
           (string-match-p "[\"']$" it))
      (substring it 1 (1- (length it)))
    it))

(defun js-imports-get-object-keys (object &optional parent-key)
  "Flatten OBJECT and extract all keys prefixed with PARENT-KEY. "
  (let ((keys))
    (when (vectorp object)
      (setq object
            (seq-map-indexed
             (lambda (it idx) (cons (js-imports-make-item
                                (concat
                                 "[" (js-imports-stringify idx) "]")
                                :start (js-imports-get-prop it :start)
                                :end (js-imports-get-prop it :end))
                               it))
             (append object nil))))
    (dolist (item object)
      (if (and (consp item)
               (car item))
          (let ((key)
                (value (cdr (assoc (car item) object)))
                (vect)
                (subitems))
            (setq key (if parent-key
                          (js-imports-add-parent-key
                           parent-key
                           (js-imports-maybe-strip-quotes
                            (car item)))
                        (js-imports-maybe-strip-quotes (car item))))
            (setq subitems (cond
                            ((vectorp value)
                             (setq vect
                                   (seq-map-indexed
                                    (lambda (it idx)
                                      (cons (js-imports-make-item
                                             (concat
                                              "["
                                              (js-imports-stringify idx)
                                              "]")
                                             :start
                                             (js-imports-get-prop it :start)
                                             :end
                                             (js-imports-get-prop it :end))
                                            it))
                                    (append value nil)))
                             (js-imports-get-object-keys vect key))
                            ((listp value)
                             (js-imports-get-object-keys value key))))
            (if subitems
                (setq keys (append `(,key) keys subitems))
              (push (js-imports-make-item key :value value) keys)))
        (push (car item) keys)))
    keys))

(defun js-imports-parse-token-at-point-old ()
  (if-let
      ((obj
        (and
         (member
          (js-imports-get-next-char)
          '("[" "{"))
         (mapcar
          (lambda (it)
            (if (and (stringp it) (string-match-p "^[\\.]\\{3\\}" it))
                (js-imports-make-item
                 (replace-regexp-in-string
                  "^[\\.]\\{3\\}" "" it)
                 :var-type "...")
              it))
          (js-imports-flatten-object-old
           (js-imports-parse-object t))))))
      obj
    (js-imports-get-obj-key t)))

(defun js-imports-flatten-object-old (object)
  (let ((results))
    (when (vectorp object)
      (setq object (append object nil)))
    (dolist (item object)
      (if (consp item)
          (let ((key (car item))
                (value (cdr (assoc (car item) object))))
            (if (and value (listp value))
                (setq results (append results
                                      (js-imports-flatten-object-old value)))
              (push (or value key) results)))
        (push item results)))
    results))

(defun js-imports-parse-token-at-point ()
  (if-let
      ((obj
        (and
         (member
          (js-imports-get-next-char)
          '("[" "{"))
         (mapcar
          (lambda (it)
            (if (and (stringp it) (string-match-p "^[\\.]\\{3\\}" it))
                (js-imports-make-item
                 (replace-regexp-in-string
                  "^[\\.]\\{3\\}" "" it)
                 :var-type "...")
              it))
          (js-imports-parse-object-pattern
           (js-imports-parse-object t))))))
      obj
    (js-imports-get-obj-key t)))

(defun js-imports-parse-object-pattern (object &optional parent-key)
  (let ((results))
    (when (vectorp object)
      (setq object (append object nil)))
    (dolist (item object)
      (when (listp item)
        (let ((key (car item))
              (value (cdr (assoc (car item) object)))
              (path)
              (vect))
          (setq path (if parent-key
                         (js-imports-add-parent-key
                          parent-key
                          (js-imports-maybe-strip-quotes key))
                       (js-imports-maybe-strip-quotes key)))
          (setq value (cond
                       ((vectorp value)
                        (setq vect
                              (seq-map-indexed
                               (lambda (it idx)
                                 (cons (js-imports-make-item
                                        (concat
                                         "["
                                         (js-imports-stringify idx)
                                         "]")
                                        :start
                                        (js-imports-get-prop it :start)
                                        :end
                                        (js-imports-get-prop it :end))
                                       it))
                               (append value nil)))
                        (js-imports-parse-object-pattern vect path))
                       ((and value (listp value)) 
                        (js-imports-parse-object-pattern value path))
                       ((stringp value)
                        (js-imports-make-item value
                                              :id value
                                              :as-name value
                                              :real-name path))
                       (t (js-imports-make-item key
                                                :id key
                                                :real-name path))))
          (push value results))))
    (flatten-list results)))

(defun js-imports-normalize-object-prop-position (item)
  (if-let ((found (seq-find
                   (lambda (it) (and it (js-imports-get-prop it :start)))
                   (reverse (split-string item "\\.\\|]\\|\\[")))))
      (js-imports-make-item item
                            :start (js-imports-get-prop found :start)
                            :end (js-imports-get-prop found :end)
                            :var-type "Property")
    item))

(defun js-imports-vector-to-obj-indexed (items)
  (if (vectorp items)
      (seq-map-indexed (lambda (it i) (cons (format "%s" i) it))
                       (append items nil))
    items))

(defun js-imports-prop (prop obj)
  (let ((arr))
    (cond ((consp obj)
           (cdr (assoc prop obj)))
          ((and (setq arr (vectorp obj))
                (numberp prop))
           (aref obj prop))
          ((and arr (stringp prop)
                (string-match-p "^[0-9]+$" prop))
           (aref obj (string-to-number prop))))))

(defun js-imports-get-obj-prop (key obj)
  (if (not (listp key))
      (js-imports-prop key obj)
    (let ((args key)
          (prop))
      (while (progn (setq prop (pop args))
                    (when prop
                      (setq obj (js-imports-prop prop obj)))))
      (if args
          nil
        obj))))

(defun js-imports-read-tsconfig-obj (file)
  "Read the json object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (js-imports-with-buffer-or-file-content file
      (goto-char (point-min))
    (js-imports-forward-whitespace)
    (let ((obj (js-imports-strip-object-props (js-imports-parse-object nil))))
      (let ((base-url (js-imports-get-obj-prop
                       '("compilerOptions" "baseUrl") obj))
            (paths (js-imports-get-obj-prop
                    '("compilerOptions" "paths") obj))
            (extends))
        (when-let ((extends (and (or (null paths) (null base-url))
                                 (js-imports-get-obj-prop "extends" obj))))
          extends)
        (when (and (or (null paths) (null base-url))
                   (js-imports-get-obj-prop "extends" obj))
          (when extends))))))

(defun js-imports-strip-object-props (item)
  (cond
   ((and item (consp item)
         (stringp (car item)))
    (let ((key (js-imports-strip-text-props (car item)))
          (value (and item (listp item) (cdr item))))
      (cons (js-imports-maybe-strip-quotes key)
            (js-imports-strip-object-props value))))
   ((vectorp item)
    (apply 'vector
           (mapcar 'js-imports-strip-object-props item)))
   ((listp item)
    (mapcar 'js-imports-strip-object-props item))
   ((numberp item)
    item)
   ((stringp item)
    (cond ((or (js-imports-get-prop item :args)
               (js-imports-get-deep-prop item :value :args))
           `((lambda ,(mapcar
                  (lambda (it) (replace-regexp-in-string
                           "[\\.][\\.][\\.]" "&rest\s"
                           (js-imports-strip-text-props it)))
                  (js-imports-get-prop item :args)))))
          (t (pcase item
               ("null" 'null)
               ("true" 't)
               ("false" '(nil))
               (_ (js-imports-maybe-strip-quotes
                   (js-imports-strip-text-props item)))))))))

(defun js-imports-get-bounds (&optional re)
  (unless re (setq re (or re "-*_~$A-Za-z0-9:\\.")))
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (save-excursion
      (let* ((a (save-excursion
                  (skip-chars-backward re)
                  (point)))
             (b (save-excursion
                  (skip-chars-forward re)
                  (point))))
        (if (string-blank-p (buffer-substring-no-properties a b))
            nil
          (cons a b))))))

(defun js-imports-get-word (&optional regexp)
  "Get thing at point matching REGEXP."
  (when-let ((bounds (js-imports-get-bounds regexp)))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))
(defun js-imports-ast-from-file (file)
  (js-imports-with-buffer-or-file-content file
      (let ((items (js-imports-parse-context t)))
        (mapcar (lambda (it) (js-imports-make-item it :real-path file))
                items))))

(defun js-imports-higlight-node-in-file (item)
  (when-let ((real-path (js-imports-get-prop item :real-path)))
    (js-imports-jump-to-item-other-window item)))

(defun js-imports-jump-to-node (node)
  (if-let ((file (js-imports-get-prop node :real-path)))
      (if (active-minibuffer-window)
          (when-let ((value
                      (js-imports-with-buffer-or-file-content file
                          (when-let
                              ((start (car
                                       (seq-sort
                                        #'<
                                        (delete nil
                                                (mapcar
                                                 (apply-partially
                                                  'js-imports-get-prop node)
                                                 '(:start :parent-start))))))
                               (end
                                (car
                                 (seq-sort #'>
                                           (delete nil
                                                   (mapcar
                                                    (apply-partially
                                                     'js-imports-get-prop
                                                     node)
                                                    '(:end :parent-end)))))))
                            (buffer-substring-no-properties start end)))))
            (js-imports-print
             (js-imports-strip-text-props value) 'js-mode))
        (find-file file)
        (js-imports-highlight-node node))
    (js-imports-highlight-node node)))

(defun js-imports-group-by (prop items)
  (seq-reduce (lambda (acc it)
                (when-let ((val (js-imports-get-prop it prop)))
                  (if-let ((vals (plist-get acc val)))
                      (setq acc (plist-put acc val (append vals
                                                           (list it))))
                    (setq acc (plist-put acc val (list it))))
                  acc))
              items '()))

(defun js-imports-map-with-children (nodes &optional deep)
  (seq-reduce
   (lambda (acc it) (if-let ((children
                         (if (listp it)
                             (if deep
                                 (js-imports-map-with-children it)
                               (mapcar
                                (lambda (n)
                                  (delete
                                   nil
                                   (append
                                    (js-imports-get-prop n :args)
                                    (js-imports-get-prop n :children))))
                                it))
                           (delete
                            nil
                            (append (js-imports-get-prop it :args)
                                    (js-imports-get-prop it :children))))))
                   (setq acc (append acc children))
                 acc))
   nodes nodes))

(defun js-imports-backward-declaration-keywords ()
  (let ((pos (point))
        (nodes))
    (while (when
               (js-imports-reserved-word-p (js-imports-get-word))
             (let ((b (point))
                   (a))
               (skip-chars-backward js-imports-regexp-name)
               (setq a (point))
               (push (cons a b) nodes)))
      (js-imports-backward-whitespace))
    (if (caar nodes)
        (goto-char (caar nodes))
      (goto-char pos)
      nil)))

(defun js-imports-export-it ()
  (interactive)
  (save-excursion
    (if (and (= 0 (car (syntax-ppss (point))))
             (looking-at js-imports-delcaration-keywords--re)
             (null (save-excursion (js-imports-backward-whitespace)
                                   (js-imports-backward-declaration-keywords))))
        (insert "export\s")
      (when-let ((parent (js-imports-find-parent-node)))
        (unless (js-imports-get-prop parent :export)
          (goto-char (js-imports-get-prop parent :start))
          (insert "export\s")
          (message "exported %s" parent))))))
(defun js-imports-beg-of-defun ()
  (interactive)
  (unless (and (= 0 (car (syntax-ppss (point))))
               (looking-at js-imports-delcaration-keywords--re)
               (null (save-excursion
                       (js-imports-backward-whitespace)
                       (js-imports-backward-declaration-keywords))))
    (when-let* ((parent (js-imports-find-parent-node))
                (keyword (seq-find
                          (apply-partially 'js-imports-get-prop parent)
                          '(:start :parent-start :value-start))))
      (goto-char (js-imports-get-prop parent keyword)))))

(defun js-imports-ast-find-closest-node ()
  (or
   (when-let* ((pos (point))
               (item
                (save-excursion
                  (js-imports-backward-whitespace)
                  (js-imports-backward-declaration-keywords)
                  (js-imports-parse-node-at-point))))
     (if (listp item)
         (seq-find
          (lambda (it) (let
                      ((start (js-imports-get-prop it :start))
                       (end (js-imports-get-prop it :end)))
                    (and (>= pos start)
                         (<= pos end))))
          item)
       item))
   (js-imports-find-parent-node)))

(defun js-imports-narrow-to-defun ()
  (interactive)
  (when-let ((node (js-imports-find-parent-node)))
    (narrow-to-region (js-imports-get-prop node :start)
                      (js-imports-get-prop node :end))))

(defun js-imports-ast-mark-it ()
  (interactive)
  (when-let ((node (js-imports-ast-find-closest-node)))
    (push-mark (js-imports-get-prop node :end) nil t)
    (activate-mark)
    (goto-char (js-imports-get-prop node :start))))

(defun js-imports-get-sorted-items-in-current-buffer ()
  (js-imports-sort-object-props-by-pos
   (flatten-list
    (js-imports-parse-context-from-current-buffer))))

(defmacro js-imports-with-preview (action)
  `(lambda (it) (if (active-minibuffer-window)
               (js-imports-print it 'js-mode)
             (,action it))))

(defun js-imports-combine-actions (action &optional persistent-action)
  (lambda (it) (if (and persistent-action (active-minibuffer-window))
              (funcall persistent-action it)
            (if action (funcall action it) it))))

(defun js-imports-ast-extract-exports (init-path)
  "Return all found exports in INIT-PATH and reexported modules."
  (let* ((external-paths)
         (cell)
         (processed-paths)
         (remove-defaults)
         (all-exports)
         (map-result
          (lambda (result)
            (let ((item
                   (cond
                    ((listp result)
                     (when
                         (js-imports-get-prop
                          (car result)
                          :export)
                       (when remove-defaults
                         (setq result (seq-remove
                                       (lambda (it)
                                         (equal 1
                                                (js-imports-get-prop it :type)))
                                       result)))
                       (setq all-exports
                             (append
                              (mapcar (lambda (it)
                                        (js-imports-make-item
                                         it
                                         :real-path (cdr cell)))
                                      result)
                              all-exports))))
                    ((null (js-imports-get-prop result :export))
                     (setq
                      all-exports
                      (append
                       (seq-filter (lambda (it) (js-imports-get-prop it :export))
                                   (js-imports-get-prop result :children))
                       all-exports)))
                    ((= 16 (js-imports-get-prop result :type))
                     (when-let ((external-path (js-imports-path-to-real
                                                (js-imports-get-prop
                                                 result
                                                 :display-path))))
                       (push (cons result external-path)
                             external-paths)))
                    ((and remove-defaults
                          (equal (js-imports-get-prop result :type) 1))
                     nil)
                    (t
                     (push (js-imports-make-item
                            result
                            :real-path (cdr cell))
                           all-exports)))))
              item))))
    (setq cell (cons nil (js-imports-path-to-real
                          init-path default-directory)))
    (push cell external-paths)
    (while (setq cell (pop external-paths))
      (push cell processed-paths)
      (when (cdr cell)
        (setq remove-defaults (and (car cell)
                                   (equal (js-imports-get-prop
                                           (car cell)
                                           :type)
                                          1)))
        (js-imports-with-buffer-or-file-content
            (cdr cell)
            (goto-char (point-min))
          (js-imports-parse-scope (point-min) (point-max) t map-result))))
    all-exports))

(defun js-imports-node-definition-p (node)
  (member (js-imports-strip-text-props
           (js-imports-get-prop node
                                :var-type))
          (append js-imports-delcaration-keywords
                  '("Argument"
                    "Property"))))

(defun js-imports-pluck-exports (items)
  (js-imports-filter-with-prop :export t
                               items))

(defun js-imports-pluck-definitions (items)
  (seq-filter 'js-imports-node-definition-p items))

(defun js-imports-pluck-imports (items)
  (js-imports-filter-with-prop
   :import t items))

(defun js-imports-highlight-node-at-point ()
  (interactive)
  (js-imports-print (js-imports-parse-node-at-point)))

(defun js-imports-ast-parse-html (html)
  (when (and (fboundp 'libxml-available-p)
             (libxml-available-p))
    (with-temp-buffer
      (insert html)
      (libxml-parse-html-region (point-min) (point-max)))))

(defun js-imports-parse-html-buffer ()
  (when (and (fboundp 'libxml-available-p)
             (libxml-available-p))
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (js-imports-ast-parse-html content))))

(defvar js-imports-ast-load-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar js-imports-ast-node-builtins-alist nil)

(defun js-imports-ast-write-builtins ()
  (when-let ((node (executable-find "node")))
    (let ((result (with-temp-buffer
                    (list
                     (apply 'call-process node nil
                            (current-buffer) nil
                            (list "--no-warnings"
                                  (expand-file-name "get-node-builtins.js"
                                                    js-imports-ast-load-dir)))
                     (js-imports-parse-object-from-string
                      (string-trim (buffer-string))
                      t)))))
      (if (equal (car result) 0)
          (setq js-imports-ast-node-builtins-alist
                (mapcar (lambda (it) (let ((module (js-imports-make-item
                                               (car it)
                                               :export t
                                               :type 1
                                               :var-type "export default"
                                               :as-name (car it)
                                               :real-name (car it)))
                                      (exports (cdr it)))
                                  (setq exports (mapcar
                                                 (lambda (e) (js-imports-make-item
                                                         (car e)
                                                         :export t
                                                         :type 4
                                                         :var-type "export"
                                                         :as-name (car e)
                                                         :real-name (car e)))
                                                 (cdr it)))
                                  (setq exports (push module exports))
                                  (cons
                                   (js-imports-strip-text-props (car it))
                                   exports)))
                        (cadr result)))
        (message (cadr result))))))

(defun js-imports-extract-node-builins ()
  (let ((modules (append
                  (js-imports-parse-object-from-string
                   (shell-command-to-string
                    (string-join '("node -p" "\"" "require('repl')._builtinLibs.reduce((acc, key) => ({ ...acc, [key]: Object.keys(require(key)).reduce((a, k) => ({ ...a, [k]: k }), {}) }), {});" "\"") "\s")))
                  nil)))
    modules))

(defun js-imports-make-builtin-exports ()
  (mapcar (lambda (it) (cons (car it) (append (list
                                          (js-imports-make-item
                                           (car it)
                                           :as-name (car it)
                                           :real-name (car it)
                                           :export t
                                           :var-type "builtin"
                                           :type 1))
                                         (mapcar
                                          (lambda (i)
                                            (js-imports-make-item
                                             (car i)
                                             :as-name (car i)
                                             :real-name (car it)
                                             :export t
                                             :var-type "builtin"
                                             :type 4))
                                          (cdr it)))))
          (js-imports-extract-node-builins)))

(defun js-imports-object-complete (obj-str)
  (interactive)
  (when-let* ((obj (js-imports-parse-object-from-string
                    obj-str))
              (obj-keys (js-imports-get-object-keys obj)))
    (let ((choices (if-let ((prefix (js-imports-get-word)))
                       (all-completions prefix
                                        obj-keys)
                     obj-keys)))
      (js-imports-insert-complete (completing-read "Complete:\s" choices)))))

(defun js-imports-insert (item &optional separator)
  (let ((parts))
    (setq parts
          (if-let ((current-word (js-imports-which-word "-*_~$A-Za-z0-9:#\\+")))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties item (length current-word)))
                  (list (or separator "\s") item)))
            (list item)))
    (apply 'insert parts)))

(defun js-imports-insert-complete (item)
  (let* ((value (when (and (js-imports-get-prop item :value)
                           (stringp (js-imports-get-prop item :value))
                           (string-match-p "^function[\s\t\n(]"
                                           (js-imports-get-prop item :value)))
                  (js-imports-with-temp-buffer
                   (save-excursion (insert (js-imports-get-prop item :value)))
                   (js-imports-parse-function-declaration))))
         (args  (if value
                    (js-imports-get-deep-prop value :args)
                  (or (js-imports-get-deep-prop item :args)
                      (js-imports-get-deep-prop item :value :args))))
         (arg))
    (let ((parts (if-let ((current-word (js-imports-get-word)))
                     (if (string-prefix-p current-word item)
                         (list (substring-no-properties item
                                                        (length current-word)))
                       (list item))
                   (list item))))
      (seq-remove 'string-blank-p (apply 'insert parts))
      (when (listp args)
        (while (setq arg (pop args))
          (let ((str (read-string (format "%s " arg))))
            (if (looking-back item 0)
                (progn (insert "(" str ")")
                       (re-search-backward "[)]" nil t 1))
              (insert ", " str))))))
    item))

(defun js-imports-read-item-complete (&optional nodes)
  (interactive)
  (let ((item (funcall-interactively
               'js-imports-read-items nodes :action 'identity)))
    (js-imports-insert-complete item)))

(defun js-imports-get-failed-files ()
  (js-imports-init-project)
  (let ((failed (seq-remove 'js-imports-path-to-real
                            (append
                             (js-imports-node-modules-candidates)
                             (js-imports-find-project-files)))))
    failed))

(defvar js-imports-ast-modules-hash (make-hash-table :test 'equal))

(defun js-imports-ast-exports-reports ()
  (interactive)
  (let ((lines))
    (maphash (lambda (key value) (let ((count (length value)))
                              (push (format "%s: %s" key count) lines)))
             js-imports-ast-modules-hash)
    (completing-read "Exports:\s" lines)))

(defun js-imports-tokenize ()
  (pcase (js-imports-get-next-char 2)
    ("/*" (js-imports-forward-whitespace))
    ("//" (js-imports-forward-whitespace)))
  (or (js-imports-get-operator-at-point)
      (when-let ((char (js-imports-get-next-char 1)))
        (pcase char
          ("/" (let ((beg (point)))
                 (when (re-search-forward "[^/]/" nil t 1)
                   (skip-chars-forward "a-z")
                   (buffer-substring-no-properties beg (point)))))
          ((or "'" "\"" "`")
           (let ((beg (point)))
             (when (js-imports-re-search-forward "." nil t 1)
               (buffer-substring-no-properties beg (point)))))
          ((or "\n" "\s" "\t")
           (js-imports-forward-whitespace)
           (js-imports-tokenize))
          (_ (if (string-match-p "[_$A-Za-z0-9]" char)
                 (progn
                   (let ((beg (point)))
                     (skip-chars-forward "_$A-Za-z0-9")
                     (buffer-substring-no-properties beg (point))))
               (forward-char 1)
               char))))))

(defun js-imports-get-tokens ()
  (let ((node)
        (nodes))
    (while (setq node (js-imports-tokenize))
      (let ((next (js-imports-tokenize)))
        (push node nodes)
        (when next
          (push next nodes))))
    (reverse nodes)))

(declare-function ivy-read "ivy")
(declare-function ivy-set-display-transformer "ivy")
(declare-function ivy-set-actions "ivy")
(declare-function with-ivy-window "ivy")
(declare-function ivy-state-current "ivy")
(defvar ivy-exit)
(defvar ivy-last)
(defvar ivy--display-transformers-alist)

(defun js-imports-print-item-ivy (&optional str)
  (interactive)
  (when-let ((item (or str
                       (and (not ivy-exit)
                            (ivy-state-current ivy-last)))))
    (js-imports-print item)))

(defun js-imports-print-item-keywords-ivy ()
  (interactive)
  (js-imports-compose #'js-imports-print #'js-imports-to-string)
  (js-imports-print
   (js-imports-to-string
    (ivy-state-current ivy-last))
   'emacs-lisp-mode))

(defvar js-imports-ivy-read-items-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p") 'js-imports-print-item-ivy)
    (define-key map (kbd "C-c C-p") 'js-imports-print-item-keywords-ivy)
    map)
  "Keymap for `js-imports-read-items'.")
(ivy-set-actions 'js-imports-read-items
                 '(("h" js-imports-highlight-node "highlight")
                   ("p" js-imports-print "pp")))

(defun js-imports-get-object-parent-name ()
  (save-excursion
    (js-imports-backward-whitespace)
    (when (equal (js-imports-get-prev-char) "=")
      (forward-char -1)
      (js-imports-backward-whitespace)
      (skip-chars-backward js-imports-regexp-name)
      (js-imports-get-obj-key t))))

(defun js-imports-get-object-items (obj &optional parent-key)
  (js-imports-sort-object-props-by-pos
   (mapcar 'js-imports-normalize-object-prop-position
           (js-imports-get-object-keys obj parent-key))))

(defun js-imports-ast-pluck-definitions (items)
  (seq-filter
   'js-imports-node-definition-p items))

(defun js-imports-show-definitions ()
  (interactive)
  (let ((definitions (seq-filter
                      'js-imports-node-definition-p
                      (js-imports-parse-context-from-current-buffer))))
    (funcall-interactively 'js-imports-read-items definitions)))

(defun js-imports-read-items (&optional passed-nodes &rest props)
  (interactive)
  (unless (memq 'js-imports-read-items ivy--display-transformers-alist)
    (ivy-set-display-transformer
     'js-imports-read-items
     'js-imports-node-display-fn))
  (let ((nodes)
        (pos (point))
        (preselect-idx))
    (setq nodes
          (or passed-nodes
              (js-imports-get-object-items
               (save-excursion (js-imports-parse-object t))
               (js-imports-get-object-parent-name))
              (js-imports-parse-context-from-current-buffer)))
    (setq preselect-idx (js-imports-extract-preselect pos nodes))
    (let ((params (js-imports-merge-plists
                   (list :preselect preselect-idx
                         :keymap js-imports-ivy-read-items-map
                         :action 'js-imports-jump-to-node
                         :caller 'js-imports-read-items)
                   props)))
      (apply 'ivy-read (append (list "Node:\s" nodes)
                               params)))))

(defun js-imports-read-object (&optional parsed-obj action)
  (interactive)
  (unless (memq 'js-imports-read-object ivy--display-transformers-alist)
    (ivy-set-display-transformer
     'js-imports-read-object
     'js-imports-node-display-fn))
  (when-let ((obj (or parsed-obj (save-excursion (js-imports-parse-object t)))))
    (let ((parent-key (save-excursion
                        (js-imports-backward-whitespace)
                        (when (equal (js-imports-get-prev-char) "=")
                          (forward-char -1)
                          (js-imports-backward-whitespace)
                          (skip-chars-backward js-imports-regexp-name)
                          (js-imports-get-obj-key t))))
          (items))
      (setq items
            (js-imports-sort-object-props-by-pos
             (mapcar 'js-imports-normalize-object-prop-position
                     (js-imports-get-object-keys obj parent-key))))
      (ivy-read "Property\s" (if parent-key
                                 (push parent-key items)
                               items)
                :caller 'js-imports-read-object
                :keymap js-imports-ivy-read-items-map
                :preselect (js-imports-get-word)
                :action (js-imports-combine-actions
                         action
                         'js-imports-jump-to-node)))))

(defun js-imports-read-exports (&optional nodes)
  (interactive)
  (js-imports-init-project)
  (unless (memq 'js-imports-read-exports ivy--display-transformers-alist)
    (ivy-set-display-transformer
     'js-imports-read-exports
     'js-imports-node-display-fn))
  (let* ((file (js-imports-read-file))
         (nodes (or nodes
                    ;; (js-imports-extract-all-exports file)
                    (js-imports-ast-extract-exports file))))
    (setq this-command 'js-imports-read-exports)
    (apply 'ivy-read (list "Node:\s" nodes
                           :keymap js-imports-ivy-read-items-map
                           :action 'js-imports-jump-to-node
                           :caller 'js-imports-read-exports))))

(defun js-imports-read-items-from-file (&optional file)
  (interactive)
  (js-imports-init-project)
  (unless (memq 'js-imports-read-items ivy--display-transformers-alist)
    (ivy-set-display-transformer
     'js-imports-read-items-from-file
     'js-imports-node-display-fn))
  (let* ((file (or file
                   (js-imports-path-to-real
                    (js-imports-read-file) default-directory)))
         (nodes (seq-filter (lambda (it) (js-imports-get-prop it :export))
                            (js-imports-ast-from-file file))))
    (setq this-command 'js-imports-read-items-from-file)
    (apply 'ivy-read (list "Node:\s" nodes
                           :keymap js-imports-ivy-read-items-map
                           :action 'js-imports-jump-to-node
                           :caller 'js-imports-read-items-from-file))))

(defun js-imports-ast-complete-node-builtins ()
  (interactive)
  (unless js-imports-ast-node-builtins-alist
    (setq js-imports-ast-node-builtins-alist
          (js-imports-extract-node-builins)))
  (ivy-set-display-transformer
   'js-imports-ast-complete-node-builtins
   'js-imports-node-display-fn)
  (let ((prefix (js-imports-get-word)))
    (if-let ((choices
              (and prefix
                   (all-completions (js-imports-get-word)
                                    (js-imports-get-object-keys
                                     js-imports-ast-node-builtins-alist)))))
        (ivy-read "Property\s" choices
                  :caller 'js-imports-ast-complete-node-builtins
                  :keymap js-imports-ivy-read-items-map
                  :preselect prefix
                  :action 'js-imports-insert-complete)
      (js-imports-insert-complete
       (js-imports-read-object js-imports-ast-node-builtins-alist)))))

(defun js-imports-ast-complete (obj)
	"Skip to char same scope.
OBJ is ."
  (interactive)
  (ivy-set-display-transformer
   'js-imports-ast-complete
   'js-imports-node-display-fn)
  (js-imports-read-object obj 'js-imports-insert-complete))

(provide 'js-imports-ast)
;;; js-imports-ast.el ends here
