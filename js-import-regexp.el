;;; -*- lexical-binding: t -*-
;;; js-import-regexp.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

;; js-import-export regexps

;;; Code:

(defcustom js-import-unsaved-file-regexp "[a-zZ-A0-9]*.#[a-zZ-A0-9/]"
  "Regexp used for filtering test files"
  :group 'js-import
  :type 'string)

(defcustom js-import-test-file-regexp "__tests__/\\|[a-zZ-A]+\\.test[s]"
  "Regexp used for filtering test files"
  :group 'js-import
  :type 'string)

(defvar js-import-word-chars-regexp "[a-zZ-A0-9_$]")

(defvar js-import-import-regexp
  "^import[ \t\\n]+\\([a-zZ-A0-9_$]+\\|*[\t\n\s]+as[\t\n\s][a-zZ-A0-9_$]\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']\\([^['\"]*\\)"
  "Regexp for searching import declarations")

(defvar js-import-file-index-regexp
  "\\(/\\|^\\)\\index\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)?$")


(defconst js-import-reserved-words
  '("abstract"
    "as"
    "arguments"
    "await"
    "boolean"
    "break"
    "byte"
    "case"
    "catch"
    "char"
    "class"
    "const"
    "continue"
    "debugger"
    "default"
    "delete"
    "do"
    "double"
    "export",
    "else"
    "enum"
    "eval"
    "export"
    "extends"
    "false"
    "final"
    "finally"
    "float"
    "for"
    "function"
    "function*"
    "goto"
    "if"
    "implements"
    "import"
    "in"
    "instanceof"
    "int"
    "interface"
    "let"
    "long"
    "native"
    "new"
    "null"
    "package"
    "private"
    "protected"
    "public"
    "return"
    "short"
    "static"
    "super"
    "switch"
    "synchronized"
    "this"
    "throw"
    "throws"
    "transient"
    "true"
    "try"
    "typeof"
    "var"
    "void"
    "volatile"
    "while"
    "with"
    "yield")
  "List of reserved words in javascript")

(defun js-import-word-reserved?(str &optional reserved-list)
  "Check if STR is js reserved word"
  (unless reserved-list (setq reserved-list js-import-reserved-words))
  (when (stringp str)
    (member str reserved-list)))

(provide 'js-import-regexp)
;;; js-import-regexp.el ends here
