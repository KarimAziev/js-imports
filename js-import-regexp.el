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

(defvar js-import-imports-regexp
  "^import[ \t\\n]+\\([a-zZ-A0-9_$]+\\|*[\t\n\s]+as[\t\n\s][a-zZ-A0-9_$]\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']\\([^['\"]*\\)")

(defvar js-import-regexp-export-all-from
  "^export[ \s\t\n]+\\*+[ \s\t\n]+from[ \t]+['\"']\\([^['\"]*\\)")

(defvar js-import-exclude-regexp
  "^export[ \t]\\([ \t]default[ \t]\\)?\\|[ \t]import[ \t]\\|const[ \t]\\|let[ \t]\\|var[ \t]\\|type[ \t]\\|interface[ \t]\\|function[\\*]\\|class[ \t]?[ \t]\\|function*[ \t]\\|[ \t]class[ \t]\\|[ \t]let[ \t]\\||[ \t]var[ \t]\\|[ \t]type[ \t]\\|[ \t]interface[ \t]\\|[,\f\t\n\r\v}{]\\|[ \t]from[ \t]")

(defvar js-import-regexp-export-as
  "[a-zZ-A0-9_$]*[ \\t]+as[ \\t]")

(defvar js-import-regexp-import-as
  "[ \\t\\s\\n]as[ \\t\\s\\n]+[a-zZ-A0-9_$]*")

(defvar js-import-regexp-export-exclude-regexp
  (concat js-import-exclude-regexp "\\|" js-import-regexp-export-as))

(defvar js-import-regexp-for-imports-exclude
  (concat js-import-exclude-regexp "\\|" js-import-regexp-import-as))

(defvar js-import-regexps-vars
  (list "\\_<\\(?:var\\|let\\|const\\)\\_>")
  "Case-sensitive regexps for detecting JS variables in JavaScript buffers. ")

(defvar js-import-export-regexp
  "^export[ \s\t\n]+\\(const\\|let\\|var\\|type\\|interface\\|function[*]?\\|class\\)[\t\s\n]+\\([a-zZ-A0-9_$,]+\\)?\\|export[ \s\t\n]+{\\([^}]+\\)\\|export[ \s\t\n]+\\(default\\)[ \s\t\n]"
  "Regexp for searching export declarations")

(defvar js-import-import-regexp
  "^import[ \t\\n]+\\([a-zZ-A0-9_$]+\\|*[\t\n\s]+as[\t\n\s][a-zZ-A0-9_$]\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']\\([^['\"]*\\)"
  "Regexp for searching import declarations")

(defvar js-import-import-regexp-exclude
  "^import[ \t\n]\\|[ \t\n]from[ \t\n]+.*\\|,[ \s\t\n]*{"
  "Regexp for excluding meta-words from import matches")

(defun js-import-make-import-regexp-from-path(path)
  "Build regexp of import statement with given path"
  (concat "import[ \t\\n]+\\([a-zZ-A0-9_$]+\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']" path "['\"']"))

(defvar js-import-file-index-regexp
  "\\(/\\|^\\)\\index\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)?$")

(defconst js-import-reserved-words
  '("abstract"
    "as"
    "arguments"
    "await*"
    "boolean"
    "break"
    "byte"
    "case"
    "catch"
    "char"
    "class*"
    "const"
    "continue"
    "debugger"
    "default"
    "delete"
    "do"
    "double"
    "else"
    "enum*"
    "eval"
    "export*"
    "extends*"
    "false"
    "final"
    "finally"
    "float"
    "for"
    "function"
    "goto"
    "if"
    "implements"
    "import"
    "in"
    "instanceof"
    "int"
    "interface"
    "let*"
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
    "super*"
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

(defun js-import-word-reserved?(str)
  "Check if STR is js reserved word"
  (member str js-import-reserved-words))

(provide 'js-import-regexp)
;;; js-import-regexp.el ends here
