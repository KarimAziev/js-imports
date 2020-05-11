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


(defvar js-import-import-regexp
  "^import[ \t\\n]+\\([a-zZ-A0-9_$]+\\|*[\t\n\s]+as[\t\n\s][a-zZ-A0-9_$]\\)*[ \t\\n]?,?[ \t\\n]?+\\({[^}]+}\\)?[ \t\\n]from[ \t]+['\"']\\([^['\"]*\\)"
  "Regexp for searching import declarations")

(defconst js-import-regexp-name
  "_$A-Za-z0-9"
  "Regexp matching the start of a js identifier.")

(defconst js-import-regexp-name-set
  (concat "[" js-import-regexp-name "]")
  "Regexp set matching the start of a js identifier.")

(defconst js-import-regexp-name-with-separators
  (concat js-import-regexp-name ",\s\n\t")
  "Regexp matching js identifier's chars with separators")

(defconst js-import-regexp-import-keyword
  "\\(^\\| +\\)import[ \t\n]+"
  "Regexp matching keyword import")

(defconst js-import-regexp-export-keyword
  "\\(^\\| +\\)export[ \t\n]+"
  "Regexp matching keyword export")

(defconst js-import-file-ext-regexp
  "\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)$"
  "Regexp matching js, jsx and ts extensions files.")

(defvar js-import-file-index-regexp
  "\\(/\\|^\\)\\index\\(\\(\\.d\\)?\\.tsx?\\|.jsx?\\)?$")

(defconst js-import-reserved-js-words '("abstract" "any" "as" "async" "await" "boolean" "bigint" "break" "case" "catch" "class" "const"
                                        "constructor" "continue" "declare" "default" "delete" "do" "else"
                                        "enum" "export" "extends" "extern" "false" "finally" "for"
                                        "function" "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
                                        "interface" "keyof" "let" "module" "namespace" "never" "new" "null" "number" "object" "of"
                                        "private" "protected" "public" "readonly" "return" "set" "static" "string"
                                        "super" "switch"  "this" "throw" "true"
                                        "try" "type" "typeof" "unknown" "var" "void"
                                        "while" "yield")
  "List of reserved words in javascript")


(defun js-import-word-reserved?(str &optional reserved-list)
  "Check if STR is js reserved word"
  (unless reserved-list (setq reserved-list js-import-reserved-js-words))
  (when (stringp str)
    (member str reserved-list)))

(provide 'js-import-regexp)
;;; js-import-regexp.el ends here
