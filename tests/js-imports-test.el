;;; -*- lexical-binding: t -*-
;;; js-imports-test.el --- This is an Emacs Lisp file with Emacs Lisp code.

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

(require 'ert)
(require 'js-imports)

(ert-deftest js-imports-test-relative-p ()
  (should (equal (js-imports-relative-p ".") t))
  (should (equal (js-imports-relative-p "./") t))
  (should (equal (js-imports-relative-p "./index") t))
  (should (equal (js-imports-relative-p "./..") t))
  (should (equal (js-imports-relative-p "./../file") t))
  (should (equal (js-imports-relative-p "..") t))
  (should (equal (js-imports-relative-p "../") t))
  (should (equal (js-imports-relative-p "../index") t))
  (should (equal (js-imports-relative-p "../..") t))
  (should (equal (js-imports-relative-p "../../") t))
  (should (equal (js-imports-relative-p "../../file") t))
  (should (equal (js-imports-relative-p ".index") nil))
  (should (equal (js-imports-relative-p "/") nil))
  (should (equal (js-imports-relative-p ".../") nil))
  (should (equal (js-imports-relative-p "...") nil))
  (should (equal (js-imports-relative-p "..ff") nil))
  (should (equal (js-imports-relative-p "..ff/") nil))
  (should (equal (js-imports-relative-p "..ff/") nil)))

(ert-deftest js-imports-test-is-index-file?()
  "Test to check if file is index"
  (should (equal (js-imports-is-index-file-p "/home/user/repos/index.js") t))
  (should (equal (js-imports-is-index-file-p "~/repos/index.js") t))
  (should (equal (js-imports-is-index-file-p "./index.js") t))
  (should (equal (js-imports-is-index-file-p "./index") t))
  (should (equal (js-imports-is-index-file-p "../index") t))
  (should (equal (js-imports-is-index-file-p "index.js") t))
  (should (equal (js-imports-is-index-file-p "@/index.js") t))
  (should (equal (js-imports-is-index-file-p "/home/user/repos/index.ts") t))
  (should (equal (js-imports-is-index-file-p "~/repos/index.ts") t))
  (should (equal (js-imports-is-index-file-p "./index.ts") t))
  (should (equal (js-imports-is-index-file-p "index.ts") t))
  (should (equal (js-imports-is-index-file-p "@/index.ts") t))
  (should (equal (js-imports-is-index-file-p "/home/user/repos/index.d.ts") t))
  (should (equal (js-imports-is-index-file-p "~/repos/index.d.ts") t))
  (should (equal (js-imports-is-index-file-p "./index.d.ts") t))
  (should (equal (js-imports-is-index-file-p "index.d.ts") t))
  (should (equal (js-imports-is-index-file-p "@/index.d.ts") t))
  (should (equal (js-imports-is-index-file-p "/home/user/repos/index.jsx") t))
  (should (equal (js-imports-is-index-file-p "~/repos/index.jsx") t))
  (should (equal (js-imports-is-index-file-p "./index.jsx") t))
  (should (equal (js-imports-is-index-file-p "index.jsx") t))
  (should (equal (js-imports-is-index-file-p "@/index.jsx") t))
  (should (equal (js-imports-is-index-file-p "index") t))

  (should (equal (js-imports-is-index-file-p "iindex") nil))
  (should (equal (js-imports-is-index-file-p "~/repos/App.js") nil))
  (should (equal (js-imports-is-index-file-p "/pathIndex.js") nil))
  (should (equal (js-imports-is-index-file-p "pathIndex.js") nil))
  (should (equal (js-imports-is-index-file-p "-index.js") nil))
  (should (equal (js-imports-is-index-file-p "/-index.js") nil))
  (should (equal (js-imports-is-index-file-p "/index-index.js") nil))
  (should (equal (js-imports-is-index-file-p "/") nil))
  (should (equal (js-imports-is-index-file-p "/iindex.js") nil))
  (should (equal (js-imports-is-index-file-p "/index$") nil))
  (should (equal (js-imports-is-index-file-p "/iindex.js") nil)))

(ert-deftest js-imports-test-remove-ext ()
  "Test to remove extension"
  (should (equal (js-imports-remove-ext "./index.jsx") "./index"))
  (should (equal (js-imports-remove-ext "index.d.tsx") "index"))
  (should (equal (js-imports-remove-ext "index.d.ts") "index"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.ts") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "indd.ex.ts") "indd.ex"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.tsx") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.js") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "app.ts/App.js") "app.ts/App"))
  (should (equal (js-imports-remove-ext "App") "App")))

;;; js-imports-test.el ends here
