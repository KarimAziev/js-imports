;;; -*- lexical-binding: t -*-
;;; js-import-test.el --- This is an Emacs Lisp file with Emacs Lisp code.

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
(require 'js-import)

(ert-deftest check-is-path-index()
  "Test to check if file is index"
  (should (equal (js-import-is-index-file? "/home/user/repos/index.js") t))
  (should (equal (js-import-is-index-file? "~/repos/index.js") t))
  (should (equal (js-import-is-index-file? "./index.js") t))
  (should (equal (js-import-is-index-file? "./index") t))
  (should (equal (js-import-is-index-file? "../index") t))
  (should (equal (js-import-is-index-file? "index.js") t))
  (should (equal (js-import-is-index-file? "@/index.js") t))

  (should (equal (js-import-is-index-file? "/home/user/repos/index.ts") t))
  (should (equal (js-import-is-index-file? "~/repos/index.ts") t))
  (should (equal (js-import-is-index-file? "./index.ts") t))
  (should (equal (js-import-is-index-file? "index.ts") t))
  (should (equal (js-import-is-index-file? "@/index.ts") t))

  (should (equal (js-import-is-index-file? "/home/user/repos/index.d.ts") t))
  (should (equal (js-import-is-index-file? "~/repos/index.d.ts") t))
  (should (equal (js-import-is-index-file? "./index.d.ts") t))
  (should (equal (js-import-is-index-file? "index.d.ts") t))
  (should (equal (js-import-is-index-file? "@/index.d.ts") t))

  (should (equal (js-import-is-index-file? "/home/user/repos/index.jsx") t))
  (should (equal (js-import-is-index-file? "~/repos/index.jsx") t))
  (should (equal (js-import-is-index-file? "./index.jsx") t))
  (should (equal (js-import-is-index-file? "index.jsx") t))
  (should (equal (js-import-is-index-file? "@/index.jsx") t))



  (should (equal (js-import-is-index-file? "~/repos/App.js") nil))
  (should (equal (js-import-is-index-file? "/pathIndex.js") nil))
  (should (equal (js-import-is-index-file? "pathIndex.js") nil))
  (should (equal (js-import-is-index-file? "-index.js") nil))
  (should (equal (js-import-is-index-file? "/-index.js") nil))
  (should (equal (js-import-is-index-file? "/index-index.js") nil))
  (should (equal (js-import-is-index-file? "/") nil))
  (should (equal (js-import-is-index-file? "/iindex.js") nil))
  (should (equal (js-import-is-index-file? "/iindex.js") nil)))


(ert-deftest remove-path-ext()
  "Test to check if file is index"
  (should (equal (js-import-remove-ext "./index.jsx") "./index"))
  (should (equal (js-import-remove-ext "index.d.tsx") "index"))
  (should (equal (js-import-remove-ext "index.d.ts") "index"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.ts") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "indd.ex.ts") "indd.ex"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.tsx") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.js") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "app.ts/App.js") "app.ts/App"))
  (should (equal (js-import-remove-ext "App") "App")))


(ert-deftest remove-path-ext()
  "Test to check if file enabled by ext"
  (should (equal (js-import-remove-ext "./index.jsx") "./index"))
  (should (equal (js-import-remove-ext "index.d.tsx") "index"))
  (should (equal (js-import-remove-ext "index.d.ts") "index"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.ts") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "indd.ex.ts") "indd.ex"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.tsx") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "/home/user/repos/App.js") "/home/user/repos/App"))
  (should (equal (js-import-remove-ext "app.ts/App.js") "app.ts/App"))
  (should (equal (js-import-remove-ext "App") "App")))

(ert-deftest is-js-ts-file()
  "Test to check if file enabled by ext"
  (should (equal (js-import-js-file? "./index.jsx") t))
  (should (equal (js-import-js-file? "index.d.tsx") t))
  (should (equal (js-import-js-file? "index.d.ts") t))
  (should (equal (js-import-js-file? "/home/user/repos/App.ts") t))
  (should (equal (js-import-js-file? "indd.ex.ts") t))
  (should (equal (js-import-js-file? "/home/user/repos/App.tsx") t))
  (should (equal (js-import-js-file? "/home/user/repos/App.js") t))
  (should (equal (js-import-js-file? "app.ts/App.js") t))

  (should (equal (js-import-js-file? "App") nil))
  (should (equal (js-import-js-file? "App.jx") nil))
  (should (equal (js-import-js-file? "indd.ex") nil))
  (should (equal (js-import-js-file? "indd.jsjs") nil)))

;;; js-import-test.el ends here
