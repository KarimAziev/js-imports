;;; js-imports-test.el --- Tests for js-imports -*- lexical-binding: t -*-

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

;; Test suite for js-imports.el

;;; Code:

(require 'ert)
(require 'js-imports)

(defun js-imports-test--write-file (root name contents)
  "Write CONTENTS to NAME below ROOT and return its absolute path."
  (let ((file (expand-file-name name root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert contents))
    file))

(defun js-imports-test--alias (alias root)
  "Build one expected ALIAS entry pointing at ROOT."
  (list (cons alias (list (file-name-as-directory root)))))

(ert-deftest js-imports-test-relative-p ()
  (should (equal (js-imports--relative-p ".") t))
  (should (equal (js-imports--relative-p "./") t))
  (should (equal (js-imports--relative-p "./index") t))
  (should (equal (js-imports--relative-p "./..") t))
  (should (equal (js-imports--relative-p "./../file") t))
  (should (equal (js-imports--relative-p "..") t))
  (should (equal (js-imports--relative-p "../") t))
  (should (equal (js-imports--relative-p "../index") t))
  (should (equal (js-imports--relative-p "../..") t))
  (should (equal (js-imports--relative-p "../../") t))
  (should (equal (js-imports--relative-p "../../file") t))
  (should (equal (js-imports--relative-p ".index") nil))
  (should (equal (js-imports--relative-p "/") nil))
  (should (equal (js-imports--relative-p ".../") nil))
  (should (equal (js-imports--relative-p "...") nil))
  (should (equal (js-imports--relative-p "..ff") nil))
  (should (equal (js-imports--relative-p "..ff/") nil))
  (should (equal (js-imports--relative-p "..ff/") nil)))

(ert-deftest js-imports-test-is-index-file?()
  "Test to check if file is index."
  (should (equal (js-imports--index-file-p "/home/user/repos/index.js") t))
  (should (equal (js-imports--index-file-p "~/repos/index.js") t))
  (should (equal (js-imports--index-file-p "./index.js") t))
  (should (equal (js-imports--index-file-p "./index") t))
  (should (equal (js-imports--index-file-p "../index") t))
  (should (equal (js-imports--index-file-p "index.js") t))
  (should (equal (js-imports--index-file-p "@/index.js") t))
  (should (equal (js-imports--index-file-p "/home/user/repos/index.ts") t))
  (should (equal (js-imports--index-file-p "~/repos/index.ts") t))
  (should (equal (js-imports--index-file-p "./index.ts") t))
  (should (equal (js-imports--index-file-p "index.ts") t))
  (should (equal (js-imports--index-file-p "@/index.ts") t))
  (should (equal (js-imports--index-file-p "/home/user/repos/index.d.ts") t))
  (should (equal (js-imports--index-file-p "~/repos/index.d.ts") t))
  (should (equal (js-imports--index-file-p "./index.d.ts") t))
  (should (equal (js-imports--index-file-p "index.d.ts") t))
  (should (equal (js-imports--index-file-p "@/index.d.ts") t))
  (should (equal (js-imports--index-file-p "/home/user/repos/index.jsx") t))
  (should (equal (js-imports--index-file-p "~/repos/index.jsx") t))
  (should (equal (js-imports--index-file-p "./index.jsx") t))
  (should (equal (js-imports--index-file-p "index.jsx") t))
  (should (equal (js-imports--index-file-p "@/index.jsx") t))
  (should (equal (js-imports--index-file-p "index") t))
  (should (equal (js-imports--index-file-p "iindex") nil))
  (should (equal (js-imports--index-file-p "~/repos/App.js") nil))
  (should (equal (js-imports--index-file-p "/pathIndex.js") nil))
  (should (equal (js-imports--index-file-p "pathIndex.js") nil))
  (should (equal (js-imports--index-file-p "-index.js") nil))
  (should (equal (js-imports--index-file-p "/-index.js") nil))
  (should (equal (js-imports--index-file-p "/index-index.js") nil))
  (should (equal (js-imports--index-file-p "/") nil))
  (should (equal (js-imports--index-file-p "/iindex.js") nil))
  (should (equal (js-imports--index-file-p "/index$") nil))
  (should (equal (js-imports--index-file-p "/iindex.js") nil)))

(ert-deftest js-imports-test-remove-ext ()
  "Test to remove extension."
  (should (equal (js-imports-remove-ext "./index.jsx") "./index"))
  (should (equal (js-imports-remove-ext "index.d.tsx") "index"))
  (should (equal (js-imports-remove-ext "index.d.ts") "index"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.ts") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "indd.ex.ts") "indd.ex"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.tsx") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "/home/user/repos/App.js") "/home/user/repos/App"))
  (should (equal (js-imports-remove-ext "app.ts/App.js") "app.ts/App"))
  (should (equal (js-imports-remove-ext "App") "App")))

(ert-deftest js-imports-read-json-as-plist-test ()
  "Test for reading json with comments as plist."
  (should (equal
           (js-imports--read-json-file (expand-file-name
                                        "tests/tsconfig.json"
                                        default-directory)
                                       'plist
                                       t)
           '(:compilerOptions
             (:target "es2018"
              :module "commonjs"
              :outDir "./dist"
              :rootDir "./"
              :strict t
              :baseUrl "./"
              :paths
              (:@/*
               ["src/*"]
               :UI/*
               ["src/components/UI/*"])
              :typeRoots
              ["node_modules/@types"]
              :types
              ["node" "mocha"]
              :esModuleInterop t
              :inlineSourceMap t)))))

(ert-deftest js-imports-read-json-as-alist-test ()
  "Test for reading json with comments as alist."
  (should (equal
           (js-imports--read-json-file (expand-file-name
                                        "tests/tsconfig.json"
                                        default-directory)
                                       'alist
                                       t)
           '((compilerOptions
              (target . "es2018")
              (module . "commonjs")
              (outDir . "./dist")
              (rootDir . "./")
              (strict . t)
              (baseUrl . "./")
              (paths
               (@/* . ["src/*"])
               (UI/* . ["src/components/UI/*"]))
              (typeRoots . ["node_modules/@types"])
              (types . ["node" "mocha"])
              (esModuleInterop . t)
              (inlineSourceMap . t))))))

(ert-deftest js-imports-read-json-invalidates-stale-cache-test ()
  "Test that JSON cache entries are refreshed after a file changes."
  (let ((root (make-temp-file "js-imports-json-cache-" t)))
    (unwind-protect
        (let ((file (js-imports-test--write-file root "config.json"
                                                 "{\"value\": 1}")))
          (should (equal (js-imports--read-json-file file 'alist)
                         '((value . 1))))
          (js-imports-test--write-file root "config.json" "{\"value\": 2}")
          (set-file-times file (time-add (current-time) 1))
          (should (equal (js-imports--read-json-file file 'alist)
                         '((value . 2)))))
      (delete-directory root t))))

(ert-deftest js-imports-read-tsconfig-paths-without-base-url-test ()
  "Test that paths without baseUrl are relative to their config."
  (let ((root (make-temp-file "js-imports-tsconfig-" t)))
    (unwind-protect
        (progn
          (js-imports-test--write-file
           root "tsconfig.json"
           "{\"compilerOptions\":{\"paths\":{\"@/*\":[\"src/*\"]}}}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "tsconfig.json")
            (js-imports-test--alias "@/" (expand-file-name "src" root)))))
      (delete-directory root t))))

(ert-deftest js-imports-read-tsconfig-inherited-path-origins-test ()
  "Test paths and baseUrl origins across an extends chain."
  (let ((root (make-temp-file "js-imports-tsconfig-" t)))
    (unwind-protect
        (progn
          (js-imports-test--write-file
           root "configs/paths.json"
           "{\"compilerOptions\":{\"paths\":{\"@/*\":[\"src/*\"]}}}")
          (js-imports-test--write-file
           root "tsconfig.json" "{\"extends\":\"./configs/paths\"}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "tsconfig.json")
            (js-imports-test--alias
             "@/" (expand-file-name "configs/src" root))))
          (js-imports-test--write-file
           root "with-base-url.json"
           (concat "{\"extends\":\"./configs/paths\","
                   "\"compilerOptions\":{\"baseUrl\":\"./app\"}}"))
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases
             root "with-base-url.json")
            (js-imports-test--alias
             "@/" (expand-file-name "app/src" root))))
          (js-imports-test--write-file
           root "configs/base-url.json"
           "{\"compilerOptions\":{\"baseUrl\":\"../shared\"}}")
          (js-imports-test--write-file
           root "child-paths.json"
           (concat "{\"extends\":\"./configs/base-url\","
                   "\"compilerOptions\":{\"paths\":"
                   "{\"lib/*\":[\"feature/*\"]}}}"))
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "child-paths.json")
            (js-imports-test--alias
             "lib/" (expand-file-name "shared/feature" root)))))
      (delete-directory root t))))

(ert-deftest js-imports-read-tsconfig-multiple-extends-and-overrides-test ()
  "Test multiple extends precedence and an explicitly empty paths map."
  (let ((root (make-temp-file "js-imports-tsconfig-" t)))
    (unwind-protect
        (progn
          (js-imports-test--write-file
           root "base-a.json"
           (concat "{\"compilerOptions\":{\"baseUrl\":\"./source\","
                   "\"paths\":{\"a/*\":[\"a/*\"]}}}"))
          (js-imports-test--write-file
           root "base-b.json"
           "{\"compilerOptions\":{\"paths\":{\"b/*\":[\"b/*\"]}}}")
          (js-imports-test--write-file
           root "tsconfig.json"
           "{\"extends\":[\"./base-a\",\"./base-b\"]}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "tsconfig.json")
            (js-imports-test--alias
             "b/" (expand-file-name "source/b" root))))
          (js-imports-test--write-file
           root "empty.json"
           (concat "{\"extends\":\"./tsconfig\","
                   "\"compilerOptions\":{\"paths\":{}}}"))
          (should-not
           (js-imports--read-tsconfig-paths-aliases root "empty.json")))
      (delete-directory root t))))

(ert-deftest js-imports-read-tsconfig-package-extends-config-dir-test ()
  "Test package extends resolution and `${configDir}' substitution."
  (let ((root (make-temp-file "js-imports-tsconfig-" t)))
    (unwind-protect
        (progn
          (js-imports-test--write-file
           root "node_modules/@example/config/package.json"
           "{\"exports\":\"./base.json\"}")
          (js-imports-test--write-file
           root "node_modules/@example/config/base.json"
           (concat "{\"compilerOptions\":{\"paths\":"
                   "{\"@/*\":[\"${configDir}/src/*\"]}}}"))
          (js-imports-test--write-file
           root "tsconfig.json" "{\"extends\":\"@example/config\"}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "tsconfig.json")
            (js-imports-test--alias "@/" (expand-file-name "src" root)))))
      (delete-directory root t))))

(ert-deftest js-imports-read-tsconfig-cycle-and-references-test ()
  "Test that extends cycles terminate and references contribute aliases."
  (let ((root (make-temp-file "js-imports-tsconfig-" t)))
    (unwind-protect
        (progn
          (js-imports-test--write-file
           root "a.json"
           (concat "{\"extends\":\"./b\",\"compilerOptions\":{\"paths\":"
                   "{\"a/*\":[\"src/*\"]}}}"))
          (js-imports-test--write-file root "b.json" "{\"extends\":\"./a\"}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "a.json")
            (js-imports-test--alias "a/" (expand-file-name "src" root))))
          (js-imports-test--write-file
           root "missing-parent.json"
           (concat "{\"extends\":\"./does-not-exist.json\","
                   "\"compilerOptions\":{\"paths\":"
                   "{\"local/*\":[\"local/*\"]}}}"))
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases
             root "missing-parent.json")
            (js-imports-test--alias
             "local/" (expand-file-name "local" root))))
          (js-imports-test--write-file
           root "referenced/tsconfig.json"
           "{\"compilerOptions\":{\"paths\":{\"ref/*\":[\"src/*\"]}}}")
          (js-imports-test--write-file
           root "references.json"
           "{\"references\":[{\"path\":\"./referenced\"}]}")
          (should
           (equal
            (js-imports--read-tsconfig-paths-aliases root "references.json")
            (js-imports-test--alias
             "ref/" (expand-file-name "referenced/src" root)))))
      (delete-directory root t))))

;;; js-imports-test.el ends here
