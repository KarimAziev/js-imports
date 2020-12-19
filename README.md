# Table of Contents

-   [js-import](#orgcce35c3)
    -   [Installation](#org2e77e2b)
        -   [straight-use-package](#org3c9a64d)
        -   [quelpa](#orgb10fdb6)
    -   [Commands](#org6c1fa1c)
    -   [Alias setup](#orgc814be5)
        -   [tsconfig](#orgfb21631)
        -   [.dir-locals](#org118ebf8)
        -   [License](#orgbea63cf)

[\![License GPL3](<https://img.shields.io/badge/license-GPLv3-blue>)](<http://www.gnu.org/licenses/gpl-3.0.txt>)


<a id="orgcce35c3"></a>

# js-import

  An Emacs package for importing JavaScript and TypeScript modules with
[helm](<https://github.com/emacs-helm/helm> "helm") and [ivy](https://github.com/abo-abo/swiper) (swiper) interface.

\![](js-import-demo.gif).

A package proposes to select a file or dependency of the current project, extract its exports symbols and inserts an import statement with selected candidates into a buffer.


<a id="org2e77e2b"></a>

## Installation


<a id="org3c9a64d"></a>

### straight-use-package

Example configuration using package managers [straight](<https://github.com/raxod502/straight.el> "straight") and [use-package](<https://github.com/jwiegley/use-package> "use-package").

    (use-package js-import
      :straight (:type git
                       :host github
                       :branch "origin/dev-latest"
                       :repo "KarimAziev/js-import"
                       :package "js-import"
                       :local-repo "js-import")
      :commands (js-import-mode)
      :hook ((js-mode . js-import-mode)
             (js2-mode . js-import-mode)
             (typescript-mode . js-import-mode))
      :bind (:map js-import-mode-map
                  ("C-c C-i" . js-import)
                  ("C-c C-j" . js-import-find-symbol-at-point)
                  ("C-c C-." . js-import-symbols-menu)))


<a id="orgb10fdb6"></a>

### quelpa

    (quelpa '(js-import
            :repo "KarimAziev/js-import"
            :fetcher git
            :url "git@github.com:KarimAziev/js-import.git"))


<a id="org6c1fa1c"></a>

## Commands

-   `js-import`
-   `js-import-from-path`
-   `js-import-jump-to-definition`
-   `js-import-find-file`
-   `js-import-find-file-other-window`
-   `js-import-find-file-at-point`
-   `js-import-symbols-menu`
-   `js-import-change-completion`
-   `js-import-convert-import-path-at-point`
-   `js-import-replace-relative-imports-to-aliases`
-   `js-import-reset-cache`
-   `js-import-reset-all-sources`


<a id="orgc814be5"></a>

## Alias setup

There are two ways to setup file aliases:


<a id="orgfb21631"></a>

### tsconfig

If project root contain the [TSConfig file](https://www.typescriptlang.org/tsconfig#paths) which be either a tsconfig.json or jsconfig.json. Path and baseUrl will be readed automatically.

    {
      "compilerOptions": {
        "baseUrl": ".",
        "paths": {
          "@/*": ["src/*"],
          "UI/*": ["src/components/UI/*"]
        }
      }
    }


<a id="org118ebf8"></a>

### .dir-locals

    ((nil . (
        (eval . (setq js-import-alias-map '("@" "src" "UI" "src/components/UI")))
        )))

Or configure it with \`M-x customize-variable js-import-alias-map\`.


<a id="orgbea63cf"></a>

### License

Copyright © 2020 Karim Aziiev.

Distributed under the GNU General Public License, version 3
