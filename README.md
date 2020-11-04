[![License GPL3](https://img.shields.io/badge/license-GPLv3-blue)](http://www.gnu.org/licenses/gpl-3.0.txt)


## js-import

An Emacs package for importing JavaScript and TypeScript modules with [helm](<https://github.com/emacs-helm/helm> "helm") or [ivy](https://github.com/abo-abo/swiper "ivy") completion systems.

<a id="org7a05a1c"></a>

![](js-import-demo.gif)


## Table of Contents
* [Requirements](#org8bb2ccf)
* [Installation](#org66d242b)
* [Commands](#orgd8bdc4e)
* [Alias setup](#orgc8d9f05)

<a id="org8bb2ccf"></a>

### Requirements

-   Emacs 26.1 or higher
-   cl-lib

<a id="org66d242b"></a>

### Installation

#### straight-use-package

Example configuration using package managers [straight](<https://github.com/raxod502/straight.el> "straight") and [use-package](https://github.com/jwiegley/use-package "use-package").

```cl
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
```
#### quelpa

Or if you want to always get the latest version:

```cl
    (quelpa '(js-import
        :repo "KarimAziev/js-import"
        :fetcher git
        :url "git@github.com:KarimAziev/js-import.git")
    :upgrade t)
```


<a id="orgd8bdc4e"></a>

### Commands

-   **`js-import`:** Main command. Read a filename to parse symbols and finally yanks selected ones in buffer. 
    - Keymap 
       * `C->`  switches to next alias or relative path
       * `C-<`  switches to previous previous webpack alias
       * `C-c o` find file
-   **`js-import-find-symbol-at-point`:** Deep jump to a definition of symbol at point through renaming, re-exports. 
-   **`js-import-symbols-menu`:** Jump or refactor to exported, imported and definitions in current buffer.
-   **`js-import-reset-all-sources`:** Reset file and symbol sources. Also remove cache.


<a id="orgc8d9f05"></a>

### Alias setup

To setup aliases you need to customize the variable `js-import-project-aliases` which is a list of paired strings (plist). First element of each pair associated with alias and the second with it's path. Path should be relative to project root.

```cl
'("aliasA" "src/pathA" "aliasB" "pathB/src/moduleB")
```

Suppose your project configured with webpack or other tools as following. 

```javascript
module.exports = {
  resolve: {
    alias: {
      @: path.resolve(__dirname, 'src'),
      UI: path.resolve(__dirname, 'src/components/UI')
    }
  }
};
```

Put to the root directory of your project `.dir-locals` following:

```cl
((nil . ((js-import-project-aliases "@" "src" "UI" "src/components/UI"))))

```

### License

Copyright © 2020 Karim Aziiev.

Distributed under the GNU General Public License, version 3
