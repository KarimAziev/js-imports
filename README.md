[![License GPL3](https://img.shields.io/badge/license-GPLv3-blue)](http://www.gnu.org/licenses/gpl-3.0.txt)


## js-import

An Emacs package for import JavaScript and TypeScript modules with [helm](<https://github.com/emacs-helm/helm> "helm") or [ivy](https://github.com/abo-abo/swiper "ivy") completion systems.

<a id="org7a05a1c"></a>

![](js-import-demo.gif)


## Table of Contents
* [Requirements](#org8bb2ccf)
* [Installation](#org66d242b)
* [Usage](#orgd8bdc4e)
* [Alias setup](#orgc8d9f05)

<a id="org8bb2ccf"></a>

### Requirements

-   Emacs 26.1 or higher
-   cl-lib

<a id="org66d242b"></a>

### Installation

You can install the package from [quelpa](<https://github.com/quelpa/quelpa> "quelpa").

```cl
    (quelpa '(js-import
        :repo "KarimAziev/js-import"
        :fetcher git
        :url "git@github.com:KarimAziev/js-import.git"))
```

Or if you want to always get the latest version:

```cl
    (quelpa '(js-import
        :repo "KarimAziev/js-import"
x        :fetcher git
        :url "git@github.com:KarimAziev/js-import.git")
    :upgrade t)
```


<a id="orgd8bdc4e"></a>

### Usage

#### `M-x js-import`

Main command which ask about a filename to extract exported symbols and automatically inserts selected an existing or a new import statement.

**Helm actions for files**

- `C-r`  switches to a relative paths
- `C->`  switches to next webpack alias
- `C-<`  switches to previous previous webpack alias
- `C-c o` find file


**Helm actions for symbols**

- makes a named import (`import { exportName as newName }`)
- jump to export definition.

#### `js-import-symbols-menu`
A command for jumping and editing symbols in current buffer.

#### `js-import-find-symbol-at-point`

Jumps to a definition of symbol at point through renaming, re-exports.

<a id="orgc8d9f05"></a>

### Alias setup

To use webpack-aliases customize variable `js-import-alias-map`. It is a list of strings with `("aliasA" "pathA" "aliasB" "pathB")`. Default is value `("" "src")`.

For example your webpack config includes two aliases *@* and *UI*:


```javascript
module.exports = {
  //...
  resolve: {
    alias: {
      @: path.resolve(__dirname, 'src'),
      UI: path.resolve(__dirname, 'src/components/UI')
    }
  }
};
```

In this case `js-import-alias-map` should be `("@" "src" "UI" "src/components/UI")`. Put to the root directory of your project `.dir-locals` following:

```cl
((nil . (
    (eval . (setq js-import-alias-map '("@" "src" "UI" "src/components/UI")))
)))

```
Or configure it globally with `M-x customize-variable js-import-alias-map`.

### License

Copyright © 2020 Karim Aziiev.

Distributed under the GNU General Public License, version 3
