
# Table of Contents

1.  [js-import](#org7a05a1c)
    1.  [Requirements](#org8bb2ccf)
    2.  [Installation](#org66d242b)
    3.  [Usage](#orgd8bdc4e)
    4.  [Alias setup](#orgc8d9f05)


<a id="org7a05a1c"></a>

# js-import

An Emacs package for import JavaScript and TypeScript modules with [helm](<https://github.com/emacs-helm/helm> "helm") interface.


![](js-import-demo.gif).


<a id="org8bb2ccf"></a>

## Requirements

-   Emacs 24.4 or higher
-   Helm 3.0 or higher
-   [Projectile](<https://github.com/bbatsov/projectile> "projectile")
-   [Quelpa](<https://github.com/quelpa/quelpa> "quelpa") - while package is under develop and is not available on Melpa


<a id="org66d242b"></a>

## Installation

Until \`js-import\` is not published on Melpa you can install the package from [quelpa](<https://github.com/quelpa/quelpa> "quelpa").

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
        :fetcher git
        :url "git@github.com:KarimAziev/js-import.git")
    :upgrade t)
```


<a id="orgd8bdc4e"></a>

## Usage

1.  Select a module with one of these commands
    - `M-x js-import`
      Includes alias, relative and node_modules
        Helm actions:
        `C->`  switch to next webpack alias
        `C-<`  switch to previous previous webpack alias
        `C-r`  switch to a relative paths

    - `M-x js-import-alias`
      Helm actions:
      `C-r`  switch to a relative paths
      Includes alias and relative sources.

    - `M-x js-import-dependency`
        Includes only dependencies.

2.  Select symbols
        `F1` insert a symbol to an existing or new import statement
        `F2` to make a named import (\`import as\`)
        `F3` visit a module
3. Edit current imports, jumping and deleting
    `M-x js-import-edit-buffer-imports`
        `F1` visit a module
        `F2` select more imports from candidate path;
        `F3` delete an imported symbol
        `F4` delete whole import statement


<a id="orgc8d9f05"></a>

## Alias setup

To resolve webpack-alias imports customize variable \`M-x js-import-alias-map\` which is a list of strings with ("aliasA" "pathA" "aliasB" "pathB"). Default is value ("" "src").
For example your webpack config includes two aliases \`@\` and \`UI\`:

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

In this case `js-import-alias-map` should be ("@" "src" "UI" "src/components/UI"). Put to the root directory of your project `.dir-locals` following:

```cl
((nil . (
(eval . (setq js-import-alias-map '("@" "src" "UI" "src/components/UI")))
)))
\\\
```
Or configure it globally with `M-x customize-variable js-import-alias-map`.
