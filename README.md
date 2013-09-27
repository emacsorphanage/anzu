# anzu.el

## Introduction

`anzu.el` is an Emacs port of [anzu.vim](https://github.com/osyo-manga/vim-anzu).
`anzu.el` provides a minor mode which displays *current match* and *total matches*
information in the mode-line in various search mode.


## Screenshot

![anzu.gif](image/anzu.gif)


## Requirements

* Emacs 24 or higher


## Installation

You can install `anzu.el` from [MELPA](http://melpa.milkbox.net/) with `package.el`

```
 M-x package-install anzu
```


## Basic Usage

#### `anzu-mode`

Enable anzu minor mode:

```lisp
(anzu-mode +1)
```

#### `global-anzu-mode`

Enable global anzu mode:

```lisp
(global-anzu-mode +1)
```

## Customization

#### `anzu-mode-line`

Face of mode-line anzu information

#### `anzu-mode-line-update-function`

Function which constructs mode-line string. If you color mode-line string,
you propertize string by yourself.

```lisp
(defun my/anzu-update-func (here total)
  (propertize (format "<%d/%d>" here total)
              'face '((:foreground "yellow" :weight bold))))
(setq anzu-mode-line-update-function 'my/update-func)
```

#### `anzu-cons-mode-line-p`(Default is `t`)

Set `nil` if you want to display anzu information at any posion in mode-line.
`anzu.el` cons search information head of `mode-line` as default.

For example, show search information tail of `minor-mode-alist`

```lisp
(setq anzu-cons-mode-line-p nil)
(setcar (cdr (assq 'isearch-mode minor-mode-alist))
        '(:eval (anzu--update-mode-line)))
```

##### Screenshot

![anzu-any-position](image/anzu-any-position.png)


#### `anzu-use-migemo`(Default is `nil`)

Set to `t` if you use [migemo](https://github.com/emacs-jp/migemo).

#### `anzu-mode-lighter`

Default is ` Anzu`.

#### `anzu-regexp-search-commands`

Commands which have regexp input. If the last command is a member of this list,
`anzu.el` treats input as regular expression.

The default value is `'(isearch-forward-regexp isearch-backward-regexp)`.


## Sample Configuration

```lisp
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
```
