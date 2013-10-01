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


#### `anzu-mode-lighter`

Mode name in `mode-line`. Default is ` Anzu`.

#### `anzu-regexp-search-commands`

Commands which have regexp input. If the last command is a member of this list,
`anzu.el` treats input as regular expression.

The default value is `'(isearch-forward-regexp isearch-backward-regexp)`.

#### `anzu-use-migemo`(Default is `nil`)

Set to `t` if you use [migemo](https://github.com/emacs-jp/migemo).

#### `anzu-search-threshold`(Default is `nil`)

Threshold of searched words. If there are searched word more than this value,
`anzu.el` stops to search and display total number like `1000+`(as default).
If this value is `nil`, `anzu.el` counts all words.

![anzu-threshold](image/anzu-threshold.png)


#### `anzu-minimum-input-length`(Default is 1)

Minimum input length to enable anzu. This parameter is useful for `migemo` users.
Searching 1 or 2 characters with `migemo` is too heavy if buffer is so large.
Please set 3 or higher if you frequently edit such file.


## Sample Configuration

```lisp
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(setq anzu-search-threshold 1000)
```
