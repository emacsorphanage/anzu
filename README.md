# anzu.el

## Introduction

`anzu.el` is Emacs port of [anzu.vim](https://github.com/osyo-manga/vim-anzu).
`anzu.el` provides minor mode which display *current point* and *total matched*
in various search mode.


## Screenshot

![anzu.gif](image/anzu.gif)


## Requirements

* Emacs 24 or higher


## Basic Usage

#### `anzu-mode`

Enable anzu minor mode

#### `global-anzu-mode`

Enable global anzu mode

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

#### `anzu-use-migemo`(Default is `nil`)

Set non-nil` if you use [migemo](https://github.com/emacs-jp/migemo).


## Sample Configuration

```lisp
(require 'anzu)
(global-anzu-mode t)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
```
