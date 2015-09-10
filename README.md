# anzu.el [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

## Introduction

`anzu.el` is an Emacs port of [anzu.vim](https://github.com/osyo-manga/vim-anzu).
`anzu.el` provides a minor mode which displays *current match* and *total matches*
information in the mode-line in various search modes.


## Screenshot

![anzu.gif](image/anzu.gif)


## Requirements

* Emacs 24 or higher
* `cl-lib` 0.5 or higher (you don't need to install `cl-lib` if you use Emacs 24.3 or higher)


## Installation

You can install `anzu.el` from [MELPA](http://melpa.org/) with `package.el`

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

#### `anzu-query-replace`

Same as `query-replace` except anzu information in mode-line

#### `anzu-query-replace-regexp`

Same as `query-replace-regexp` except anzu information in mode-line


Add following S-exp in your configuration if you want to use anzu's replace commands by default.

```lisp
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
```

[anzu-replace-demo](image/anzu-replace-demo.gif)


#### `anzu-query-replace-at-cursor`

Same as `anzu-query-replace` except *from-string* is symbol at cursor

#### `anzu-query-replace-at-cursor-thing`

Same as `anzu-query-replace-at-cursor` except replaced region is
specified by `anzu-replace-at-cursor-thing`.

#### `anzu-replace-at-cursor-thing`

Same as `anzu-query-replace-at-cursor-thing` except not query.
This command is useful in refactoring such as changing variable name
in the function.

![anzu-replace-demo](image/anzu-replace-demo-noquery.gif)


## Customization

#### `anzu-mode-line`

Face of mode-line anzu information

#### `anzu-replace-highlight`

Face of from-string of replacement

#### `anzu-replace-to`

Face of to-string of replacement

#### `anzu-mode-line-update-function`

Function which constructs mode-line string. If you color mode-line string,
you propertize string by yourself. The function takes 2 integer arguments, current position,
and total matched. This function is called at searching, inputting replaced word,
replacing. Global variable `anzu--state` indicates those states(`'search`, `'replace-query`, `replace`).

```lisp
(defun my/anzu-update-func (here total)
  (when anzu--state
    (let ((status (cl-case anzu--state
                    (search (format "<%d/%d>" here total))
                    (replace-query (format "(%d Replaces)" total))
                    (replace (format "<%d/%d>" here total)))))
      (propertize status 'face 'anzu-mode-line))))
(setq anzu-mode-line-update-function #'my/anzu-update-func)
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


#### `anzu-input-idle-delay`(Default is `0.05`)

Delay second of updating mode-line information when you input from-string

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

#### `anzu-deactivate-region`(Default is `nil`)

Deactivate region at anzu replace command if this value is non-nil.
It is hard to see with anzu replace command when region is active.


#### `anzu-replace-at-cursor-thing`(Default is 'defun)

Thing at point of `anzu-query-replace-at-cursor-thing`.
This parameter is same as `thing-at-point`.

#### `anzu-replace-to-string-separator`(Default is "")

Separator of `to` string.


## Sample Configuration

```lisp
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))
```

[melpa-link]: https://melpa.org/#/anzu
[melpa-stable-link]: https://stable.melpa.org/#/anzu
[melpa-badge]: https://melpa.org/packages/anzu-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/anzu-badge.svg
