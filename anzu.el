;;; anzu.el --- Show number of matches in mode-line while searching

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu
;; Version: 0.08

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

;; `anzu.el' is an Emacs port of `anzu.vim'.
;;
;; `anzu.el' provides a minor mode which displays 'current match/total
;; matches' in the mode-line in various search modes.  This makes it
;; easy to understand how many matches there are in the current buffer
;; for your search query.

;; To use this package, add following code to your init.el or .emacs
;;   (require 'anzu)
;;   (global-anzu-mode +1)
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar migemo-isearch-enable-p))

(defgroup anzu nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu-mode-lighter " Anzu"
  "Lighter of anzo-mode"
  :type 'string
  :group 'anzu)

(defcustom anzu-cons-mode-line-p t
  "Set nil if you use your own mode-line setting"
  :type 'boolean
  :group 'anzu)

(defcustom anzu-minimum-input-length 1
  "Minimum input length to enable anzu"
  :type 'integer
  :group 'anzu)

(defcustom anzu-search-threshold nil
  "Limit of search number"
  :type '(choice (integer :tag "Threshold of search")
                 (boolean :tag "No threshold" nil))
  :group 'anzu)

(defcustom anzu-use-migemo nil
  "Flag of using migemo"
  :type 'boolean
  :group 'anzu)

(defcustom anzu-mode-line-update-function nil
  "Function which return mode-line string"
  :type 'function
  :group 'anzu)

(defcustom anzu-regexp-search-commands '(isearch-forward-regexp
                                         isearch-backward-regexp)
  "Search function which use regexp."
  :type '(repeat function)
  :group 'anzu)

(defface anzu-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline"
  :group 'anzu)

(defvar anzu--total-matched 0)
(defvar anzu--current-posion 0)
(defvar anzu--overflow-p nil)
(defvar anzu--last-isearch-string nil)
(defvar anzu--cached-positions nil)
(defvar anzu--last-command nil)

(defun anzu--validate-regexp (regexp)
  (condition-case err
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defsubst anzu--construct-position-info (count overflow positions)
  (list :count count :overflow overflow :positions positions))

(defun anzu--search-all-position (str)
  (unless anzu--last-command
    (setq anzu--last-command last-command))
  (when (and (not (memq anzu--last-command anzu-regexp-search-commands))
             (not isearch-regexp))
    (setq str (regexp-quote str)))
  (if (not (anzu--validate-regexp str))
      anzu--cached-positions
    (save-excursion
      (goto-char (point-min))
      (let ((positions '())
            (count 0)
            (overflow nil)
            (finish nil)
            (search-func (if (and anzu-use-migemo migemo-isearch-enable-p)
                             'migemo-forward
                           're-search-forward)))
        (while (and (not finish) (funcall search-func str nil t))
          (push (cons (match-beginning 0) (match-end 0)) positions)
          (incf count)
          (when (= (match-beginning 0) (match-end 0)) ;; Case of anchor such as "^"
            (if (eobp)
                (setq finish t)
              (forward-char 1)))
          (when (and anzu-search-threshold (>= count anzu-search-threshold))
            (setq overflow t finish t)))
        (let ((result (anzu--construct-position-info count overflow (reverse positions))))
          (setq anzu--cached-positions (copy-sequence result))
          result)))))

(defun anzu--where-is-here (positions here)
  (loop for (start . end) in positions
        for i = 1 then (1+ i)
        when (and (>= here start) (<= here end))
        return i
        finally return 0))

(defun anzu--update ()
  (when (>= (length isearch-string) anzu-minimum-input-length)
    (let ((result (if (string= isearch-string anzu--last-isearch-string)
                      anzu--cached-positions
                    (anzu--search-all-position isearch-string))))
      (let ((curpos (anzu--where-is-here (plist-get result :positions) (point))))
        (setq anzu--total-matched (plist-get result :count)
              anzu--overflow-p (plist-get result :overflow)
              anzu--current-posion curpos
              anzu--last-isearch-string isearch-string)
        (force-mode-line-update)))))

(defsubst anzu--mode-line-not-set-p ()
  (and (listp mode-line-format)
       (equal (car mode-line-format) '(:eval (anzu--update-mode-line)))))

(defun anzu--cons-mode-line ()
  (when (and anzu-cons-mode-line-p (not (anzu--mode-line-not-set-p)))
    (setq mode-line-format (cons '(:eval (anzu--update-mode-line))
                                 mode-line-format))))

(defsubst anzu--reset-status ()
  (setq anzu--total-matched 0
        anzu--current-posion 0
        anzu--last-command nil
        anzu--overflow-p nil))

(defun anzu--reset-mode-line ()
  (anzu--reset-status)
  (when (and anzu-cons-mode-line-p (anzu--mode-line-not-set-p))
    (setq mode-line-format (cdr mode-line-format))))

(defsubst anzu--format-here-position (here total)
  (if (and anzu--overflow-p (zerop here))
      (format "%d+" total)
    here))

(defun anzu--update-mode-line-default (here total)
  (propertize (format "(%s/%d%s)"
                      (anzu--format-here-position here total)
                      total (if anzu--overflow-p "+" ""))
              'face 'anzu-mode-line))

(defun anzu--update-mode-line ()
  (let ((update-func (or anzu-mode-line-update-function
                         'anzu--update-mode-line-default)))
    (funcall update-func anzu--current-posion anzu--total-matched)))

;;;###autoload
(define-minor-mode anzu-mode
  "minor-mode which display search information in mode-line."
  :group      'anzu
  :init-value nil
  :global     nil
  :lighter    anzu-mode-lighter
  (if anzu-mode
      (progn
        (add-hook 'isearch-update-post-hook 'anzu--update nil t)
        (add-hook 'isearch-mode-hook 'anzu--cons-mode-line nil t)
        (add-hook 'isearch-mode-end-hook 'anzu--reset-mode-line nil t))
    (remove-hook 'isearch-update-post-hook 'anzu--update t)
    (remove-hook 'isearch-mode-hook 'anzu--cons-mode-line t)
    (remove-hook 'isearch-mode-end-hook 'anzu--reset-mode-line t)
    (anzu--reset-mode-line)))

;;;###autoload
(define-global-minor-mode global-anzu-mode
  anzu-mode
  (lambda ()
    (unless (minibufferp)
      (anzu-mode t)))
  :group 'anzu)

(provide 'anzu)
;;; anzu.el ends here
