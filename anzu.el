;;; anzu.el --- Emacs Port of anzu.vim

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu
;; Version: 0.01

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

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup anzu nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu-mode-lighter " Anzu"
  "Lighter of anzo-mode"
  :type 'string
  :group 'anzu)

(defcustom anzu-mode-line-format "(%s/%s)"
  "Format string of mode-line. This value should have two `%d'
First `%d' is current position, second `%d' is total number of matched"
  :type 'string
  :group 'anzu)

(defcustom anzu-mode-line-update-function nil
  "Function which return mode-line string"
  :type 'function
  :group 'anzu)

(defface anzu-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline"
  :group 'anzu)

(defun anzu--search-all-position (str)
  (save-excursion
    (goto-char (point-min))
    (let ((positions '())
          (count 0))
      (while (re-search-forward str nil t)
        (push (cons (match-beginning 0) (match-end 0)) positions)
        (incf count))
      (let ((result (cons count (reverse positions))))
        (setq anzu--cached-positions (copy-sequence result))
        result))))

(defun anzu--where-is-here (positions here)
  (loop for (start . end) in positions
        for i = 1 then (1+ i)
        when (and (>= here start) (<= here end))
        return i
        finally return 0))

(defvar anzu--total-matched 0)
(defvar anzu--current-posion 0)
(defvar anzu--last-isearch-string nil)
(defvar anzu--cached-positions nil)

(defun anzu--update ()
  (unless (string= isearch-string "")
    (let ((result (if (string= isearch-string anzu--last-isearch-string)
                      anzu--cached-positions
                    (anzu--search-all-position isearch-string))))
      (let ((total (car result))
            (positions (cdr result)))
       (setq anzu--total-matched total
             anzu--current-posion (anzu--where-is-here positions (point))
             anzu--last-isearch-string isearch-string)
       (force-mode-line-update)))))

(defsubst anzu--mode-line-not-set-p ()
  (and (listp mode-line-format)
       (equal (car mode-line-format) '(:eval (anzu--update-mode-line)))))

(defun anzu--cons-mode-line ()
  (unless (anzu--mode-line-not-set-p)
    (setq mode-line-format (cons '(:eval (anzu--update-mode-line))
                                 mode-line-format))))

(defun anzu--reset-mode-line ()
  (when (anzu--mode-line-not-set-p)
    (setq mode-line-format (cdr mode-line-format))))

(defun anzu--update-mode-line-default (here total)
  (propertize (format "(%d/%d)" here total) 'face 'anzu-mode-line))

(defun anzu--update-mode-line ()
  (let ((update-func (or anzu-mode-line-update-function
                         'anzu--update-mode-line-default)))
    (funcall update-func  anzu--current-posion anzu--total-matched)))

;;;###autoload
(define-minor-mode anzu-mode
  "anzu"
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
    (remove-hook 'isearch-mode-hook 'anzu--cons-mode-line nil t)
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
