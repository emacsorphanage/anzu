;;; anzu.el --- Show number of matches in mode-line while searching

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu
;; Version: 0.22

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

(require 'thingatpt)

(defgroup anzu nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu-mode-lighter " Anzu"
  "Lighter of anzu-mode"
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

(defcustom anzu-input-idle-delay 0.05
  "Idle second for updating modeline at replace commands"
  :type 'number
  :group 'anzu)

(defcustom anzu-deactivate-region nil
  "Deactive region if you use anzu a replace command with region"
  :type 'boolean
  :group 'anzu)

(defcustom anzu-replace-at-cursor-thing 'defun
  "Replace thing. This parameter is same as `thing-at-point'"
  :type 'symbol
  :group 'anzu)

(defface anzu-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline"
  :group 'anzu)

(defface anzu-replace-highlight
  '((t :inherit query-replace))
  "highlight of replaced string"
  :group 'anzu)

(defface anzu-replace-to
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "yellow"))
  "highlight of replace string"
  :group 'anzu)

(defvar anzu--total-matched 0)
(defvar anzu--current-posion 0)
(defvar anzu--overflow-p nil)
(defvar anzu--last-isearch-string nil)
(defvar anzu--cached-positions nil)
(defvar anzu--last-command nil)
(defvar anzu--state nil)
(defvar anzu--cached-count 0)
(defvar anzu--last-replace-input "")

(defun anzu--validate-regexp (regexp)
  (condition-case err
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defsubst anzu--construct-position-info (count overflow positions)
  (list :count count :overflow overflow :positions positions))

(defsubst anzu--case-fold-search (input)
  (let ((case-fold-search nil))
    (not (string-match-p "[A-Z]" input))))

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
                           're-search-forward))
            (case-fold-search (anzu--case-fold-search str)))
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

(defun anzu--cons-mode-line-search ()
  (anzu--cons-mode-line 'search))

(defun anzu--cons-mode-line (state)
  (setq anzu--state state)
  (when (and anzu-cons-mode-line-p (not (anzu--mode-line-not-set-p)))
    (setq mode-line-format (cons '(:eval (anzu--update-mode-line))
                                 mode-line-format))))

(defsubst anzu--reset-status ()
  (setq anzu--total-matched 0
        anzu--current-posion 0
        anzu--last-command nil
        anzu--last-isearch-string nil
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
  (case anzu--state
    (search (propertize (format "(%s/%d%s)"
                                (anzu--format-here-position here total)
                                total (if anzu--overflow-p "+" ""))
                        'face 'anzu-mode-line))
    (replace (propertize (format "(%d replace)" total)
                         'face 'anzu-mode-line))
    (otherwise "")))

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
        (add-hook 'isearch-mode-hook 'anzu--cons-mode-line-search nil t)
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

(defsubst anzu--query-prompt-base (use-region use-regexp)
  (concat "Query replace"
          (if current-prefix-arg " word" "")
          (if use-regexp " regexp" "")
          (if use-region " in region" ""))  )

(defun anzu--query-prompt (use-region use-regexp)
  (let ((prompt (anzu--query-prompt-base use-region use-regexp)))
    (if query-replace-defaults
        (format "%s (default %s -> %s) " prompt
                (query-replace-descr (car query-replace-defaults))
                (query-replace-descr (cdr query-replace-defaults)))
      prompt)))

(defun anzu--add-overlay (regexp beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'from-regexp regexp)
    (overlay-put ov 'from-string (buffer-substring-no-properties beg end))
    (overlay-put ov 'face 'anzu-replace-highlight)
    (overlay-put ov 'anzu-replace t)))

(defun anzu--count-matched (buf str replace-beg replace-end use-regexp overlay-limit)
  (when (not use-regexp)
    (setq str (regexp-quote str)))
  (if (not (anzu--validate-regexp str))
      anzu--cached-count
    (with-current-buffer buf
      (save-excursion
        (let* ((overlay-beg replace-beg)
               (overlay-end (min replace-end overlay-limit)))
          (goto-char overlay-beg)
          (let ((count 0)
                (finish nil)
                (case-fold-search (anzu--case-fold-search str)))
            (while (and (not finish) (re-search-forward str replace-end t))
              (incf count)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (= beg end)
                  (if (eobp)
                      (setq finish t)
                    (forward-char 1)))
                (when (and (>= beg overlay-beg) (<= end overlay-end) (not finish))
                  (anzu--add-overlay str beg end))))
            (setq anzu--cached-count count)))))))

(defun anzu--check-minibuffer-input (buf beg end use-regexp overlay-limit)
  (let* ((content (minibuffer-contents))
         (matched (if (string= content "")
                      (setq anzu--cached-count 0)
                    (anzu--count-matched buf content beg end use-regexp overlay-limit))))
    (setq anzu--total-matched matched)
    (force-mode-line-update)))

(defun anzu--clear-overlays (buf beg end)
  (with-current-buffer buf
    (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (overlay-get ov 'anzu-replace)
        (delete-overlay ov)))))

(defun anzu--read-from-string (prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (anzu--clear-overlays curbuf nil nil)
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu--check-minibuffer-input
                                  curbuf beg end use-regexp overlay-limit))))))
          (read-from-minibuffer (format "%s: " prompt)
                                nil nil nil
                                query-replace-from-history-variable nil t))
      (when timer
        (cancel-timer timer)
        (setq timer nil)))))

(defun anzu--query-validate-from-regexp (from)
  (when (string-match "\\(?:\\`\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
    (let ((match (match-string 1 from)))
      (cond
       ((string= match "\\n")
        (message "`\\n' here doesn't match a newline; type C-q C-j instead!!"))
       ((string= match "\\t")
        (message "\\t' here doesn't match a tab; to do that, just type TAB!!")))
      (sit-for 2))))

(defun anzu--query-from-string (prompt beg end use-regexp overlay-limit)
  (let ((from (anzu--read-from-string prompt beg end use-regexp overlay-limit)))
    (if (and (string= from "") query-replace-defaults)
        (cons (car query-replace-defaults)
              (query-replace-compile-replacement
               (cdr query-replace-defaults) use-regexp))
      (add-to-history query-replace-from-history-variable from nil t)
      (when use-regexp
        (anzu--query-validate-from-regexp from))
      from)))

(defun anzu--compile-replace-text (str)
  (let ((compiled (query-replace-compile-replacement str t)))
    (cond ((stringp compiled) compiled)
          ((and (consp compiled) (functionp (car compiled)))
           compiled)
          ((and (consp compiled) (stringp (car compiled)))
           (car compiled)))))

(defun anzu--evaluate-occurrence (ov to-regexp replace-count)
  (let ((from-regexp (overlay-get ov 'from-regexp))
        (from-string (overlay-get ov 'from-string))
        (compiled (anzu--compile-replace-text to-regexp)))
    (with-temp-buffer
      (insert from-string)
      (goto-char (point-min))
      (when (re-search-forward from-regexp nil t)
        (if (consp compiled)
            (replace-match (funcall (car compiled) (cdr compiled)
                                    replace-count))
          (replace-match compiled))
        (buffer-substring (point-min) (point-max))))))

(defun anzu--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst anzu--overlays-in-range (beg end)
  (loop for ov in (overlays-in beg end)
        when (overlay-get ov 'anzu-replace)
        collect ov into anzu-overlays
        finally return (sort anzu-overlays 'anzu--overlay-sort)))

(defun anzu--append-replaced-string (buf beg end use-regexp overlay-limit)
  (let ((content (minibuffer-contents))
        (replace-count 0))
    (unless (string= content anzu--last-replace-input)
      (setq anzu--last-replace-input content)
      (with-current-buffer buf
        (dolist (ov (anzu--overlays-in-range beg (min end overlay-limit)))
          (let ((replace-evaled (and use-regexp (anzu--evaluate-occurrence
                                                 ov content replace-count))))
            (if replace-evaled
                (incf replace-count)
              (setq replace-evaled content))
            (overlay-put ov 'after-string
                         (propertize replace-evaled 'face 'anzu-replace-to))))))))

(defun anzu--read-to-string (from prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (timer nil)
        (to-prompt (format "%s %s with: " prompt (query-replace-descr from)))
        (history-add-new-input nil))
    (setq anzu--last-replace-input "")
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu--append-replaced-string
                                  curbuf beg end use-regexp overlay-limit))))))
          (read-from-minibuffer to-prompt
                                nil nil nil
                                query-replace-from-history-variable nil t))
      (when timer
        (cancel-timer timer)
        (setq timer nil)))))

(defun anzu--query-replace-read-to (from prompt beg end use-regexp overlay-limit)
  (query-replace-compile-replacement
   (save-excursion
     (let ((to (anzu--read-to-string from prompt beg end use-regexp overlay-limit)))
       (add-to-history query-replace-to-history-variable to nil t)
       (setq query-replace-defaults (cons from to))
       to))
   use-regexp))

(defun anzu--overlay-limit ()
  (save-excursion
    (move-to-window-line -1)
    (forward-line 1)
    (point)))

(defun anzu--query-from-at-cursor (prompt buf beg end overlay-limit)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (let ((symbol-regexp (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (setq anzu--total-matched
            (anzu--count-matched buf symbol-regexp beg end t overlay-limit))
      (force-mode-line-update)
      symbol)))

(defun anzu--thing-begin (thing)
  (let ((bound (bounds-of-thing-at-point thing)))
    (and bound (car bound))))

(defun anzu--thing-end (thing)
  (let ((bound (bounds-of-thing-at-point thing)))
    (and bound (cdr bound))))

(defun anzu--region-begin (use-region thing)
  (if thing
      (or (anzu--thing-begin thing) (point))
    (if use-region
        (region-beginning)
      (point))))

(defun anzu--region-end (use-region thing)
  (if thing
      (or (anzu--thing-end thing) (point-max))
    (if use-region
        (region-end)
      (point-max))))

(defun anzu--begin-thing (at-cursor thing)
  (cond ((and at-cursor thing) thing)
        ((and at-cursor (not thing)) 'symbol)
        (t nil)))

(defun anzu--query-replace-common (use-regexp &optional at-cursor thing)
  (anzu--cons-mode-line 'replace)
  (let* ((use-region (use-region-p))
         (overlay-limit (anzu--overlay-limit))
         (beg (anzu--region-begin use-region (anzu--begin-thing at-cursor thing)))
         (end (anzu--region-end use-region thing))
         (prompt (anzu--query-prompt use-region use-regexp))
         (delimited current-prefix-arg)
         (curbuf (current-buffer))
         (clear-overlay nil))
    (when (and anzu-deactivate-region use-region)
      (deactivate-mark t))
    (unwind-protect
        (let* ((from (if (and at-cursor beg)
                         (progn
                           (setq delimited nil)
                           (anzu--query-from-at-cursor prompt curbuf beg end overlay-limit))
                       (anzu--query-from-string prompt beg end use-regexp overlay-limit)))
               (to (if (consp from)
                       (prog1 (cdr from) (setq from (car from)))
                     (anzu--query-replace-read-to
                      from prompt beg end use-regexp overlay-limit))))
          (anzu--clear-overlays curbuf beg end)
          (setq clear-overlay t)
          (if use-regexp
              (perform-replace from to t t delimited nil nil beg end)
            (query-replace from to delimited beg end)))
      (progn
        (unless clear-overlay
          (anzu--clear-overlays curbuf beg end))
        (anzu--reset-mode-line)
        (force-mode-line-update)))))

;;;###autoload
(defun anzu-query-replace-at-cursor ()
  (interactive)
  (anzu--query-replace-common t t))

;;;###autoload
(defun anzu-query-replace-at-cursor-thing ()
  (interactive)
  (anzu--query-replace-common t t anzu-replace-at-cursor-thing))

;;;###autoload
(defun anzu-query-replace ()
  (interactive)
  (anzu--query-replace-common nil))

;;;###autoload
(defun anzu-query-replace-regexp ()
  (interactive)
  (anzu--query-replace-common t))

(provide 'anzu)
;;; anzu.el ends here
