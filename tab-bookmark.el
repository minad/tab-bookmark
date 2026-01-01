;;; tab-bookmark.el --- Tab bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Daniel Mendler

;; Author: Daniel Mendler
;; Created: 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/tab-bookmark

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides commands which allow storing the window
;; configuration of the current tab as a bookmark.

;;; Code:

(require 'bookmark)
(require 'tab-bar)
(eval-when-compile
  (require 'cl-lib))

(defgroup tab-bookmark nil
  "Tab bookmarks."
  :group 'convenience
  :prefix "tab-bookmark-")

(defcustom tab-bookmark-filter-function
  #'tab-bookmark--filter-default
  "Filter function called for each buffer.
Return t if the current buffer is supposed to be bookmarked."
  :type 'symbol)

(defvar tab-bookmark-history nil
  "History of tab bookmarks, used by `tab-bookmark--read'.")

(defconst tab-bookmark--unnamed "unnamed")

(defun tab-bookmark--filter-default ()
  "Default filter function called for each buffer.
Return t if the current buffer is supposed to be bookmarked."
  (not (and (eq bookmark-make-record-function #'bookmark-make-record-default)
            (string-match-p "\\` " (buffer-name)))))

(defun tab-bookmark--make-record ()
  "Return a new bookmark record for the current buffer.
The current buffer must not have a backing file."
  (if (and (not buffer-file-name)
           (eq bookmark-make-record-function #'bookmark-make-record-default))
      `(,(bookmark-buffer-name)
        (buffer . ,(buffer-name))
        (handler . ,#'tab-bookmark-handler-fallback))
    (bookmark-make-record)))

(defun tab-bookmark--buffers ()
  "Return list of buffers to be bookmarked."
  (cl-loop for win in (window-list nil 'no-minibuf)
           for buf = (window-buffer win)
           if (with-current-buffer buf
                (funcall tab-bookmark-filter-function))
           collect buf))

(defun tab-bookmark--get ()
  "Get tab state as a bookmark record."
  (let* ((bufs (tab-bookmark--buffers))
         (n (length bufs)))
    `((buffer . ,(cl-loop for buf in bufs collect
                          (with-current-buffer buf
                            (tab-bookmark--make-record))))
      (filename . ,(if (= n 1) "1 buffer" (format "%s buffers" n)))
      (window . ,(window-state-get (frame-root-window) 'writable))
      (handler . ,#'tab-bookmark-handler))))

(defun tab-bookmark--put (name state)
  "Put tab STATE into tab NAME, restoring windows and buffers."
  (if (equal name tab-bookmark--unnamed)
      (unless (equal (tab-bookmark--current-tab-name) tab-bookmark--unnamed)
        (tab-bar-new-tab))
    (if (cl-loop for tab in (funcall tab-bar-tabs-function)
                 thereis (equal (alist-get 'name tab) name))
        (tab-bar-select-tab-by-name name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)))
  (save-window-excursion
    (dolist (buf (alist-get 'buffer state))
      (condition-case err
          (bookmark-jump buf #'ignore)
        (error (delay-warning 'tab-bookmark
                              (format "Error %S when opening %S" err buf))))))
  (window-state-put (alist-get 'window state) (frame-root-window)))

(defun tab-bookmark--current-tab-name ()
  "Return current tab name."
  (let ((tab (assq 'current-tab (funcall tab-bar-tabs-function))))
    (if (alist-get 'explicit-name tab)
        (alist-get 'name tab)
      tab-bookmark--unnamed)))

(defun tab-bookmark--top (&optional existing)
  "Return top tab bookmark from stack.
If EXISTING is nil generate a new name."
  (let* ((current (tab-bookmark--current-tab-name))
         (regex (format "\\`@%s <\\([0-9]+\\)>\\'"
                        (regexp-quote current)))
         (idx 0))
    (dolist (name (tab-bookmark--names))
      (when (string-match regex name)
        (setq idx (max idx (string-to-number (match-string 1 name))))))
    (unless existing
      (setq idx (1+ idx)))
    (when (> idx 0)
      (format "@%s <%s>" current idx))))

(defun tab-bookmark--read (prompt &optional default)
  "Prompting with PROMPT for bookmarked tab.
Return DEFAULT if user input is empty."
  (let* ((width 0)
         (candidates
          (cl-loop
           for name in (sort (tab-bookmark--names) #'string<)
           for item = (if-let (pos (string-match-p " " name))
                          (list (substring name 0 pos)
                                (string-trim (substring name pos))
                                name)
                        (list name "" name))
           collect (progn
                     (setq width (max (string-width (car item)) width))
                     item)))
         (fmt (format "%%-%ds %%s" width))
         (candidates
          (cl-loop for (tab comment id) in candidates collect
                   (cons (format fmt tab
                                 (propertize
                                  comment 'face
                                  (if (string-match-p "\\`<[0-9]+>\\'" comment)
                                      'font-lock-keyword-face
                                    'font-lock-comment-face)))
                         id)))
         (name (completing-read
                (if default
                    (format "%s (default %s): " prompt default)
                  (format "%s: " prompt))
                (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (category . bookmark)
                        (cycle-sort-function . identity)
                        (display-sort-function . identity))
                    (complete-with-action action candidates str pred)))
                nil (not default) nil 'tab-bookmark-history default)))
    (if (string-prefix-p "#" name)
        (or (cdr (assoc name candidates)) name)
      (format "@%s %s"
              (tab-bookmark--current-tab-name)
              (string-trim name)))))

(defun tab-bookmark--names ()
  "Return a list of names of all tab bookmarks."
  (bookmark-maybe-load-default-file)
  (cl-loop for bm in bookmark-alist
           if (eq #'tab-bookmark-handler
                  (alist-get 'handler (cdr bm)))
           collect (car bm)))

;;;###autoload
(defun tab-bookmark-handler-fallback (bm)
  "Handle buffer bookmark BM, used for buffers without file."
  (let* ((bm (bookmark-get-bookmark-record bm))
         (name (alist-get 'buffer bm)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (insert (format "tab-bookmark: Buffer %s not found" name))))))

;;;###autoload
(defun tab-bookmark-handler (bm)
  "Handle tab bookmark BM."
  ;; Restore the tab in the bookmark-after-jump-hook, since
  ;; it must happen after the execution of the display function.
  (letrec ((hook
            (lambda ()
              (setq bookmark-after-jump-hook (delq hook bookmark-after-jump-hook))
              (let ((name (bookmark-name-from-full-record bm)))
                (tab-bookmark--put
                 (if (string-match "\\`@\\([^ +]+\\)" name)
                     (match-string 1 name)
                   name)
                 (bookmark-get-bookmark-record bm))))))
    (push hook bookmark-after-jump-hook)))

;;;###autoload
(defun tab-bookmark (name)
  "If tab bookmark NAME exists, open it, otherwise save current tab."
  (interactive (list (tab-bookmark--read "Tab bookmark" (tab-bookmark--top))))
  (if (assoc name bookmark-alist)
      (tab-bookmark-open name)
    (tab-bookmark-save name)))

;;;###autoload
(defun tab-bookmark-save (name &optional no-overwrite)
  "Save current tab under the given NAME.
If NO-OVERWRITE is non-nil push to the bookmark list
without overwriting an already existing bookmark."
  (interactive (list (tab-bookmark--read "Save tab bookmark" (tab-bookmark--top))))
  (bookmark-store name (tab-bookmark--get) no-overwrite)
  (message "Tab bookmark `%s' saved" name))

;;;###autoload
(defun tab-bookmark-open (bm)
  "Open tab bookmark BM."
  (interactive (list (tab-bookmark--read "Open tab bookmark")))
  (bookmark-jump bm #'ignore))

;;;###autoload
(defun tab-bookmark-delete (name)
  "Delete tab bookmark NAME."
  (interactive (list (tab-bookmark--read "Delete tab bookmark")))
  (bookmark-delete name)
  (message "Tab bookmark `%s' deleted" name))

;;;###autoload
(defun tab-bookmark-rename (old &optional new)
  "Rename bookmark from OLD name to NEW name."
  (interactive (list (tab-bookmark--read "Rename tab bookmark")))
  (bookmark-rename old new))

;;;###autoload
(defun tab-bookmark-push ()
  "Push current tab as a bookmark."
  (interactive)
  (let ((name (tab-bookmark--top)))
    (tab-bookmark-save name 'no-overwrite)
    ;; Add to tab history to mark the new item as recent
    (add-to-history 'tab-bookmark-history name)))

;;;###autoload
(defun tab-bookmark-pop ()
  "Pop the last tab bookmark."
  (interactive)
  (let ((name (or (tab-bookmark--top 'existing)
                  (user-error "Tab bookmark stack is empty"))))
    (tab-bookmark-open name)
    (tab-bookmark-delete name)))

(provide 'tab-bookmark)
;;; tab-bookmark.el ends here
