;;; ibuffer-directory.el --- filter by directory in ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Maintainer: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1
;; Package-Requires: ()

;; This program is not part of GNU Emacs.

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

;; This library adds a filter, filter groups, and a sorter for ibuffer
;; to work with buffers by directory.

;; Suggested keybindings:

;;   (define-key ibuffer-mode-map
;;     (kbd "s p") 'ibuffer-do-sort-by-directory)
;;   (define-key ibuffer-mode-map
;;     (kbd "/ D") 'ibuffer-set-filter-groups-by-directory)
;;   (define-key ibuffer-mode-map
;;     (kbd "/ d") 'ibuffer-filter-by-directory)

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'ibuffer)
  (require 'ibuf-ext)
  (require 'ibuf-macs))

(defun ibuffer-directory--buffer-directory (buffer)
  "Get buffer's directory if a regular file or dired buffer, nil otherwise."
  (with-current-buffer buffer
    (or (when (buffer-file-name)
         (file-name-directory (buffer-file-name)))
       (when (eq major-mode 'dired-mode)
         (expand-file-name dired-directory))
       nil)))

(defun ibuffer-directory--get-all-buffer-directories ()
  "Return a list of all directories that have at least one file being visited."
  (interactive)
  (let ((dirnames (mapcar #'ibuffer-directory--buffer-directory (buffer-list))))
    (setq dirnames (cl-remove-if-not #'identity dirnames))
    (setq dirnames (sort dirnames #'string<))
    (setq dirnames (cl-remove-duplicates dirnames :test #'string=))
    dirnames))

(define-ibuffer-sorter directory
  "Sort by directory"
  (:description "directory")
  (string< (ibuffer-directory--buffer-directory (car a))
           (ibuffer-directory--buffer-directory (car b))))

(define-ibuffer-filter directory
    "Toggle current view to buffers with in a directory DIRECTORY."
  (:description
   "directory name"
   :reader
   (intern
    (completing-read "Filter by directory: "
                     (ibuffer-directory--get-all-buffer-directories)
                     #'identity
                     t nil nil nil nil)))
  (string= qualifier (ibuffer-directory--buffer-directory buf)))

;;;###autoload
(defun ibuffer-set-filter-groups-by-directory ()
  "Set the current filter groups to filter by directory."
  (interactive)
  (setq ibuffer-filter-groups
        (mapcar (lambda (dir)
                  (cons dir `((directory . ,dir))))
                (ibuffer-directory--get-all-buffer-directories)))
  (ibuffer-update nil t))

(provide 'ibuffer-directory)
;;; ibuffer-directory.el ends here
