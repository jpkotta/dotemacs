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

(require 'cl-lib)
(require 'ibuf-ext)
(require 'ibuf-macs)

(define-ibuffer-sorter directory
  "Sort by directory"
  (:description "directory")
  (cl-flet ((get-directory
             (data)
             (with-current-buffer (car data)
               (or buffer-file-name
                  (when (eq major-mode 'dired-mode)
                    (expand-file-name dired-directory))
                  ;; so that all non directories are at the end
                  ""))))
    (string< (get-directory a) (get-directory b))))

(defun ibuffer-directory--get-all-buffer-directories ()
  "Return a list of all directories that have at least one file being visited."
  (interactive)
  (let ((dirnames (mapcar #'buffer-file-name (buffer-list))))
    (setq dirnames (remove-if-not #'identity dirnames))
    (setq dirnames (mapcar #'file-name-directory dirnames))
    (setq dirnames (sort dirnames #'string<))
    (setq dirnames (remove-duplicates dirnames :test #'string=))
    dirnames))

(define-ibuffer-filter directory
    "Toggle current view to buffers with in a directory DIRECTORY."
  (:description
   "directory name"
   :reader
   (intern
    (completing-read "Filter by directory: "
                     (ibuffer-directory--get-all-buffer-directories)
                     'identity
                     t nil nil nil nil)))
  (string= qualifier
           (and (buffer-file-name buf)
              (file-name-directory (buffer-file-name buf)))))

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
