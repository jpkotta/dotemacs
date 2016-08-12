;;; ibuffer-unsaved.el --- filter by save status in ibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2016 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Maintainer: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.

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

;; This file adds a filter and filter groups for ibuffer to filter
;; buffers by modified status (`buffer-modified-p').  I find this
;; useful for quickly looking at all files that need to be saved,
;; e.g. to go through all unsaved files and either ignore, save, or
;; revert (which `save-some-buffers' (C-x s) doesn't offer).

;; Suggested keybindings:

;;   (define-key ibuffer-mode-map
;;     (kbd "/ 8") 'ibuffer-filter-by-unsaved)
;;   (define-key ibuffer-mode-map
;;     (kbd "/ *") 'ibuffer-set-filter-groups-by-unsaved)

;;; Code:

(require 'ibuf-ext)
(require 'ibuf-macs)

(define-ibuffer-filter unsaved
    "Only show unsaved buffers backed by a real file."
  (:description "Unsaved")
  (and (buffer-file-name buf)
     (buffer-modified-p buf)))

(defun ibuffer-set-filter-groups-by-unsaved ()
  "Set the current filter groups to filter by `buffer-modified-p'."
  (interactive)
  (setq ibuffer-filter-groups
        `(("Unsaved" . ((unsaved . t)))))
  (ibuffer-update nil t))

(provide 'ibuffer-unsaved)

;;; ibuffer-unsaved.el ends here
