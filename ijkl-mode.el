;; ijkl-mode.el --- Map arrow pad to letter keys

;; Copyright (C) 2013 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; URL: FIXME

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

;; This was inspired by ergo-movement-mode.el by Teemu Likonen.
;;
;; This binds arrow keys, backspace, and delete to some more
;; comfortable keys.
;;
;;   M-    u i o   =   <backspace>   <up>     <delete>
;;         j k l   =   <left>        <down>   <right>
;;
;;   C-M-  u i o   =   C-<backspace> C-<up>   C-<delete>
;;         j k l   =   C-<left>      C-<down> C-<right>
;;
;; In addition, C-d and C-D are bound to delete and backspace, and M-d
;; and M-D are bound to delete and C-delete and C-backspace.

;; INSTALLATION
;;
;; Put this file somewhere in your load-path. You can add an autoload
;; function to your ~/.emacs file
;;
;;     (autoload 'ijkl-mode "ijkl-mode"
;;       "Ergonomic keybindings for cursor movement" 'interactive)
;;
;; Or if you want to turn on the mode automatically when Emacs is
;; started put these lines in your ~/.emacs file:
;;
;;     (require 'ijkl-mode)
;;     (ijkl-mode 1)

;;; Code:

(defvar ijkl-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (e '(("<left>"        "M-j")
                 ("<right>"       "M-l")
                 ("<up>"          "M-i")
                 ("<down>"        "M-k")
                 ("S-<left>"      "M-J")
                 ("S-<right>"     "M-L")
                 ("S-<up>"        "M-I")
                 ("S-<down>"      "M-K")
                 
                 ("C-<left>"      "C-M-j")
                 ("C-<right>"     "C-M-l")
                 ("C-<up>"        "C-M-i")
                 ("C-<down>"      "C-M-k")
                 ("C-S-<left>"    "C-M-S-j")
                 ("C-S-<right>"   "C-M-S-l")
                 ("C-S-<up>"      "C-M-S-i")
                 ("C-S-<down>"    "C-M-S-k")
                 
                 ("DEL"           "M-u")
                 ("<deletechar>"  "M-o")
                 ("C-<backspace>" "C-M-u")
                 ("C-<delete>"    "C-M-o")
                 
                 ("C-<backspace>" "M-D")
                 ("DEL"           "C-S-d")
                 ("C-<delete>"    "M-d")
                 ("<deletechar>"  "C-d")
                 ))
      (define-key map (kbd (cadr e)) (kbd (car e))))
    map)
  "Key map for `ijkl-mode'.")

;;;###autoload
(define-minor-mode ijkl-mode
  "Ergonomic keybindings for cursor movement

IJKL mode is a global minor mode which defines ergonomic
keybindings for cursor movement. This is suitable for QWERTY
keyboards.

\\{ijkl-mode-map}"

  :global t
  :lighter ""
  :keymap ijkl-mode-map
  )

(provide 'ijkl-mode)

;;; ijkl-mode.el ends here
