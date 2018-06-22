;; ergo-movement-mode.el --- use M-ijkl for cursor movement -*- lexical-binding: t -*-

;; Copyright (C) 2009 Teemu Likonen <tlikonen@iki.fi>

;; Author: Teemu Likonen <tlikonen@iki.fi>
;; License: GPLv3
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; DESCRIPTION
;;
;; Ergo Movement mode is a global minor mode which defines ergonomic
;; keybindings for cursor movement. See the function documentation
;; string below for more information.
;;
;; The movement keys are inspired by Xah Lee's Ergoemacs keybindings:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;; LICENSE
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;; INSTALLATION
;;
;; Put this file somewhere in your load-path. You can add an autoload
;; function to your ~/.emacs file
;;
;;     (autoload 'ergo-movement-mode "ergo-movement-mode"
;;       "Ergonomic keybindings for cursor movement" 'interactive)
;;
;; Or if you want to turn on the mode automatically when Emacs is
;; started put these lines in your ~/.emacs file:
;;
;;     (require 'ergo-movement-mode)
;;     (ergo-movement-mode 1)

;; The way this works is to create commands that dynamically look up a
;; keybinding and run it.  Emacs allows keys to run keyboard macros,
;; which is almost the same thing and arguably simpler.  The problem
;; is that certain things like selection-mode don't work with the
;; keyboard macro method.

;;; Code:

(defun ergo-movement--make-func (key-spec &optional cua-movement)
  "Return a command suitable for binding with global-set-key or
  similar functions.  The command should run whatever command is
  bound to the key specified by KEY-SPEC.  If CUA-MOVEMENT is
  non-nil, then the new command will extend the region when shift
  is held down."
  (let ((name (concat "ergo-movement--call-" key-spec "-keybind")))
    (unless (commandp (intern name))
      (fset (intern name)
            `(lambda ()
               ,(concat "Call the command bound to " key-spec) ;; doc string
               (interactive)
               (call-interactively
                (key-binding (read-kbd-macro ,key-spec))))))
    (when cua-movement
      (put (intern name) 'CUA 'move))
    (intern name)))

(defvar ergo-movement-key-alist
  '(("M-j"   . "<left>")
    ("M-l"   . "<right>")
    ("M-i"   . "<up>")
    ("M-k"   . "<down>")

    ("C-M-j" . "C-<left>")
    ("C-M-l" . "C-<right>")
    ("C-M-i" . "C-<up>")
    ("C-M-k" . "C-<down>")

    ("M-u"   . "DEL")
    ("M-o"   . "<deletechar>")
    ("C-M-u" . "C-<backspace>")
    ("C-M-o" . "C-<delete>")

    ("C-S-d" . "DEL")
    ("M-D"   . "C-<backspace>")
    ("M-d"   . "C-<delete>"))
  "Alist for `ergo-movement--make-keymap'.  car is new bind, cdr is
  old bind.")

(defun ergo-movement--make-keymap ()
  "Create a keymap with `ergo-movement-key-alist'."
  (let ((map (make-sparse-keymap)))
    (dolist (k ergo-movement-key-alist)
      (define-key map
        (read-kbd-macro (car k))
        (ergo-movement--make-func (cdr k) t)))
    map))

(defvar ergo-movement-mode-map
  (ergo-movement--make-keymap)
  "Key map for `ergo-movement-mode'.")

;;;###autoload
(define-minor-mode ergo-movement-mode
  "Ergonomic keybindings for cursor movement

Ergo Movement mode is a global minor mode which defines ergonomic
keybindings for cursor movement. This is suitable for QWERTY
keyboard.

    M-    u i o   =   <backspace>   <up>     <delete>
          j k l   =   <left>        <down>   <right>

    C-M-  u i o   =   C-<backspace> C-<up>   C-<delete>
          j k l   =   C-<left>      C-<down> C-<right>

The original bindings of the above movement commands are kept
untouched. The new bindings override other commands though.
\\{ergo-movement-mode-map}"

  :global t
  :lighter ""
  :keymap ergo-movement-mode-map
  )

(provide 'ergo-movement-mode)

;;; ergo-movement-mode.el ends here
