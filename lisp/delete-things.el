;;; delete-things.el --- miscellaneous utilities to delete and kill text

;; Copyright (C) 2012 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1

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

;; This is a small set of functions to delete chunks (words, lines,
;; etc.).  Most built-in commands will kill text instead of deleting;
;; this library provides equivalents that do not save to the kill
;; ring.

;; Recommended keybindings:

;; (global-set-key (kbd "C-k") 'jpk/delete-line)
;; (global-set-key (kbd "C-S-k") 'kill-line)

;; (global-set-key (kbd "M-SPC") 'delete-horizontal-space)
;; (global-set-key (kbd "M-S-SPC") 'delete-blank-lines)

;; (global-set-key (kbd "<C-S-delete>") 'jpk/delete-syntax)
;; (global-set-key (kbd "<C-S-backspace>") 'jpk/backward-delete-syntax)
;; (global-set-key (kbd "C-<delete>") 'jpk/delete-word)
;; (global-set-key (kbd "M-d") 'jpk/delete-word)
;; (global-set-key (kbd "C-<backspace>") 'jpk/backward-delete-word)

;;; Code:


(require 'cl-lib)

;; from Jonathan Arkell (http://stackoverflow.com/questions/154097/whats-in-your-emacs/154980#154980)
;;;###autoload
(defun jpk/kill-syntax (&optional arg)
  "Kill ARG sets of syntax characters after point."
  (interactive "p")
  (let ((arg (or arg 1))
        (inc (if (and arg (< arg 0)) 1 -1))
        (opoint (point)))
    (while (not (= arg 0))
      (if (> arg 0)
          (skip-syntax-forward (string (char-syntax (char-after))))
        (skip-syntax-backward (string (char-syntax (char-before)))))
      (setq arg (+ arg inc)))
    (kill-region opoint (point))))

;;;###autoload
(defun jpk/kill-syntax-backward (&optional arg)
  "Kill ARG sets of syntax characters preceding point."
  (interactive "p")
  (jpk/kill-syntax (- 0 (or arg 1))))


(defmacro jpk/delete-instead-of-kill (&rest body)
  "Replaces `kill-region' with `delete-region' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end &optional yank-handler)
                (delete-region beg end))))
     ,@body))

;;;###autoload
(defun jpk/delete-syntax (arg)
  "Like `jpk/kill-syntax', but does not save to the `kill-ring'."
  (interactive "*p")
  (jpk/delete-instead-of-kill
   (jpk/kill-syntax arg)))
(put 'jpk/delete-syntax 'CUA 'move)

;;;###autoload
(defun jpk/backward-delete-syntax (arg)
  "Like `syntax-subword-backward-kill-syntax', but does not save to the `kill-ring'."
  (interactive "*p")
  (jpk/delete-syntax (- arg)))
(put 'jpk/backward-delete-syntax 'CUA 'move)

;;;###autoload
(defun jpk/delete-word (arg)
  "Like `kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (jpk/delete-instead-of-kill (kill-word arg)))
(put 'jpk/delete-word 'CUA 'move)

;;;###autoload
(defun jpk/backward-delete-word (arg)
  "Like `backward-kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (jpk/delete-word (- arg)))
(put 'jpk/backward-delete-word 'CUA 'move)

;;;###autoload
(defun jpk/delete-line (&optional arg)
  "Like `kill-line', but does not save to the `kill-ring'."
  (interactive "*P")
  (jpk/delete-instead-of-kill (kill-line arg)))


(provide 'delete-things)
