;;; highlight-fixme.el --- a face for highlighting "FIXME" and similar strings -*- lexical-binding: t -*-

;; Copyright (C) 2016 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Maintainer: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines a face named `highlight-fixme-face' used for
;; highlighting strings like "FIXME" and "TODO" in programming modes.
;; The strings should be highlighted in comments only.
;; `highlight-fixme-mode' is a minor mode that enables this extra
;; highlighting.  Add `highlight-fixme-mode' to your favorite
;; programming major mode hooks (e.g. (add-hook 'python-mode
;; 'highlight-fixme-mode)) or add it to `prog-mode-hook'.  There is
;; also a globalized minor mode `global-highlight-fixme-mode' that
;; applies to all buffers, and you can use hooks to turn off the mode.

;; This is based on fic-mode and fixme-mode.

;;; Code:

(defgroup highlight-fixme nil
  "Highlighting for strings like \"FIXME\" in programming modes."
  :group 'font-lock-extra-types
  :group 'faces)

(defcustom highlight-fixme-strings '("FIXME" "TODO" "BUG" "XXX" "DEBUG" "KLUDGE")
  "List of strings to highlight in `highlight-fixme-mode'."
  :group 'highlight-fixme)
;;(make-variable-buffer-local 'highlight-fixme-strings)

(defface highlight-fixme-face '((t (:bold t :foreground "Red")))
  "Face for strings like \"FIXME\"
This face is only used if `highlight-fixme-mode' is turned on."
  :group 'highlight-fixme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun highlight-fixme--highlightable-p (point)
  "True if POINT is in a comment in the current major mode."
  (memq (get-text-property point 'face)
        '(font-lock-doc-face font-lock-comment-face highlight-fixme-face)))

(defun highlight-fixme--matcher (limit &optional backward)
  "Matcher function for `font-lock-keywords'."
  (let ((regexp (regexp-opt highlight-fixme-strings 'words))
        (case-fold-search nil)
        (no-match t))
    (while (and no-match
              (if backward
                  (re-search-backward regexp limit t)
                (re-search-forward regexp limit t)))
      (when (and (highlight-fixme--highlightable-p (match-beginning 0))
               (highlight-fixme--highlightable-p (match-end 0)))
        (setq no-match nil)
        (goto-char (if backward
                       (match-beginning 0)
                     (match-end 0)))))
    (not no-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun highlight-fixme-next (arg)
  "Jump to the next FIXME-type word."
  (interactive "p")
  (when (< arg 0)
    (highlight-fixme-prev (- arg)))
  (save-match-data
    (dotimes (_ arg)
      (highlight-fixme--matcher (point-max) nil))))

;;;###autoload
(defun highlight-fixme-prev (arg)
  "Jump to the previous FIXME-type word."
  (interactive "p")
  (when (< arg 0)
    (highlight-fixme-next (- arg)))
  (save-match-data
    (dotimes (_ arg)
      (highlight-fixme--matcher (point-min) t))))

;;;###autoload
(defun highlight-fixme-show-all ()
  "Use `occur' to find all FIXME-type strings.

This actually finds a superset of the highlighted words, because
it uses a regexp instead of a more sophisticated matcher."
  (interactive)
  (occur (regexp-opt highlight-fixme-strings 'word)))

;;;###autoload
(define-minor-mode highlight-fixme-mode
  "Highlighting for strings like \"FIXME\" in programming modes."
  :lighter ""
  :group 'highlight-fixme
  (let ((font-lock-spec '((highlight-fixme--matcher 0 'highlight-fixme-face t))))
    (if highlight-fixme-mode
        (font-lock-add-keywords nil font-lock-spec)
      (font-lock-remove-keywords nil font-lock-spec))
    (when (called-interactively-p 'any)
      (font-lock-fontify-buffer))))

;;;###autoload
(define-globalized-minor-mode global-highlight-fixme-mode
  highlight-fixme-mode (lambda () (highlight-fixme-mode 1))
  :group 'highlight-fixme)

(provide 'highlight-fixme)

;;; highlight-fixme.el ends here
