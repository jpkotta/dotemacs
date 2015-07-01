;;; ediff-tweak --- Things to make ediff more pleasant.

;; Copyright (C) 2014 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Keywords: ediff, window, hexl, diff
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides three features to improve your ediff
;; experience: saving/restoring window configuration, automatically
;; using hexl mode for binary files, and not asking about applying
;; diff hunks.  All features are enabled by default, so if you don't
;; want any of them, disable them by setting the corresponding
;; variable in your init.el.

;; Window state

;; When ediff is starts, the window configuration is saved, and can be
;; restored with `ediff-tweak-restore-ediff-window-config'.  If you
;; change the window configuration, switch buffers, etc., you can run
;; this command to get back at the initial ediff window configuration.

;; Optionally, if `ediff-tweak-restore-window-state' is non-nil, the
;; window state is saved before ediff starts, so when ediff quits, the
;; original window configuration is restored automatically.

;; Hexl diffs

;; Normally, if you try to compare binary files with `ediff-files',
;; you get an error.  If `ediff-tweak-auto-hexl-diff' is non-nil,
;; trying to diff binary files automatically opens the files in
;; `hexl-mode' and does a diff on the hexl buffers.

;; Apply diff hunks with no confirmation

;; Normally, if you're in e.g. `ediff-merge-with-ancestor', you can
;; copy a hunk from either input buffer to the output buffer.  If you
;; change your mind, Emacs will ask for confirmation with
;; `yes-or-no-p', which I feel is overkill.  If
;; `ediff-tweak-copy-diff-silent' is non-nil, there is no
;; confirmation.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ediff))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ediff-tweak--ediff-window-config nil
  "Stores the window configuration while ediff is running, so it can be restored if it gets messed up.  See `ediff-tweak-restore-ediff-window-config'.")

(defun ediff-tweak--save-ediff-window-config ()
  (setq ediff-tweak--ediff-window-config (current-window-configuration)))
(add-hook 'ediff-after-setup-windows-hook
          'ediff-tweak--save-ediff-window-config
          'append)

;;;###autoload
(defun ediff-tweak-restore-ediff-window-config ()
  "Restore initial ediff window state."
  (interactive)
  (when (window-configuration-p ediff-tweak--ediff-window-config)
    (set-window-configuration ediff-tweak--ediff-window-config)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ediff-tweak-restore-window-state t
  "If non-nil, save the window config before running ediff and restore it on exit.")

(defvar ediff-tweak--before-window-config nil
  "Stores the window configuration before running ediff, so it can be restored afterwards.")

(defun ediff-tweak--save-window-config ()
  (unless (and (not ediff-tweak-restore-window-state)
             (window-configuration-p ediff-tweak--before-window-config))
    (setq ediff-tweak--before-window-config (current-window-configuration))))
(add-hook 'ediff-before-setup-hook 'ediff-tweak--save-window-config)

(defun ediff-tweak--restore-window-config ()
  (when (and ediff-tweak-restore-window-state
           (window-configuration-p ediff-tweak--before-window-config))
    (set-window-configuration ediff-tweak--before-window-config)
    (setq ediff-tweak--before-window-config nil))
  (setq ediff-tweak--ediff-window-config nil))
(add-hook 'ediff-quit-hook 'ediff-tweak--restore-window-config)

(eval-after-load "ediff"
  '(progn

     (dolist (func '(ediff-regions-wordwise ediff-regions-linewise))
       (advice-add func
                   :around
                   (lambda (orig &rest args)
                     "Save window config before running ediff."
                     (ediff-tweak--save-window-config)
                     (let (ediff-tweak-restore-window-state)
                       (apply orig args)))))

     (defun ediff-regions-linewise-this-buffer ()
       "Like `ediff-regions-linewise', but automatically use the current buffer instead of asking."
       (interactive)
       (ediff-regions-linewise (buffer-name) (buffer-name)))

     (defun ediff-regions-wordwise-this-buffer ()
       "Like `ediff-regions-wordwise', but automatically use the current buffer instead of asking."
       (interactive)
       (ediff-regions-wordwise (buffer-name) (buffer-name)))

  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary diff with hexl-mode
;; http://trey-jackson.blogspot.com/2010/10/emacs-tip-38-automatically-diff-binary.html

(defvar ediff-tweak-auto-hexl-diff t
  "If non-nil, automatically do a hexl diff when running ediff on binary files.")

(defvar ediff-tweak--do-hexl-diff t
  "Stores a trigger for doing diff in `hexl-mode'.")

(advice-add 'ediff-files-internal
            :around
            (lambda (orig &rest args)
              "Catch the condition when the binary files differ.

The reason for catching the error out here (when re-thrown from
the inner advice) is to let the stack continue to unwind before
we start the new diff otherwise some code in the middle of the
stack expects some output that isn't there and triggers an
error."
              (if ediff-tweak-auto-hexl-diff
                  (let ((file-A (ad-get-arg 0))
                        (file-B (ad-get-arg 1))
                        ediff-tweak--do-hexl-diff)
                    (condition-case err
                        (apply orig args)
                      (error
                       (if (ediff-tweak--do-hexl-diff
                            (let ((buf-A (find-file-noselect file-A))
                                  (buf-B (find-file-noselect file-B)))
                              (with-current-buffer buf-A
                                (hexl-mode 1))
                              (with-current-buffer buf-B
                                (hexl-mode 1))
                              (ediff-buffers buf-A buf-B))
                            (error (error-message-string err)))))))
                (apply orig args))))

(advice-add 'ediff-setup-diff-regions
            :around
            (lambda (orig &rest args)
              "When binary files differ, set the variable `ediff-tweak--do-hexl-diff'."
              (condition-case err
                  (apply orig args)
                (error
                 (setq ediff-tweak--do-hexl-diff
                       (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                          (error-message-string err))
                          (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                          (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))
                          (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
                 (error (error-message-string err))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't bother me about previously manipulated hunks

(defvar ediff-tweak-copy-diff-silent t
  "If non-nil, do not ask if a previously copied hunk should be changed.")

(advice-add 'ediff-test-save-region
            :around
            (lambda (orig &rest args)
              "Honor `ediff-tweak-copy-diff-silent'."
              (if ediff-tweak-copy-diff-silent
                  (cl-flet ((yes-or-no-p (prompt) t))
                    (apply orig args))
                (apply orig args))))

(provide 'ediff-tweak)

;;; ediff-tweak.el ends here
