;;; sudo-toggle.el --- toggle opening a file with sudo via tramp -*- lexical-binding: t -*-

;; Copyright 2017 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Keywords: files
;; License: GPLv3
;; Version: 0.1
;; URL: http://bitbucket.org/jpkotta/sudo-toggle

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Toggle between opening a file normally and with sudo using tramp.
;; This should work with remote tramp files as well (multihop).
;;
;;   (global-set-key (kbd "C-c r") #'sudo-toggle)
;;
;; This was originally dired-toggle-sudo by Sebastien Gross.  I
;; rewrote it to use tramp functions instead of directly manipulating
;; strings, and to add the header line.

;;; Code:

(require 'files)
(require 'tramp)
(require 'dired)
(require 'cl-lib)

(defface sudo-toggle-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
  :group 'tramp)

;;;###autoload
(defun sudo-toggle-set-header ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (setq header-line-format
          (propertize "--- WARNING: EDITING FILE AS ROOT! %-"
                      'face 'sudo-toggle-header-face))))

(add-hook 'find-file-hook #'sudo-toggle-set-header)
(add-hook 'dired-mode-hook #'sudo-toggle-set-header)

(defun sudo-toggle-sudo-p (path)
  "Non-nil if PATH is a tramp sudo path."
  (when (and (stringp path)
           (not (string= path ""))
           (tramp-tramp-file-p path))
    (with-parsed-tramp-file-name path nil
      (string= method "sudo"))))

;; tramp API compat
(eval-and-compile
  (if (version< tramp-version "2.3.2")
      (defalias 'sudo-toggle--make-file-name #'tramp-make-tramp-file-name)
    (defun sudo-toggle--make-file-name (method user host local &optional hop)
      (tramp-make-tramp-file-name method user nil host nil local hop))))

(defun sudo-toggle--internal (path &optional sudo-user)
  "Convert PATH to its sudoed version.
root is used by default unless SUDO-USER is provided."
  (let ((path (expand-file-name path)))
    (if (not (tramp-tramp-file-p path))
        ;; local, no sudo
        (sudo-toggle--make-file-name "sudo" sudo-user nil path)
      (with-parsed-tramp-file-name path nil
        (if (not (string= method "sudo"))
            ;; add sudo
            (sudo-toggle--make-file-name
             "sudo" sudo-user host localname
             (let ((tramp-postfix-host-format tramp-postfix-hop-format)
                   (tramp-prefix-format nil))
               (sudo-toggle--make-file-name
                method user host "" hop)))
          ;; already has sudo, remove
          (if hop
              ;; remove sudo (last hop)
              (sudo-toggle--make-file-name
               nil nil nil localname
               (replace-regexp-in-string ;; remove trailing "|"
                (format "%s$" (regexp-quote tramp-postfix-hop-format))
                ""
                hop))
            ;; just use localname
            localname))))))

(defun sudo-toggle--tests ()
  "Simple test for `sudo-toggle--internal'."
  (let (orig known-good xform fail)
    (dolist (x `(("/ssh:gwuser@gateway|ssh:user@remote|sudo:root@remote:/etc/fstab"
                  . "/ssh:gwuser@gateway|ssh:user@remote:/etc/fstab")
                 ("/ssh:gwuser@gateway|ssh:user@remote:/etc/fstab"
                  . "/ssh:gwuser@gateway|ssh:user@remote|sudo:remote:/etc/fstab")
                 ("/ssh:user@remote:/etc/fstab"
                  . "/ssh:user@remote|sudo:remote:/etc/fstab")
                 ("/ssh:user@remote|sudo:root@remote:/etc/fstab"
                  . "/ssh:user@remote:/etc/fstab")
                 ("/sudo::/etc/fstab"
                  . "/etc/fstab")
                 ("/etc/fstab"
                  . "/sudo::/etc/fstab")
                 (,(expand-file-name "~/foo")
                  . ,(concat "/sudo::" (expand-file-name "~/foo")))
                 (,(format "/sudo:root@%s:%s/foo" (system-name) (getenv "HOME"))
                  . ,(expand-file-name "~/foo"))))
      (setq orig (car x)
            known-good (cdr x)
            xform (sudo-toggle--internal orig))
      (unless (string= xform known-good)
        (message "%s\n\t-> %s\n\t!= %s\n" orig xform known-good)
        (setq fail t)))
    (if fail
        (message "Fail!")
      (message "Pass!"))))
;;(sudo-toggle--tests)

;;;###autoload
(defun sudo-toggle (&optional sudo-user)
  "Reopen current file or dired buffer with sudo.

If SUDO-USER is nil assume root.

If called with a prefix argument, ask for username."
  (interactive "P")
  (let* ((fname (or buffer-file-name
                   dired-directory))
         (sudo-user (if current-prefix-arg
                        (read-string "Username: ")
                      sudo-user))
         (save-point (point))
         (save-window-start (window-start)))
    (when fname
      (setq fname (sudo-toggle--internal fname sudo-user))
      (cl-letf (((symbol-function 'server-buffer-done)
                 (lambda (buffer &optional for-killing)
                   (ignore buffer)
                   (ignore for-killing)
                   nil))
                ((symbol-function 'server-kill-buffer-query-function)
                 (lambda () t)))
        (find-alternate-file fname))
      (goto-char save-point)
      (set-window-start (selected-window) save-window-start))))

(provide 'sudo-toggle)

;;; sudo-toggle.el ends here
