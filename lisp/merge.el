;;; -*- lexical-binding: t; no-byte-compile: t -*-
;; emacs -q -l merge.el --local local --other other --base base --output output

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(add-hook 'ediff-quit-hook #'save-buffers-kill-emacs)

(defvar file-alist ())

(defun get-next-command-line-arg (arg)
  (message "arg: %S" arg)
  (message "clal: %S" command-line-args-left)
  (let ((file (car command-line-args-left)))
    (if (not (file-exists-p file))
        (progn
          (message "%s is not a file." file)
          (sleep-for 1)
          (kill-emacs))
      (push (cons arg file) file-alist)
      (setq command-line-args-left (cdr command-line-args-left)))))

(dolist (arg '("--local" "--other" "--base" "--output"))
  (add-to-list 'command-switch-alist `(,arg . get-next-command-line-arg)))

(ediff-merge-with-ancestor (cdr (assoc "--local" file-alist))
                           (cdr (assoc "--other" file-alist))
                           (cdr (assoc "--base" file-alist))
                           nil
                           (cdr (assoc "--output" file-alist)))
