(require 'ediff)
(require 'assoc)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-quit-hook 'save-buffers-kill-emacs)

(defvar file-alist ())

(defun get-next-command-line-arg (arg)
  (let ((file (car command-line-args-left)))
    (if (not (file-exists-p file))
        (progn
          (message "%s is not a file." file)
          (sleep-for 1)
          (kill-emacs))
      (aput 'file-alist arg file)
      (setq command-line-args-left (cdr command-line-args-left)))))

(dolist (arg '("--local" "--other" "--base" "--output"))
  (add-to-list 'command-switch-alist `(,arg . get-next-command-line-arg)))

;; (ediff-merge-with-ancestor (aget file-alist "--local")
;;                            (aget file-alist "--other")
;;                            (aget file-alist "--base")
;;                            nil
;;                            (aget file-alist "--output"))
