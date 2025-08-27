;;; early-init.el -*- lexical-binding: t -*-

;; Copyright (C) Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>

(when init-file-debug
  (message "Loading jpkotta's early-init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(defun standard-value (symbol)
  "Return the `standard-value' of `symbol'.

Only defcustoms usually have a `standard-value'."
  (when (not (symbolp symbol))
    (error "Not a symbol: %s" symbol))
  (let ((sv (get symbol 'standard-value)))
    (when (null sv)
      (error "No standard-value: %s" symbol))
    (eval (car sv))))

(setq gc-cons-threshold (* 1024 (expt 2 20))
      gc-cons-percentage 0.6
      fnha-old file-name-handler-alist
      file-name-handler-alist nil)

(defun jpk/emacs-startup-hook ()
  (message "init.el loaded in %s." (emacs-init-time))
  (setq gc-cons-threshold (standard-value 'gc-cons-threshold)
        gc-cons-percentage (standard-value 'gc-cons-percentage)
        file-name-handler-alist fnha-old)
  (makunbound 'fnha-old)
  (garbage-collect))
(add-hook 'emacs-startup-hook #'jpk/emacs-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paths

(defvar extra-lisp-directory (expand-file-name "lisp/" user-emacs-directory)
  "Directory for Emacs lisp files that are not part of Emacs or in packages.")
(add-to-list 'load-path extra-lisp-directory)

(defvar test-lisp-directory (expand-file-name "test/" user-emacs-directory)
  "Directory for Emacs lisp files that in an unfinished state.")
(add-to-list 'load-path test-lisp-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(setq package-user-dir (expand-file-name
                        (format "elpa-%d" emacs-major-version)
                        user-emacs-directory))

(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("onpa" . "https://olanilsson.bitbucket.io/packages/")))
(setq package-archive-priorities '(;;("melpa-stable" . 20)
                                   ("gnu" . 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist '((vertical-scroll-bars . right)
                            (menu-bar-lines . 0)
                            (background-mode . dark)
                            (tool-bar-lines . 0)
                            (width . 81)
                            (scroll-bar-height . 5)
                            (scroll-bar-width . 10)))

(setq frame-inhibit-implied-resize t)

;; ignore Xresources
(advice-add #'x-apply-session-resources :override #'ignore)

(when init-file-debug
  (message "early-init.el loaded."))
