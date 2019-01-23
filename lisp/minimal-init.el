;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq package-user-dir (expand-file-name
                        (format "elpa-test")
                        user-emacs-directory))

(setq jpk/delete-elpa 'ask) ; t, ask, nil
(when (and (file-directory-p package-user-dir)
         jpk/delete-elpa
         (if (eq jpk/delete-elpa 'ask)
             (yes-or-no-p (format "Delete %s? " package-user-dir))
           t))
  (delete-directory package-user-dir 'recursive))

(package-initialize)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("gnu" . 10)
                                   ("melpa" . 0)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(when init-file-debug ;; --debug-init
  (setq use-package-verbose 'debug
        use-package-debug t
        use-package-minimum-reported-time 0
        debug-on-error t))

(use-package no-littering)
