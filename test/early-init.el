;; -*- lexical-binding: t; no-byte-compile: t -*-

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-archive-priorities '(("gnu" . 10)))

(if (package-installed-p 'use-package)
    (require 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

(when init-file-debug ;; --debug-init
  (setq use-package-verbose 'debug
        use-package-debug t
        use-package-minimum-reported-time 0
        debug-on-error t))

(use-package no-littering
  :config
  (no-littering-theme-backups)
  (setq custom-file (expand-file-name "custom.el" temporary-file-directory))
  (add-to-list 'auto-save-file-name-transforms
               `(".*" ,temporary-file-directory t) 'append)
  )
