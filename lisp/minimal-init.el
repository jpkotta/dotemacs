
(setq package-user-dir (expand-file-name
                        (format "elpa-test")
                        user-emacs-directory))

(when (and (file-directory-p package-user-dir)
         (yes-or-no-p (format "Delete %s? " package-user-dir)))
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
        use-package-minimum-reported-time 0))

(use-package no-littering)