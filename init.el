;;; init.el --- jpkotta's emacs init file

;; Copyright (C) Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>

(message "Loading jpkotta's init.el.")

(setq gc-cons-threshold most-positive-fixnum)
(defun jpk/emacs-startup-hook ()
  (message "init.el loaded in %s." (emacs-init-time))
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(add-hook 'emacs-startup-hook #'jpk/emacs-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO

;; Patches
;;
;; ibuffer
;; graphlog stuff in vc-hg

;; a better way to do global key bindings
;; http://shallowsky.com/blog/linux/editors/emacs-global-key-bindings.html

;; ;; override function but retain original definition!
;; ;; doesn't seem to work in :around advice
;; (defun foo ()
;;   (message "old foo"))
;; (cl-letf (((symbol-function 'old-foo) (symbol-function 'foo))
;;           ((symbol-function 'foo) (lambda () (message "new foo"))))
;;   (foo)
;;   (old-foo))

;; rectangle-mark-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paths

(require 'cl-lib) ;; not autoloaded

(defun file-newer-than-all-in-dir-p (filename dirname)
  "Non-nil if any files in DIRNAME are newer than FILENAME.

Ignores files in the directory that are not regular
files (e.g. directories, fifos, etc.)."
  (cl-notany (lambda (x) (file-newer-than-file-p x filename))
             (cl-remove-if-not #'file-regular-p
                               (directory-files dirname t))))

(defvar extra-lisp-directory (concat user-emacs-directory "lisp/")
  "Directory for Emacs lisp files that are not part of Emacs or in packages.")
(add-to-list 'load-path extra-lisp-directory)
(let* ((prefix "lisp")
       (autoload-file (concat extra-lisp-directory prefix "-autoloads.el")))
  (unless (and (file-exists-p autoload-file)
             (file-newer-than-all-in-dir-p autoload-file extra-lisp-directory))
    (require 'package)
    (package-generate-autoloads prefix extra-lisp-directory))
  (load-file autoload-file))

;; set up specific to the local machine
(let ((local-init-file (concat extra-lisp-directory "local-init.el")))
  (when (file-exists-p local-init-file)
    (load-file local-init-file)))

;; info directory
(with-eval-after-load "info"
  (add-to-list 'Info-additional-directory-list "~/.info/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(setq package-selected-packages
      '(
        ac-c-headers
        ac-math
        adaptive-wrap
        anchored-transpose
        atomic-chrome
        auctex
        auto-complete
        backup-walker
        bitbake
        bm
        boxquote
        browse-kill-ring
        buffer-move
        calmer-forest-theme
        csharp-mode
        csv-mode
        dictionary
        diff-hl
        diminish
        dired+
        dired-imenu
        dired-narrow
        dired-ranger
        easy-repeat
        edit-list
        expand-region
        figlet
        flyspell-correct
        fuzzy ;; for auto-complete
        fvwm-mode
        ggtags
        go-mode
        go-scratch
        hgrc-mode
        hide-lines
        highlight-numbers
        highlight-operators
        highlight-quoted
        hl-todo
        htmlize
        ibuffer-projectile
        ibuffer-tramp
        iedit
        immortal-scratch
        keychain-environment
        ;;list-unicode-display
        lua-mode
        magit
        markdown-mode
        mediawiki
        mic-paren
        modeline-posn
        morlock
        multi-compile
        mwim
        openwith
        paren-face
        pkgbuild-mode
        projectile
        pydoc-info
        pylint
        python-info
        rainbow-mode
        sane-term
        save-visited-files
        smart-shift
        smart-tabs-mode
        sphinx-doc
        sqlup-mode
        ssh-config-mode
        syntax-subword
        systemd
        undo-tree
        visual-regexp
        wrap-region
        yaml-mode
        ))

(setq package-user-dir (expand-file-name
                        (format "elpa-%d" emacs-major-version)
                        user-emacs-directory))

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun jpk/install-selected-packages ()
  (interactive)
  (if (not (called-interactively-p))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) t)))
        (when (null package-archive-contents)
          (package-refresh-contents))
        ;;(package-autoremove)
        (package-install-selected-packages))
    (package-refresh-contents)
    ;;(package-autoremove)
    (package-install-selected-packages)))

(jpk/install-selected-packages)

(defmacro with-library (feature &rest body)
  "Evaluate BODY only if FEATURE is provided.  (require FEATURE) will be attempted."
  (declare (indent defun))
  `(when (require ,feature nil 'noerror)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom

;; store settings from customization system in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(message "loading customizations from %s" custom-file)
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance

(setq inhibit-startup-screen t
      initial-scratch-message "")

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq visible-bell t)

(set-scroll-bar-mode 'right)
(size-indication-mode 1)
(column-number-mode 1)
(transient-mark-mode 1)
(setq set-mark-command-repeat-pop t)

(setq truncate-lines t)

(setq font-lock-maximum-decoration t)

(setq default-frame-alist '((vertical-scroll-bars . right)
                            (menu-bar-lines . 0)
                            (background-mode . dark)
                            (tool-bar-lines . 0)
                            (width . 81)
                            (scroll-bar-height . 5)
                            (scroll-bar-width . 10)))

(setq default-font-family "DejaVu Sans Mono")
(when (string= system-type "windows-nt")
  (setq default-font-family "Consolas"))
(setq custom-safe-themes t)
(with-library 'calmer-forest-theme
  (load-theme 'calmer-forest 'noconfirm)
  (custom-theme-set-faces
   'calmer-forest

   `(default
      ((t (:family
           ,default-font-family
           :inherit nil :stipple nil :overline nil :underline nil
           :background "gray12" :foreground "green"
           :inverse-video nil :box nil :strike-through nil
           :slant normal :weight normal :height 100 :width normal
           :foundry "unknown"))))

   '(fringe ((t (:background "gray10" :foreground "dark green"))))
   '(highlight ((t (:background "gray6"))))

   '(mode-line ((t (:background "gray37" :foreground "gray85" :overline "black"))))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(modelinepos-region ((t (:inverse-video t))))

   '(region ((t (:background "#400060"))))
   '(mouse ((t (:background "orange"))))
   '(mouse-flash-position ((t (:background "gray75"))))
   '(secondary-selection ((t (:background "#600040"))))

   '(ac-completion-face ((t (:foreground "green3"))))
   '(ac-selection-face ((t (:background "gray9" :foreground "magenta"))))
   '(ac-candidate-face ((t (:background "gray16" :foreground "lavender"))))
   '(ac-gtags-selection-face ((t (:inherit ac-selection-face))))
   '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-yasnippet-selection-face ((t (:inherit ac-selection-face))))
   '(ac-yasnippet-candidate-face ((t (:inherit ac-candidate-face))))

   '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))

   '(diff-refine-changed ((t (:weight bold :background "gray24"))))
   '(diff-refine-added ((t (:inherit diff-refine-change :foreground "green1"))))
   '(diff-refine-removed ((t (:inherit diff-refine-change :foreground "red1"))))
   '(diff-nonexistent ((t (:inherit diff-file-header :weight bold :foreground "plum"))))

   '(ediff-even-diff-A ((t (:inherit diff-refine-changed))))
   '(ediff-even-diff-B ((t (:inherit diff-refine-changed))))
   '(ediff-even-diff-C ((t (:inherit diff-refine-changed))))
   '(ediff-even-diff-Ancestor ((t (:inherit diff-refine-changed))))
   '(ediff-odd-diff-A ((t (:inherit diff-refine-changed))))
   '(ediff-odd-diff-B ((t (:inherit diff-refine-changed))))
   '(ediff-odd-diff-C ((t (:inherit diff-refine-changed))))
   '(ediff-odd-diff-Ancestor ((t (:inherit diff-refine-changed))))

   '(term ((t (:foreground "lavender blush"))))

   '(Info-quoted ((t (:family "Luxi Mono"))))
   ))

;; Indicates when you're beyond a column (e.g. 80) and also shows the
;; size of the region if it's active.
(require 'modeline-posn nil 'noerror)
(setq modelinepos-column-limit 80)

;; frame title
(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            (car (split-string (system-name) "\\.")))
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-undecided "(EOL?)"
      eol-mnemonic-unix "(LF)")

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default indicate-empty-lines t)

(setq uniquify-buffer-name-style 'post-forward)

(setq whitespace-style '(newline space-mark tab-mark newline-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ergomovement

(with-library 'ergo-movement-mode
  (ergo-movement-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(defun jpk/bury-some-buffers ()
  "Bury uninteresting/duplicate buffers."
  (interactive)
  (dolist (w (append (window-list) (window-list)))
    (let* ((buf (window-buffer w))
           (bufname (buffer-name buf))
           (all-window-bufs (mapcar 'window-buffer (window-list))))
      (when (or (string-match-p "^\\*.*\\*$" bufname)
               (memq buf (cdr (memq buf all-window-bufs))))
        (unless (eq w (selected-window))
          (bury-buffer buf)
          (switch-to-prev-buffer w 'bury))))))

(defun jpk/startup ()
  "Set up emacs the way I like it."
  (interactive)
  (let ((svfm (and (boundp 'save-visited-files-mode)
                 (not save-visited-files-mode)
                 (y-or-n-p "Restore session? "))))
    (split-windows-in-quarters)
    (modify-frame-parameters nil '((fullscreen . maximized)))
    (when svfm
      (let ((save-visited-files-ignore-tramp-files
             save-visited-files-ignore-tramp-files))
        (when (y-or-n-p "Tramp files? ")
          (setq save-visited-files-ignore-tramp-files t))
        (save-visited-files-mode 1))))
  (jpk/bury-some-buffers))

(when (and (string= system-type "windows-nt")
         (executable-find "bash"))
  (setq shell-file-name (executable-find "bash")))

(defun advice-remove-all (symbol)
  "Removes all advice from function symbol SYMBOL."
  (interactive (find-function-read))
  (advice-mapc (lambda (advice props)
                 (advice-remove symbol advice))
               symbol))

(defun suspend-frame-if-not-gui ()
  "Like `suspend-frame', but does not suspend GUI frames."
  (interactive)
  (if (display-graphic-p)
      (message "Use `M-x suspend-frame' instead.")
    (suspend-frame)))

(global-set-key (kbd "C-x C-z") 'suspend-frame-if-not-gui)

;; Don't create the .#filename files and don't ask about stealing.
(setq create-lockfiles nil)

;; enable with M-x sticky-buffer-mode
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; narrowing makes a region effectively the entire buffer
;; useful for not mangling the entire buffer
;; disable with widen
(put 'narrow-to-region 'disabled nil)

;; goal column is a way to prefer a specific column when moving to
;; different lines.  Bound to C-x C-n, disable with C-u C-x C-n.
(put 'set-goal-column 'disabled nil)

;; I hit this by accident sometimes, and don't find the default
;; binding compose-mail useful.
(global-unset-key (kbd "C-x m"))

(dolist (f '(mouse-set-point mouse-set-region))
  (advice-add f
              :before-until
              (lambda (&rest args)
                "no mouse select window if minibuffer active"
                (or (active-minibuffer-window)
                   (window-minibuffer-p)
                   (minibuffer-window-active-p (selected-window))))))

;; cancel everything, including active minibuffers and recursive edits
(global-set-key (kbd "C-M-g") 'top-level)

;;(delete-selection-mode 1)

;; Disable highlighting clickable stuff when the mouse moves over it.
;; Turning it off speeds up remote X.
(setq mouse-highlight nil)

;; never shrink windows
(defvar allow-window-shrinking nil
  "If non-nil, effectively disable shrinking windows by making `shrink-window-if-larger-than-buffer' a no-op.")
(advice-add 'shrink-window-if-larger-than-buffer
            :before-while
            (lambda (&rest args)
              "Do nothing if `allow-window-shrinking' is nil."
              allow-window-shrinking))

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(global-set-key [remap exchange-point-and-mark]
                'exchange-point-and-mark-no-activate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing

(with-library 'printing
  (setq pr-gv-command "okular")
  (pr-update-menus t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ggtags

(setq ggtags-global-window-height nil
      ggtags-enable-navigation-keys nil
      ggtags-update-on-save t)

;; This works even if ggtags-find-tag-dwim is just marked for
;; autoloading but isn't loaded yet.
(when (commandp (symbol-function 'ggtags-find-tag-dwim))
  (global-set-key (kbd "M-.") #'ggtags-find-tag-dwim)
  (global-set-key (kbd "M-?") #'ggtags-find-reference)

  ;; keep default definition for emacs-lisp-mode
  (define-key emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "M-?") #'xref-find-references)
  )

;; stops ggtags-create-tags from asking
(setenv "GTAGSLABEL" "default")

(defun ggtags-create-tags-pygments ()
  "Like `ggtags-create-tags', but use pygments backend."
  (interactive)
  (let ((orig-label (getenv "GTAGSLABEL")))
    (setenv "GTAGSLABEL" "pygments")
    (ignore-errors
      (call-interactively 'ggtags-create-tags))
    (setenv "GTAGSLABEL" orig-label)))

;; GTAGSLABEL has no effect unless there's a ~/.globalrc
(let ((rcfile "~/.globalrc")
      (dist-rcfiles '("/usr/share/gtags/gtags.conf"
                      "/usr/local/share/gtags/gtags.conf")))
  (unless (file-exists-p rcfile)
    (dolist (dist-rcfile dist-rcfiles)
      (when (file-exists-p dist-rcfile)
        (copy-file dist-rcfile rcfile)))))

;; xref is the unified cross reference subsystem.  ggtags actually
;; uses it for some things like tag history.  It's labeled as
;; experimental in Emacs 25, and seems like it needs more work.  gxref
;; is a package to use global as an xref backend.  Once xref is more
;; mature, it will probably be better than ggtags.

;; (defun gxref-create-db-pygments (project-root-dir)
;;   "Like `gxref-create-db', but set GTAGSLABEL to pygments.

;; Pygments supports more languages, but is much slower than the
;; default label."
;;   (interactive "DCreate db in directory: ")
;;   (let ((gxref-gtags-label "pygments"))
;;     (gxref-create-db project-root-dir)))

;; (with-eval-after-load 'xref
;;   (add-hook 'xref-backend-functions #'gxref-xref-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile

(with-library 'projectile
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-ack-function 'ack
        ;; If ggtags-mode is on, projectile automatically uses it.
        projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
  (projectile-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repeatable Commands

(with-library 'easy-repeat
  (dolist (f '(bm-next bm-previous evil-numbers/inc-at-pt evil-numbers/dec-at-pt
                       gud-next gud-step
                       help-go-back help-go-forward
                       next-buffer previous-buffer))
    (add-to-list 'easy-repeat-command-list f))
  (easy-repeat-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode

(use-package org
  ;;:pin gnu
  :config
  (defun jpk/org-mode-hook ()
    (setq adaptive-wrap-extra-indent 0)
    (visual-line-mode 1)
    (when (featurep 'flyspell)
      (flyspell-mode 1)))
  (add-hook 'org-mode-hook #'jpk/org-mode-hook)

  (setq org-ellipsis "…"
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-fontify-emphasized-text nil
        org-support-shift-select 'always)

  ;; http://www.howardism.org/Technical/Emacs/literate-devops.html
  (setq org-babel-load-languages
        '((sh . t)
          (emacs-lisp . t)
          (python . t)
          (sql . t) ;; see also ob-sql-mode
          (sqlite . t)
          (C . t)
          ))

  (defun org-babel-reload-languages ()
    (interactive)
    (org-babel-do-load-languages
     'org-babel-load-languages org-babel-load-languages))
  (org-babel-reload-languages)

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-babel-python-command "python -ic ''")

  :bind (:map org-mode-map
         ("C-<tab>" . nil)
         ("C-S-<iso-lefttab>" . nil)
         ("<S-iso-lefttab>" . nil)
         ("M-<up>" . nil)
         ("M-<down>" . nil)
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("C-S-<left>" . nil)
         ("C-S-<right>" . nil)
         ("C-<return>" . nil)
         ("C-S-<down>" . nil)
         ("C-S-<up>" . nil)
         ([remap forward-paragraph] . nil)
         ([remap backward-paragraph] . nil))
  )

(use-package ob-ipython
  :after (org ipython)
  :config
  (add-to-list 'org-babel-load-languages (ipython . t))
  (org-babel-reload-languages)
  )

(use-package ob-async
  :if nil
  :after org
  :config
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ob-async-org-babel-execute-src-block)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc insertions

(defun insert-lorem-ipsum ()
  "Insert placeholder text."
  (interactive "*")
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing "
          "elit, sed do eiusmod tempor incididunt ut labore et "
          "dolore magna aliqua. Ut enim ad minim veniam, quis "
          "nostrud exercitation ullamco laboris nisi ut aliquip "
          "ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore "
          "eu fugiat nulla pariatur. Excepteur sint occaecat "
          "cupidatat non proident, sunt in culpa qui officia "
          "deserunt mollit anim id est laborum."))

(defun insert-look-of-disapproval (arg)
  "Insert a Look of Disapproval (ಠ_ಠ).
With prefix arg, insert a large ASCII art version.
 _____)        _____)
 /   \         /   \
(  O  )       (  O  )
 \___/         \___/
       =======
"
  (interactive "*P")
  (if (not arg)
      ;;(mapconcat 'insert-char '(3232 95 3232) "")
      (insert "ಠ_ಠ")
    (insert " _____)        _____)\n"
            " /   \\         /   \\\n"
            "(  O  )       (  O  )\n"
            " \\___/         \\___/\n"
            "       ======= \n")
    ))
(global-set-key (kbd "C-c w t f") 'insert-look-of-disapproval)

(defun insert-awesome-face (arg)
  "Insert a happy face (☻).
With prefix arg, insert a large ASCII art version.
  __     __
 /  o   /  o
|____| |____|

-------------
|            )
 \    ,-----,
  '-./____-'
"
  (interactive "*P")
  (if (not arg)
      ;;(insert-char 9787)
      (insert "☻")
    (insert "  __     __   \n"
            " /  o   /  o  \n"
            "|____| |____| \n"
            "              \n"
            "------------- \n"
            "|            )\n"
            " \\    ,-----, \n"
            "  '-./____-'  \n")
    ))

(defun insert-current-time ()
  "Insert the current date and time in the buffer."
  (interactive "*")
  (let* ((fmt-alist '(("time" "%T")
                      ("date" "%F")
                      ("datetime" "%F %T")
                      ("iso" "%FT%T%z")))
         (type (completing-read "Type: "
                                (mapcar 'car fmt-alist)
                                nil t nil nil "datetime"))
         (fmt (cadr (assoc type fmt-alist))))
    (insert (format-time-string fmt (current-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ssh

;; keychain is a way to manage ssh-agents.  In particular, it looks
;; for a running ssh-agent and stores it's env vars in a file, any
;; program that knows how to read the file can use the agent.
(with-library 'keychain-environment
  (keychain-refresh-environment))

(dolist (x '(".ssh/config\\'" "sshd?_config"))
  (add-to-list 'auto-mode-alist `(,x . ssh-config-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C SubWordMode
;; FindSubWordsInCamelCase

(with-library 'syntax-subword
  (setq syntax-subword-skip-spaces 'consistent)
  (global-syntax-subword-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diminish
;; Hide some minor mode indicators in the modeline.

(with-library 'diminish
  (with-eval-after-load "abbrev" (diminish 'abbrev-mode))
  (with-eval-after-load "auto-complete" (diminish 'auto-complete-mode))
  (with-eval-after-load "doxymacs" (diminish 'doxymacs-mode))
  (with-eval-after-load "eldoc" (diminish 'eldoc-mode))
  (with-eval-after-load "face-remap" (diminish 'buffer-face-mode))
  (with-eval-after-load "flyspell" (diminish 'flyspell-mode))
  (with-eval-after-load "ggtags" (diminish 'ggtags-mode))
  (with-eval-after-load "projectile" (diminish 'projectile-mode))
  (with-eval-after-load "sphinx-doc" (diminish 'sphinx-doc-mode))
  (with-eval-after-load "wrap-region" (diminish 'wrap-region-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help

(setq message-log-max 10000)
(global-set-key (kbd "C-h C-f") 'find-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers and strings

(use-package evil-numbers
  :bind (("C-c =" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hexl mode

(use-package hexl
  :config
  (setq hexl-bits 8)

  (defun hexl-forward-256 (&optional arg)
    "Move vertically down ARG blocks of 256 bytes (16 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (ignore-errors
      (hexl-goto-address (+ (* 256 arg) (hexl-current-address))))
    (recenter))

  (defun hexl-backward-256 (&optional arg)
    "Move vertically up ARG blocks of 256 bytes (16 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (hexl-forward-256 (- arg)))

  (defun hexl-forward-1k (&optional arg)
    "Move vertically down ARG blocks of 1024 bytes (64 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (ignore-errors
      (hexl-goto-address (+ (* 1024 arg) (hexl-current-address))))
    (recenter))

  (defun hexl-backward-1k (&optional arg)
    "Move vertically up ARG blocks of 1024 bytes (64 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (hexl-forward-1k (- arg)))

  (defun hexl-forward-4k (&optional arg)
    "Move vertically down ARG blocks of 4096 bytes (256 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (ignore-errors
      (hexl-goto-address (+ (* 4096 arg) (hexl-current-address))))
    (recenter))

  (defun hexl-backward-4k (&optional arg)
    "Move vertically up ARG blocks of 4096 bytes (256 lines)."
    (interactive "p")
    (setq arg (if (= 0 arg) 1 arg))
    (hexl-forward-4k (- arg)))

  :bind (:map hexl-mode-map
         ("<next>" . hexl-forward-256)
         ("<prior>" . hexl-backward-256)
         ("C-<next>" . hexl-forward-1k)
         ("C-<prior>" . hexl-backward-1k)
         ("C-S-<next>" . hexl-forward-4k)
         ("C-S-<prior>" . hexl-backward-4k))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUA mode

(use-package cua-rect
  :ensure nil
  :config
  (defun string-to-number-c (str &optional base)
    "Like `string-to-number', but with C-style prefixes for
non-decimal strings. This can be used as a drop-in replacement
for `string-to-number'."
  (let ((case-fold-search t))
    (cond
     ((string-match-p "0x[0-9A-F]+" str)
      (string-to-number (replace-regexp-in-string "0x" "" str) 16))
     ((string-match-p "0o[0-7]+" str)
      (string-to-number (replace-regexp-in-string "0o" "" str) 8))
     ((string-match-p "0d[0-9]+" str)
      (string-to-number (replace-regexp-in-string "0d" "" str) 10))
     ((string-match-p "0b[01]+" str)
      (string-to-number (replace-regexp-in-string "0b" "" str) 2))
     (t
      (string-to-number str base)))))

  (defvar cua--sequence-rectangle-first-hist ()
    "History list for the initial value in `cua-sequence-rectangle'.")
  (defvar cua--sequence-rectangle-incr-hist ()
    "History list for the increment in `cua-sequence-rectangle'.")

  (advice-add 'cua-sequence-rectangle
              :filter-args
              (lambda (&rest args)
                "Use `string-to-number-c' instead of
`string-to-number', and save more history."
                (interactive)
                (list (if current-prefix-arg
                          (prefix-numeric-value current-prefix-arg)
                        (string-to-number-c
                         (read-string "Start value: "
                                      (if cua--sequence-rectangle-first-hist
                                          (car cua--sequence-rectangle-first-hist)
                                        "0")
                                      'cua--sequence-rectangle-first-hist
                                      "0")))
                      (string-to-number-c
                       (read-string "Increment: "
                                    (if cua--sequence-rectangle-incr-hist
                                        (car cua--sequence-rectangle-incr-hist)
                                      "1")
                                    'cua--sequence-rectangle-incr-hist
                                    "1"))
                      (read-string (concat "Format: (" cua--rectangle-seq-format ") ")))))

  :bind (("C-<return>" . cua-rectangle-mark-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)
(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-to-list 'revert-without-query "\\.rom\\'")

(global-set-key (kbd "<f5>") 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard

(setq select-enable-clipboard t
      select-enable-primary t
      select-active-regions t)
(setq save-interprogram-paste-before-kill t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode

;; tip - use insert-char to insert unicode characters by name

;; from Jonathan Arkell (http://stackoverflow.com/questions/154097/whats-in-your-emacs/154980#154980)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq default-input-method "TeX"
      read-quoted-char-radix 16)

(defun dos2unix ()
  "Convert a DOS file ('\\r\\n') to Unix ('\\n')"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix t))

(defun force-unix-line-endings ()
  "Replace all occurances of \\r\\n with \\n."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\015\012" nil t)
      (replace-match "\012" nil t))))

(defun unix2dos ()
  "Convert a Unix ('\n') file to DOS ('\r\n')"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Daemon/Server

(global-set-key (kbd "C-x C-S-c") #'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-c") #'delete-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Browser

(setq eww-search-prefix "https://google.com/search?q=")
(setq shr-color-visible-luminance-min 70)

;; default is "chromium"
(catch 'break
  (dolist  (e '("google-chrome-beta" "google-chrome" "google-chrome-stable"))
    (when (executable-find e)
      (setq browse-url-chromium-program e)
      (throw 'break t))))
(setq browse-url-browser-function 'browse-url-chromium
      browse-url-new-window-flag t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;; TAB completion with a nice UI

(defun jpk/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'jpk/dabbrev-friend-buffer)

(with-library 'auto-complete-config
  (ac-config-default))

(with-eval-after-load "auto-complete"

  (setq ac-auto-start 3
        ac-auto-show-menu t
        ac-use-quick-help nil
        ac-ignore-case nil)

  (add-to-list 'ac-dictionary-files "~/.ispell_american")
  (add-to-list 'ac-dictionary-files "~/.aspell.en.pws")

  (add-to-list 'ac-modes 'latex-mode)

  (add-to-list 'ac-sources 'ac-source-filename)
  (setq-default ac-sources ac-sources)

  ;; keybinds
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "M-k") 'ac-next)
  (define-key ac-completing-map (kbd "M-i") 'ac-previous)
  (define-key ac-completing-map (kbd "<backtab>") 'ac-expand-previous)
  (define-key ac-completing-map (kbd "S-TAB") 'ac-expand-previous)
  (define-key ac-completing-map (kbd "C-s") 'ac-isearch)
  (global-set-key (kbd "C-<tab>") 'auto-complete)

  ;; workaround for flyspell-mode
  (ac-flyspell-workaround)
  )

;; hippie-expand is like dabbrev-expand, but more
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flyspell

;; TODO: http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(setq ispell-program-name "aspell")

(with-eval-after-load "flyspell"
  (setq flyspell-use-meta-tab nil)
  (define-key flyspell-mode-map (kbd "M-TAB") nil)

  (with-library 'flyspell-correct-ido
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookmarks

(setq bookmark-save-flag nil)

;; bm.el is a visible bookmark package.  Bookmarks are indicated with
;; highlighting in the text and/or the fringe.  They are optionally
;; (per-buffer) persistent.

(setq bm-marker 'bm-marker-right
      bm-recenter t
      bm-highlight-style 'bm-highlight-only-fringe)

(global-set-key (kbd "C-c m m") 'bm-toggle)
(global-set-key (kbd "<right-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<right-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<right-fringe> <mouse-1>") 'bm-toggle-mouse)

(with-eval-after-load "bm"
  (defvar bm-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "m") 'bm-toggle)
      (define-key map (kbd "n") 'bm-next)
      (define-key map (kbd "p") 'bm-previous)
      (define-key map (kbd "L") 'bm-show-all)
      (define-key map (kbd "l") 'bm-show)
      (define-key map (kbd "s") 'bm-save)
      (define-key map (kbd "r") 'bm-load-and-restore)
      map)
    "Keymap for `bm.el'.")
  (global-set-key (kbd "C-c m") bm-mode-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; re-builder

(setq reb-re-syntax 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-lines

(global-set-key (kbd "C-c s a") 'hide-lines-show-all)
(global-set-key (kbd "C-c s s") 'hide-lines-not-matching)
(global-set-key (kbd "C-c s d") 'hide-lines-matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido - Interactively Do Things

(use-package ido
  :config
  (setq ido-confirm-unique-completion t
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t
        ido-ignore-buffers '("\\` " "^\\*Completion" "^\\*Ido")
        ido-max-work-file-list 50
        ido-rotate-file-list-default t
        ido-show-dot-for-dired t
        ido-work-directory-match-only nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers 'auto
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t)

  (dolist (e '("-pkg.el" "-autoloads.el"))
    (add-to-list 'completion-ignored-extensions e))

  (ido-mode 'both) ;; both buffers and files
  (ido-everywhere 1)

  (defun ido-initiate-auto-merge-this-buffer ()
    "Calls `ido-initiate-auto-merge' on the current buffer.

ido's automerge is not particularly well documented.  Basically,
if no matches are found, it starts looking in subdirectories for
matches.  This can be annoying if you didn't really want it, so
it's probably better to explicitly request a merge."
    (interactive)
    (ido-initiate-auto-merge (current-buffer)))

  (defun jpk/ido-minibuffer-setup-hook ()
    ;; disallow wrapping of the minibuffer
    (setq truncate-lines t))
  (add-hook 'ido-minibuffer-setup-hook #'jpk/ido-minibuffer-setup-hook)

  :bind (:map ido-completion-map
         ("C-c C-s" . ido-initiate-auto-merge-this-buffer)
         :map ido-file-dir-completion-map
         ([remap backward-kill-word] . nil)
         ("C-<backspace>" . ido-delete-backward-word-updir))
  )

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  ;; see also the amx package
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup

(setq make-backup-files t
      vc-make-backup-files t
      version-control t
      kept-new-versions 128
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autosave

(let ((dir (concat no-littering-var-directory "auto-save/")))
  (make-directory dir t)
  (setq auto-save-file-name-transforms `((".*" ,dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save-visited-files
;; Saves a list of the open files (via auto-save-hook), which can be
;; restored with save-visited-files-restore.

(setq save-visited-files-ignore-tramp-files t
      save-visited-files-ignore-directories nil
      save-visited-files-auto-restore t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recentf (recently visited files)

(setq recentf-max-saved-items 1024)

(defun jpk/recentf-keep-predicate (file)
  "Faster than `recentf-keep-default-predicate'."
  (cond
   ((file-remote-p file nil t) (file-readable-p file))
   ((file-remote-p file))
   ;; file-readable-p can be slow for network filesystems, hopefully
   ;; file-exists-p mitigates that
   ((file-exists-p file) (file-readable-p file))))

(setq recentf-keep (list #'jpk/recentf-keep-predicate))

(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; savehist

(setq history-delete-duplicates t)
(savehist-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; midnight

(with-library 'midnight
  (midnight-delay-set 'midnight-delay "04:00")
  (midnight-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP

(let ((backup-dir (concat no-littering-var-directory "tramp/backup/")))
  (setq tramp-backup-directory-alist nil
        tramp-auto-save-directory (concat no-littering-var-directory
                                          "tramp/auto-save/"))
  (dolist (d (list tramp-auto-save-directory backup-dir))
    (make-directory d t)))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq tramp-copy-size-limit nil) ; for Edison

;; normally this is bound to find-file-read-only
(global-set-key (kbd "C-x C-r") 'sudo-toggle)

;; Multihop: /ssh:gwuser@gateway|ssh:user@remote:/path/to/file
;; sudo on remote: /ssh:user@remote|sudo:remote:/path/to/file

;; In Windows, use the sshx method (it's a cygwinized version of ssh).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse

(setq mouse-1-click-follows-link nil
      mouse-1-click-in-non-selected-windows nil
      mouse-drag-copy-region t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))

;; move mouse pointer away when the cursor gets near
(mouse-avoidance-mode 'cat-and-mouse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows

(setq-default cursor-in-non-selected-windows nil)

(defun split-windows-in-quarters (&optional arg)
  "Configure a frame to have 4 similarly sized windows.  Splits
  the selected window with prefix arg."
  (interactive "P")
  (when (not arg)
    (delete-other-windows))
  (let ((this-window (selected-window)))
    (split-window-horizontally)
    (other-window 1)
    (split-window-vertically)
    (select-window this-window)
    (split-window-vertically))
  (dotimes (i 4)
    (unless (= i 0)
      (next-buffer))
    (other-window 1)))

(use-package ace-window
  :config
  (setf (alist-get ?o aw-dispatch-alist) '(aw-flip-window))
  (setq aw-scope 'frame)
  (setq aw-leading-char-style 'path)
  (global-set-key (kbd "C-x o") #'ace-window))

;; Save point position per-window instead of per-buffer.
(use-package winpoint
  :if nil
  :init
  (winpoint-mode 1))

(setq help-window-select 'never)

(use-package shackle
  :config
  (setq shackle-default-rule '(:inhibit-window-quit t)
        shackle-rules
        '((Man-mode :select t)
          (completion-list-mode :inhibit-window-quit nil :align 'below :size 0.3)
          (("*vc-incoming*" "*vc-outgoing*") :same t)
          ))
  (shackle-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC mode

(setq vc-handled-backends '(Git Hg SVN))

(setq vc-hg-log-switches '("-v"))

(with-eval-after-load "vc-hg"
  (require 'vc-hg-fixes)
  )

(with-eval-after-load "vc-dir"

  (defun vc-dir-toggle ()
    "Toggle the mark on a file in `vc-dir-mode'."
    (interactive)
    (let (line col)
      (setq line (line-number-at-pos))
      ;; This automatically moves to the next line, but that isn't
      ;; as useful for toggling (I think), so we work around it.
      (vc-dir-mark-unmark 'vc-dir-toggle-mark-file)
      (setq col (current-column))
      (goto-line line)
      (move-to-column col)))

  (define-key vc-dir-mode-map (kbd "SPC") 'vc-dir-toggle)

  )

;; toggle-read-only prints an annoying message
(advice-add 'toggle-read-only
            :around
            (lambda (orig &rest args)
              "Suppress VC message."
              (with-temp-message ""
                (apply orig args))))

(global-set-key (kbd "C-x g") #'magit-status)
(setq magit-diff-refine-hunk 'all)
(add-hook 'magit-diff-mode-hook #'jpk/diff-mode-hook)

(defalias 'git-grep #'vc-git-grep)

;; TODO
;; vc-revert bug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff

(setq diff-switches "-u"
      diff-default-read-only t)

(require 'ediff-tweak)

(advice-add 'ediff-setup-windows
            :around
            (lambda (orig &rest args)
              "Set `allow-windows-shrinking' non-nil for ediff."
              (let ((allow-window-shrinking t))
                (apply orig args))))

;; put the ediff control window in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Skip over whitespace-only differences in ediff mode.  Still finds
;; such regions, only changes navigation.  Toggle with # # in ediff
;; mode.
(setq-default ediff-ignore-similar-regions t)

;; show fine differences
(setq-default ediff-auto-refine 'on)

(autoload 'commit-patch-buffer "commit-patch-buffer.el"
  "Use diff-mode buffers as commits for VC." t)

(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "C-c C-k") 'diff-hunk-kill)
  (define-key diff-mode-map (kbd "C-c C-S-k") 'diff-file-kill)
  (define-key diff-mode-map (kbd "K") nil) ;; diff-file-kill
  (define-key diff-mode-map (kbd "M-K") nil) ;; diff-file-kill
  (define-key diff-mode-map (kbd "C-c C-c") 'commit-patch-buffer)
  (define-key diff-mode-map (kbd "C-c C-m") 'diff-add-trailing-CR-in-hunk)
  (define-key diff-mode-map (kbd "C-c C-j") 'diff-remove-trailing-CR-in-hunk)
  (define-key diff-mode-map (kbd "C-c C-o") 'diff-goto-source)
  )

(defun diff-delete-trailing-CR ()
  "Delete trailing carriage returns (^M) in a `diff-mode' buffer."
  (when (and (derived-mode-p 'diff-mode)
           (buffer-file-name)
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "\r$" nil t))
           (y-or-n-p "Strip trailing carriage returns? "))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\r+$" nil t)
          (replace-match "" nil nil)))
      (set-buffer-modified-p nil))))

(defun jpk/diff-mode-hook ()
  ;; FIXME why? special-mode-map suppress-keymap
  (local-set-key (kbd "M-1") nil)
  (local-set-key (kbd "M-2") nil)

  (setq adaptive-wrap-extra-indent 1)
  (visual-line-mode 1)

  (setq imenu-prev-index-position-function nil)
  (setq imenu-generic-expression '((nil "^--- .+/\\([^/]+\\)\t" 1)))

  ;;(remove-hook 'before-save-hook #'delete-trailing-whitespace)
  )

(add-hook 'diff-mode-hook #'jpk/diff-mode-hook)
(add-hook 'diff-mode-hook #'diff-make-unified)
(add-hook 'diff-mode-hook #'diff-delete-trailing-CR)

;;;;;;;;;;;;;;;;;;;;;;;;
;; commit-patch-buffer is very picky about the patch buffer.
;; Apparently, it needs the line endings to match the original file,
;; but the diff metadata needs unix line endings.
;;
;; TODO
;;
;; make diff-add-trailing-CR-in-hunk more fool-proof and use it to
;; operate on the whole diff buffer

(defun insert-CR-eol (b e)
  (interactive "*r")
  (save-restriction
    (narrow-to-region b e)
    (while (re-search-forward "\\([^]\\)$" nil t)
      (replace-match "\\1" nil nil))))

(defun remove-CR-eol (b e)
  (interactive "*r")
  (save-restriction
    (narrow-to-region b e)
    (while (re-search-forward "$" nil t)
      (replace-match "" nil nil))))

(defun diff-add-or-remove-trailing-CR-in-hunk (add-not-remove)
  "Add or remove trailing carriage returns in the current hunk.

If ADD-NOT-REMOVE is non-nil, add CRs, otherwise remove any CRs (leaving only LFs)."
  (save-excursion
    (diff-beginning-of-hunk)
    (when (diff-unified-hunk-p)
      (forward-line 1))
    (let* ((start (point))
           ;; Search the second match, since we're looking at the first.
           (nexthunk (when (re-search-forward diff-hunk-header-re nil t 2)
		     (match-beginning 0)))
           (firsthunk (ignore-errors
                        (goto-char start)
                        (diff-beginning-of-file) (diff-hunk-next) (point)))
           (nextfile (ignore-errors (diff-file-next) (point)))
           (inhibit-read-only t))
      (goto-char start)
      (diff-end-of-hunk)
      (when (eobp)
        (forward-line -1)
        (end-of-line))
      (if add-not-remove
          (insert-CR-eol start (point))
        (remove-CR-eol start (point))))))

(defun diff-add-trailing-CR-in-hunk ()
  "Add carriage returns to the line endings of the current diff hunk (EOL = CRLF)."
  (interactive "*")
  (diff-add-or-remove-trailing-CR-in-hunk t))

(defun diff-remove-trailing-CR-in-hunk ()
  "Remove carriage returns from the line endings of the current diff hunk (EOL = LF)."
  (interactive "*")
  (diff-add-or-remove-trailing-CR-in-hunk nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smerge-mode

(setq smerge-command-prefix (read-kbd-macro "C-c c"))

(defun smerge-help ()
  "Describe keybindings for `smerge-mode'."
  (interactive)
  (with-temp-buffer
    (smerge-mode 1) ;; ensure smerge-mode-map is active
    (describe-bindings smerge-command-prefix)))

(defun smerge-quick-keys ()
  "Set `smerge-basic-map' as the transient keymap.
This effectively makes `smerge-command-prefix' unnecessary."
  (interactive)
  (set-transient-map smerge-basic-map t))

(add-hook 'smerge-mode-hook 'smerge-quick-keys)

(with-eval-after-load "smerge-mode"
  (define-key smerge-basic-map (kbd "h") 'smerge-help))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-hl

(with-library 'diff-hl
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dir-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint - command interpreter

(setq-default comint-input-ignoredups t
              comint-prompt-read-only t
              comint-scroll-to-bottom-on-input 'all)

(defvar comint-eob-on-send t
  "Like comint-eol-on-send, but moves to the end of buffer.")
(advice-add 'comint-send-input
            :before
            (lambda (&rest args)
              "Move to end of buffer when `comint-eob-on-send' is non-nil."
              (when comint-eob-on-send
                (goto-char (point-max)))))

;; used for interactive terminals
(with-eval-after-load "comint"
  (define-key comint-mode-map
    (kbd "<up>") 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map
    (kbd "<down>") 'comint-next-matching-input-from-input)
  )

(autoload 'ansi-color-for-comint-mode-on "ansi-color")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminals

;; t is usually better for shell terminals, but nil is better for
;; serial terminals
(setq term-suppress-hard-newline nil)

(with-eval-after-load "term"
  (defun term-send-backward-kill-word ()
    "Backward kill word in `term-mode'."
    (interactive)
    (term-send-raw-string "\C-w"))

  (defun term-send-forward-kill-word ()
    "Kill word in `term-mode'."
    (interactive)
    (term-send-raw-string "\ed"))

  (defun term-send-backward-word ()
    "Move backward word in `term-mode'."
    (interactive)
    (term-send-raw-string "\eb"))

  (defun term-send-forward-word ()
    "Move forward word in `term-mode'."
    (interactive)
    (term-send-raw-string "\ef"))

  (defun term-send-M-x ()
    "Send `M-x' in `term-mode'."
    (interactive)
    (term-send-raw-string "\ex"))

  (defun term-interrupt-subjob-or-C-c ()
    "Run `term-interrupt-subjob' or send `C-c'."
    (interactive)
    (if (process-id (get-process (buffer-name)))
        (term-interrupt-subjob)
      ;; serial processes don't have a process-id
      (term-send-raw-string "\C-c")))

  (defun term-stop-subjob-or-C-z ()
    "Run `term-stop-subjob' or send `C-z'."
    (interactive)
    (if (process-id (get-process (buffer-name)))
        (term-stop-subjob)
      ;; serial processes don't have a process-id
      (term-send-raw-string "\C-z")))

  (defun term-toggle-char-or-line-mode ()
    "Toggle between `term-line-mode' and `term-char-mode'.

    `term-char-mode' (default) is more termy, `term-line-mode' is more Emacsy."
    (interactive)
    (cond
     ((term-in-char-mode)
      (term-line-mode))
     ((term-in-line-mode)
      (term-char-mode))
     (t
      (error "Term in neither line nor char mode."))))

  (dolist
      (bind '(;; from multi-term
              ("C-x" . nil)
              ("C-h" . nil)
              ("<ESC>" . nil)
              ("C-c C-e" . term-send-esc)
              ("C-p" . previous-line)
              ("C-n" . next-line)
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)

              ;; personal
              ("C-v" . nil)
              ("C-<right>" . term-send-forward-word)
              ("C-<left>" . term-send-backward-word)
              ("C-<backspace>" . term-send-backward-kill-word)
              ("C-<delete>" . term-send-forward-kill-word)
              ("C-k" . term-send-raw)
              ("C-y" . term-send-raw)
              ("C-c C-c" . term-interrupt-subjob-or-C-c)
              ("C-c C-z" . term-stop-subjob-or-C-z)
              ("C-c C-y" . term-paste)
              ("C-c C-u" . universal-argument)
              ("<S-down>" . sane-term-create)
              ("<S-left>" . sane-term-prev)
              ("<S-right>" . sane-term-next)
              ("C-c C-j" . term-toggle-char-or-line-mode)
              ("C-c C-v" . term-send-raw) ;; quote
              ("C-c C-x" . term-send-raw) ;; C-x
              ))
    (define-key term-raw-map
      (read-kbd-macro (car bind)) (cdr bind)))

  (define-key term-mode-map (kbd "C-c C-j") #'term-toggle-char-or-line-mode)
  )

(defun term-revert-buffer (ignore-auto noconfirm)
  "Clear the buffer.  Suitable for `revert-buffer-function'."
  (when (or (not noconfirm)
           (and noconfirm (y-or-n-p "Clear buffer? ")))
    (term-reset-terminal)
    (term-send-raw-string "\C-l")))

(defun jpk/term-mode-hook ()
  (setq cua--ena-cua-keys-keymap nil)
  (when (featurep 'yasnippet)
    (yas-minor-mode 0))
  (setq-local revert-buffer-function #'term-revert-buffer)
  )
(add-hook 'term-mode-hook 'jpk/term-mode-hook)

(global-set-key (kbd "C-c t") 'sane-term)

(defun shell-command-from-region (&optional arg output-buffer error-buffer)
  "Run the current line as a shell command.  If the region is
  active, use that for the command instead.  With a prefix
  argument, insert the command output into the current buffer,
  just below the region.  Otherwise, send the output to
  OUTPUT-BUFFER, just like `shell-command'.  See also
  `shell-command-on-region' (\\[shell-command-on-region])."

  (interactive "P")
  (let ((beg (if (region-active-p)
                 (region-beginning)
               (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (if arg
        (save-excursion
          (goto-char end)
          (end-of-line)
          (insert "\n")
          (insert (shell-command-to-string
                   (buffer-substring beg end))))
      (shell-command (buffer-substring beg end) output-buffer error-buffer))))

(global-set-key (kbd "C-<kp-enter>") 'shell-command-from-region)
(global-set-key (kbd "C-!") 'shell-command-from-region)

(defun sudo-shell-command (cmd &optional output-buffer error-buffer)
  "Works just like `shell-command', but runs the command with
  sudo.  If you have sudo configured to remember authentication
  for a period of time, you can just hit enter when it asks for a
  password if your previous authentication hasn't expired."
  (interactive "sShell command: ")
  (shell-command (concat "echo '" (read-passwd
                                   (concat "[sudo] password for "
                                           user-real-login-name ": "))
                                   "\n'"
                         " | sudo -S " cmd)
                 current-prefix-arg))

(defun shell-command-on-region-or-buffer (start end command &optional output-buffer replace error-buffer display-error-buffer)
  "Like `shell-command-on-region', but uses the whole buffer if the region is inactive."
  (interactive (let (beg en string)
                 (if (use-region-p)
                     (setq beg (region-beginning)
                           en (region-end))
                   (setq beg (point-min)
                         en (point-max)))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list beg en
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t)))
  (shell-command-on-region start end command
                           output-buffer replace error-buffer display-error-buffer))

(global-set-key (kbd "M-|") 'shell-command-on-region-or-buffer)

(defun copy-eterm-color-terminfo (hostspec)
  "Copy the eterm-color terminfo file to a remote host.
HOSTSPEC is a tramp host specification, e.g. \"/ssh:HOSTSPEC:/remote/path\"."
  (interactive
   (let ((hosts (mapcar (lambda (x) (cond ((stringp x) x)
                                     ((null (car x)) (cadr x))
                                     (t (concat (car x) "@" (cadr x)))))
                        (apply 'append
                               (mapcar
                                (lambda (x) (remove-if-not 'identity (apply (car x) (cdr x))))
                                (tramp-get-completion-function "ssh"))))))
     (list (completing-read "Hostname: " hosts nil 'confirm nil nil hosts nil))))
  (let ((destdir (format "/ssh:%s:.terminfo/e/" hostspec)))
    (ignore-errors
      (dired-create-directory destdir))
    (copy-file (concat data-directory "e/eterm-color")
               (concat destdir "eterm-color"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; man pages

(setenv "MANWIDTH" "72")
(setq woman-use-own-frame nil
      woman-fill-column 72
      woman-cache-filename nil)
(define-key help-map (kbd "C-m") #'man)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file functions

(defun rename-file-and-buffer (new-name)
  "Rename the current buffer and file it is visiting to NEW-NAME."
  (interactive
   (list
    (let ((current-name (buffer-file-name (current-buffer))))
      (if current-name
          (setq current-name (file-name-nondirectory current-name))
        (error "Buffer is not visiting a file!"))
      (read-string "New name: " current-name nil current-name))))
  (let ((cur-name (buffer-file-name)))
    (cond
     ((and nil ;; causing too many problems
         (vc-backend cur-name)
         (y-or-n-p "Use vc-rename-file? "))
      (vc-rename-file cur-name new-name))
     (t
      (rename-file cur-name new-name t)
      (rename-buffer new-name)
      (set-visited-file-name new-name t t)
      (set-buffer-modified-p nil)))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match-p dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun kill-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defalias 'delete-this-buffer-and-file 'kill-this-buffer-and-file
  "I always think that this should be called 'delete'.  It's not
  getting saved to the kill-ring...")

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; set-fill-column is dumb
(global-set-key (kbd "C-x f") 'find-file-at-point)

;; this will e.g. find a .h find if you're looking at the .c file
(global-set-key (kbd "C-x F") 'ff-find-related-file)

(defun get-file-name-in-path (dirs suffixes)
  "Use completing-read to select a file DIRS is a list of strings
  that are directory names (like `load-path').  SUFFIXES is a
  list of strings that the files must end with, e.g. '(\".h\"
  \".hpp\")."
  (let ((def (thing-at-point 'symbol)))
    (when def
      (setq def (and (locate-file-completion-table
                    dirs suffixes def nil 'lambda)
                   def)))
    (completing-read (if def (format "File name (default %s): " def)
                       "File name: ")
                     (apply-partially 'locate-file-completion-table
                                      dirs suffixes)
                     (when suffixes
                       (lambda (x) (string-match-p (concat (regexp-opt suffixes t) "$") x)))
                     nil nil nil def)))

(defun find-file-in-path (dirs suffixes)
  "Find a file with a certain extension in a list of directories.
  See `get-file-name-in-path' for more info."
  (let* ((library (get-file-name-in-path dirs suffixes))
         (buf (find-file-noselect (locate-file library dirs))))
    (condition-case nil (switch-to-buffer buf) (error (pop-to-buffer buf)))))

(defun create-directory-if-necessary ()
  "Create the directory tree above `buffer-file-name', if it
  doesn't completely exist yet.  For use in `find-file-hook'."
  (interactive)
  (let ((dirname (file-name-directory (buffer-file-name))))
    (when (and dirname
             (not (file-directory-p dirname))
             (y-or-n-p (format "Directory %s does not exist.  Create it? " dirname)))
      (make-directory dirname t))))

(add-hook 'find-file-not-found-functions #'create-directory-if-necessary)

(defun jpk/save-buffer-maybe (&optional args)
  "Like `save-buffer' but just prints a message if the buffer has no file."
  (interactive "p")
  (if (buffer-file-name)
      (save-buffer args)
    (error "Buffer is not associated with a file.  Use `write-file' instead.")))
(global-set-key [remap save-buffer] 'jpk/save-buffer-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-filenames-regexp)

  (with-library 'dired+
    (setq diredp-wrap-around-flag nil)
    (toggle-diredp-find-file-reuse-dir 1)
    (define-key dired-mode-map (kbd "<mouse-2>")
      'diredp-mouse-find-file-reuse-dir-buffer)
    (define-key dired-mode-map (kbd "f") 'diredp-up-directory-reuse-dir-buffer)
    )

  (require 'dired-imenu nil 'noerror)

  (with-library 'dired-narrow
    (define-key dired-mode-map (kbd "/") 'dired-narrow)
    (define-key dired-mode-map (kbd "C-x n n") 'dired-narrow)
    (define-key dired-mode-map (kbd "C-x n r") 'dired-narrow-regexp)
    (define-key dired-mode-map (kbd "C-x n f") 'dired-narrow-fuzzy)
    (define-key dired-mode-map (kbd "C-x n w") 'revert-buffer))

  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^.].*$")
  (with-library 'dired-x
    (setq dired-omit-verbose nil)
    (delete ".bin" dired-omit-extensions)
    (define-key dired-mode-map (kbd "C-c C-o") 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-omit-mode))

  (define-key dired-mode-map (kbd "S-<return>")
    'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "S-<down-mouse-2>")
    'dired-mouse-find-file-other-window)

  (setq dired-deletion-confirmer 'y-or-n-p
        dired-dwim-target t)

  (setq wdired-allow-to-change-permissions t)

  (advice-add 'dired-rename-file
              :before
              (lambda (&rest args)
                "Create parent dirs"
                (let* ((new-name (nth 1 args))
                       (dir (file-name-directory new-name)))
                  (unless (file-directory-p dir)
                    (message "Creating dir for file %s" new-name)
                    (make-directory dir 'parents)))))

  (with-library 'dired-ranger
    (define-key dired-mode-map (kbd "C-c C-c") 'dired-ranger-copy)
    (define-key dired-mode-map (kbd "C-c C-x") 'dired-ranger-move)
    (define-key dired-mode-map (kbd "C-c C-v") 'dired-ranger-paste))
  )

(with-eval-after-load 'view
  (defun dired-view-next (&optional reversed)
    "Move to next dired line and view."
    (interactive "P")
    (quit-window)
    (catch 'break
      (while (if reversed
                 (dired-previous-line 1)
               (dired-next-line 1))
        (let ((file (dired-get-file-for-visit)))
          (unless (file-directory-p file)
            (view-file file))))))

  (defun dired-view-prev (&optional reversed)
    "Move to next dired line and view."
    (interactive "P")
    (dired-view-next (not reversed)))

  (define-key view-mode-map (kbd "n") 'dired-view-next)
  (define-key view-mode-map (kbd "p") 'dired-view-prev)
  )

;; word wrap looks terrible in dired buffers
(defun jpk/dired-before-readin-hook ()
  (visual-line-mode 0)
  (setq truncate-lines t)
  )
(add-hook 'dired-before-readin-hook 'jpk/dired-before-readin-hook)

(with-library 'openwith
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4" "asf"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg"
                  "svg"))
               "geeqie"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "docx" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         '("\\.lyx" "lyx" (file))
         '("\\.chm" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "okular"
               '(file))
         ))
  ;;(openwith-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; image-mode

(with-eval-after-load "image-mode"
  (define-key image-mode-map (kbd "<next>") 'image-scroll-up)
  (define-key image-mode-map (kbd "<prior>") 'image-scroll-down)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSV mode

(with-eval-after-load "csv-mode"

  ;; It seems silly to set csv separators for all buffers the same, so
  ;; this makes everything buffer local and easily settable from the
  ;; minibuffer.

  (make-variable-buffer-local 'csv-separators)
  (make-variable-buffer-local 'csv-separator-chars)
  (make-variable-buffer-local 'csv-separator-regexp)
  (make-variable-buffer-local 'csv-font-lock-keywords)
  (make-variable-buffer-local 'csv-skip-regexp)
  (make-variable-buffer-local 'csv-field-quotes)

  (defun csv-get-new-separators ()
    "Helper function for `csv-set-separators'."
    (interactive)
    (let (finished new-char new-list)
      (while (not finished)
        (setq new-char
              (read-char
               (format "Separators = %S.  Next delimiter or enter to finish: "
                       new-list)))
        (if (not (char-equal new-char 13))
            (add-to-list 'new-list (char-to-string new-char))
          (setq finished t)))
      (if new-list
          new-list
        (message "Empty separator list, reusing old: %S." csv-separators)
        csv-separators)))

  (defun csv-set-separators (separators)
    (interactive (list (csv-get-new-separators)))
    (mapc (lambda (x)
            (if (or (/= (length x) 1)
                   (and (boundp 'csv-field-quotes)
                      (member x csv-field-quotes)))
                (error)))
          separators)
    (setq csv-separators separators)
    (setq csv-separator-chars (mapcar 'string-to-char separators)
          csv-skip-regexp (apply 'concat "^\n" csv-separators)
          csv-separator-regexp (apply 'concat `("[" ,@separators "]"))
          csv-font-lock-keywords
          ;; NB: csv-separator-face variable evaluates to itself.
          `((,csv-separator-regexp . csv-separator-face))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IBuffer

(setq ibuffer-default-sorting-mode 'major-mode
      ibuffer-formats '((mark modified read-only " "
                              (name 28 28 :left :elide) " "
                              (size 9 -1 :right) " "
                              (mode 16 16 :left :elide) " "
                              filename-and-process)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-movement-cycle nil
      ibuffer-saved-filter-groups nil
      ibuffer-saved-filters nil
      ibuffer-show-empty-filter-groups nil)

(with-eval-after-load "ibuffer"
  (require 'ibuf-ext)
  (require 'ibuf-macs)

  (define-key ibuffer-mode-map
    (kbd "/ M") 'ibuffer-set-filter-groups-by-mode)
  (define-key ibuffer-mode-map
    (kbd "/ m") 'ibuffer-filter-by-used-mode)

  (with-library 'ibuffer-directory
    (define-key ibuffer-mode-map
      (kbd "s p") 'ibuffer-do-sort-by-directory)
    (define-key ibuffer-mode-map
      (kbd "/ D") 'ibuffer-set-filter-groups-by-directory)
    (define-key ibuffer-mode-map
      (kbd "/ d") 'ibuffer-filter-by-directory))

  (with-library 'ibuffer-projectile
    ;; from ibuffer-projectile
    (define-key ibuffer-mode-map
      (kbd "/ P") 'ibuffer-projectile-set-filter-groups)
    ;; from projectile
    (define-key ibuffer-mode-map
      (kbd "/ p") 'ibuffer-filter-by-projectile-files))

  (with-library 'ibuffer-unsaved
    (define-key ibuffer-mode-map
      (kbd "/ 8") 'ibuffer-filter-by-unsaved)
    (define-key ibuffer-mode-map
      (kbd "/ *") 'ibuffer-set-filter-groups-by-unsaved))

  (with-library 'ibuffer-tramp
    (define-key ibuffer-mode-map
      (kbd "/ T") 'ibuffer-tramp-set-filter-groups-by-tramp-connection))

  ;; TODO make cycling work with count
  (defun ibuffer-forward-filter-group (&optional count)
    "Move point forwards by COUNT filtering groups."
    (interactive "P")
    (unless count
      (setq count 1))
    (when (> count 0)
      (when (get-text-property (point) 'ibuffer-filter-group-name)
        (goto-char (next-single-property-change
                    (point) 'ibuffer-filter-group-name
                    nil (point-max))))
      (goto-char (next-single-property-change
                  (point) 'ibuffer-filter-group-name
                  nil (point-max)))
      (ibuffer-forward-filter-group (1- count)))
    (when (eobp)
      (if (not ibuffer-movement-cycle)
          (ibuffer-backward-filter-group)
        (goto-char (point-min))
        (ibuffer-forward-filter-group (1- count))))
    (ibuffer-forward-line 0))

  ;; TODO make cycling work with count
  (defun ibuffer-backward-filter-group (&optional count)
    "Move point backwards by COUNT filtering groups."
    (interactive "P")
    (unless count
      (setq count 1))
    (when (> count 0)
      (when (get-text-property (point) 'ibuffer-filter-group-name)
        (goto-char (previous-single-property-change
                    (point) 'ibuffer-filter-group-name
                    nil (point-min))))
      (goto-char (previous-single-property-change
                  (point) 'ibuffer-filter-group-name
                  nil (point-min)))
      (ibuffer-backward-filter-group (1- count)))
    (when (and ibuffer-movement-cycle (bobp))
      (goto-char (point-max))
      (ibuffer-backward-filter-group 1))
    (ibuffer-forward-line 0))

  (define-key ibuffer-mode-map
    (kbd "<down>") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map
    (kbd "<up>") 'ibuffer-backward-line)
  (define-key ibuffer-mode-map
    (kbd "C-<down>") 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map
    (kbd "C-<up>") 'ibuffer-backward-filter-group)

  (dolist (func '(ibuffer-forward-filter-group
                  ibuffer-backward-filter-group
                  ibuffer-mark-interactive
                  ibuffer-toggle-filter-group-1))
    (advice-add func
                :after
                (lambda (&rest args)
                  "Recenter"
                  (recenter))))

  (defun ibuffer-mark-toggle (arg)
    (interactive "P")
    (ibuffer-forward-line 0)
    (ibuffer-aif (get-text-property (point) 'ibuffer-filter-group-name)
                 ;; on a group name
                 (ibuffer-toggle-marks) ;; buggy, but not my fault
                 ;; on a buffer name
                 (if (eq (ibuffer-current-mark) ibuffer-marked-char)
                     (ibuffer-mark-interactive arg ?\s 0)
                   (ibuffer-mark-interactive arg ibuffer-marked-char 0))))
  (define-key ibuffer-mode-map
    (kbd "SPC") 'ibuffer-mark-toggle)
  )

;; run when ibuffer buffer is created
(defun jpk/ibuffer-mode-hook ()
  (ibuffer-auto-mode 1)
  (setq truncate-lines t))
(add-hook 'ibuffer-mode-hook 'jpk/ibuffer-mode-hook)

;; run when ibuffer command is invoked
(defun jpk/ibuffer-hook ()
  (ibuffer-set-filter-groups-by-mode)
  (setq ibuffer-sorting-mode 'pathname))
(add-hook 'ibuffer-hook 'jpk/ibuffer-hook)

;; jump to most recent buffer
(advice-add 'ibuffer
            :around
            (lambda (orig &rest args)
              "Move point to most recent."
              (let ((recent-buffer-name (buffer-name)))
                (apply orig args)
                (unless (string-match-p "*Ibuffer*" recent-buffer-name)
                  (ibuffer-jump-to-buffer recent-buffer-name)))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immortal-scratch

(with-library 'immortal-scratch
  (setq immortal-scratch-switch-to-respawned-scratch t)
  (immortal-scratch-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; figlet

(with-eval-after-load "figlet.el"
  (add-to-list 'figlet-options "-k") ;; kerning
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify-symbols

;; prettify-symbols-mode is enabled in major mode hooks.
;; Major modes can append more symbols before enabling prettify-symbols-mode.
(setq-default
 prettify-symbols-alist
 '(("" . ?§)
   ("<=" . ?≤)
   (">=" . ?≥)
   ("alpha" . ?α)
   ("beta" . ?β)
   ("gamma" . ?γ)
   ("delta" . ?Δ)
   ("epsilon" . ?ε)
   ("theta" . ?θ)
   ("lambda" . ?λ)
   ("mu" . ?μ)
   ("pi" . ?π)
   ("phi" . ?φ)
   ("sigma" . ?σ)
   ("sqrt" . ?√)
   ))

(global-prettify-symbols-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile

(put 'compile-command 'safe-local-variable 'stringp)

(setq compilation-scroll-output 'first-error)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match-p "compilation" (buffer-name buffer))
         (string-match-p "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq compilation-read-command nil) ;; only prompt when compile is run with prefix

(global-set-key (kbd "C-c b") 'compile)
(global-set-key (kbd "C-x ~") 'previous-error)

(defun doxygen-compile ()
 (interactive)
 (compile "doxygen 2>&1 1>/dev/null | sed s%`pwd`/%%"))

(defun locate-repo-dir (&optional file-or-dir)
  "Find the root of the version control repository."
  (let* ((file-or-dir (or file-or-dir (buffer-file-name) default-directory))
         (file-dir (if (file-directory-p file-or-dir)
                       file-or-dir
                     (file-name-directory file-or-dir)))
         (root-dir (vc-call-backend (vc-deduce-backend) 'root file-dir)))
    root-dir))

(with-eval-after-load "multi-compile"
  (dolist (e '(("%cflags" . (or (getenv "CFLAGS") "-Wall -g3 -std=c11"))
               ("%cxxflags" . (or (getenv "CXXFLAGS") "-Wall -g3"))
               ("%repo-dir" . (locate-repo-dir))))
    (add-to-list 'multi-compile-template e))

  (setq multi-compile-alist
        '((".*" . (("make-simple" .
                    "make -k")
                   ("make-repo" .
                    "make -k --no-print-directory -C '%repo-dir'")
                   ("make-top" .
                    "make -k --no-print-directory -C '%make-dir'")))
          (c-mode . (("c-simple" .
                      "gcc -o '%file-sans' %cflags '%file-name'")
                     ("c-simple32" .
                      "gcc -o '%file-sans' %cflags -m32 '%file-name'")))
          (c++-mode . (("c++-simple" .
                        "g++ -o '%file-sans' %cxxflags '%file-name'")))))
  )
(global-set-key (kbd "C-c b") #'multi-compile-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-todo

(with-library 'hl-todo
  (setq hl-todo-keyword-faces
        (mapcar (lambda (w) (cons w "red"))
                '("TODO" "FIXME" "KLUDGE" "XXX" "DEBUG")))
  (add-hook 'prog-mode-hook 'hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Programming

(setq jit-lock-stealth-time 5)

(setq-default comment-column 0
              comment-style 'extra-line)

(global-set-key (kbd "M-;") 'comment-line)

(defun previous-defun (&optional arg)
  "Like `beginning-of-defun', and also recenters."
  (interactive "^p")
  (beginning-of-defun arg)
  (recenter))

(defun next-defun (&optional arg)
  "Like `beginning-of-defun' with a negative arg, and also recenters."
  (interactive "^p")
  (beginning-of-defun (- arg))
  (recenter))

(defun insert-comment-bar (count)
  "Insert COUNT comment characters.  COUNT defaults to 72."
  (interactive "*p")
  (when (<= count 1)
    (setq count 72))
  (beginning-of-line)
  (when (looking-at "^[[:space:]]*$")
    (end-of-line))
  (let ((comment-start (if (stringp comment-start)
                           comment-start
                         "# ")))
    (insert (make-string count (string-to-char comment-start))))
  (insert "\n"))

(defun isearch-forward-symbol-dwim ()
  "Run `isearch-forward-symbol-at-point' or `isearch-repeat-forward'."
  (interactive)
  (if isearch-mode
      (isearch-repeat-forward)
    (isearch-forward-symbol-at-point)))

(defun isearch-backward-symbol-dwim ()
  "Run `isearch-forward-symbol-at-point' or `isearch-repeat-backward'."
  (interactive)
  (if isearch-mode
      (isearch-repeat-backward)
    (isearch-forward-symbol-at-point)))

(global-set-key (kbd "M-n") #'isearch-forward-symbol-dwim)
(global-set-key (kbd "M-p") #'isearch-backward-symbol-dwim)
(define-key isearch-mode-map (kbd "M-n") #'isearch-forward-symbol-dwim)
(define-key isearch-mode-map (kbd "M-p") #'isearch-backward-symbol-dwim)

(global-set-key (kbd "C-c i") #'iedit-mode)

;; visual-regexp
(global-set-key [remap replace-regexp] #'vr/replace)
(global-set-key [remap query-replace-regexp] #'vr/query-replace)

(defun jpk/prog-mode-hook ()
  ;;(smart-tabs-mode 0) ;; default to using spaces

  ;; for doxygen
  (with-library 'doxymacs
    (setq doxymacs-command-character "\\")
    (doxymacs-mode 1)
    (doxymacs-font-lock))

  ;; check spelling on the fly, but only in comments and strings
  (with-library 'flyspell
    (flyspell-mode 0)
    (flyspell-prog-mode))

  ;; e.g. insert '(' and ')' around the region if it is active and '('
  ;; is typed; works for other delimiters too
  (with-library 'wrap-region
    (wrap-region-mode 1))

  ;; highlight numeric literals
  (with-library 'highlight-numbers
    (highlight-numbers-mode 1))

  ;; highlight brackets
  (with-library 'paren-face
    (setq paren-face-regexp (regexp-opt '("[" "]" "(" ")" "{" "}")))
    (set-face-attribute 'parenthesis nil :foreground "cyan3")
    (paren-face-mode 1))

  ;; highlight operators like '+' and '&'
  (with-library 'highlight-operators
    (highlight-operators-mode 1))

  (setq adaptive-wrap-extra-indent 1)
  (visual-line-mode 1)

  ;;(add-hook 'after-save-hook 'imenu-force-rescan 'append 'local)

  (local-set-key (kbd "C-M-;") 'insert-comment-bar)
  (local-set-key (kbd "C-m") 'newline-and-indent)
  (local-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)
  (local-set-key (kbd "C-e") 'mwim-end-of-line-or-code)
  (local-set-key (kbd "C-M-a") 'previous-defun)
  (local-set-key (kbd "C-M-e") 'next-defun)

  (dolist (x '(ac-source-gtags ac-source-imenu ac-source-yasnippet))
    (add-to-list 'ac-sources x))
  )

(add-hook 'prog-mode-hook 'jpk/prog-mode-hook)

(defun imenu-force-rescan ()
  "Doesn't rescan, but forces a rescan the next time imenu is invoked."
  (interactive)
  (save-excursion
    (imenu--cleanup)
    (setq imenu--index-alist nil)))

(with-eval-after-load "doxymacs"
  (load "doxymacs-hacks.el")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C programming language

(defun cpp-highlight-if-0/1 ()
  "Modify the face of things in between #if 0 ... #endif."
  (interactive)
  (setq cpp-known-face '(background-color . "dim gray"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
        '((#("1" 0 1
             (fontified nil))
           nil
           (background-color . "dim gray")
           both nil)
          (#("0" 0 1
             (fontified nil))
           (background-color . "dim gray")
           nil
           both nil)))
  (cpp-highlight-buffer t))

(setq c-types-regexp
      (concat
       "\\<[_a-zA-Z][_a-zA-Z0-9]*_t\\>" "\\|"
       (regexp-opt '("unsigned" "int" "char" "float" "void") 'words)))

(with-eval-after-load "cc-mode"
  (font-lock-add-keywords
   'c-mode
   (list
    (cons c-types-regexp 'font-lock-type-face)))
  (font-lock-add-keywords
   'c++-mode
   (list
    (cons c-types-regexp 'font-lock-type-face)))

  ;; TODO find a better way to do this
  (with-library 'ffap
    (add-to-list 'ffap-c-path "/usr/lib/avr/include/"))
  )

(defun jpk/c-mode-common-hook ()
  (smart-tabs-mode 1)
  (setq tab-width 4)
  (setq c-basic-offset 4
        c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "bsd")))
  (imenu-add-to-menubar "IMenu")
  (setq comment-start "// "
        comment-end "")
  (dolist (x '(("!=" . ?≠)
               ("NULL" . ?∅)
               ("&&" . ?⋀)
               ("||" . ?⋁)
               ("!" . ?¬)
               ("HUGE_VAL" . ?∞)
               ("->" . ?→)
               ("M_PI" . ?π)
               ))
    (add-to-list 'prettify-symbols-alist x))
  (electric-indent-local-mode 1)
  (with-library 'ac-c-headers
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-header-symbols 'append))
  ;;(cpp-highlight-if-0/1)
  ;;(add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  )

(add-hook 'c-mode-common-hook 'jpk/c-mode-common-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUD (Grand Unified Debugger)

(with-eval-after-load 'gud
  (define-key gud-minor-mode-map (kbd "C-c C-n") 'gud-next)
  (define-key gud-minor-mode-map (kbd "C-c C-s") 'gud-step)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bitbake

;; also: .conf and .inc
(add-to-list 'auto-mode-alist
             '("\\.bb\\(append\\|class\\)?\\'" . bitbake-mode))
(add-to-list 'auto-mode-alist
             '("\\.inc\\'" . bitbake-mode))

(defun jpk/bitbake-mode-hook ()
  (setq comment-start "#"
        comment-stop "") ;; FIXME upstream
  (setq indent-line-function #'indent-relative-dwim)
  (local-set-key (kbd "C-M-;") #'insert-comment-bar)
  (local-set-key (kbd "C-a") #'mwim-beginning-of-line-or-code)
  (local-set-key (kbd "C-e") #'mwim-end-of-line-or-code)

  (with-library 'hl-todo
    (hl-todo-mode 1))

  ;; highlight numeric literals
  (with-library 'highlight-numbers
    (highlight-numbers-mode 1))

  ;; FIXME DRY
  ;; highlight brackets
  (with-library 'paren-face
    (setq paren-face-regexp (regexp-opt '("[" "]" "(" ")" "{" "}")))
    (set-face-attribute 'parenthesis nil :foreground "cyan3")
    (paren-face-mode 1))

  ;; highlight operators like '+' and '&'
  (with-library 'highlight-operators
    (highlight-operators-mode 1))

  (setq adaptive-wrap-extra-indent 1)
)
(add-hook 'bitbake-mode-hook #'jpk/bitbake-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-script

(defun jpk/sh-mode-hook ()
  (dolist (s '(paragraph-start paragraph-separate))
    (set s (default-value s)))
  )

(add-hook 'sh-mode-hook 'jpk/sh-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl

(defalias 'perl-mode 'cperl-mode)

(setq cperl-invalid-face nil)

(defun jpk/cperl-mode-hook ()
  (dolist (x '(("!=" . ?≠)
               ("->" . ?→)
               ("=>" . ?⇒)
               ))
    (add-to-list 'prettify-symbols-alist x))
  (cperl-set-style "BSD")
  )

(add-hook 'cperl-mode-hook 'jpk/cperl-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

(defun jpk/python-eval-insert-region (beg end)
  "Evaluate the region with Python and insert the result.
If region is inactive, use the entire current line."
  (interactive "*r")
  (let ((str (if (region-active-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))))
    (save-excursion
      (forward-line)
      (beginning-of-line)
      (insert (shell-command-to-string
               (concat "python -c " (shell-quote-argument
                                     (concat "print(repr(" str "))"))))))))

(global-set-key (kbd "C-c e p") 'jpk/python-eval-insert-region)

(setq python-indent-offset 4)

(with-eval-after-load "python"

  (with-library 'pylint
    ;; N.B. you probably want to set shell-file-name to something like
    ;; "C:/cygwin/bin/bash.exe" on Windows, because the default shell
    ;; will fuck up the command line.
    (setq pylint-options '("--rcfile=./.pylintrc"
                           "--jobs=4"
                           "--reports=n"
                           "--msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}]\n  {msg}'"
                           "--disable=C,R,locally-disabled"
                           ))

    (define-key python-mode-map (kbd "C-c C-v") 'pylint)
    (define-key python-mode-map (kbd "C-c C-i") 'pylint-insert-ignore-comment)
    )

  (define-key python-mode-map (kbd "<backtab>") 'delete-indentation)
  (define-key python-mode-map (kbd "S-TAB") 'delete-indentation)
  (define-key python-mode-map (kbd "C-c C-3") 'python-2to3)
  )

(when (executable-find "ipython2")
  (setq python-shell-interpreter "ipython2"
        python-shell-interpreter-args "--simple-prompt -i"))

(defun python-2to3 ()
  "Run 2to3 on the current buffer and put the diff in a new buffer."
  (interactive)
  (let ((output-buffer "*Python 2to3*")
        (shell-file-name (getenv "SHELL")))
    (shell-command (concat "2to3 " (buffer-file-name))
                   (get-buffer-create output-buffer))
    (with-current-buffer output-buffer
      (diff-mode))
    (switch-to-buffer-other-window output-buffer)))

(defun jpk/python-mode-hook ()
  (dolist (x '(("!=" . ?≠)
               ("None" . ?∅)
               ("and" . ?⋀)
               ("or" . ?⋁)
               ("!" . ?¬)
               ("all" . ?∀)
               ("any" . ?∃)
               ("**" . ?⁑)
               ("*" . ?∙)
               ("**2" . ?²)
               ("**3" . ?³)
               ("**n" . ?ⁿ)
               ("Ellipsis" . ?…)
               ("..." . ?…)
               ("in" . ?∈)
               ("not in" . ?∉)
               ("sum" . ?∑)
               ))
    (add-to-list 'prettify-symbols-alist x))
  (with-library 'sphinx-doc
    (sphinx-doc-mode 1))
  )
(add-hook 'python-mode-hook 'jpk/python-mode-hook)

;;(setq gud-pdb-command-name "/usr/lib/python3.6/pdb.py")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Octave/Matlab

(setq matlab-shell-command "octave"
      matlab-shell-command-switches nil
      octave-block-offset 4
      octave-continuation-string "...")

;; ;; hack for octave mode "end" keyword
;; (with-eval-after-load "octave-mod"
;;   (add-to-list 'octave-end-keywords "end[[:space:]]*\\([%#].*\\|$\\)")
;;   (setq octave-block-end-regexp
;;         (concat "\\<\\("
;;                 (string-join octave-end-keywords "\\|")
;;                 "\\)\\>"))
;;   )

(defun jpk/octave-mode-hook ()
  (local-set-key (kbd "C-c C-s") 'octave-send-buffer)
  (local-set-key (kbd "C-c C-l") 'octave-send-line)
  (local-set-key (kbd "C-c C-r") 'octave-send-region))

(add-hook 'octave-mode-hook 'jpk/octave-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verilog

(setq verilog-number-font-lock-spec '("\\<[0-9]+'[bdh]". font-lock-type-face))

(with-eval-after-load "verilog-mode"
  (font-lock-add-keywords
   'verilog-mode
   (list
    verilog-number-font-lock-spec))
  )

(defun jpk/verilog-mode-hook ()
  (dolist (x '(("begin" . ?{)
               ("end" . ?})
               ("<=" . ?⇐)
               ))
    (add-to-list 'prettify-symbols-alist x))
  )
(add-hook 'verilog-mode-hook 'jpk/verilog-mode-hook)

(add-to-list 'auto-mode-alist '("\\.vams\\'" . verilog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp

(defun describe-symbol-at-point ()
  "Get help for the symbol at point."
  (interactive)
  (let ((sym (intern-soft (current-word))))
    (unless
        (cond ((null sym))
              ((not (eq t (help-function-arglist sym)))
               (describe-function sym))
              ((boundp sym)
               (describe-variable sym)))
      (message "nothing"))))

;; http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun eval-and-replace-last-sexp ()
  "Replace the preceding sexp with its value."
  (interactive "*")
  (save-excursion
    (with-syntax-table emacs-lisp-mode-syntax-table
      (backward-kill-sexp)))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace-last-sexp)

(setq eval-expression-print-length nil) ;; unlimited

;; https://emacs.stackexchange.com/a/10233/651
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
      ;; car of form doesn't seem to be a symbol, or is a keyword
      ((and (elt state 2)
          (or (not (looking-at "\\sw\\|\\s_"))
             (looking-at ":")))
       (if (not (> (save-excursion (forward-line 1) (point))
                 calculate-lisp-indent-last-sexp))
           (progn (goto-char calculate-lisp-indent-last-sexp)
                  (beginning-of-line)
                  (parse-partial-sexp (point)
                                      calculate-lisp-indent-last-sexp 0 t)))
       ;; Indent under the list or under the first sexp on the same
       ;; line as calculate-lisp-indent-last-sexp.  Note that first
       ;; thing on that line has to be complete sexp since we are
       ;; inside the innermost containing sexp.
       (backward-prefix-chars)
       (current-column))
      ((and (save-excursion
            (goto-char indent-point)
            (skip-syntax-forward " ")
            (not (looking-at ":")))
          (save-excursion
            (goto-char orig-point)
            (looking-at ":")))
       (save-excursion
         (goto-char (+ 2 (elt state 1)))
         (current-column)))
      (t
       (let ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             method)
         (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
         (cond ((or (eq method 'defun)
                   (and (null method)
                      (> (length function) 3)
                      (string-match "\\`def" function)))
                (lisp-indent-defform state indent-point))
               ((integerp method)
                (lisp-indent-specform method state
                                      indent-point normal-indent))
               (method
                (funcall method indent-point state))))))))

(with-library 'morlock
  (font-lock-add-keywords 'emacs-lisp-mode morlock-font-lock-keywords)
  (font-lock-add-keywords 'lisp-interaction-mode morlock-font-lock-keywords))

(autoload 'edit-list "edit-list.el"
  "Edit a lisp list in a buffer." t)

(defun jpk/lisp-modes-hook ()
  (eldoc-mode 1)
  (local-set-key (kbd "C-M-S-x") 'eval-region)
  (with-library 'highlight-quoted
    (highlight-quoted-mode 1))
  (with-library 'highlight-operators
    (highlight-operators-mode -1))
  (dolist (x (append
              (default-value 'prettify-symbols-alist)
              '(("/=" . ?≠)
                ("nil" . ?∅)
                ("and" . ?⋀)
                ("or" . ?⋁)
                ("not" . ?¬)
                )))
    (add-to-list 'prettify-symbols-alist x))
  (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
  )

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  "Add an imenu expression to find lines like \";;; foobar\"."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) t))

(with-eval-after-load "lisp-mode"
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'jpk/lisp-modes-hook))

  (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
  )

(with-library 'auto-complete
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode))

(defun jpk/ielm-mode-hook ()
  (with-library 'auto-complete
    (dolist (x '(ac-source-functions
                 ac-source-variables
                 ac-source-features
                 ac-source-symbols
                 ac-source-words-in-same-mode-buffers))
      (add-to-list 'ac-sources x))))
(add-hook 'ielm-mode-hook 'jpk/ielm-mode-hook)

(add-hook 'ielm-mode-hook 'jpk/lisp-modes-hook)

(defalias 'emacs-repl 'ielm)

;; just for fun, an ielm quine:
;; (let ((s "(let ((s %S)) (insert (format s s)))")) (insert (format s s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FVWM

(defun jpk/fvwm-mode-hook ()
  ;;(fvwm-enable-indentation)
  (local-set-key (kbd "RET") 'newline)
  (setq indent-line-function 'indent-relative-dwim)
  (setq tab-width 4))

(add-hook 'fvwm-mode-hook 'jpk/fvwm-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PKGBUILD

(use-package pkgbuild-mode
  :mode ("\\`PKGBUILD\\'" "\.install\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML/XML

(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(fset 'sgml-mode 'nxml-mode)

;; Not really HTML editing related, but this will make an HTML file of
;; the buffer, with all the syntax highlighting.
(with-library 'htmlize
  (with-library 'htmlize-view
    (htmlize-view-add-to-files-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config files

(dolist (re '("\\.list\\'"
              "Doxyfile"
              "\\.rules\\'"
              "\\`fstab\\'"
              "\\.env\\'"
              ))
  (add-to-list 'auto-mode-alist `(,re . conf-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YAML mode

(defun jpk/yaml-mode-hook ()
  (setq tab-width 2))
(add-hook 'yaml-mode-hook #'jpk/yaml-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dts (flattened device tree)

(use-package dts-mode
  :config
  (defun jpk/dts-mode-hook ()
    (setq indent-tabs-mode t)
    (setq tab-width 4))
  (add-hook 'dts-mode-hook #'jpk/dts-mode-hook)

  :mode "\\.its\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-mode

(defun jpk/text-mode-hook ()
  (setq indent-line-function 'indent-relative-dwim)
  (with-library 'flyspell
    (flyspell-mode 1))
  (with-library 'auto-complete
    (setq ac-sources
          '(ac-source-dictionary ac-source-words-in-buffer ac-source-filename))
    (auto-complete-mode 1))
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  )

(add-hook 'text-mode-hook 'jpk/text-mode-hook)
(add-hook 'text-mode-hook 'text-mode-hook-identify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LaTeX (AUCTeX mode)

(setq LaTeX-item-indent -1)

(defun jpk/LaTeX-mode-hook ()
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (with-library 'flyspell
    (flyspell-mode 1))
  (setq fill-column 80)
  (local-set-key (kbd "C-M-;") 'insert-comment-bar)
  (setq indent-line-function 'LaTeX-indent-line)
  (with-library 'ac-math
    (dolist (x '(ac-source-math-unicode
                 ac-source-math-latex
                 ac-source-latex-commands))
      (add-to-list 'ac-sources x)))
  (local-set-key [remap next-error] nil)
  (local-set-key [remap previous-error] nil)
  )

(add-hook 'LaTeX-mode-hook 'jpk/LaTeX-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL

;; I have a feeling that there's some sort of hook that could do this
;; more cleanly.
(defun insert-semicolon-and-send-input (&optional no-newline artificial)
  "Like `comint-send-input', but inserts a semicolon first."
  (interactive "*")
  (save-excursion
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (when (> (point) (marker-position pmark))
        (end-of-line)
        (unless (save-excursion (search-backward ";" (line-beginning-position) 'noerror))
          (insert ";")))))
  (comint-send-input no-newline artificial))

(defun jpk/sql-mode-hook ()
  (setq adaptive-wrap-extra-indent 2)
  (visual-line-mode 1)

  (with-library 'wrap-region
    (wrap-region-add-wrapper "`" "`"))

  (with-library 'sqlup-mode
    (sqlup-mode 1))

  (sql-highlight-ansi-keywords))

(with-eval-after-load "sql"

  (define-key sql-interactive-mode-map (kbd "RET") 'insert-semicolon-and-send-input)

  ;;;;

  (advice-add 'sql-sqlite
              :around
              (lambda (orig &rest args)
                "Complete and process file."
                (let (sql-user sql-server insert-default-directory)
                  (setq sql-database (if (and (stringp sql-database)
                                            (file-exists-p sql-database))
                                         sql-database
                                       default-directory))
                  (setq sql-database (read-file-name "SQLite file: "
                                                     (file-name-directory sql-database)
                                                     sql-database))
                  (apply orig args))))

  (setq sql-sqlite-program "sqlite3")

  ;;;;

  (sql-set-product-feature 'mysql :sqli-login
                           '(user password server database))
  (sql-set-product-feature 'mysql :prompt-regexp
                           "^\\(?:mysql\\|mariadb\\).*> ")

  (defun sql-make-alternate-buffer-name ()
    "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'.

If the session was started with `sql-connect' then the alternate
name would be the name of the connection.

Otherwise, it uses the parameters identified by the :sqlilogin
parameter.

If all else fails, the alternate name would be the user and
server/database name."

    (let ((name ""))

      ;; Build a name using the :sqli-login setting
      (setq name
            (apply 'concat
                   (cdr
                    (apply 'append nil
                           (sql-for-each-login
                            (sql-get-product-feature sql-product :sqli-login)
                            (lambda (token plist)
                              (cond
                               ((eq token 'user)
                                (unless (string= "" sql-user)
                                  (list " " sql-user)))
                               ((eq token 'port)
                                (unless (or (not (numberp sql-port))
                                           (= 0 sql-port))
                                  (list ":" (number-to-string sql-port))))
                               ((eq token 'server)
                                (unless (string= "" sql-server)
                                  (list "@"
                                        (if (plist-member plist :file)
                                            (file-name-nondirectory sql-server)
                                          sql-server))))
                               ((eq token 'database)
                                (unless (string= "" sql-database)
                                  (list "/"
                                        (if (plist-member plist :file)
                                            (file-name-nondirectory sql-database)
                                          sql-database))))

                               ((eq token 'password) nil)
                               (t                    nil))))))))

      ;; If there's a connection, use it and the name thus far
      (if sql-connection
          (format "<%s>%s" sql-connection (or name ""))

        ;; If there is no name, try to create something meaningful
        (if (string= "" (or name ""))
            (concat
             (if (string= "" sql-user)
                 (if (string= "" (user-login-name))
                     ()
                   (concat (user-login-name) "/"))
               (concat sql-user "/"))
             (if (string= "" sql-database)
                 (if (string= "" sql-server)
                     (system-name)
                   sql-server)
               sql-database))

          ;; Use the name we've got
          name))))

  (add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)
  (add-hook 'sql-mode-hook 'jpk/sql-mode-hook)
  )

(defun sql-mysql-diff-tables (user host passwd database table-a table-b)
  (interactive "sUser: \nsHost: \nsPassword: \nsDatabase: \nsTable A: \nsTable B: ")
  (let ((table-a-buf (get-buffer-create "*SQL-diff-table-A*"))
        (table-b-buf (get-buffer-create "*SQL-diff-table-B*"))
        (cmd (concat "mysqldump --skip-comments --skip-extended-insert --single-transaction"
                     " -u " user " -h " host " -p" passwd
                     " " database " ")))

    (save-window-excursion
      (shell-command (concat cmd table-a) table-a-buf)
      (shell-command (concat cmd table-b) table-b-buf))

    (with-current-buffer table-b-buf
      (goto-char (point-min))
      (re-search-forward "^INSERT INTO")
      (while (re-search-forward (regexp-quote table-b))
        (replace-match table-a nil nil)))

    (if (and (> (buffer-size table-a-buf) 0)
           (> (buffer-size table-b-buf) 0))
        (ediff-buffers table-a-buf table-b-buf)
      (kill-buffer table-a-buf)
      (kill-buffer table-b-buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assembly

(dolist (x '("\\.dsl\\'" "\\.lst\\'"))
  (add-to-list 'auto-mode-alist `(,x . asm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNDO/REDO

;; treat edit history like the tree it is
(with-library 'undo-tree
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo))

;; C-x u starts the undo-tree visualizer
(defvar undo-tree-visualize-window-state nil
  "Stores the window state before undo-tree-visualize is called,
  so that it can be restored by undo-tree-visualizer-quit")
(advice-add 'undo-tree-visualize
            :before
            (lambda (&rest args)
              "Save window layout."
              (setq undo-tree-visualize-window-state
                    (current-window-configuration))))
(advice-add 'undo-tree-visualizer-quit
            :after
            (lambda (&rest args)
              "Restore window layout."
              (when undo-tree-visualize-window-state
                (set-window-configuration undo-tree-visualize-window-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch

;; Allow page up/down, C-l, and anything else that doesn't move the
;; point during isearch.
(setq isearch-allow-scroll t)
(put 'recenter-top-bottom 'isearch-scroll t)

;; normally M-<tab> which is obviously not going to work
;; get a literal tab with C-q <tab>
(define-key isearch-mode-map (kbd "<tab>") 'isearch-complete)

;; recenter the cursor after finding a match
(advice-add 'isearch-search
            :after
            (lambda (&rest args)
              "Recenter"
              (when isearch-success
                (recenter))))

;; make backspace behave in a more intuitive way
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(defun isearch-unfail ()
  "Like `isearch-abort', but will not quit isearch.  Effectively,
it undoes any edits to the isearch string that caused it to not
match.  It should be idempotent."
  (interactive)
  ;; If search is failing, or has an incomplete regexp,
  ;; rub out until it is once more successful.
  (while (or (not isearch-success) isearch-error)
    (isearch-pop-state))
  (isearch-update))

(define-key isearch-mode-map (kbd "<C-backspace>") 'isearch-unfail)

(define-key isearch-mode-map (kbd "M-k") nil)

(defvar isearch-auto-use-region-max-length 24
  "Upper threshold to automatically use the region in isearch.")

(defun isearch-auto-use-region-advice (&rest args)
  "Automatically use region as search string unless it's too big or empty."
  (when (region-active-p)
    (let* ((b (region-beginning))
           (e (region-end))
           (l (- e b)))
      (when (and (< 0 l) (<= l isearch-auto-use-region-max-length))
        (add-to-history 'search-ring (buffer-substring-no-properties b e))
        (deactivate-mark)))))
(advice-add 'isearch-forward :before #'isearch-auto-use-region-advice)
(advice-add 'isearch-backward :before #'isearch-auto-use-region-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep

;; recenter after running next-error
(setq next-error-recenter '(4))

(defun occur-next-error (&optional argp reset)
  "Move to the Nth (default 1) next match in an Occur mode buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  ;; we need to run occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
          (current-buffer)
        (next-error-find-buffer nil nil
                                (lambda ()
                                  (eq major-mode 'occur-mode))))

    (goto-char (cond (reset (point-min))
                     ((< argp 0) (line-beginning-position))
                     ((> argp 0) (line-end-position))
                     ((point))))
    (occur-find-match
     (abs argp)
     (if (> 0 argp)
         #'previous-single-property-change
       #'next-single-property-change)
     "No more matches")
    ;; In case the *Occur* buffer is visible in a nonselected window.
    (let ((win (get-buffer-window (current-buffer) t)))
      (if win (set-window-point win (point))))
    (occur-mode-goto-occurrence)))

(defun jpk/next-error-hook ()
  ;; FIXME this breaks xref
  (let ((win (get-buffer-window next-error-last-buffer))
        (recenter-redisplay nil))
    (when win
      (with-selected-window win
        (recenter)))))
(add-hook 'next-error-hook #'jpk/next-error-hook)

(use-package grep
  :config
  (defun jpk/grep/compilation-filter-hook ()
    (when (eq major-mode 'grep-mode)
      (setq adaptive-wrap-extra-indent 4)
      (visual-line-mode 1)
      (hide-lines-matching "^find")))
  (add-hook 'compilation-filter-hook #'jpk/grep/compilation-filter-hook)

  (defvar grep-context-lines 2
    "Default number of context lines (non-matching lines before and
  after the matching line) for `rgrep-context'.")

  (defun rgrep-context (arg)
    "Like `rgrep', but adds a '-C' parameter to get context lines around matches.

Default number of context lines is `grep-context-lines', and can
be specified with a numeric prefix."
    (interactive "p")
    (setq arg (or arg grep-context-lines))
    (let ((grep-find-template
           (format "find <D> <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -C %d -e <R>"
                   arg))
          grep-host-defaults-alist
          current-prefix-arg)
      (call-interactively 'rgrep)))
  )

(use-package wgrep
  :bind (:map grep-mode-map
         ("C-x C-q" . wgrep-change-to-wgrep-mode))
  )

(use-package rg
  :config
  (add-hook 'rg-mode-hook #'wgrep-ag-setup)
  )

(use-package ag
  :config
  (setq ag-highlight-search t)

  (defun jpk/ag-mode-hook ()
    (setq adaptive-wrap-extra-indent 4)
    (visual-line-mode 1))
  (add-hook 'ag-mode-hook #'jpk/ag-mode-hook)
  )

(use-package wgrep-ag
  :after (wgrep ag)
  :bind (:map ag-mode-map
         ("C-x C-q" . wgrep-change-to-wgrep-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and replace

(define-key query-replace-map (kbd "b") 'backup)
(define-key query-replace-map (kbd "B") 'backup)
(define-key query-replace-map (kbd "RET") 'act-and-show)

(defun query-exchange (string-1 string-2)
  "Exchange string-1 and string-2 interactively.
The user is prompted at each instance like query-replace."
  (interactive "sString 1: \nsString 2: ")
  (perform-replace
   (concat "\\(" string-1 "\\)\\|" string-2)
   '(replace-eval-replacement replace-quote
                              (if (match-string 1) string-2 string-1))
   t t nil))

(defun search-forward-and-center (string &optional bound noerror count)
  "Just like `search-forward', but centers the point in the window."
  (let ((ret (search-forward string bound noerror count)))
    (recenter)
    ret))
(setq replace-search-function 'search-forward-and-center)

(defun re-search-forward-and-center (regexp &optional bound noerror count)
  "Just like `re-search-forward', but centers the point in the window."
  (let ((ret (re-search-forward regexp bound noerror count)))
    (recenter)
    ret))
(setq replace-re-search-function 're-search-forward-and-center)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; locate
;; front end for the Unix locate command

;; When the locate results buffer is reverted, updatedb is run, and
;; this makes it run as root.
(setq locate-update-path "/sudo::"
      locate-update-when-revert t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paren insertion

(with-library 'wrap-region
  (defun jpk/wrap-region-after-hook ()
    (goto-char (1+ wrap-region-end)))
  (add-hook 'wrap-region-after-hook 'jpk/wrap-region-after-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paren Highlighting

(show-paren-mode 1)

(with-library 'mic-paren
  (setq paren-max-message-length 80)
  (paren-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thing at point

;; bounce between matching parens
(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis. Else go to the
opening parenthesis one level up."
  (interactive)
  (if (looking-at "\\s\(")
      (forward-list 1)
    (backward-char 1)
    (if (looking-at "\\s\)")
        (progn (forward-char 1) (backward-list 1))
      (while (not (looking-at "\\s("))
        (backward-char 1)
        (when (looking-at "\\s\)")
          (message "->> )")
          (forward-char 1)
          (backward-list 1)
          (backward-char 1))))))

(defun bounce-thing-boundary (thing &optional tweak-beginning tweak-end n)
  "Move point to beginning or end of THING.
If in the middle of THING, go to the beginning.  If at the
beginning, go to end.  If at the end, go to beginning.  If not in
a THING, search forward N THINGs (negative N searches backward).
The optional TWEAK-BEGINNING and TWEAK-END are added to THING's
beginning or end position, respectively, before moving the
point."
  (interactive)
  (unless tweak-beginning
    (setq tweak-beginning 0))
  (unless tweak-end
    (setq tweak-end 0))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if (not bounds)
        (progn
          (when n
            (forward-thing thing n))
          (unless (thing-at-point thing)
            (error "No %s here." thing)))
      (setq bounds (cons (+ (car bounds) tweak-beginning)
                         (+ (cdr bounds) tweak-end)))
      (cond
       ((eq (point) (car bounds))
        (goto-char (cdr bounds)))
       ((eq (point) (cdr bounds))
        (goto-char (car bounds)))
       (t
        (goto-char (car bounds)))))))

(defun bounce-string-or-list ()
  "Toggle point between the beginning or end of a string, or list if not in a string."
  (interactive)
  (if (> (length (thing-at-point 'string)) 0)
      ;; FIXME apparently strings aren't things anymore
      (bounce-thing-boundary 'string)
    (goto-match-paren)))
(global-set-key (kbd "C-S-<iso-lefttab>") 'bounce-string-or-list)
(global-set-key (kbd "C-%") 'bounce-string-or-list)

(with-library 'transpose-params
  (global-set-key (kbd "C-M-S-t") 'transpose-params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region

(with-library 'expand-region
  (defun isearch-yank-selection ()
    "Put selection from buffer into search string."
    (interactive)
    (when (region-active-p)
      (deactivate-mark))  ;; fully optional, but I don't like unnecessary highlighting
    (let ((isearch-case-fold-search nil))
      (isearch-yank-internal (lambda () (mark)))))

  (global-set-key (kbd "C-=") 'er/expand-region)
  (define-key isearch-mode-map (kbd "C-S-y") 'isearch-yank-selection)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet (Yet Another Template Mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)

  :config
  (setq yas-prompt-functions (cons 'yas-ido-prompt
                                   (remove 'yas-ido-prompt
                                           yas-prompt-functions)))

  :bind (("C-c y" . yas-insert-snippet)
         :map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil))
  )

(use-package yankpad
  :config
  (setq yankpad-file (expand-file-name "yankpad.org" no-littering-var-directory))

  :bind (("C-c Y" . yankpad-insert))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous keybindings
;; the best way to find the symbol for a key is to run C-h k <key>

;; normally bound to C-<mouse-3>
(global-set-key (kbd "<down-mouse-3>") 'mouse-popup-menubar-stuff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changing case (capitalization)

;; enable the region case changing commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun downcase-region-or-word ()
  "Downcase the current word if the region is inactive, otherwise
  downcase the region."
  (interactive "*")
  (let ((deactivate-mark nil))
    (if (use-region-p)
        (downcase-region (region-beginning) (region-end))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if bounds
            (downcase-region (car bounds) (cdr bounds))
          (message "Nothing to downcase."))))))

(defun upcase-region-or-word ()
  "Upcase the current word if the region is inactive, otherwise
  upcase the region."
  (interactive "*")
  (let ((deactivate-mark nil))
    (if (use-region-p)
        (upcase-region (region-beginning) (region-end))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if bounds
            (upcase-region (car bounds) (cdr bounds))
          (message "Nothing to upcase."))))))

(defun capitalize-region-or-word ()
  "Capitalize the current word if the region is inactive, otherwise
  capitalize the region."
  (interactive "*")
  (let ((deactivate-mark nil))
    (if (use-region-p)
        (capitalize-region (region-beginning) (region-end))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if bounds
            (capitalize-region (car bounds) (cdr bounds))
          (message "Nothing to capitalize."))))))

(global-set-key (kbd "C-x C-u") 'upcase-region-or-word)
(global-set-key (kbd "C-x C-i") 'capitalize-region-or-word)
(global-set-key (kbd "C-x C-l") 'downcase-region-or-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete things

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-S-SPC") 'delete-blank-lines)
(global-set-key (kbd "C-d") 'delete-forward-char)

(defalias 'delete-vertical-space 'delete-blank-lines)

(defmacro copy-instead-of-kill (&rest body)
  "Replaces `kill-region' with `kill-ring-save' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end &optional region)
                (kill-ring-save beg end region)
                (setq this-command 'kill-region))))
     ,@body))

(defmacro delete-instead-of-kill (&rest body)
  "Replaces `kill-region' with `delete-region' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end &optional region)
                ;; FIXME: account for region arg
                (delete-region beg end))))
     ,@body))

(setq kill-whole-line t)

(defun copy-line (&optional arg)
  "Like `kill-line', but copies instead of killing."
  (interactive "P")
  (copy-instead-of-kill (kill-line arg)))

(defun delete-word (&optional arg)
  "Like `kill-word', but deletes instead of killing."
  (interactive "p")
  (delete-instead-of-kill (kill-word arg)))

(global-set-key (kbd "C-S-k") 'copy-line)

(with-library 'browse-kill-ring
  (advice-add 'yank-pop :around
              (lambda (orig &rest args)
                "Run `browse-kill-ring' if last command was not `yank'."
                (if (eq last-command 'yank)
                    (apply orig args)
                  (browse-kill-ring)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq backward-delete-char-untabify-method nil)

(electric-indent-mode -1)

(global-set-key (kbd "C-m") #'newline-and-indent)

(defun indent-relative-dwim ()
  "Indent current line based on previous line; cycle between alternate indents

Indents the current line to the same level as the previous line.

If called repeatedly, cycle the indent as follows:
    same as previous line
    one tab-width more than previous
    one tab-width less than previous
    beginning of line
    same as previous, etc."

  (interactive "*")

  (let ((last-line-indent 0)
        (next-indent 0)
        (prev-indent 0))

    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (re-search-backward "[^\t ]" nil t)
          (beginning-of-line)
          (re-search-forward "[^\t ]" nil t)
          (goto-char (match-beginning 0))
          (setq last-line-indent (current-column)))))
    (setq next-indent (* (1+ (/ last-line-indent tab-width)) tab-width))
    (setq prev-indent (max 0 (* (1- (/ last-line-indent tab-width)) tab-width)))

    (save-excursion
      (beginning-of-line-text)
      (cond
       ((= (current-column) last-line-indent)
        (unless (eq this-command 'newline-and-indent)
          (indent-line-to next-indent)))
       ((= (current-column) next-indent)
        (indent-line-to prev-indent))
       ((= (current-column) prev-indent)
        (indent-line-to 0))
       (t
        (indent-line-to last-line-indent))))

    (when (or (bolp) (looking-back "^[[:space:]]+"))
      (beginning-of-line-text))))

(setq-default indent-line-function 'indent-relative-dwim)

;; new buffers default to using 4 spaces for indent
(setq-default tab-width 4
              indent-tabs-mode nil)

;; smart-tabs-mode will indent with tabs, align with spaces
(with-library 'smart-tabs-mode
  (smart-tabs-advice cperl-indent-line cperl-indent-level)
  (smart-tabs-advice c-indent-line     c-basic-offset)
  (smart-tabs-advice c-indent-region   c-basic-offset)
  )

;; shift-tab should undo what tab does
(global-set-key (kbd "<backtab>") 'delete-indentation)
(global-set-key (kbd "S-TAB") 'delete-indentation)

;; rigidly indent
;; see EmacsWiki://MovingRegionHorizontally
;; FIXME: neither this nor smart-shift respects indent-tabs-mode
(defun move-horizontally (beg en arg)
  "Move the region defined by (beg en) by arg characters.
Positive arg means right; negative means left"
  (save-excursion
    (let ((deactivate-mark nil)
          beg-bol)
      (goto-char beg)
      (setq beg-bol (line-beginning-position))
      (goto-char en)
      (indent-rigidly beg-bol en arg)
      (push-mark beg t t))))

(defun move-left-1 (beg en &optional arg)
  "Move the active region or the current line ARG columns to the left."
  (interactive "*r\np")
  (if (use-region-p)
      (move-horizontally beg en arg)
    (move-horizontally (line-beginning-position) (line-end-position) arg)))

(defun move-right-1 (beg en &optional arg)
  "Move the active region or the current line ARG columns to the right."
  (interactive "*r\np")
  (move-left-1 beg en (- arg)))

(require 'smart-shift nil 'noerror)
(defun move-left-dwim (beg en &optional arg)
  "Move the active region or the current line The Right Number of columns to the left."
  (interactive "*r\np")
  (let* ((shift (or (and (boundp 'smart-shift-indentation-level)
                     smart-shift-indentation-level)
                  (and (functionp 'smart-shift-infer-indentation-level)
                     (smart-shift-infer-indentation-level))
                  tab-width))
         (indent-tabs-mode (or indent-tabs-mode smart-tabs-mode)))
    (move-left-1 beg en (* arg shift))))

(defun move-right-dwim (beg en &optional arg)
  "Move the active region or the current line The Right Number of columns to the right."
  (interactive "*r\np")
  (move-left-dwim beg en (- arg)))

(global-set-key (kbd "C-0") 'move-left-1)
(global-set-key (kbd "C-9") 'move-right-1)
(global-set-key (kbd "C-)") 'move-left-dwim)
(global-set-key (kbd "C-(") 'move-right-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fill

(use-package fill
  :ensure nil
  :init
  ;; see also fill-individual-paragraphs

  ;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
  (defun unfill-paragraph ()
    "Takes a multi-line paragraph and makes it into a single line
of text."
    (interactive "*")
    (let ((fill-column (point-max))
          (emacs-lisp-docstring-fill-column nil))
      (fill-paragraph)))

  (defun fill-or-unfill-paragraph ()
    "Toggle between `fill-paragraph' and `unfill-paragraph'."
    (interactive "*")
    (if (eq last-command 'fill-or-unfill-paragraph)
        (progn
          (setq this-command nil)
          (call-interactively 'unfill-paragraph))
      (call-interactively 'fill-paragraph)))

  :config
  (advice-add 'fill-paragraph
              :after
              (lambda (&rest args)
                "Scroll to the left."
                (scroll-right (window-hscroll))))

  :bind (("M-q" . fill-or-unfill-paragraph))
  )

(use-package align
  :ensure nil
  :config
  (defun align-regexp-space (beginning end)
    "Align columns using space as a delimiter."
    (interactive "*r")
    (align-regexp beginning end
                  "\\(\\s-*\\)\\s-" 1 0 t))

  ;; FIXME
  (defun align-regexp-comma (beginning end)
    "Align columns using comma as a delimiter."
    (interactive "*r")
    (save-window-excursion
      (save-excursion
        (save-restriction
          (narrow-to-region beginning end)
          (with-temp-message ""
            (replace-regexp "\\s-*," ", " t (point-min) (point-max)))
          (align-regexp (point-min) (point-max)
                        ",\\(\\s-*\\)" 1 1 t)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adaptive word wrap

(with-library 'adaptive-wrap
  (put 'adaptive-wrap-extra-indent 'safe-local-variable 'integerp)
  (setq-default adaptive-wrap-extra-indent 2)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scrolling

(setq hscroll-margin 1 ;; how close to the edge to get before scrolling
      hscroll-step 1 ;; how many columns to scroll once past hscroll-margin
      auto-hscroll-mode t)

(setq scroll-preserve-screen-position t)

;; makes scrolling less jittery
(setq auto-window-vscroll nil
      redisplay-dont-pause t
      scroll-conservatively most-positive-fixnum
      )

;; scroll the other buffer
(global-set-key (kbd "S-<next>") 'scroll-other-window)
(global-set-key (kbd "S-<prior>") 'scroll-other-window-down)

(with-library 'smart-hscroll
  ;;(smart-hscroll-mode 1) ;; interferes with rectangle-mark-mode
  (mouse-hscroll-mode 1)
  (global-set-key (kbd "C->") 'scroll-left-8)
  (global-set-key (kbd "C-<") 'scroll-right-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el ends here
