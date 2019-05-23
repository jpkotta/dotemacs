;;; init.el --- jpkotta's emacs init file -*- lexical-binding: t -*-

;; Copyright (C) Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>

(message "Loading jpkotta's init.el.")

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
      file-name-handler-alist nil
      )
(defun jpk/emacs-startup-hook ()
  (message "init.el loaded in %s." (emacs-init-time))
  (setq gc-cons-threshold (standard-value 'gc-cons-threshold)
        gc-cons-percentage (standard-value 'gc-cons-percentage)
        file-name-handler-alist fnha-old)
  (makunbound 'fnha-old)
  (garbage-collect))
(add-hook 'emacs-startup-hook #'jpk/emacs-startup-hook)

;; use Emacs bindings in all GTK apps:
;; $ gsettings set org.gnome.desktop.interface gtk-key-theme Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO

;; a better way to do global key bindings
;; http://shallowsky.com/blog/linux/editors/emacs-global-key-bindings.html

;; ;; override function but retain original definition!
;; ;; doesn't seem to work in :around advice
;; (defun foo ()
;;   (message "orig foo"))
;; (cl-letf (((symbol-function 'orig-foo) (symbol-function 'foo))
;;           ((symbol-function 'foo) (lambda () (orig-foo) (message "and new foo"))))
;;   (foo))

;; https://github.com/alphapapa/unpackaged.el

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

(defvar extra-lisp-directory (expand-file-name "lisp/" user-emacs-directory)
  "Directory for Emacs lisp files that are not part of Emacs or in packages.")
(add-to-list 'load-path extra-lisp-directory)

(defvar test-lisp-directory (expand-file-name "test/" user-emacs-directory)
  "Directory for Emacs lisp files that in an unfinished state.")
(add-to-list 'load-path test-lisp-directory)

;; set up specific to the local machine
(let ((local-init-file (expand-file-name "local-init.el" extra-lisp-directory)))
  (when (file-exists-p local-init-file)
    (load-file local-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(setq package-user-dir (expand-file-name
                        (format "elpa-%d" emacs-major-version)
                        user-emacs-directory))

(package-initialize)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("onpa" . "https://olanilsson.bitbucket.io/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("gnu" . 10)))

(byte-recompile-directory extra-lisp-directory 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(use-package no-littering
  :pin melpa
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
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

(setq frame-resize-pixelwise t)
(setq default-frame-alist '((vertical-scroll-bars . right)
                            (menu-bar-lines . 0)
                            (background-mode . dark)
                            (tool-bar-lines . 0)
                            (width . 81)
                            (scroll-bar-height . 5)
                            (scroll-bar-width . 10)))

;; ;; better font config for weird chars
;; (when (find-font (font-spec :name "Symbola"))
;;   (set-fontset-font t '(#x10000 . #x1ffff) "Symbola"))

(setq custom-safe-themes t)
(use-package calmer-forest-theme
  :init
  (load-theme 'calmer-forest 'noconfirm)

  (set-face-attribute 'default nil
                      :family (if (string= system-type "windows-nt")
                                  "Consolas"
                                "DejaVu Sans Mono")
                      :height 100)
  (set-face-attribute 'mode-line nil :height 1.0)

  :config
  (custom-theme-set-faces
   'calmer-forest

   '(fixed-pitch ((t (:family "Luxi Mono"))))

   '(fringe ((t (:background "grey10" :foreground "dark green"))))
   '(highlight ((t (:background "#001000"))))

   '(mode-line-buffer-id ((t (:weight bold))))

   '(region ((t (:background "#400060"))))
   '(mouse ((t (:background "orange"))))
   '(mouse-flash-position ((t (:background "grey75"))))
   '(secondary-selection ((t (:background "DeepPink4"))))

   '(ac-completion-face ((t (:foreground "green3"))))
   '(ac-selection-face ((t (:background "grey9" :foreground "magenta"))))
   '(ac-candidate-face ((t (:background "grey16" :foreground "lavender"))))
   '(ac-gtags-selection-face ((t (:inherit ac-selection-face))))
   '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-yasnippet-selection-face ((t (:inherit ac-selection-face))))
   '(ac-yasnippet-candidate-face ((t (:inherit ac-candidate-face))))

   '(company-tooltip ((t (:background "grey16" :foreground "lavender"))))
   '(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
   '(company-tooltip-annotation ((t (:inherit company-tooltip))))
   '(company-tooltip-selection ((t (:background "grey9" :foreground "magenta"))))
   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold))))
   '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
   '(company-preview ((t (:foreground "green3"))))
   '(company-preview-common ((t (:inherit company-preview :weight bold))))
   '(company-scrollbar-bg ((t (:background "lavender"))))
   '(company-scrollbar-fg ((t (:background "grey9"))))

   '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))

   '(diff-refine-changed ((t (:weight bold :background "grey24"))))
   '(diff-refine-added ((t (:inherit diff-refine-change :foreground "green2"))))
   '(diff-refine-removed ((t (:inherit diff-refine-change :foreground "red2"))))
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

   '(Info-quoted ((t (:family "Luxi Mono")))))
  )

(setq mode-line-percent-position '(6 . "%q"))

(use-package modeline-posn
  :ensure nil
  :config
  (set-face-attribute 'modelinepos-region nil :inverse-video t)
  (setq modelinepos-column-limit 80)
  )

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

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-undecided "(EOL?)"
      eol-mnemonic-unix "(LF)")

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(put 'visual-line-mode 'safe-local-variable 'booleanp)
(setq-default indicate-empty-lines t)

(setq uniquify-buffer-name-style 'post-forward)

(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style '(face tabs spaces lines newline space-mark tab-mark newline-mark))
  (set-face-attribute 'whitespace-space nil :inherit whitespace-newline)
  (set-face-attribute 'whitespace-tab nil :inherit whitespace-newline)
  (set-face-attribute 'whitespace-line nil :underline t :foreground nil :background nil)
  )

(use-package face-remap
  :ensure nil
  :diminish buffer-face-mode
  )

(setq delete-selection-save-to-register ?d)
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ergomovement

(use-package hydra
  ;; unlike most packages, there are no featurep tests for this, it's
  ;; assumed to be installed
  :config
  ;; blue exits immediately, red keeps the hydra active
  ;; teal and amaranth are like magit popups
  ;; pink is like a minor mode
  (setq lv-use-separator t)
  )

(use-package ergo-movement-mode
  :ensure nil
  :commands (ergo-movement-mode)
  :init
  (ergo-movement-mode 1)
  )

(use-package mwim
  :bind (:map prog-mode-map
         ("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

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
  (dolist (w (window-list))
    (with-selected-window w
      (switch-to-buffer "*scratch*"))))

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

(global-set-key (kbd "C-x C-z") #'suspend-frame-if-not-gui)

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

(defun jpk/mouse-select-window (&rest args)
  "no mouse select window if minibuffer active"
  (or (active-minibuffer-window)
     (window-minibuffer-p)
     (minibuffer-window-active-p (selected-window))))
(advice-add 'mouse-set-point :before-until #'jpk/mouse-select-window)
(advice-add 'mouse-set-region :before-until #'jpk/mouse-select-window)

;; cancel everything, including active minibuffers and recursive edits
(global-set-key (kbd "C-M-g") #'top-level)

;; Disable highlighting clickable stuff when the mouse moves over it.
;; Turning it off speeds up remote X.
(setq mouse-highlight nil)

;; never shrink windows by default
(defvar jpk/allow-window-shrinking nil
  "If non-nil, effectively disable shrinking windows by making `shrink-window-if-larger-than-buffer' a no-op.")
(defun jpk/prevent-window-shrinking (&rest args)
  "Do nothing if `jpk/allow-window-shrinking' is nil."
  jpk/allow-window-shrinking)
(advice-add 'shrink-window-if-larger-than-buffer
            :before-while #'jpk/prevent-window-shrinking)

;; add this :around advice to enable shrinking windows
(defun jpk/allow-window-shrinking (orig &rest args)
  "Set `jpk/allow-windows-shrinking' non-nil."
  (let ((jpk/allow-window-shrinking t))
    (apply orig args)))

(defvar jpk/saved-window-state nil
  "Stores the window state so that it can be restored later.")
(defun jpk/save-window-state (&rest args)
  "Save window layout in `jpk/saved-window-state'."
  (setq jpk/saved-window-state (current-window-configuration)))
(defun jpk/restore-window-state (&rest args)
  "Restore window layout from `jpk/saved-window-state'."
  (when (window-configuration-p jpk/saved-window-state)
    (set-window-configuration jpk/saved-window-state)))

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") #'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") #'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(global-set-key [remap exchange-point-and-mark]
                #'exchange-point-and-mark-no-activate)

(defvar important-messages-regexp "recover-this-file"
  "Regexp for `important-messages'.")

(defun important-messages ()
  "Create a buffer full of important messages.

Creates a buffer called \"*Important Messages*\" and fills it
with any lines from the \"*Messages*\" buffer that match
`important-messages-regexp'.  If the buffer contains any matches,
switch to it.  Recommended to add to `emacs-startup-hook'."
  (interactive)
  (let ((out (get-buffer-create "*Important Messages*"))
        (case-fold-search t))
    (with-current-buffer out
      (erase-buffer))
    (with-current-buffer "*Messages*"
      (goto-char (point-min))
      (while (re-search-forward important-messages-regexp (point-max) 'noerror)
        (append-to-buffer out (line-beginning-position) (1+ (line-end-position)))))
    (if (> 0 (buffer-size (get-buffer "*Important Messages*")))
        (switch-to-buffer out)
      (kill-buffer out))))

(add-hook 'emacs-startup-hook #'important-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing

(use-package printing
  :ensure nil
  :disabled
  :config
  (setq pr-gv-command "okular")
  (pr-update-menus t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tags

;; GTAGSLABEL has no effect unless there's a ~/.globalrc
(let ((rcfile "~/.globalrc")
      (dist-rcfiles '("/usr/share/gtags/gtags.conf"
                      "/usr/local/share/gtags/gtags.conf"
                      "/usr/share/doc/global/examples/gtags.conf")))
  (unless (file-exists-p rcfile)
    (dolist (dist-rcfile dist-rcfiles)
      (when (file-exists-p dist-rcfile)
        (copy-file dist-rcfile rcfile)))))

(use-package ggtags
  :disabled
  :if (executable-find "global")
  :diminish ggtags-mode
  :config
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
  )

;; xref is the unified cross reference subsystem.  ggtags actually
;; uses it for some things like tag history.  It's labeled as
;; experimental in Emacs 25, and seems like it needs more work.  gxref
;; is a package to use global as an xref backend.  Once xref is more
;; mature, it will probably be better than ggtags.

;; FIXME gxref appears to be unmaintained

(use-package gxref
  ;;:disabled
  :after (xref projectile)
  :if (executable-find "global")
  :config
  ;; (remove-hook 'next-error-hook #'jpk/next-error-hook)
  ;; (setq next-error-recenter nil)

  (put 'gxref-gtags-label 'safe-local-variable 'stringp)

  (defun gxref-create-db-pygments (project-root-dir)
    "Like `gxref-create-db', but set GTAGSLABEL to pygments.

Pygments supports more languages, but is much slower than the
default label."
    (interactive "DCreate GTAGS db in directory: ")
    (let ((gxref-gtags-label "pygments"))
      (gxref-create-db project-root-dir)))

  (defun gxref--global-to-list-async (file-name args)
    "Like `gxref--global-to-list', but asynchronous."
    (let* ((process-environment (gxref--prepare-process-environment))
           (name (format "*Gxref Update %s*" file-name))
           (proc-buffer (get-buffer name))
           (cmd (mapconcat #'shell-quote-argument (cons gxref-global-exe args) " ")))
      ;; only one proc-buffer at a time
      (unless proc-buffer
        (setq proc-buffer (get-buffer-create name))
        (set-process-sentinel
         (start-file-process-shell-command name proc-buffer cmd)
         (lambda (_proc event)
           (let ((regexp (regexp-opt '("finished" "exit" "deleted" "failed")))
                 (inhibit-message t))
             (message "%s: %s" name (replace-regexp-in-string "\n+$" "" event))
             (when (string-match regexp event)
               (kill-buffer proc-buffer))))))))

  (defun gxref-update-db ()
    "Update GTAGS project database for current project."
    (interactive)
    (let ((root (gxref--find-project-root)))
      (unless root (error "Not under a GTAGS project"))
      (gxref--global-to-list-async root '("-u"))))

  (defun gxref-single-update-db ()
    "Update GTAGS project database for the current file."
    (interactive)
    (let ((root (gxref--find-project-root))
          (bfn (buffer-file-name)))
      (unless root (error "Not under a GTAGS project"))
      (unless bfn (error "Buffer has no file associated with it"))
      (gxref--global-to-list-async bfn (list "--single-update" bfn))))

  (add-hook 'xref-backend-functions #'gxref-xref-backend)

  ;; see projectile-command-map
  (defun gxref-create-db-projectile ()
    "Like `gxref-create-db', but uses `projectile-project-root' to set the directory."
    (interactive)
    (gxref-create-db (projectile-project-root)))

  (defun gxref-create-db-pygments-projectile ()
    "Like `gxref-create-db-pygments', but uses `projectile-project-root' to set the directory."
    (interactive)
    (gxref-create-db-pygments (projectile-project-root)))

  ;; FIXME make the completion table cache every time the db is updated
  (defvar gxref-enable-completion-table t
    "If nil, create an empty completion-table, because it can be slow.")
  (make-variable-buffer-local 'gxref-enable-completion-table)
  (setq-default gxref-enable-completion-table nil)

  (cl-defmethod
    xref-backend-identifier-completion-table ((_backend (eql gxref)))
  "Return a list of terms for completions taken from the symbols in the current buffer.
The current implementation returns all the words in the buffer,
which is really sub optimal."
  (when gxref-enable-completion-table
    (gxref--global-to-list '("-c"))))

  (defhydra hydra/gxref (:color blue)
    "gxref"
    ("c" gxref-create-db "create db")
    ("u" gxref-update-db "update db")
    ("p" gxref-create-db-pygments "create pygments db")
    ("j" gxref-create-db-projectile "create db (projectile)")
    ("k" gxref-create-db-pygments-projectile "create pygments db (projectile)")
    )

  :bind (("M-/" . xref-find-references)
         ("C-c /" . hydra/gxref/body)
         ;; :map projectile-mode-map
         ;; ("R" . gxref-update-db)
         )
  )

(use-package xref
  :ensure nil
  :hook
  (xref--xref-buffer-mode-hook . hl-line-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile

(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (setq projectile-indexing-method 'alien ;; FIXME 'turbo-alien
        projectile-enable-caching t
        projectile-tags-backend 'xref
        ;;projectile-tags-command "gtags"
        projectile-tags-command ""
        projectile-current-project-on-switch 'move-to-end
        projectile-switch-project-action #'projectile-dired)
  (projectile-mode 1)

  :bind (("C-c p" . projectile-command-map)
         ("C-<tab>" . projectile-next-project-buffer)
         ("C-S-<iso-lefttab>" . projectile-previous-project-buffer))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode

;; TODO: add to hydra
;; org-previous-visible-heading
;; org-next-visible-heading
;; org-forward-heading-same-level
;; org-backward-heading-same-level

(use-package org
  ;;:pin gnu
  :config
  (defun jpk/org-mode-hook ()
    (setq adaptive-wrap-extra-indent 0)
    (visual-line-mode 1)
    (when (featurep 'flyspell)
      (flyspell-mode 1))
    (hl-line-mode 1)
    )
  (add-hook 'org-mode-hook #'jpk/org-mode-hook)

  (setq org-ellipsis "…"
        org-startup-indented t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-fontify-emphasized-text nil
        org-support-shift-select 'always)

  ;; http://www.howardism.org/Technical/Emacs/literate-devops.html
  (setq org-babel-load-languages
        '((shell . t)
          (emacs-lisp . t)
          (python . t)
          (sql . t) ;; see also ob-sql-mode
          (sqlite . t)
          (C . t)
          (js . t)
          ))

  ;; https://blog.d46.us/advanced-emacs-startup/
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
  :disabled
  :if (and (executable-find "ipython")
         (= 0 (shell-command "python -c 'import jupyter_client ; import jupyter_console'")))
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(ipython . t))
  (org-babel-reload-languages)
  )

(use-package ob-async
  :after org
  )

(use-package ob-http
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(http . t))
  (org-babel-reload-languages)
  )

(use-package ob-mongo
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mongo . t))
  (org-babel-reload-languages)

  ;; example:

  ;; #+PROPERTY: header-args:mongo :db dbname :host localhost :port 33621 :user username :password passwd

  ;; #+BEGIN_SRC mongo
  ;; db.things.findOne();
  ;; #+END_SRC
  )

;; TODO: check out ob-shstream.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc insertions

(use-package lorem-ipsum)

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

(defun insert-shrug (&optional arg)
  "Insert shrug emoticon (¯\\_(ツ)_/¯)"
  ;; emoji: 🤷
  (interactive "P*")
  (insert "¯\\_(ツ)_/¯"))

(setq calendar-latitude 46.877222
      calendar-longitude -96.789444
      calendar-location-name "Fargo, ND, USA"
      calendar-standard-time-zone-name "CST"
      calendar-daylight-time-zone-name "CDT")

(defun insert-current-time ()
  "Insert the current date and time in the buffer."
  (interactive "*")
  (let* ((fmt-alist '(("time" "%T")
                      ("date" "%F")
                      ("datetime" "%F %T")
                      ("iso" "%FT%T%z")))
         (type (completing-read "Type: "
                                (mapcar #'car fmt-alist)
                                nil t nil nil "datetime"))
         (fmt (cadr (assoc type fmt-alist))))
    (insert (format-time-string fmt (current-time)))))

(defhydra hydra/inserts (:color blue)
  "Insert Text"
  ("l" lorem-ipsum-insert-list "lorem ipsum list" :color red)
  ("s" lorem-ipsum-insert-sentences "lorem ipsum sentence" :color red)
  ("p" lorem-ipsum-insert-paragraphs "lorem ipsum paragraph" :color red)
  ("d" insert-look-of-disapproval "Look of Disapproval")
  ("D"
   (let ((current-prefix-arg '(4)))
     (call-interactively #'insert-look-of-disapproval))
   "Look of Disapproval (big)")
  ("a" insert-awesome-face "awesome face")
  ("A"
   (let ((current-prefix-arg '(4)))
     (call-interactively #'insert-awesome-face))
   "awesome face (big)")
  ("h" insert-shrug "shrug")
  ("H"
   (let ((current-prefix-arg '(4)))
     (call-interactively #'insert-shrug))
   "shrug (escaped)")
  ("t" insert-current-time "current time")
  )

(global-set-key (kbd "C-c w") #'hydra/inserts/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ssh

(use-package keychain-environment
  :defer 2
  :config
  (keychain-refresh-environment)
  )

(use-package ssh-config-mode
  :mode (".ssh/config\\'" "sshd?_config")
  :bind (:map ssh-config-mode-map
         ("C-<up>" . nil)
         ("C-<down>" . nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C SubWordMode
;; FindSubWordsInCamelCase

;; FIXME reimplement using subword-{forward,backward}-function
;; (no need to remap bindings, etc.)

(use-package syntax-subword
  :init
  (setq syntax-subword-skip-spaces 'consistent)
  (global-syntax-subword-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help

(setq message-log-max 10000)
(add-hook 'help-mode-hook #'hl-line-mode)

(use-package find-func
  :ensure nil
  :config
  (let ((dir (expand-file-name "~/src/emacs/src")))
    (when (and (null find-function-C-source-directory)
	       (file-directory-p dir))
      (setq find-function-C-source-directory dir)))

  :bind (("C-h C-f" . find-function))
)

(use-package info
  :ensure nil
  :config
  (add-to-list 'Info-additional-directory-list "~/.local/share/info/")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers and strings

(use-package evil-numbers
  :init
  (defhydra hydra/evil-numbers (:color red
                                :hint nil)
    "Evil Numbers"
    ("=" evil-numbers/inc-at-pt "increase")
    ("+" evil-numbers/inc-at-pt "increase")
    ("-" evil-numbers/dec-at-pt "decrease")
    ("_" evil-numbers/dec-at-pt "decrease")
    ("q" nil "quit")
    )

  :bind (("C-c =" . hydra/evil-numbers/body)
         ("C-c -" . hydra/evil-numbers/body))
  )

(setq display-raw-bytes-as-hex t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hexl mode

(use-package nhexl-mode
  ;; TODO
  ;; indirect buffer (but set-buffer-multibyte doesn't work with that)
  ;; mode map
  ;; hexl-bits configuration
  ;; bytes not decoded °
  :config
  (defun jpk/nhexl-mode-hook ()
    (nhexl-overwrite-only-mode 1)
    (nhexl-nibble-edit-mode 1)
    (hl-line-mode 0)
    (face-remap-add-relative 'highlight :inverse-video t))
  (add-hook 'nhexl-mode-hook #'jpk/nhexl-mode-hook)

  (defun nhexl-goto-addr (n)
    "Move to the Nth byte, 0-indexed."
    (goto-char n))

  (defun nhexl-forward-256 (&optional arg)
    "Move vertically down ARG blocks of 256 bytes (16 lines)."
    (interactive "p")
    (nhexl-goto-addr (+ (* 256 arg) (point)))
    (recenter))

  (defun nhexl-backward-256 (&optional arg)
    "Move vertically up ARG blocks of 256 bytes (16 lines)."
    (interactive "p")
    (nhexl-forward-256 (- arg)))

  (defun nhexl-forward-1k (&optional arg)
    "Move vertically down ARG blocks of 1024 bytes (64 lines)."
    (interactive "p")
    (nhexl-goto-addr (+ (* 1024 arg) (point)))
    (recenter))

  (defun nhexl-backward-1k (&optional arg)
    "Move vertically up ARG blocks of 1024 bytes (64 lines)."
    (interactive "p")
    (nhexl-forward-1k (- arg)))

  (defun nhexl-forward-4k (&optional arg)
    "Move vertically down ARG blocks of 4096 bytes (256 lines)."
    (interactive "p")
    (nhexl-goto-addr (+ (* 4096 arg) (point)))
    (recenter))

  (defun nhexl-backward-4k (&optional arg)
    "Move vertically up ARG blocks of 4096 bytes (256 lines)."
    (interactive "p")
    (nhexl-forward-4k (- arg)))

  (define-derived-mode bin-mode fundamental-mode "Binary"
    "Major mode for binary files.

Uses `nhexl-mode'."
    :group 'nhexl
    (set-buffer-multibyte nil)
    (nhexl-mode 1))

  (add-to-list 'auto-mode-alist
               (cons (concat (regexp-opt '(".bin" ".imx")) "\\'") 'bin-mode))

  :bind (:map nhexl-mode-map
         ("<next>" . nhexl-forward-256)
         ("<prior>" . nhexl-backward-256)
         ("C-<next>" . nhexl-forward-1k)
         ("C-<prior>" . nhexl-backward-1k)
         ("C-S-<next>" . nhexl-forward-4k)
         ("C-S-<next>" . nhexl-backward-4k))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rectangles

(use-package rect
  :ensure nil
  :defer t
  :config
  (setq rectangle-preview t)

  (defun rectangle-number-lines-format ()
    "Like `rectangle-number-lines' with a `current-prefix-arg'."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'rectangle-number-lines)))

  (defhydra hydra/rectangle (:body-pre (rectangle-mark-mode 1)
                             :color pink
                             :hint nil
                             :post (deactivate-mark))
    "Rectangle"
    ("d" kill-rectangle "kill" :color blue)
    ("y" yank-rectangle "yank" :color blue)
    ("w" copy-rectangle-as-kill "copy" :color blue)
    ("o" open-rectangle "open" :color blue)
    ("t" string-rectangle "string" :color blue)
    ("c" clear-rectangle "clear" :color blue)
    ("e" rectangle-exchange-point-and-mark "exchange")
    ("n" rectangle-number-lines "number" :color blue)
    ("N" rectangle-number-lines-format "number (fmt)" :color blue)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) "toggle mark")
    ("C-<return>" nil "quit")
    )

  :bind (("C-<return>" . hydra/rectangle/body))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(use-package autorevert
  :ensure nil
  :defer 1
  :diminish auto-revert-mode
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-to-list 'revert-without-query "\\.rom\\'")

  (global-auto-revert-mode 1)

  :bind (("<f5>" . revert-buffer))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard

(setq select-enable-clipboard t
      select-enable-primary t
      select-active-regions t)
(setq save-interprogram-paste-before-kill t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode

;; tip - use insert-char to insert unicode characters by name

(use-package list-unicode-display
  :pin melpa
  )

(prefer-coding-system 'utf-8)

;; "coding" seems to be the standard spelling
(put 'encoding 'safe-local-variable 'coding-system-p)

(setq default-input-method "TeX"
      read-quoted-char-radix 16)

(defun dos2unix ()
  "Convert a DOS file (CRLF) to Unix (LF)"
  (interactive)
  (set-buffer-file-coding-system 'unix 'force))

(defun force-unix-line-endings ()
  "Replace all occurances of CRLF with LF."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\x0d\x0a" nil t)
      (replace-match "\x0a" nil t))))

(defun unix2dos ()
  "Convert a Unix (LF) file to DOS (CRLF)"
  (interactive)
  (set-buffer-file-coding-system 'dos 'force))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Daemon/Server

(global-set-key (kbd "C-x C-S-c") #'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-c") #'delete-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Browser

(setq browse-url-browser-function #'eww-browse-url
      browse-url-new-window-flag t
      eww-search-prefix "https://google.com/search?q="
      shr-color-visible-luminance-min 70
      shr-external-browser #'browse-url-chrome)

(use-package eww
  :ensure nil
  :config
  (defun eww-browse-with-external-browser-and-quit (&optional url)
    (interactive)
    (eww-browse-with-external-browser url)
    (quit-window))

  :bind (:map eww-mode-map
         ([remap eww-browse-with-external-browser]
          . eww-browse-with-external-browser-and-quit))
  )

(use-package atomic-chrome
  :defer 5
  :init
  (when (daemonp)
    (atomic-chrome-start-server))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completion

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  )

(defun jpk/dabbrev-friend-buffer (other-buffer)
  (and (dabbrev--same-major-mode-p other-buffer)
     (< (buffer-size other-buffer) (* 1 1024 1024))))
(setq dabbrev-friend-buffer-function #'jpk/dabbrev-friend-buffer)

(use-package auto-complete
  :disabled
  :diminish auto-complete-mode
  :init
  (ac-config-default)
  (when (boundp 'global-company-mode)
    (global-company-mode 0))

  :config
  (setq ac-auto-start 3
        ac-auto-show-menu t
        ac-use-quick-help nil
        ac-ignore-case nil)

  (add-to-list 'ac-dictionary-files "~/.ispell_american")
  (add-to-list 'ac-dictionary-files "~/.aspell.en.pws")

  (add-to-list 'ac-modes 'latex-mode)
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)

  (add-to-list 'ac-sources 'ac-source-filename)
  (setq-default ac-sources ac-sources)

  ;; workaround for flyspell-mode
  (ac-flyspell-workaround)

  (add-to-list 'ac-modes 'text-mode)

  (defun jpk/ielm-mode-hook ()
    (dolist (x '(ac-source-functions
                 ac-source-variables
                 ac-source-features
                 ac-source-symbols
                 ac-source-words-in-same-mode-buffers))
      (add-to-list 'ac-sources x)))
  (add-hook 'ielm-mode-hook #'jpk/ielm-mode-hook)

  :bind (("C-<tab>" . auto-complete)
         :map ac-completing-map
         ("C-n" . ac-next)
         ("C-p" . ac-previous)
         ("M-k" . ac-next)
         ("M-i" . ac-previous)
         ("<backtab>" . ac-expand-previous)
         ("S-TAB" . ac-expand-previous)
         ("C-s" . ac-isearch))
  )

(use-package fuzzy
  :disabled
  :requires auto-complete
  )

(use-package ac-c-headers
  :disabled
  :requires auto-complete
  :after auto-complete
  :config
  (defun jpk/ac-c-headers ()
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-header-symbols 'append))

  (add-hook 'c-mode-common-hook #'jpk/ac-c-headers)
  )

(use-package ac-math
  :disabled
  :requires auto-complete
  :after auto-complete)

(use-package company
  :pin melpa
  :diminish company-mode
  :init
  (global-company-mode 1)
  (when (boundp 'global-auto-complete-mode)
    (global-auto-complete-mode 0))

  :config
  ;; configuration similar to auto-complete
  (company-tng-configure-default)
  (setq company-idle-delay 0.1
        company-auto-complete nil
        company-search-regexp-function #'company-search-flex-regexp
        company-tooltip-align-annotations t)

  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-backends (standard-value 'company-backends))
  (setq company-backends (delete 'company-clang company-backends))

  (setq company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers t ;; same major-mode
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)

  :bind (:map company-active-map
         ("C-s" . company-filter-candidates)
         ("SPC" . nil)
         :map company-search-map
         ("S-<iso-lefttab>" . company-select-previous)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous)
         ("<tab>" . company-select-next)
         ("TAB" . company-select-next)
         )
  )

;; hippie-expand is like dabbrev-expand, but more
;;(global-set-key (kbd "M-/") #'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flyspell

;; FIXME try enchant
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init
  (setq ispell-program-name "aspell")

  :config
  (setq flyspell-use-meta-tab nil)

  :bind (:map flyspell-mode-map
         ("M-TAB" . nil))
  )

(use-package flyspell-correct
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-word-generic))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookmarks

(use-package bm
  :init
  (setq bm-marker 'bm-marker-right
        bm-recenter t
        bm-highlight-style 'bm-highlight-only-fringe)

  :config
  (defhydra hydra/bookmarks (:color blue)
    "Bookmarks"
    ("m" bm-toggle "toggle")
    ("n" bm-next "next" :color red)
    ("p" bm-previous "previous" :color red)
    ("l" bm-show "show")
    ("L" bm-show-all "show all")
    ("s" bm-save "save")
    ("r" bm-load-and-restore "restore")
    ("q" nil "quit")
    )

  :bind (("C-c m" . hydra/bookmarks/body)
         ("<right-fringe> <mouse-5>" . bm-next-mouse)
         ("<right-fringe> <mouse-4>" . bm-previous-mouse)
         ("<right-fringe> <mouse-1>" . bm-toggle-mouse))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq reb-re-syntax 'string)

(use-package hide-lines
  :config
  (defhydra hydra/hide-lines (:color blue)
    "Hide Lines"
    ("a" hide-lines-show-all "show all")
    ("s" hide-lines-not-matching "hide non-matching")
    ("d" hide-lines-matching "hide only matching")
    ("q" nil "quit")
    )

  :bind (("C-c s" . hydra/hide-lines/body))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido - Interactively Do Things

(use-package ido
  :ensure nil
  :config
  (setq ido-case-fold t
        ido-confirm-unique-completion t
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-enable-flex-matching t
        ido-ignore-buffers '("\\` " "^\\*Completion" "^\\*Ido")
        ido-max-work-file-list 50
        ido-rotate-file-list-default t
        ido-show-dot-for-dired t
        ido-work-directory-match-only nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-use-filename-at-point nil
        ido-use-url-at-point nil)

  ;; this also affects dired-omit-extensions
  (dolist (e '("-pkg.el" "-autoloads.el"))
    (add-to-list 'completion-ignored-extensions e))
  (dolist (e '(".bin" ".so" ".a"))
    (delete e completion-ignored-extensions))

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
  :after ido
  :defer 1
  :config
  (setq ido-cr+-max-items (expt 2 16))
  (ido-ubiquitous-mode 1))

(use-package crm-custom
  :disabled
  :after ido-completing-read+
  :defer 1
  :config
  (crm-custom-mode 1)
  )

(use-package amx
  :defer 1
  :config
  (amx-mode 1))

(use-package smex
  :disabled
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backups, saving, restoring

(setq make-backup-files t
      vc-make-backup-files t
      version-control t
      kept-new-versions 128
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook #'force-backup-of-buffer)

(use-package backup-walker)

(let ((dir (no-littering-expand-var-file-name "auto-save/")))
  (make-directory dir t)
  (add-to-list 'auto-save-file-name-transforms `(".*" ,dir t) 'append))

(use-package save-visited-files
  :init
  (setq save-visited-files-ignore-tramp-files t
        save-visited-files-ignore-directories nil
        save-visited-files-auto-restore t)
  )

(use-package recentf
  :ensure nil
  :defer 2
  :config
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
  )

(setq history-delete-duplicates t)
(savehist-mode 1)

(use-package midnight
  :ensure nil
  :init
  (midnight-delay-set 'midnight-delay "04:00")
  (midnight-mode 1)

  (defun midnight-make-all-buffers-cleanable ()
    "Set `buffer-display-time' for all buffers.

`clean-buffer-list' ignores buffers that have never been
displayed (e.g. they were restored with `save-visited-files').
This sets all buffers as displayed."
    (interactive)
    (save-window-excursion
      (dolist (b (buffer-list))
        (unless (buffer-local-value 'buffer-display-time b)
          (set-window-buffer (selected-window) b)))))
  (add-hook 'save-visited-files-mode-hook #'midnight-make-all-buffers-cleanable)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;(setq tramp-copy-size-limit nil) ; for Edison

(use-package sudo-edit
  :pin melpa
  :defer 2
  :config
  (sudo-edit-indicator-mode 1)
  :bind ("C-x C-r" . sudo-edit)
  )

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

(setq mouse-drag-and-drop-region t)

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
  :commands (ace-window)
  :config
  (setf (alist-get ?o aw-dispatch-alist) '(aw-flip-window))
  (setq aw-scope 'frame)
  (setq aw-leading-char-style 'path)

  :bind (("C-x o" . ace-window))
  )

;; Save point position per-window instead of per-buffer.
(use-package winpoint
  :disabled
  :defer 2
  :config
  (winpoint-mode 1))

(setq help-window-select 'never)

;; TODO: display-buffer-alist https://gitlab.com/jabranham/emacs/blob/master/init.el#L2564
(use-package shackle
  :defer 2
  :config
  (setq shackle-default-rule '(:inhibit-window-quit t)
        shackle-rules
        '(("[*]Man .*" :select t :regexp t)
          (completion-list-mode :inhibit-window-quit nil :align 'below :size 0.3)
          (("*vc-incoming*" "*vc-outgoing*") :same t)
          ))

  (shackle-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC mode

(use-package hgrc-mode)

(setq vc-handled-backends '(Git Hg SVN))

(use-package vc-hgcmd
  ; FIXME overzealous diff-hl
  :if (executable-find "hg")
  :pin melpa
  :config
  (defun vc-hgcmd-ignore-tramp (orig &rest args)
    "Ignore tramp files because it doesn't seem to be supported yet."
    (unless (and (fboundp 'tramp-tramp-file-p)
               (tramp-tramp-file-p (car args)))
      (apply orig args)))
  ;;(advice-add #'vc-hgcmd-registered :around #'vc-hgcmd-ignore-tramp)

  (let ((idx (seq-position vc-handled-backends 'Hg)))
    (when idx
      (setf (seq-elt vc-handled-backends idx) 'Hgcmd)))
  )

(use-package magit
  :if (executable-find "git")
  :init
  ;; show margin (author+time) in a magit log buffer with L d
  (setq magit-log-margin '(nil age-abbreviated magit-log-margin-width :author 11))

  :config
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'magit-diff-mode-hook #'jpk/diff-mode-hook)

  ;;(setq vc-handled-backends (delete 'Git vc-handled-backends))
  (when (featurep 'diff-hl)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (global-magit-file-mode 0)

  (defun magit-switch-to-status-buffer (buffer)
    "Like `switch-to-buffer', but only for `magit-status-mode' buffers."
    (interactive (list (completing-read
                        "Magit status: "
                        (mapcar #'buffer-name (buffer-list))
                        (lambda (b)
                          (with-current-buffer b
                            (derived-mode-p 'magit-status-mode))))))
    (unless (string-empty-p buffer)
      (switch-to-buffer buffer)))

  (defhydra hydra/magit (:color blue)
    "Magit"
    ("g" magit-status "status")
    ("b" magit-switch-to-status-buffer "switch")
    ("d" magit-dispatch-popup "dispatch")
    ("f" magit-file-popup "file")
    )

  :bind (("C-x g" . hydra/magit/body))
  )

(defalias 'git-grep #'vc-git-grep)

;; TODO
;; vc-revert bug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff

(setq diff-switches "-u"
      diff-default-read-only t)

;; diff uses the current file and the most recent backup by default.
;; This makes it hard to pick two different files, and backup-walker
;; handles the backups much better, so change the interactive form to
;; look for normal existing files.
(defun jpk/diff-default-args (&rest args)
  "Better default arguments for `diff'."
  (interactive "fOld: \nfNew: \n"))
(advice-add 'diff :before #'jpk/diff-default-args)

(defun diff-directories (old new &optional switches no-async)
  "Like `diff', but run recursively on directories."
  (interactive "DOld: \nDNew: \n")
  (unless switches
    (setq switches "-u -r -w"))
  (diff old new switches no-async))

(with-eval-after-load "ediff"
  (require 'ediff-tweak))

(advice-add 'ediff-setup-windows :around #'jpk/allow-window-shrinking)

;; put the ediff control window in the same frame
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Skip over whitespace-only differences in ediff mode.  Still finds
;; such regions, only changes navigation.  Toggle with # # in ediff
;; mode.
(setq-default ediff-ignore-similar-regions t)

;; show fine differences
(setq-default ediff-auto-refine 'on)

(use-package commit-patch-buffer
  :ensure nil
  :commands (commit-patch-buffer)
  )

(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "C-c C-k") #'diff-hunk-kill)
  (define-key diff-mode-map (kbd "C-c C-S-k") #'diff-file-kill)
  (define-key diff-mode-map (kbd "K") nil) ;; diff-file-kill
  (define-key diff-mode-map (kbd "M-K") nil) ;; diff-file-kill
  (define-key diff-mode-map (kbd "C-c C-c") #'commit-patch-buffer)
  (define-key diff-mode-map (kbd "C-c C-m") #'diff-add-trailing-CR-in-hunk)
  (define-key diff-mode-map (kbd "C-c C-j") #'diff-remove-trailing-CR-in-hunk)
  (define-key diff-mode-map (kbd "C-c C-o") #'diff-goto-source)
  )

(defun diff-delete-trailing-CR ()
  "Delete trailing carriage returns (^M) in a `diff-mode' buffer."
  ;; TODO: with-silent-modifications, with-coding-priority, set-buffer-file-coding-system
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

  (hl-line-mode 1)

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
    (while (re-search-forward "\\([^\x0D]\\)$" nil t)
      (replace-match "\\1\x0D" nil nil))))

(defun remove-CR-eol (b e)
  (interactive "*r")
  (save-restriction
    (narrow-to-region b e)
    (while (re-search-forward "\x0D$" nil t)
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

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix "")

  :config
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (defhydra hydra/smerge
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  :bind (("C-c c" . hydra/smerge/body))
  )

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)

  :config
  (setq diff-hl-fringe-bmp-function #'diff-hl-fringe-bmp-from-type)
  ;;(add-hook 'dired-mode-hook #'diff-hl-dired-mode) ;; pretty slow
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint - command interpreter

(setq-default comint-input-ignoredups t
              comint-prompt-read-only t
              comint-scroll-to-bottom-on-input 'all)

(defun jpk/eob (&rest args)
  "Move to end of buffer."
  (goto-char (point-max)))
(advice-add 'comint-send-input :before #'jpk/eob)

;; used for interactive terminals
(with-eval-after-load "comint"
  (define-key comint-mode-map
    (kbd "<up>") #'comint-previous-matching-input-from-input)
  (define-key comint-mode-map
    (kbd "<down>") #'comint-next-matching-input-from-input)
  )

(autoload 'ansi-color-for-comint-mode-on "ansi-color")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminals

(use-package eterm-256color
  :init
  (add-hook 'term-mode-hook #'eterm-256color-mode)

  :config
  (set-face-attribute 'eterm-256color-default nil :inherit 'term)
  (setq eterm-256color-disable-bold t) ;; makes "bold" mean "bright"
  )

(use-package sane-term
  :bind (("C-c t" . sane-term)
         :map term-raw-map
         ("C-S-t" . sane-term-create)
         ("<C-prior>" . sane-term-prev)
         ("<C-next>" . sane-term-next))
  )

(use-package term
  :ensure nil
  :config

  ;; t is usually better for shell terminals, but nil is better for
  ;; serial terminals
  (setq term-suppress-hard-newline nil)
  (setq term-buffer-maximum-size 8192)

  (setq serial-speed-history '("115200"))

  (defun term-send-backward-kill-word ()
    "Backward kill word in `term-mode'."
    (interactive)
    (term-send-raw-string "\C-w"))

  (defun term-send-forward-kill-word ()
    "Kill word in `term-mode'."
    (interactive)
    (term-send-raw-string "\ed"))

  (defun term-send-M-x ()
    "Send `M-x' in `term-mode'."
    (interactive)
    (term-send-raw-string "\ex"))

  (defun term-send-esc ()
    "Send escape char in `term-mode'."
    (interactive)
    (term-send-raw-string "\e"))

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

  (defvar term--arrow-char "O"
    "Arrow keys send escape codes like \"\e[A\" or \"\eOA\".
    This is the middle character, and should be either \"[\" or
    \"O\".")

  (defun term-toggle-arrow-char ()
    "Toggle `term--arrow-char' between \"O\" and \"[\"."
    (interactive)
    (cond
     ((string= term--arrow-char "O")
      (setq term--arrow-char "["))
     ((string= term--arrow-char "[")
      (setq term--arrow-char "O"))
     (t
      (setq term--arrow-char "O")))
    (message "Term arrow character set to \"%s\"." term--arrow-char))

  (defun term-send-up ()
    (interactive)
    (term-send-raw-string (format "\e%sA" term--arrow-char)))
  (defun term-send-down ()
    (interactive)
    (term-send-raw-string (format "\e%sB" term--arrow-char)))
  (defun term-send-right ()
    (interactive)
    (term-send-raw-string (format "\e%sC" term--arrow-char)))
  (defun term-send-left ()
    (interactive)
    (term-send-raw-string (format "\e%sD" term--arrow-char)))

  (defun term-revert-buffer (ignore-auto noconfirm)
    "Clear the buffer.  Suitable for `revert-buffer-function'."
    (when (or (not noconfirm)
             (and noconfirm (y-or-n-p "Clear buffer? ")))
      (let ((inhibit-read-only t))
        (setq term-terminal-state 0)
        (term-reset-terminal)
        (term-send-raw-string "\C-l"))))

  (defun jpk/term-mode-hook ()
    (setq cua--ena-cua-keys-keymap nil)
    (when (featurep 'yasnippet)
      (yas-minor-mode 0))
    (setq-local revert-buffer-function #'term-revert-buffer)
    )
  (add-hook 'term-mode-hook #'jpk/term-mode-hook)

  :bind (:map term-raw-map
         ("C-x" . nil)
         ("C-h" . nil)
         ("<ESC>" . nil)
         ("C-c C-e" . term-send-esc)
         ("C-s" . isearch-forward)
         ("C-r" . isearch-backward)
         ("C-v" . nil)
         ("C-<backspace>" . term-send-backward-kill-word)
         ("C-<delete>" . term-send-forward-kill-word)
         ("C-c C-c" . term-interrupt-subjob-or-C-c)
         ("C-c C-z" . term-stop-subjob-or-C-z)
         ("C-c C-y" . term-paste)
         ("C-c C-u" . universal-argument)
         ("C-c C-v" . term-send-raw) ;; quote
         ("C-c C-x" . term-send-raw) ;; C-x
         ("C-c b" . nil))
  )

(defun copy-eterm-color-terminfo (hostspec)
  "Copy the eterm-color terminfo file to a remote host.
HOSTSPEC is a tramp host specification, e.g. \"/ssh:HOSTSPEC:/remote/path\"."
  (interactive
   (let ((hosts (mapcar (lambda (x) (cond ((stringp x) x)
                                     ((null (car x)) (cadr x))
                                     (t (concat (car x) "@" (cadr x)))))
                        (apply #'append
                               (mapcar
                                (lambda (x) (cl-remove-if-not #'identity (apply (car x) (cdr x))))
                                (tramp-get-completion-function "ssh"))))))
     (list (completing-read "Hostname: " hosts nil 'confirm nil nil hosts nil))))
  (let ((destdir (format "/ssh:%s:.terminfo/e/" hostspec)))
    (ignore-errors
      (dired-create-directory destdir))
    (copy-file (concat data-directory "e/eterm-color")
               (concat destdir "eterm-color"))))

(setq shell-command-dont-erase-buffer 'beg-last-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell

(use-package eshell
  :ensure nil
  :commands (eshell)
  :config
  (require 'em-smart)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)

  (defun jpk/eshell-mode-hook ()
    (defun eshell--revert-buffer-function (&optional ignore-auto noconfirm)
      (eshell/clear))
    (setq-local revert-buffer-function #'eshell--revert-buffer-function))
  (add-hook 'eshell-mode-hook #'jpk/eshell-mode-hook)

  (require 'em-prompt)
  (defun jpk/eshell-prompt-function ()
    (format "[%s][%s]\n$ "
            (format-time-string "%H:%M:%S")
            (abbreviate-file-name (eshell/pwd))))
  (setq eshell-prompt-function #'jpk/eshell-prompt-function
        eshell-prompt-regexp "^[$] ")

  (set-face-attribute 'eshell-prompt nil
                      :foreground "cyan3" :background "grey11")

  ;; (defun jpk/eshell-default-face ()
  ;;   (face-remap-add-relative 'default '(:background "grey12")))
  ;; (add-hook 'eshell-mode-hook #'jpk/eshell-default-face)

  (defun eshell-insert-history ()
    (interactive)
    (insert (completing-read
             "Eshell history: "
             (delete-dups (ring-elements eshell-history-ring)))))

  (require 'em-term)
  (add-to-list 'eshell-visual-subcommands '("make" "menuconfig"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "iftop")
  (add-to-list 'eshell-visual-commands "iotop")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proced

(use-package proced
  :ensure nil
  :config
  (setq-default proced-auto-update-flag t
                proced-filter 'all
                proced-format 'short
                proced-sort 'pcpu)
  )

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

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; set-fill-column is dumb
(global-set-key (kbd "C-x f") #'find-file-at-point)

;; this will e.g. find a .h find if you're looking at the .c file
(global-set-key (kbd "C-x F") #'ff-find-related-file)

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
                     (apply-partially #'locate-file-completion-table
                                      dirs suffixes)
                     (when suffixes
                       (lambda (x) (string-match-p (concat (regexp-opt suffixes t) "$") x)))
                     nil nil nil def)))

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
(global-set-key [remap save-buffer] #'jpk/save-buffer-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired

;; TODO:
;; https://truongtx.me/tmtxt-dired-async.html
;; dired-rsync

(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^.].*$"
        dired-omit-verbose nil)
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (setq dired-deletion-confirmer 'y-or-n-p
        dired-dwim-target t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (defun jpk/create-parent-dirs (&rest args)
    "Create parent directories if necessary."
    (let* ((new-name (nth 1 args))
           (dir (file-name-directory new-name)))
      (unless (file-directory-p dir)
        (message "Creating dir for file %s" new-name)
        (make-directory dir 'parents))))
  (advice-add 'dired-rename-file :before #'jpk/create-parent-dirs)

  (setq wdired-allow-to-change-permissions t)

  (defun jpk/dired-before-readin-hook ()
    (visual-line-mode 0)
    (hl-line-mode 1)
    (setq truncate-lines t))
  (add-hook 'dired-before-readin-hook #'jpk/dired-before-readin-hook)

  (when (featurep 'dired-async)
    (dired-async-mode 1))

  :bind (:map dired-mode-map
         ("C-s" . dired-isearch-filenames)
         ("C-M-s" . dired-isearch-filenames-regexp)
         ("S-<return>" . dired-find-file-other-window)
         ("S-<down-mouse-2>" . dired-mouse-find-file-other-window)
         ("C-c C-o" . dired-omit-mode))
  )

(use-package dired-single
  :after dired
  :config
  ;; stolen from diredp-up-directory
  (defun dired-single-up-directory ()
    "Like `dired-up-directory' but with `dired-single-buffer'."
    (interactive)
    (let* ((dir (dired-current-directory))
           (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
         (and (cdr dired-subdir-alist) (dired-goto-subdir up))
         (progn (dired-single-buffer up)
                (dired-goto-file dir)))))

  :bind (:map dired-mode-map
         ("f" . dired-single-up-directory)
         ([remap dired-up-directory] . dired-single-up-directory)
         ([remap dired-find-file] . dired-single-buffer)
         ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse))
  )

(use-package diredfl
  :after dired
  :init
  (add-hook 'dired-mode-hook #'diredfl-mode)
  )

(use-package dired+
  ;; deprecated
  ;; replaced by diredfl and dired-single
  :disabled
  :after dired
  :config
  (setq diredp-wrap-around-flag nil)
  (toggle-diredp-find-file-reuse-dir 1)

  :bind (:map dired-mode-map
         ("<mouse-2>" . diredp-mouse-find-file-reuse-dir-buffer)
         ("f" . diredp-up-directory-reuse-dir-buffer))
  )

(use-package dired-imenu
  :after dired)

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
         ("/" . dired-narrow)
         ("C-x n n" . dired-narrow)
         ("C-x n r" . dired-narrow-regexp)
         ("C-x n f" . dired-narrow-fuzzy)
         ("C-x n w" . revert-buffer))
  )

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-c" . dired-ranger-copy)
         ("C-c C-x" . dired-ranger-move)
         ("C-c C-v" . dired-ranger-paste))
  )

(use-package dired-open
  :after dired
  :config
  (dolist (prog-exts
           '(("geeqie"
              . ("xbm" "pbm" "pgm" "ppm" "pnm" "png" "gif" "bmp" "tif" "jpeg" "jpg" "svg"))
             ("vlc"
              . ("mpg" "mpeg" "mp3" "mp4" "asf" "avi" "wmv" "wav" "mov" "flv" "ogm" "ogg" "mkv"))
             ("libreoffice"
              . ("doc" "docx" "xls" "ppt" "odt" "ods" "odg" "odp"))
             ("okular"
              . ("pdf" "ps" "ps.gz" "dvi"))))
    (dolist (ext (cdr prog-exts))
      (add-to-list 'dired-open-extensions (cons ext (car prog-exts)))))
  (setq dired-open-functions '(dired-open-by-extension)
        dired-open-find-file-function #'dired-single-buffer)
  )

(use-package dired-avfs
  :if (executable-find "mountavfs")
  :after dired
  :config
  (setq dired-avfs-file-size-threshold 1000) ;; MB
  (unless (= 0 (shell-command "mountavfs"))
    (message "Could not start avfs."))
  )

(use-package view
  :ensure nil
  :after dired
  :config
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

  :bind (:map view-mode-map
         ("n" . dired-view-next)
         ("p" . dired-view-prev))
  )

(use-package disk-usage)

(use-package openwith
  :disabled
  :init
  (openwith-mode 1)

  :config
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; image-mode

(with-eval-after-load "image-mode"
  (define-key image-mode-map (kbd "<next>") #'image-scroll-up)
  (define-key image-mode-map (kbd "<prior>") #'image-scroll-down)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSV mode

(use-package csv-mode
  :config
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
                (error "Invalid separator: %s" x)))
          separators)
    (setq csv-separators separators)
    (setq csv-separator-chars (mapcar #'string-to-char separators)
          csv-skip-regexp (apply #'concat "^\n" csv-separators)
          csv-separator-regexp (apply #'concat `("[" ,@separators "]"))
          csv-font-lock-keywords
          ;; NB: csv-separator-face variable evaluates to itself.
          `((,csv-separator-regexp . csv-separator-face))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IBuffer

(use-package ibuffer
  :ensure nil
  :init
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

  :config
  (require 'ibuf-ext)
  (require 'ibuf-macs)

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

  (defun jpk/recenter (&rest args)
    "Recenter"
    (recenter))
  (dolist (func '(ibuffer-forward-filter-group
                  ibuffer-backward-filter-group
                  ibuffer-mark-interactive
                  ibuffer-toggle-filter-group-1))
    (advice-add func :after #'jpk/recenter))

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

  ;; run when ibuffer buffer is created
  (defun jpk/ibuffer-mode-hook ()
    (ibuffer-auto-mode 1)
    (hl-line-mode 1)
    (setq truncate-lines t))
  (add-hook 'ibuffer-mode-hook #'jpk/ibuffer-mode-hook)

  ;; run when ibuffer command is invoked
  (defun jpk/ibuffer-hook ()
    (ibuffer-set-filter-groups-by-mode)
    (setq ibuffer-sorting-mode 'pathname))
  (add-hook 'ibuffer-hook #'jpk/ibuffer-hook)

  ;; jump to most recent buffer
  (defun jpk/jump-to-most-recent (orig &rest args)
    "Move point to most recent."
    (let ((recent-buffer-name (buffer-name)))
      (apply orig args)
      (unless (string-match-p "*Ibuffer*" recent-buffer-name)
        (ibuffer-jump-to-buffer recent-buffer-name))))
  (advice-add 'ibuffer :around #'jpk/jump-to-most-recent)

  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ M" . ibuffer-set-filter-groups-by-mode)
         ("/ m" . ibuffer-filter-by-used-mode)
         ("<down>" . ibuffer-forward-line)
         ("<up>" . ibuffer-backward-line)
         ("C-<down>" . ibuffer-forward-filter-group)
         ("C-<up>" . ibuffer-backward-filter-group)
         ("SPC" . ibuffer-mark-toggle))
  )

(use-package ibuffer-projectile
  :after (projectile ibuffer)
  :bind (:map ibuffer-mode-map
         ("/ P" . ibuffer-projectile-set-filter-groups)
         ("/ p" . ibuffer-filter-by-projectile-files))
  )

(use-package ibuffer-tramp
  :after ibuffer
  :bind (:map ibuffer-mode-map
         ("/ T" . ibuffer-tramp-set-filter-groups-by-tramp-connection))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immortal-scratch

(use-package immortal-scratch
  :init
  (immortal-scratch-mode 1)

  :config
  (setq immortal-scratch-switch-to-respawned-scratch t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package boxquote)

(use-package figlet
  :if (executable-find "figlet")
  :defer t
  :config
  (add-to-list 'figlet-options "-k") ;; kerning
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify-symbols

;; prettify-symbols-mode is enabled in major mode hooks.
;; Major modes can append more symbols before enabling prettify-symbols-mode.
(setq-default
 prettify-symbols-alist
 '(("\x0C" . ?§)
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

;; this can be dangerous
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
(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

(defun jpk/compilation-finish (buffer string)
  (call-process "notify-send" nil nil nil
        "-t" "3000"
        "-i" "emacs"
        "Compilation finished in Emacs"
        string))
(add-hook 'compilation-finish-functions #'jpk/compilation-finish)

(setq compilation-read-command nil) ;; only prompt when compile is run with prefix

(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-x ~") #'previous-error)

(use-package defrepeater
  :defer 2
  :config
  (global-set-key [remap next-error] (defrepeater #'next-error))
  (global-set-key [remap previous-error] (defrepeater #'previous-error))

  ;; (defrepeater #'next-error)
  ;; (defrepeater #'previous-error)

  ;; :bind (([remap next-error] . next-error-repeat)
  ;;        ([remap previous-error] . previous-error-repeat))
  )

(use-package multi-compile
  :config

  (defun locate-repo-dir (&optional file-or-dir)
    "Find the root of the version control repository."
    (let* ((file-or-dir (or file-or-dir (buffer-file-name) default-directory))
           (file-dir (if (file-directory-p file-or-dir)
                         file-or-dir
                       (file-name-directory file-or-dir)))
           (backend (or (vc-deduce-backend) (vc-responsible-backend file-dir)))
           (root-dir (or (vc-call-backend backend 'root file-dir)
                        default-directory)))
      (expand-file-name root-dir)))

  (dolist (e '(("%cflags" . (or (getenv "CFLAGS") "-Wall -g3 -std=c11"))
               ("%cxxflags" . (or (getenv "CXXFLAGS") "-Wall -g3"))
               ("%repo-dir" . (locate-repo-dir))))
    (add-to-list 'multi-compile-template e))

  (setq multi-compile-alist
        `((".*" . (("make-simple" .
                    "make -k")
                   ("make-repo" .
                    "make -k --no-print-directory -C '%repo-dir'")
                   ("make-top" .
                    "make -k --no-print-directory -C '%make-dir'")
                   ("build.sh" .
                    "cd '%repo-dir' && bash build.sh")))
          (c-mode . (("c-simple" .
                      "gcc -o '%file-sans' %cflags '%file-name'")
                     ("c-simple32" .
                      "gcc -o '%file-sans' %cflags -m32 '%file-name'")))
          (c++-mode . (("c++-simple" .
                        "g++ -o '%file-sans' %cxxflags '%file-name'")))))

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

  :bind (("C-c b" . multi-compile-run))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Programming

(use-package hl-todo
  :defer 1
  :init
  ;;(add-hook 'prog-mode-hook #'hl-todo-mode)

  ;; FIXME bitbake-mode breaks without global mode, because of mmm
  (global-hl-todo-mode 1)

  :config
  (setq hl-todo-keyword-faces
        (mapcar (lambda (w) (cons w "red"))
                '("TODO" "FIXME" "KLUDGE" "XXX" "DEBUG")))
  )

(setq jit-lock-stealth-time 5)

(setq-default comment-column 0
              comment-style 'extra-line)
;; make comment-dwim basically a no-op with no region
(setq comment-insert-comment-function #'beginning-of-line)

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

(use-package symbol-overlay
  :config
  (defhydra hydra/symbol-overlay (:color red)
    "Symbol Overlay"
    ("i" symbol-overlay-put "Toggle")
    ("n" symbol-overlay-jump-next "Next")
    ("p" symbol-overlay-jump-prev "Previous")
    ("w" symbol-overlay-save-symbol "Copy")
    ("t" symbol-overlay-toggle-in-scope "Toggle Scope")
    ("e" symbol-overlay-echo-mark "Jump to Mark")
    ("d" symbol-overlay-jump-to-definition "Jump to Definition")
    ("s" symbol-overlay-isearch-literally "Search")
    ("q" symbol-overlay-query-replace "Query Replace")
    ("r" symbol-overlay-rename "Rename")
    )
  :bind (("C-c a" . hydra/symbol-overlay/body))
  )

(use-package iedit
  :commands (iedit-mode)
  )

(use-package visual-regexp
  :bind (([remap replace-regexp] . vr/replace)
         ([remap query-replace-regexp] . vr/query-replace))
  )

(use-package paren-face
  :commands (paren-face-mode)
  :init
  (add-hook 'prog-mode-hook #'paren-face-mode)
  :config
  (setq paren-face-regexp (regexp-opt '("[" "]" "(" ")" "{" "}")))
  (set-face-attribute 'parenthesis nil :foreground "cyan3")
  )

(use-package highlight-operators
  :commands (highlight-operators-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-operators-mode)
  )

(use-package highlight-numbers
  :commands (highlight-numbers-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  )

(use-package rainbow-mode
  :commands (rainbow-mode)
  )

(add-hook 'prog-mode-hook #'hl-line-mode)

(defun jpk/prog-mode-hook ()
  ;; check spelling on the fly, but only in comments and strings
  (when (featurep 'flyspell)
    (flyspell-mode 0)
    (flyspell-prog-mode))

  (setq adaptive-wrap-extra-indent 1)
  (visual-line-mode 1)

  (local-set-key (kbd "C-M-;") #'insert-comment-bar)
  (local-set-key (kbd "C-m") #'newline-and-indent)
  (local-set-key (kbd "C-M-a") #'previous-defun)
  (local-set-key (kbd "C-M-e") #'next-defun)

  (when (featurep 'auto-complete)
    (dolist (x '(ac-source-gtags ac-source-imenu ac-source-yasnippet))
      (add-to-list 'ac-sources x)))
  )

(add-hook 'prog-mode-hook #'jpk/prog-mode-hook)

(use-package lsp-mode)

(use-package imenu
  :ensure nil
  :defer 2
  :config
  (setq imenu-auto-rescan t
        imenu-auto-rescan-maxout (* 1024 1024))
  :bind (("C-c i" . imenu))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C programming language

(use-package cc-mode
  :ensure nil
  :config
  (setq c-types-regexp
        (concat
         "\\<[_a-zA-Z][_a-zA-Z0-9]*_t\\>" "\\|"
         (regexp-opt '("unsigned" "int" "char" "float" "void") 'words)))
  (dolist (m '(c-mode c++-mode))
    (font-lock-add-keywords m `((,c-types-regexp . 'font-lock-type-face))))

  (defun jpk/c-mode-common-hook ()
    (setq tab-width 4)
    (setq c-basic-offset 4
          c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "bsd")))
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
    )

  (add-hook 'c-mode-common-hook #'jpk/c-mode-common-hook)

  :mode (("\\.asl\\'" . c-mode))
)

(use-package hideif
  :disabled
  :ensure nil
  :config
  (setq hide-ifdef-initially t
        hide-ifdef-shadow t)
  (add-hook 'c-mode-common-hook #'hide-ifdef-mode)

  (defun jpk/hide-ifdefs-on-save ()
    (add-hook 'after-save-hook #'hide-ifdefs 'append 'local))
  (add-hook 'c-mode-common-hook #'jpk/hide-ifdefs-on-save)
  )

(use-package ffap
  :ensure nil
  :config
  ;; TODO find a better way to do this
  (let ((avr-include-dir "/usr/avr/include"))
    (when (file-directory-p avr-include-dir)
      (add-to-list 'ffap-c-path avr-include-dir)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous programming modes

(use-package rust-mode)

(use-package groovy-mode)

(use-package gud
  :ensure nil
  :config
  :bind (:map gud-minor-mode-map
         ("C-c C-n" . gud-next)
         ("C-c C-s" . gud-step))
  )

(use-package make-mode
  :ensure nil
  :mode (("Makefile" . makefile-gmake-mode))
  :bind (:map makefile-gmake-mode-map
         ("M-n" . isearch-forward-symbol-dwim)
         ("M-p" . isearch-backward-symbol-dwim))
  )

(use-package bitbake
  :disabled
  :config
  (defun jpk/bitbake-mode-hook ()
    ;; FIXME upstream
    (setq comment-start "#"
          comment-stop "")
    (setq indent-line-function #'indent-relative-dwim)
    )
  (add-hook 'bitbake-mode-hook #'jpk/prog-mode-hook)
  (add-hook 'bitbake-mode-hook #'jpk/bitbake-mode-hook)

  (with-eval-after-load 'company
    (add-to-list 'company-dabbrev-code-modes 'bitbake-mode))

  :mode (("\\.bb\\(append\\|class\\)?\\'" . bitbake-mode)
         ("\\.inc\\'" . bitbake-mode)
         ("/tmp/work/.*/temp/log\\.do_.*\\'" . compilation-mode))

  :bind (:map bitbake-mode-map
         ("C-M-;" . insert-comment-bar)
         ("C-a" . mwim-beginning-of-line-or-code)
         ("C-e" . mwim-end-of-line-or-code))
  )

(use-package bitbake-modes
  :init
  ;; Basically bitbake-modes.el only requires other libraries in the
  ;; the package, but nothing autoloads bitbake-modes, so :config
  ;; never runs unless we require it manually.
  (with-eval-after-load 'bitbake
    (require 'bitbake-modes))

  :config
  (with-eval-after-load 'company
    (add-to-list 'company-dabbrev-code-modes 'bitbake-mode))

  (dolist (f '(bitbake-python-function-face
               bitbake-python-task-face
               bitbake-python-expansion-face
               bitbake-shell-function-face))
    (set-face-attribute f nil :background "#1f241f"))

  (defun bitbake-deploy-dir-dired (&optional bb-file)
    "Open the deploy dir of BB-FILE with `dired'."
    (interactive)
    (require 'bitbake-build-dir)
    (let ((dir (bitbake-recipe-build-dir bb-file)))
      (dired (expand-file-name "tmp/deploy/" dir))))

  :bind (:map bitbake-mode-map
         ("C-c C-d" . bitbake-deploy-dir-dired)
         ("C-c C-f" . bitbake-recipe-build-dir-dired))
  )

(use-package csharp-mode
  :disabled)

(use-package sh-script
  :ensure nil
  :config
  (defun jpk/sh-mode-hook ()
    (dolist (s '(paragraph-start paragraph-separate))
      (set s (default-value s))))
  (add-hook 'sh-mode-hook #'jpk/sh-mode-hook)

  ;; https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html
  (defun my-match-variables-in-quotes (limit)
    "Match variables in double-quotes in `sh-mode'."
    (with-syntax-table sh-mode-syntax-table
      (catch 'done
        (while (re-search-forward
                ;; `rx' is cool, mkay.
                (rx (or line-start (not (any "\\")))
                    (group "$")
                    (group
                     (or (and "{" (+? nonl) "}")
                        (and (+ (any alnum "_")))
                        (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
                limit t)
          (-when-let (string-syntax (nth 3 (syntax-ppss)))
            (when (= string-syntax 34)
              (throw 'done (point))))))))
  (font-lock-add-keywords
   'sh-mode '((my-match-variables-in-quotes
               (1 'default t)
               (2 font-lock-variable-name-face t))))
  )

(use-package cperl-mode
  :ensure nil
  :init
  (defalias 'perl-mode #'cperl-mode)

  :config
  (setq cperl-invalid-face nil)

  (defun jpk/cperl-mode-hook ()
    (dolist (x '(("!=" . ?≠)
                 ("->" . ?→)
                 ("=>" . ?⇒)
                 ))
      (add-to-list 'prettify-symbols-alist x))
    (cperl-set-style "BSD"))
  (add-hook 'cperl-mode-hook #'jpk/cperl-mode-hook)
  )

(use-package go-mode
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (defun jpk/go-mode-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save nil 'local))
  (add-hook 'go-mode-hook #'jpk/go-mode-hook)
  )

(use-package lsp-go
  :after lsp-mode
  :if (executable-find "go-langserver")
  :config
  ;; gocodecompletion causes high cpu?
  (setq lsp-go-language-server-flags
        (delete "-gocodecompletion" lsp-go-language-server-flags))

  (add-hook 'go-mode-hook #'lsp-go-enable)

  (defun jpk/lsp-go-revert-hook ()
    (add-hook 'after-revert-hook #'lsp-restart-workspace nil 'local))
  ;;(add-hook 'go-mode-hook #'jpk/lsp-go-revert-hook)
  )

(use-package company-go
  :after (company go-mode)
  :if (executable-find "gocode")
  )

(use-package go-scratch
  :after go-mode
  )

(use-package protobuf-mode
  :init
  (defun jpk/protobuf-mode-hook ()
    (c-add-style "jpk/protobuf-style"
                 '((c-basic-offset . 2)
                   (indent-tabs-mode . nil))
                 'set-p)
    (smart-tabs-mode 0))
  (add-hook 'protobuf-mode-hook #'jpk/protobuf-mode-hook)
  (add-hook 'protobuf-mode-hook #'highlight-operators-mode)
  (add-hook 'protobuf-mode-hook #'paren-face-mode)
  )

(use-package lua-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript

(use-package js2-mode
  :config
  (setq js2-strict-trailing-comma-warning nil
        js-switch-indent-offset 2
        js2-basic-offset 2)

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  (defun jpk/js2-mode-hook ()
    (setq tab-width js2-basic-offset))
  (add-hook 'js2-mode-hook #'jpk/js2-mode-hook)

  (require 'compile-eslint)
  (push 'eslint compilation-error-regexp-alist)

  :mode "\\.js\\'"
  :bind (:map js2-mode-map
         ("M-." . nil))
  )

(use-package json-mode)

(use-package jq-mode
  :disabled)

(use-package xref-js2
  :after js2-mode
  :init
  (defun jpk/xref-js2/js2-mode-hook ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil 'local))
  (add-hook 'js2-mode-hook #'jpk/xref-js2/js2-mode-hook)
  )

(use-package handlebars-mode)

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

(global-set-key (kbd "C-c e p") #'jpk/python-eval-insert-region)

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

(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4)
  (when (executable-find "ipython2")
    (setq python-shell-interpreter "ipython2"
          python-shell-interpreter-args "--simple-prompt -i"))
  :bind (:map python-mode-map
         ("<backtab>" . delete-indentation)
         ("S-TAB" . delete-indentation)
         ("C-c C-3" . python-2to3))
  )

(use-package pylint
  :after python
  :config
  ;; N.B. you probably want to set shell-file-name to something like
  ;; "C:/cygwin/bin/bash.exe" on Windows, because the default shell
  ;; will fuck up the command line.
  (setq pylint-options '("--jobs=4"
                         "--reports=n"
                         "--msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}]\n  {msg}'"
                         "--disable=C,R,locally-disabled"
                         ))

  (if (executable-find "pylint3")
      ;; debian
      (setq pylint-command "pylint3"
            pylint-alternate-pylint-command "pylint")
    ;; arch
    (setq pylint-command "pylint"
          pylint-alternate-pylint-command "pylint2"))

  :bind (:map python-mode-map
         ("C-c C-v" . pylint)
         ("C-c C-i" . pylint-insert-ignore-comment))
  )

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
  )
(add-hook 'python-mode-hook #'jpk/python-mode-hook)

(use-package sphinx-doc
  :after python
  :diminish sphinx-doc-mode
  :config
  (add-hook 'python-mode-hook #'sphinx-doc-mode)
  )

(use-package ipython-shell-send
  :disabled
  :after python
  )

(use-package rst
  :ensure nil
  :config
  (defun jpk/rst-mode-hook ()
    (setq fill-column 80))
  (add-hook 'rst-mode-hook #'jpk/rst-mode-hook)
  (add-hook 'rst-mode-hook #'flyspell-mode)
  )

(use-package pydoc-info)

(use-package python-info)

(use-package pynt
  :disabled
  )

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
  (local-set-key (kbd "C-c C-s") #'octave-send-buffer)
  (local-set-key (kbd "C-c C-l") #'octave-send-line)
  (local-set-key (kbd "C-c C-r") #'octave-send-region))

(add-hook 'octave-mode-hook #'jpk/octave-mode-hook)

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
(add-hook 'verilog-mode-hook #'jpk/verilog-mode-hook)

(add-to-list 'auto-mode-alist '("\\.vams\\'" . verilog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp

(use-package package-lint)
(use-package flycheck-package)

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

(global-set-key (kbd "C-c e") #'eval-and-replace-last-sexp)

(setq print-length 256
      print-level 16
      eval-expression-print-length print-length
      eval-expression-print-level print-level)

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

(use-package morlock
  :defer 1
  :init
  (global-morlock-mode 1)
  )

(use-package edit-list)

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  )

(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :init
  (add-hook 'lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  )

(defun jpk/lisp-modes-hook ()
  (eldoc-mode 1)
  (local-set-key (kbd "C-M-S-x") #'eval-region)
  (when (featurep 'highlight-operators)
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

  (add-hook 'before-save-hook #'check-parens nil 'local)
  )

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  "Add an imenu expression to find lines like \";;; foobar\"."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) 'append))
(add-hook 'emacs-lisp-mode-hook #'imenu-elisp-sections)

(add-hook 'lisp-mode-hook #'jpk/lisp-modes-hook)
(add-hook 'emacs-lisp-mode-hook #'jpk/lisp-modes-hook)
(add-hook 'ielm-mode-hook #'jpk/lisp-modes-hook)

(defalias 'emacs-repl 'ielm)

(use-package elisp-def
  :disabled
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-def-mode))
  )

(use-package bug-hunter
  :commands (bug-hunter-init-file)
  )

;; http://wikemacs.org/wiki/User's_Initialization_File#Debugging_the_Init_file
(defun test-init.el ()
  "Start emacs and try loading init.el."
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
          "emacs --batch --eval \"
(condition-case e
    (progn
      (load user-init-file)
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

;; just for fun, an ielm quine:
;; (let ((s "(let ((s %S)) (insert (format s s)))")) (insert (format s s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML/XML

(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(fset 'sgml-mode 'nxml-mode)

(use-package htmlize)

(use-package htmlize-view
  :ensure nil
  :after htmlize
  :commands (htmlize-view-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config files

(use-package conf-mode
  :ensure nil
  :mode ("\\.list\\'" "\\.rules\\'" "\\`fstab\\'" "\\.env\\'"
         "\\.env\\.sample\\'" "mkosi\\.default\\'")
  )

(use-package systemd)

(use-package yaml-mode
  :config
  (defun jpk/yaml-mode-hook ()
    (setq tab-width 2))
  (add-hook 'yaml-mode-hook #'jpk/yaml-mode-hook)
  )

(use-package dts-mode
  :config
  (defun dts--calculate-indentation ()
    (interactive)
    (save-excursion
      (let ((end (point-at-eol))
            (cnt 0)
            (initial-point (point)))
        (goto-char 0)
        (while (re-search-forward "\\([{}<>]\\)" end t)
          (if (string-match "[{<]" (match-string-no-properties 0))
              (setq cnt (1+ cnt))
            (setq cnt (1- cnt))))
        ;; subtract one if the current line has an opening brace since we
        ;; shouldn't add the indentation level until the following line
        (goto-char initial-point)
        (beginning-of-line)
        (when (re-search-forward "[{<]" (point-at-eol) t)
          (setq cnt (1- cnt)))
        cnt)))

  (defun jpk/dts-mode-hook ()
    (setq indent-tabs-mode t)
    (setq tab-width 4)
    (setq comment-start "// "
          comment-end ""))
  (add-hook 'dts-mode-hook #'jpk/dts-mode-hook)

  ;; FIXME push upstream
  (let ((table dts-mode-syntax-table))
    (modify-syntax-entry ?<  "(>" table)
    (modify-syntax-entry ?>  ")<" table))

  :mode "\\.its\\'"
  )

(use-package fvwm-mode
  :config
  (defun jpk/fvwm-mode-hook ()
    ;;(fvwm-enable-indentation)
    (local-set-key (kbd "RET") #'newline)
    (setq indent-line-function #'indent-relative-dwim)
    (hl-line-mode 1)
    (setq tab-width 4))
  (add-hook 'fvwm-mode-hook #'jpk/fvwm-mode-hook)

  :mode "\\.fvwm\\'"
  )

(use-package pkgbuild-mode
  :if (executable-find "pacman")
  :mode ("\\`PKGBUILD\\'" "\.install\\'")
  )

(use-package pacfiles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text and markup modes

(defun jpk/text-mode-hook ()
  (setq indent-line-function #'indent-relative-dwim)
  (when (featurep 'flyspell)
    (flyspell-mode 1))
  (when (featurep 'auto-complete)
    (setq ac-sources
          '(ac-source-dictionary ac-source-words-in-buffer ac-source-filename)))
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (hl-line-mode 1)
  )

(add-hook 'text-mode-hook #'jpk/text-mode-hook)
(add-hook 'text-mode-hook #'text-mode-hook-identify)

(use-package markdown-mode
  :init
  (defun jpk/markdown-mode-hook ()
    (setq fill-column 80))
  (add-hook 'markdown-mode-hook #'jpk/markdown-mode-hook)
  )

(use-package auctex
  :if (executable-find "pdflatex")

  :config
  (setq LaTeX-item-indent -1)

  (defun jpk/LaTeX-mode-hook ()
    (setq adaptive-wrap-extra-indent 0)
    (visual-line-mode 1)
    (when (featurep 'flyspell)
      (flyspell-mode 1))
    (setq fill-column 80)
    (setq indent-line-function #'LaTeX-indent-line)
    (when (featurep 'ac-math)
      (dolist (x '(ac-source-math-unicode
                   ac-source-math-latex
                   ac-source-latex-commands))
        (add-to-list 'ac-sources x)))
    (hl-line-mode 1)
    )
  (add-hook 'LaTeX-mode-hook #'jpk/LaTeX-mode-hook)

  :bind (:map LaTeX-mode-map
         ([remap next-error] . nil)
         ([remap previous-error] . nil)
         ("C-M-;" . insert-comment-bar))
  )

(use-package mediawiki)

(use-package sdcv
  :if (and (executable-find "sdcv")
         (file-exists-p "~/.stardict/dic/dictd_www.dict.org_web1913.dict.dz"))
  ;; TODO hydra
  )

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

(use-package sqlup-mode
  :commands (sqlup-mode)
  :init
  (add-hook 'sql-mode-hook #'sqlup-mode)
  )

(defun jpk/sql-mode-hook ()
  (setq adaptive-wrap-extra-indent 2)
  (visual-line-mode 1)

  (when (featurep 'wrap-region)
    (wrap-region-add-wrapper "`" "`"))

  (sql-highlight-ansi-keywords))

(with-eval-after-load "sql"

  ;;(define-key sql-interactive-mode-map (kbd "RET") #'insert-semicolon-and-send-input)

  ;;;;

  ;; FIXME broken
  (defun jpk/sqlite-args (orig &rest args)
    (let (sql-user sql-server insert-default-directory)
      (setq sql-database (if (and (stringp sql-database)
                                (file-exists-p sql-database))
                             sql-database
                           default-directory))
      (setq sql-database (read-file-name "SQLite file: "
                                         (file-name-directory sql-database)
                                         sql-database))
      (apply orig args)))
  (advice-add 'sql-sqlite :around #'jpk/sqlite-args)

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
            (apply #'concat
                   (cdr
                    (apply #'append nil
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

  (add-hook 'sql-interactive-mode-hook #'sql-rename-buffer)
  (add-hook 'sql-mode-hook #'jpk/sql-mode-hook)
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

(use-package dockerfile-mode)

(use-package docker-tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assembly

(use-package asm-mode
  :ensure nil
  :mode ("\\.dsl\\'" "\\.lst\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNDO/REDO

(use-package undo-tree
  :defer 2
  :init
  (setq undo-tree-mode-lighter "")

  :config
  (global-undo-tree-mode 1)

  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch

;; Allow page up/down, C-l, and anything else that doesn't move the
;; point during isearch.
(setq isearch-allow-scroll t)
(put 'recenter-top-bottom 'isearch-scroll t)

;; normally M-<tab> which is obviously not going to work
;; get a literal tab with C-q <tab>
(define-key isearch-mode-map (kbd "<tab>") #'isearch-complete)

(defun jpk/isearch-update-post-hook ()
  (when (and isearch-success (not isearch-just-started))
    (recenter)))

(add-hook 'isearch-update-post-hook #'jpk/isearch-update-post-hook)

;; make backspace behave in a more intuitive way
(define-key isearch-mode-map (kbd "<backspace>") #'isearch-del-char)

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

(define-key isearch-mode-map (kbd "<C-backspace>") #'isearch-unfail)

(define-key isearch-mode-map (kbd "M-k") nil)

;; quickly search for a string by selecting it and typing `C-s C-s`
(defvar isearch-auto-use-region-max-length 24
  "Upper threshold to automatically use the region in isearch.")
(defun jpk/isearch-auto-use-region (&rest args)
  "Automatically use region as search string unless it's too big or empty."
  (when (region-active-p)
    (let* ((b (region-beginning))
           (e (region-end))
           (l (- e b)))
      (when (and (< 0 l) (<= l isearch-auto-use-region-max-length))
        (add-to-history 'search-ring (buffer-substring-no-properties b e))
        (deactivate-mark)))))
(advice-add 'isearch-forward :before #'jpk/isearch-auto-use-region)
(advice-add 'isearch-backward :before #'jpk/isearch-auto-use-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep

;; recenter after running next-error
(setq next-error-recenter '(4))

(defun jpk/next-error-hook ()
  (let ((win (get-buffer-window next-error-last-buffer))
        (buf (buffer-name next-error-last-buffer))
        (recenter-redisplay nil))
    (when (and win (not (string= "*xref*" buf)))
      (with-selected-window win ;; this is what breaks xref
        (recenter)))))
(add-hook 'next-error-hook #'jpk/next-error-hook)

(use-package grep
  :ensure nil
  :config
  ;; FIXME: see new option grep-find-hide
  (defun jpk/grep/compilation-filter-hook ()
    (hide-lines-matching "^find"))
  (defun jpk/grep-mode-hook ()
    (setq adaptive-wrap-extra-indent 4)
    (visual-line-mode 1)
    (add-hook 'compilation-filter-hook #'jpk/grep/compilation-filter-hook nil 'local))
  (add-hook 'grep-mode-hook #'jpk/grep-mode-hook)

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

(use-package wgrep)

(use-package ripgrep
  :if (executable-find "rg")
  :pin melpa
  :init
  (defalias 'rg 'ripgrep-regexp)

  :config
  (setq ripgrep-arguments '("--no-ignore" "--smart-case"
                            "--max-columns" "1024"
                            "--search-zip"
                            "--sort path"))

  (setq ripgrep--match-regexp "\e\\[31m\\(.*?\\)\e\\[[0-9]*m")
  (defun jpk/ripgrep-search-mode-hook ()
    (setq adaptive-wrap-extra-indent 4)
    (visual-line-mode 1))
  (add-hook 'ripgrep-search-mode-hook #'jpk/ripgrep-search-mode-hook)
  )

(use-package rg
  :disabled ;; ripgrep is better
  :if (executable-find "rg")
  :config
  (add-hook 'rg-mode-hook #'wgrep-ag-setup)
  (add-to-list 'rg-custom-type-aliases '("dts" . "*.dts *.dtsi"))
  )

(use-package ag
  :disabled ;; ripgrep is better
  :if (executable-find "ag")
  :config
  (setq ag-highlight-search t)

  (defun jpk/ag-mode-hook ()
    (setq adaptive-wrap-extra-indent 4)
    (visual-line-mode 1))
  (add-hook 'ag-mode-hook #'jpk/ag-mode-hook)
  )

(use-package wgrep-ag
  :disabled ;; normal wgrep works with ripgrep
  :after (wgrep ag)
  :bind (:map ag-mode-map
         ("C-x C-q" . wgrep-change-to-wgrep-mode))
  )

(use-package grep-context
  :bind (:map grep-mode-map
         ("+" . grep-context-more-around-point)
         ("-" . grep-context-less-around-point)
         :map ripgrep-search-mode-map
         ("+" . grep-context-more-around-point)
         ("-" . grep-context-less-around-point))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and replace

(define-key query-replace-map (kbd "b") #'backup)
(define-key query-replace-map (kbd "B") #'backup)
(define-key query-replace-map (kbd "RET") #'act-and-show)

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
(setq replace-search-function #'search-forward-and-center)

(defun re-search-forward-and-center (regexp &optional bound noerror count)
  "Just like `re-search-forward', but centers the point in the window."
  (let ((ret (re-search-forward regexp bound noerror count)))
    (recenter)
    ret))
(setq replace-re-search-function #'re-search-forward-and-center)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; locate
;; front end for the Unix locate command

;; When the locate results buffer is reverted, updatedb is run, and
;; this makes it run as root.
(setq locate-update-path "/sudo::"
      locate-update-when-revert t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wrap-region
  :diminish wrap-region-mode
  :commands (wrap-region-mode)
  :init
  (add-hook 'prog-mode-hook #'wrap-region-mode)
  :config
  (defun jpk/wrap-region-after-wrap-hook ()
    (goto-char (1+ (region-end))))
  (add-hook 'wrap-region-after-wrap-hook #'jpk/wrap-region-after-wrap-hook)
  )

(show-paren-mode 1)

(use-package mic-paren
  :defer 1
  :config
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
(global-set-key (kbd "C-%") #'bounce-string-or-list)

(use-package transpose-params
  :ensure nil
  :bind (("C-M-S-t" . transpose-params))
  )

(use-package expand-region
  :config
  (defun isearch-yank-selection ()
    "Put selection from buffer into search string."
    (interactive)
    (when (region-active-p)
      (deactivate-mark))  ;; fully optional, but I don't like unnecessary highlighting
    (let ((isearch-case-fold-search nil))
      (isearch-yank-internal (lambda () (mark)))))

  :bind (("C-=" . er/expand-region)
         :map isearch-mode-map
         ("C-S-y" . isearch-yank-selection))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet (Yet Another Template Mode)

(use-package yasnippet-snippets
  :defer 2
  )

(use-package yasnippet
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (setq yas-prompt-functions (cons 'yas-ido-prompt
                                   (remove 'yas-ido-prompt
                                           yas-prompt-functions)))
  (yas-global-mode 1)

  :bind (("C-c y" . yas-insert-snippet)
         :map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil))
  )

(use-package yankpad
  :config
  (setq yankpad-file (no-littering-expand-var-file-name "yankpad.org"))

  :bind (("C-c Y" . yankpad-insert))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous keybindings
;; the best way to find the symbol for a key is to run C-h k <key>

;; normally bound to C-<mouse-3>
(global-set-key (kbd "<down-mouse-3>") #'mouse-popup-menubar-stuff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changing case (capitalization)

(use-package string-inflection
  :config
  (defhydra hydra/string-inflection (:color blue)
    "String Inflection"
    ("c" string-inflection-lower-camelcase "camelCase")
    ("C" string-inflection-camelcase "CamelCase")
    ("m" string-inflection-camelcase "CamelCase")
    ("_" string-inflection-underscore "under_score")
    ("l" string-inflection-underscore "under_score")
    ("k" string-inflection-kebab-case "kebab-case")
    ("-" string-inflection-kebab-case "kebab-case")
    ("t" string-inflection-capital-underscore "Title_Case")
    ("i" string-inflection-capital-underscore "Title_Case")
    ("u" string-inflection-upcase "UP_CASE")
    )
  :bind (("C-x C-l" . hydra/string-inflection/body)
         ("C-x C-u" . hydra/string-inflection/body))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete things

(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-S-SPC") #'delete-blank-lines)
(global-set-key (kbd "C-d") #'delete-forward-char)

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

(global-set-key (kbd "C-S-k") #'copy-line)

(use-package browse-kill-ring
  :defer 2
  :config
  (defun jpk/maybe-browse-kill-ring (orig &rest args)
    "Run `browse-kill-ring' if last command was not `yank'."
    (if (eq last-command 'yank)
        (apply orig args)
      (browse-kill-ring)))
  (advice-add 'yank-pop :around #'jpk/maybe-browse-kill-ring)
  )

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

    (when (or (bolp) (looking-back "^[[:space:]]+" (point-min)))
      (beginning-of-line-text))))

(setq-default indent-line-function #'indent-relative-dwim)

;; new buffers default to using 4 spaces for indent
(setq-default tab-width 4
              indent-tabs-mode nil)

(use-package smart-tabs-mode
  :commands (smart-tabs-mode)
  :init
  (add-hook 'c-mode-common-hook #'smart-tabs-mode)
  :config
  (smart-tabs-advice cperl-indent-line cperl-indent-level)
  (smart-tabs-advice c-indent-line     c-basic-offset)
  (smart-tabs-advice c-indent-region   c-basic-offset)
  )

;; shift-tab should undo what tab does
(global-set-key (kbd "<backtab>") #'delete-indentation)
(global-set-key (kbd "S-TAB") #'delete-indentation)

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

(global-set-key (kbd "C-0") #'move-left-1)
(global-set-key (kbd "C-9") #'move-right-1)

(use-package smart-shift
  :defer 1
  :config
  (defun move-left-dwim (beg en &optional arg)
    "Move the active region or the current line The Right Number of columns to the left."
    (interactive "*r\np")
    (let* ((shift (or (and (boundp 'smart-shift-indentation-level)
                        smart-shift-indentation-level)
                     (and (functionp 'smart-shift-infer-indentation-level)
                        (smart-shift-infer-indentation-level))
                     tab-width))
           (indent-tabs-mode (or indent-tabs-mode
                                (and (boundp 'smart-tabs-mode) smart-tabs-mode))))
      (move-left-1 beg en (* arg shift))))

  (defun move-right-dwim (beg en &optional arg)
    "Move the active region or the current line The Right Number of columns to the right."
    (interactive "*r\np")
    (move-left-dwim beg en (- arg)))

  :bind (("C-)" . move-left-dwim)
         ("C-(" . mode-right-dwim))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fill

(use-package fill
  :ensure nil
  :init
  ;; see also fill-individual-paragraphs

  (setq-default fill-column 72)

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
  (defun jpk/scroll-left (&rest args)
    "Scroll to the left."
    (scroll-right (window-hscroll)))
  (advice-add 'fill-paragraph :after #'jpk/scroll-left)

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
          (goto-char (point-min))
          (while (re-search-forward "\\s-*,")
            (replace-match ", "))
          (align-regexp (point-min) (point-max)
                        ",\\(\\s-*\\)" 1 1 t)))))
  )

(use-package ialign
  :disabled)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adaptive word wrap

(use-package adaptive-wrap
  :hook
  (visual-line-mode . adaptive-wrap-prefix-mode)

  :config
  (setq-default adaptive-wrap-extra-indent 2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scrolling

(setq hscroll-margin 1 ;; how close to the edge to get before scrolling
      hscroll-step 1 ;; how many columns to scroll once past hscroll-margin
      auto-hscroll-mode t
      scroll-preserve-screen-position t
      auto-window-vscroll nil ;; less jittery
      scroll-conservatively most-positive-fixnum)

;; scroll the other buffer
(global-set-key (kbd "S-<next>") #'scroll-other-window)
(global-set-key (kbd "S-<prior>") #'scroll-other-window-down)

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

(defun scroll-left-8 ()
  "Like `scroll-left' with an arg of 8."
  (interactive)
  (scroll-left 8))

(defun scroll-right-8 ()
  "Like `scroll-right' with an arg of 8."
  (interactive)
  (scroll-right 8))

(global-set-key (kbd "M-<mouse-5>") #'scroll-left-8)
(global-set-key (kbd "M-<mouse-4>") #'scroll-right-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el ends here
