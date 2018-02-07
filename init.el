;;; init.el --- jpkotta's emacs init file

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

(setq gc-cons-threshold most-positive-fixnum)
(defun jpk/emacs-startup-hook ()
  (message "init.el loaded in %s." (emacs-init-time))
  (setq gc-cons-threshold (standard-value 'gc-cons-threshold)))
(add-hook 'emacs-startup-hook #'jpk/emacs-startup-hook)

;; use Emacs bindings in all GTK apps:
;; $ gsettings set org.gnome.desktop.interface gtk-key-theme Emacs

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
        auctex
        backup-walker
        bm
        boxquote
        browse-kill-ring
        buffer-move
        csharp-mode
        csv-mode
        dictionary
        diff-hl
        easy-repeat
        expand-region
        go-mode
        go-scratch
        hide-lines
        highlight-numbers
        highlight-operators
        highlight-quoted
        htmlize
        ibuffer-projectile
        ibuffer-tramp
        ;;list-unicode-display
        lua-mode
        markdown-mode
        mediawiki
        mic-paren
        modeline-posn ; deprecated
        morlock
        mwim
        paren-face
        rainbow-mode
        save-visited-files
        smart-shift
        smart-tabs-mode
        sqlup-mode
        syntax-subword
        undo-tree
        ))

(setq package-user-dir (expand-file-name
                        (format "elpa-%d" emacs-major-version)
                        user-emacs-directory))

(package-initialize)
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("gnu" . 10)
                                   ("melpa" . 0)))

(defun jpk/install-selected-packages (&optional arg)
  (interactive "p")
  (if (not arg) ;; not called interactively, see called-interactively-p
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

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(use-package no-littering
  :pin melpa
  )

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

(setq custom-safe-themes t)
(use-package calmer-forest-theme
  :init
  (load-theme 'calmer-forest 'noconfirm)

  (set-face-attribute 'default nil
                      :family (if (string= system-type "windows-nt")
                                  "Consolas"
                                "DejaVu Sans Mono")
                      :foreground "green1"
                      :height 100)
  (set-face-attribute 'mode-line nil :height 1.0)

  :config
  (custom-theme-set-faces
   'calmer-forest

   '(fringe ((t (:background "grey10" :foreground "dark green"))))
   '(highlight ((t (:background "#001000"))))

   '(mode-line-buffer-id ((t (:weight bold))))
   '(modelinepos-region ((t (:inverse-video t))))

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

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-undecided "(EOL?)"
      eol-mnemonic-unix "(LF)")

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
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

(defun jpk/mouse-select-window (&rest args)
  "no mouse select window if minibuffer active"
  (or (active-minibuffer-window)
     (window-minibuffer-p)
     (minibuffer-window-active-p (selected-window))))
(advice-add 'mouse-set-point :before-until #'jpk/mouse-select-window)
(advice-add 'mouse-set-region :before-until #'jpk/mouse-select-window)

;; cancel everything, including active minibuffers and recursive edits
(global-set-key (kbd "C-M-g") 'top-level)

;;(delete-selection-mode 1)

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
;;; tags

;; GTAGSLABEL has no effect unless there's a ~/.globalrc
(let ((rcfile "~/.globalrc")
      (dist-rcfiles '("/usr/share/gtags/gtags.conf"
                      "/usr/local/share/gtags/gtags.conf")))
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
      (setenv "GTAGSLABEL" orig-label))))

;; xref is the unified cross reference subsystem.  ggtags actually
;; uses it for some things like tag history.  It's labeled as
;; experimental in Emacs 25, and seems like it needs more work.  gxref
;; is a package to use global as an xref backend.  Once xref is more
;; mature, it will probably be better than ggtags.

(use-package gxref
  :after xref
  :if (executable-find "global")
  :config
  ;; (remove-hook 'next-error-hook #'jpk/next-error-hook)
  ;; (setq next-error-recenter nil)

  (defun gxref-create-db-pygments (project-root-dir)
    "Like `gxref-create-db', but set GTAGSLABEL to pygments.

Pygments supports more languages, but is much slower than the
default label."
    (interactive "DCreate GTAGS db in directory: ")
    (let ((gxref-gtags-label "pygments"))
      (gxref-create-db project-root-dir)))

  (add-hook 'xref-backend-functions #'gxref-xref-backend)

  :bind (("M-/" . xref-find-references))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile

(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-tags-backend 'xref
        projectile-tags-command "gtags -f \"%s\" %s")
  (projectile-global-mode 1)
  )

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
      (flyspell-mode 1))
    (hl-line-mode 1)
    )
  (add-hook 'org-mode-hook #'jpk/org-mode-hook)

  (setq org-ellipsis "…"
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
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(ipython . t))
  (org-babel-reload-languages)
  )

(use-package ob-async
  :after org
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

(use-package keychain-environment
  :init
  (keychain-refresh-environment)
  )

(use-package ssh-config-mode
  :mode (".ssh/config\\'" "sshd?_config")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C SubWordMode
;; FindSubWordsInCamelCase

;; FIXME reimplement using subword-{forward,backward}-function
;; (no need to remap bindings, etc.)

(with-library 'syntax-subword
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers and strings

(use-package evil-numbers
  :bind (("C-c =" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hexl mode

(use-package nhexl-mode
  ;; TODO
  ;; indirect buffer (but set-buffer-multibyte doesn't work with that)
  ;; mode map
  ;; hexl-bits configuration
  ;; bytes not decoded °
  :disabled
  :config
  (defun nhexl-goto-addr (addr)
    "Move vertically down ARG blocks of 256 bytes (16 lines)."
    (interactive "nAddress (use `#x' prefix for hex): ")
    (let ((pos (byte-to-position (1+ addr))))
      (cond
       (pos (goto-char pos))
       ((> 0 addr) (goto-char (point-min)))
       (t (goto-char (point-max))))))

  (defun nhexl-current-addr ()
    (interactive)
    (position-bytes (point)))

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

  (setq nhexl-mode-map (make-sparse-keymap))
  :bind (:map nhexl-mode-map
         ("<next>" . nhexl-forward-256)
         ("<prior>" . nhexl-backward-256)
         ("C-<next>" . nhexl-forward-1k)
         ("C-<prior>" . nhexl-backward-1k)
         ("C-S-<next>" . nhexl-forward-4k)
         ("C-S-<next>" . nhexl-backward-4k))
  )

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

  (defun jpk/rectangle-args (&rest args)
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
          (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))

  (advice-add 'cua-sequence-rectangle :filter-args #'jpk/rectangle-args)

  :bind (("C-<return>" . cua-rectangle-mark-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode 1)

  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-to-list 'revert-without-query "\\.rom\\'")

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

(use-package atomic-chrome)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completion

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  )

(defun jpk/dabbrev-friend-buffer (other-buffer)
  (and (dabbrev--same-major-mode-p other-buffer)
     (< (buffer-size other-buffer) (* 1 1024 1024))))
(setq dabbrev-friend-buffer-function 'jpk/dabbrev-friend-buffer)

(use-package auto-complete
  :disabled
  :diminish auto-complete-mode
  :init
  (use-package fuzzy)
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

(use-package ac-c-headers
  :after auto-complete)
(use-package ac-math
  :after auto-complete)

(use-package company
  ;;:disabled
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
        company-search-regexp-function 'company-search-flex-regexp
        company-tooltip-align-annotations t)

  ;;(setq company-frontends (standard-value 'company-frontends))
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-backends (standard-value 'company-backends))
  ;;(setq company-backends '(company-dabbrev-code company-dabbrev))

  (setq company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers t ;; same major-mode
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)

  :bind (:map company-active-map
         ;;("M-i" . company-select-previous)
         ;;("M-k" . company-select-next)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous)
         ("<tab>" . company-complete-common-or-cycle)
         ("TAB" . company-complete-common-or-cycle)
         ("C-s" . company-filter-candidates)
         :map company-search-map
         ;;("M-i" . company-select-previous)
         ;;("M-k" . company-select-next)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous)
         ("<tab>" . company-complete-common-or-cycle)
         ("TAB" . company-complete-common-or-cycle)
         )
  )

;; hippie-expand is like dabbrev-expand, but more
;;(global-set-key (kbd "M-/") #'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flyspell

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
  :ensure nil
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
  :after ido
  :init
  (ido-ubiquitous-mode 1))

(use-package amx
  :init
  (amx-mode 1))

(use-package smex
  :disabled
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

  :bind (("C-x o" . ace-window))
  )

;; Save point position per-window instead of per-buffer.
(use-package winpoint
  :disabled
  :init
  (winpoint-mode 1))

(setq help-window-select 'never)

(use-package shackle
  :init
  (shackle-mode 1)

  :config
  (setq shackle-default-rule '(:inhibit-window-quit t)
        shackle-rules
        '(("[*]Man .*" :select t :regexp t)
          (completion-list-mode :inhibit-window-quit nil :align 'below :size 0.3)
          (("*vc-incoming*" "*vc-outgoing*") :same t)
          ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC mode

(use-package hgrc-mode)

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
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column col)))

  (define-key vc-dir-mode-map (kbd "SPC") 'vc-dir-toggle)

  )

(defun jpk/suppress-messages (orig &rest args)
  "Suppress any messages using `with-temp-message'."
  (with-temp-message ""
    (apply orig args)))

;; toggle-read-only prints an annoying message
(advice-add 'toggle-read-only :around #'jpk/suppress-messages)

(use-package magit
  :if (executable-find "git")
  :init
  ;; show margin (author+time) in a magit log buffer with L d
  (setq magit-log-margin '(nil age-abbreviated magit-log-margin-width :author 11))
  :config
  (setq magit-diff-refine-hunk 'all)
  (add-hook 'magit-diff-mode-hook #'jpk/diff-mode-hook)
  (global-magit-file-mode 1)

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

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-switch-to-status-buffer)
         ("C-x M-g" . magit-dispatch-popup))
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

(require 'ediff-tweak)

(advice-add 'ediff-setup-windows :around #'jpk/allow-window-shrinking)

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

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix (read-kbd-macro "C-c c"))

  :config
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

  :bind (:map smerge-basic-map
         ("h" . smerge-help)
         ("c" . smerge-quick-keys))
  :bind-keymap ("C-c c" . smerge-mode-map)
  )

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

(defun jpk/eob (&rest args)
  "Move to end of buffer."
  (goto-char (point-max)))
(advice-add 'comint-send-input :before #'jpk/eob)

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

(use-package sane-term
  :bind (("C-c t" . sane-term))
  )

(use-package term
  :ensure nil
  :config

  ;; t is usually better for shell terminals, but nil is better for
  ;; serial terminals
  (setq term-suppress-hard-newline nil)
  (setq term-buffer-maximum-size 32768)

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
      (term-reset-terminal)
      (term-send-raw-string "\C-l")))

  (defun jpk/term-mode-hook ()
    (setq cua--ena-cua-keys-keymap nil)
    (when (featurep 'yasnippet)
      (yas-minor-mode 0))
    (setq-local revert-buffer-function #'term-revert-buffer)
    )
  (add-hook 'term-mode-hook 'jpk/term-mode-hook)

  :bind (:map term-raw-map
         ("C-x" . nil)
         ("C-h" . nil)
         ("<ESC>" . nil)
         ("C-c C-e" . term-send-esc)
         ("C-s" . isearch-forward)
         ("C-r" . isearch-backward)
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
         ("C-S-t" . sane-term-create)
         ("<C-prior>" . sane-term-prev)
         ("<C-next>" . sane-term-next)
         ("C-c C-v" . term-send-raw) ;; quote
         ("C-c C-x" . term-send-raw)) ;; C-x
  )

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

(defun copy-eterm-color-terminfo (hostspec)
  "Copy the eterm-color terminfo file to a remote host.
HOSTSPEC is a tramp host specification, e.g. \"/ssh:HOSTSPEC:/remote/path\"."
  (interactive
   (let ((hosts (mapcar (lambda (x) (cond ((stringp x) x)
                                     ((null (car x)) (cadr x))
                                     (t (concat (car x) "@" (cadr x)))))
                        (apply 'append
                               (mapcar
                                (lambda (x) (cl-remove-if-not 'identity (apply (car x) (cdr x))))
                                (tramp-get-completion-function "ssh"))))))
     (list (completing-read "Hostname: " hosts nil 'confirm nil nil hosts nil))))
  (let ((destdir (format "/ssh:%s:.terminfo/e/" hostspec)))
    (ignore-errors
      (dired-create-directory destdir))
    (copy-file (concat data-directory "e/eterm-color")
               (concat destdir "eterm-color"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell

(use-package eshell
  :ensure nil
  :config
  (require 'em-smart)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)

  (defun jpk/eshell-mode-hook ()
    (defun eshell--revert-buffer-function (&optional ignore-auto noconfirm)
      (eshell/clear))
    (setq-local revert-buffer-function #'eshell--revert-buffer-function))
  (add-hook 'eshell-mode-hook #'jpk/eshell-mode-hook)

  (defun jpk/eshell-prompt-function ()
    (format "[%s][%s]\n$ "
            (format-time-string "%H:%M:%S")
            (abbreviate-file-name (eshell/pwd))))
  (setq eshell-prompt-function #'jpk/eshell-prompt-function
        eshell-prompt-regexp "^[$] ")

  (defun eshell-insert-history ()
    (interactive)
    (insert (completing-read
             "Eshell history: "
             (delete-dups (ring-elements eshell-history-ring)))))
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

(use-package dired
  :ensure nil
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^.].*$"
        dired-omit-verbose nil)
  (delete ".bin" dired-omit-extensions)
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
  :bind (:map dired-mode-map
         ("C-c C-c" . dired-ranger-copy)
         ("C-c C-x" . dired-ranger-move)
         ("C-c C-v" . dired-ranger-paste))
  )

(use-package view
  :ensure nil
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
                (error "Invalid separator: %s" x)))
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
  (define-key ibuffer-mode-map (kbd "SPC") 'ibuffer-mark-toggle)
  )

;; run when ibuffer buffer is created
(defun jpk/ibuffer-mode-hook ()
  (ibuffer-auto-mode 1)
  (hl-line-mode 1)
  (setq truncate-lines t))
(add-hook 'ibuffer-mode-hook 'jpk/ibuffer-mode-hook)

;; run when ibuffer command is invoked
(defun jpk/ibuffer-hook ()
  (ibuffer-set-filter-groups-by-mode)
  (setq ibuffer-sorting-mode 'pathname))
(add-hook 'ibuffer-hook 'jpk/ibuffer-hook)

;; jump to most recent buffer
(defun jpk/jump-to-most-recent (orig &rest args)
  "Move point to most recent."
  (let ((recent-buffer-name (buffer-name)))
    (apply orig args)
    (unless (string-match-p "*Ibuffer*" recent-buffer-name)
      (ibuffer-jump-to-buffer recent-buffer-name))))
(advice-add 'ibuffer :around #'jpk/jump-to-most-recent)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immortal-scratch

(use-package immortal-scratch
  :init
  (immortal-scratch-mode 1)

  :config
  (setq immortal-scratch-switch-to-respawned-scratch t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; figlet

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

(use-package multi-compile
  :config

  (defun locate-repo-dir (&optional file-or-dir)
    "Find the root of the version control repository."
    (let* ((file-or-dir (or file-or-dir (buffer-file-name) default-directory))
           (file-dir (if (file-directory-p file-or-dir)
                         file-or-dir
                       (file-name-directory file-or-dir)))
           (root-dir (vc-call-backend (vc-deduce-backend) 'root file-dir)))
      root-dir))

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

  :bind (("C-c b" . multi-compile-run))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-todo

(use-package hl-todo
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'bitbake-mode-hook #'hl-todo-mode)

  :config
  (setq hl-todo-keyword-faces
        (mapcar (lambda (w) (cons w "red"))
                '("TODO" "FIXME" "KLUDGE" "XXX" "DEBUG")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Programming

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

(use-package iedit
  :bind (("C-c i" . iedit-mode))
  )

(use-package visual-regexp
  :bind (([remap replace-regexp] . vr/replace)
         ([remap query-replace-regexp] . vr/query-replace))
  )

(defun jpk/prog-mode-hook ()
  ;;(smart-tabs-mode 0) ;; default to using spaces

  ;; check spelling on the fly, but only in comments and strings
  (when (featurep 'flyspell)
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

  (hl-line-mode 1)

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
;;; miscellaneous programming modes

(use-package rust-mode)

(use-package groovy-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUD (Grand Unified Debugger)

(use-package gud
  :ensure nil
  :bind (:map gud-minor-mode-map
         ("C-c C-n" . gud-next)
         ("C-c C-s" . gud-step))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bitbake

(use-package bitbake
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-script

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
  (setq pylint-options '("--rcfile=./.pylintrc"
                         "--jobs=4"
                         "--reports=n"
                         "--msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}]\n  {msg}'"
                         "--disable=C,R,locally-disabled"
                         ))

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
(add-hook 'python-mode-hook 'jpk/python-mode-hook)

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
  (add-hook 'rst-mode-hook #'flyspell-mode)
  )

(use-package pydoc-info)

(use-package python-info)

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

(global-set-key (kbd "C-c e") 'eval-and-replace-last-sexp)

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

(with-library 'morlock
  (font-lock-add-keywords 'emacs-lisp-mode morlock-font-lock-keywords)
  (font-lock-add-keywords 'lisp-interaction-mode morlock-font-lock-keywords))

(use-package edit-list)

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  )

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

(defun jpk/ielm-mode-hook ()
  (when (featurep 'auto-complete
    (dolist (x '(ac-source-functions
                 ac-source-variables
                 ac-source-features
                 ac-source-symbols
                 ac-source-words-in-same-mode-buffers))
      (add-to-list 'ac-sources x)))))
(add-hook 'ielm-mode-hook 'jpk/ielm-mode-hook)

(add-hook 'ielm-mode-hook 'jpk/lisp-modes-hook)

(defalias 'emacs-repl 'ielm)

;; just for fun, an ielm quine:
;; (let ((s "(let ((s %S)) (insert (format s s)))")) (insert (format s s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FVWM

(use-package fvwm-mode
  :config
  (defun jpk/fvwm-mode-hook ()
    ;;(fvwm-enable-indentation)
    (local-set-key (kbd "RET") 'newline)
    (setq indent-line-function 'indent-relative-dwim)
    (hl-line-mode 1)
    (setq tab-width 4))
  (add-hook 'fvwm-mode-hook 'jpk/fvwm-mode-hook)

  :mode "\\.fvwm\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PKGBUILD

(use-package pkgbuild-mode
  :if (executable-find "pacman")
  :mode ("\\`PKGBUILD\\'" "\.install\\'")
  )

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

(use-package config-mode
  :ensure nil
  :mode ("\\.list\\'" "\\.rules\\'" "\\`fstab\\'" "\\.env\\'")
  )

(use-package systemd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YAML mode

(use-package yaml-mode
  :config
  (defun jpk/yaml-mode-hook ()
    (setq tab-width 2))
  (add-hook 'yaml-mode-hook #'jpk/yaml-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dts (flattened device tree)

(use-package dts-mode
  :config
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-mode

(defun jpk/text-mode-hook ()
  (setq indent-line-function 'indent-relative-dwim)
  (when (featurep 'flyspell)
    (flyspell-mode 1))
  (when (featurep 'auto-complete)
    (setq ac-sources
          '(ac-source-dictionary ac-source-words-in-buffer ac-source-filename)))
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (hl-line-mode 1)
  )

(add-hook 'text-mode-hook 'jpk/text-mode-hook)
(add-hook 'text-mode-hook 'text-mode-hook-identify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LaTeX (AUCTeX mode)

(setq LaTeX-item-indent -1)

(defun jpk/LaTeX-mode-hook ()
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (when (featurep 'flyspell)
    (flyspell-mode 1))
  (setq fill-column 80)
  (local-set-key (kbd "C-M-;") 'insert-comment-bar)
  (setq indent-line-function 'LaTeX-indent-line)
  (when (featurp 'ac-math)
    (dolist (x '(ac-source-math-unicode
                 ac-source-math-latex
                 ac-source-latex-commands))
      (add-to-list 'ac-sources x)))
  (local-set-key [remap next-error] nil)
  (local-set-key [remap previous-error] nil)
  (hl-line-mode 1)
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

(use-package asm-mode
  :ensure nil
  :mode ("\\.dsl\\'" "\\.lst\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNDO/REDO

(use-package undo-tree
  :init
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode 1)

  :config
  (advice-add 'undo-tree-visualize :before #'jpk/save-window-state)
  (advice-add 'undo-tree-visualizer-quit :after #'jpk/restore-window-state)

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
(define-key isearch-mode-map (kbd "<tab>") 'isearch-complete)

(defun jpk/isearch-update-post-hook ()
  (when (and isearch-success (not isearch-just-started))
    (recenter)))

(add-hook 'isearch-update-post-hook #'jpk/isearch-update-post-hook)

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

(use-package ripgrep
  :if (executable-find "rg")
  :init
  (defalias 'rg 'ripgrep-regexp)
  :config
  (defun jpk/ripgrep-search-mode-hook ()
    (setq adaptive-wrap-extra-indent 4)
    (visual-line-mode 1))
  (add-hook 'ripgrep-search-mode-hook #'jpk/ripgrep-search-mode-hook)
  (add-hook 'ripgrep-search-mode-hook #'wgrep-setup)

  :bind (:map ripgrep-search-mode-map
         ("C-x C-q" . wgrep-change-to-wgrep-mode))
  )

(use-package projectile-ripgrep
  :after projectile
  :bind (:map projectile-command-map
         ("s r" . projectile-ripgrep))
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

(use-package wrap-region
  :diminish wrap-region-mode
  :config
  (defun jpk/wrap-region-after-hook ()
    (goto-char (1+ wrap-region-end)))
  (add-hook 'wrap-region-after-hook #'jpk/wrap-region-after-hook)
  )

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
  (defun jpk/maybe-browse-kill-ring (orig &rest args)
    "Run `browse-kill-ring' if last command was not `yank'."
    (if (eq last-command 'yank)
        (apply orig args)
      (browse-kill-ring)))
  (advice-add 'yank-pop :around #'jpk/maybe-browse-kill-ring))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adaptive word wrap

(use-package adaptive-wrap
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

  :config
  (put 'adaptive-wrap-extra-indent 'safe-local-variable 'integerp)
  (setq-default adaptive-wrap-extra-indent 2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scrolling

(setq hscroll-margin 1 ;; how close to the edge to get before scrolling
      hscroll-step 1 ;; how many columns to scroll once past hscroll-margin
      auto-hscroll-mode t)

(setq scroll-preserve-screen-position t)

;; makes scrolling less jittery
(setq auto-window-vscroll nil
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
