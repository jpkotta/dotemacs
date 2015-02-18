;;; init.el --- jpkotta's emacs init file

;; Copyright (C) Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>

(message "Loading jpkotta's init.el.")

(defun make-tictoc ()
  (current-time))
(defmacro tic (tt)
  `(setq ,tt (current-time)))
(defun toc (tt)
  (- (float-time (current-time))
     (float-time tt)))

;; profile this file's evaluation time
(setq init-el-time (make-tictoc))

;; this init file uses principles from:
;; http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO

;; fix ac-source-yasnippet to work with latest yasnippet

;; Patches
;;
;; ibuffer
;; graphlog stuff in vc-hg
;; pretty-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paths

;; add my elisp files to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; this is a place to try out random elisp files
(add-to-list 'load-path (concat user-emacs-directory "staging"))

;; info directory
(with-eval-after-load "info"
  (add-to-list 'Info-additional-directory-list "~/.info"))

;; a place to put various
(setq emacs-persistence-directory (concat user-emacs-directory "persistence/"))
(unless (file-exists-p emacs-persistence-directory)
  (make-directory emacs-persistence-directory t))

;; set up specific to the local machine
(require 'local-init nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(setq jpk-packages
      '(
        ac-dabbrev
        adaptive-wrap
        anchored-transpose
        auctex
        aurel
        auto-complete
        backup-walker
        bm
        boxquote
        buffer-move
        csharp-mode
        csv-mode
        diff-hl
        diminish
        dired+
        drag-stuff
        evil-numbers
        expand-region
        figlet
        findr
        flex-isearch
        full-ack
        fvwm-mode
        hide-lines
        highlight-numbers
        ;;highlight-operators
        highlight-quoted
        htmlize
        ido-ubiquitous
        isearch+
        keychain-environment
        lua-mode
        markdown-mode
        mediawiki
        mic-paren
        modeline-posn
        morlock
        mouse+
        multi-term
        openwith
        paren-face
        ;;php-mode
        pkgbuild-mode
        projectile
        pylint
        ;;python-pep8
        rainbow-mode
        save-visited-files
        sqlup-mode
        ssh-config-mode
        smart-tabs-mode
        smex
        ;;syntax-subword
        undo-tree
        wgrep
        workgroups
        wrap-region
        yasnippet
        ))

(setq package-user-dir (concat user-emacs-directory "elpa-" emacs-version))
  
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg jpk-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `jpk-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (memq x jpk-packages))
                               (not (package-built-in-p x))
                               (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom

;; store settings from customization system in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(message "loading customizations from %s" custom-file)
(let ((custom-el-time (make-tictoc)))
  (load custom-file 'noerror)
  (message "custom.el loaded in %f s" (toc custom-el-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance 

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist '((vertical-scroll-bars . right)
                            (menu-bar-lines . 0)
                            (background-mode . dark)
                            (tool-bar-lines . 0)
                            (width . 81)))

(load-library "calm-forest-theme.el")
(load-theme 'calm-forest 'noconfirm)

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
                            (car (split-string system-name "\\.")))
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

;; cursor
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

(setq eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-undecided "(EOL?)"
      eol-mnemonic-unix "(LF)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(when (and (string= system-type "windows-nt")
         (executable-find "bash"))
  (setq shell-file-name (executable-find "bash")))

(defmacro with-library (feature &rest body)
  "Evaluate BODY only if FEATURE is provided.  (require FEATURE) will be attempted."
  (declare (indent defun))
  `(when (require ,feature nil 'noerror)
     ,@body))

(defun setup-RAIDE-file ()
  "View RAIDE files from Eagle."
  (interactive)
  (recode-region (point-min) (point-max)
                 'cp437-dos 'iso-latin-1-dos)
  (buffer-face-set 'fixed-pitch)
  (view-mode))

(defun defadvice-list (funclist class name lambda-expr)
  "A convenience function to define the same advice on several
  functions at once.  FUNCLIST is a list of functions.  CLASS is
  one of `before', `after', or `around'.  NAME is a name for the
  advice.  LAMBDA-EXPR is a lambda expression that is the body of
  the advice.  The advice is not protected, is put at the
  beginning of the functions' advice lists, and is activated
  immediately."
 (dolist (func funclist)
    (ad-add-advice func
                   `(,name
                     nil ;; protect
                     t ;; activate
                     (advice . ,lambda-expr))
                   class
                   'first) ;; position
    (ad-activate func)))

(defun suspend-frame-if-not-gui ()
  "Like `suspend-frame', but does not suspend GUI frames."
  (interactive)
  (if (display-graphic-p)
      (message "Use `M-x suspend-frame' instead.")
    (suspend-frame)))

(global-set-key (kbd "C-x C-z") 'suspend-frame-if-not-gui)

(defun pl-tr (STRING FROM TO)
  "perlish transpose: similar to STRING =~ tr/FROM/TO/"
  (replace-regexp-in-string
   (concat "\[" FROM "\]")
   (lambda (s)
     (string (elt TO (cl-search s FROM))))
   STRING))

;; Don't create the .#filename files and don't ask about stealing.
(setq create-lockfiles nil)

;; enable with M-x sticky-buffer-mode
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; from http://svn.red-bean.com/repos/kfogel/trunk/.emacs
(defmacro do-on-lines (start end &rest body)
  "Run BODY at each line start of every line from START to END."
  (declare (indent defun))
  `(save-excursion
     (save-restriction
       (save-match-data
         (goto-char ,start)
         (while (< (point) ,end)
           (beginning-of-line)
           ,@body
           (forward-line 1))))))

;; not used anywhere but interesting
(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
    ;; Otherwise, take an element, e, out of the bag.
    ;; Generate all permutations of the remaining elements,
    ;; And add e to the front of each of these.
    ;; Do this for all possible e to generate all permutations.
    (cl-mapcan (lambda (e)
          (mapcar (lambda (p) (cons e p))
              (permutations
               (cl-remove e bag :count 1))))
        bag)))

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

(defun minibuffer-refocus ()
  "Refocus the minibuffer if it is waiting for input."
  (interactive)
  (when (active-minibuffer-window)
    (message "") ;; clear the echo area, in case it overwrote the minibuffer
    (select-window (minibuffer-window))))

;; FIXME: mouse drag breaks it
(defadvice mouse-set-point (around no-mouse-select-window-if-minibuffer-active activate)
  (unless (or
           (active-minibuffer-window)
           (window-minibuffer-p)
           (minibuffer-window-active-p (selected-window))
           )
    ad-do-it))

;; cancel everything, including active minibuffers and recursive edits
(global-set-key (kbd "C-M-g") 'top-level)

;;(delete-selection-mode 1)

;; Disable highlighting clickable stuff when the mouse moves over it.
;; Turning it off speeds up remote X.
(setq mouse-highlight nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing

(with-library 'printing
  (setq pr-gv-command "okular")
  (pr-update-menus t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile

(with-library 'projectile
  (setq
   projectile-cache-file (concat emacs-persistence-directory "projectile.cache")
   projectile-ack-function 'ack
   projectile-known-projects-file (concat emacs-persistence-directory "projectile-bookmarks.eld"))
  (projectile-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repeatable Commands

;; From http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/44728fda08f1ec8f?hl=en&tvc=2
(require 'repeat)
(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode

(defun jpk/org-mode-hook ()
  (interactive)
  (local-unset-key (kbd "C-<tab>"))
  (local-unset-key (kbd "C-S-<iso-lefttab>"))
  ;;(local-unset-key (kbd "<backtab>"))
  (local-unset-key (kbd "<S-iso-lefttab>"))
  (local-unset-key (kbd "M-<up>"))
  (local-unset-key (kbd "M-<down>"))
  (local-unset-key (kbd "S-<left>"))
  (local-unset-key (kbd "S-<right>"))
  (local-unset-key (kbd "C-S-<left>"))
  (local-unset-key (kbd "C-S-<right>"))
  (local-set-key (kbd "C-<down>") 'outline-forward-same-level)
  (local-set-key (kbd "C-<up>") 'outline-backward-same-level)
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (with-library 'flyspell
    (flyspell-mode 1))
  (with-library 'ac-org-mode
    (add-to-list 'ac-sources 'ac-source-org)))

(with-eval-after-load "org"
  (setq-default org-hide-leading-stars t)
  (add-to-list 'org-mode-hook 'jpk/org-mode-hook)
  (put 'org-end-of-line 'CUA 'move)
  (put 'org-beginning-of-line 'CUA 'move)
  (put 'org-fontify-emphasized-text 'safe-local-variable 'booleanp)
  )

;; (setq org-src-fontify-natively t)

;; remember mode lets you quickly record notes without distracting you

;; it's probably best if this is an org-mode file
(setq remember-data-file (concat user-emacs-directory "remember.org"))
(global-set-key (kbd "C-c r") 'org-remember)

(defun find-remember-data-file ()
  (interactive)
  (find-file remember-data-file))

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

(defun insert-euro-sign ()
  (interactive "*")
  (insert "€"))
(global-set-key (kbd "C-x 8 E") 'insert-euro-sign)
(global-set-key (kbd "C-x 8 e") 'insert-euro-sign)

;; Doesn't modify the buffer.  Similar to M-x butterfly.
;; From http://thread.gmane.org/gmane.emacs.devel/147660/focus=147675
(defun cat-command ()
  "A command for cats."
  (interactive)
  (require 'animate)
  (let ((mouse "
           ___00
        ~~/____'>
          \"  \"")
        (h-pos (floor (/ (window-height) 2)))
        (contents (buffer-string))
        (mouse-buffer (generate-new-buffer "*mouse*")))
    (save-excursion
      (switch-to-buffer mouse-buffer)
      (insert contents)
      (setq truncate-lines t)
      (animate-string mouse h-pos 0)
      (dotimes (_ (window-width))
        (sit-for 0.01)
        (dotimes (n 3)
          (goto-char (point-min))
          (forward-line (+ h-pos n 1))
          (move-to-column 0)
          (insert " "))))
    (kill-buffer mouse-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date and Time

(defvar insert-time-format "%H:%M:%S"
  "*Format for `insert-time', see `format-time-string' for syntax.")

(defvar insert-date-format "%Y-%m-%d"
  "*Format for `insert-date', see `format-time-string' for syntax.")

(defun insert-time-str (fmt)
  "Insert a formatted timestamp string in the buffer.  See
  `format-time-string' for syntax of FMT."
  (insert (format-time-string fmt (current-time))))

(defun insert-time ()
  "Insert the current time in the buffer."
  (interactive "*")
  (insert-time-str insert-time-format))

(defun insert-date ()
  "Insert the current date in the buffer."
  (interactive "*")
  (insert-time-str insert-date-format))

(defun insert-date-and-time ()
  "Insert the current date and time in the buffer."
  (interactive "*")
  (insert-time-str (concat insert-date-format " " insert-time-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ssh

;; keychain is a way to manage ssh-agents.  In particular, it looks
;; for a running ssh-agent and stores it's env vars in a file, any
;; program that knows how to read the file can use the agent.
(with-library 'keychain-environment
  (keychain-refresh-environment))

(dolist (x '(".ssh/config\\'" "sshd?_config\\'"))
  (add-to-list 'auto-mode-alist `(,x . ssh-config-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C SubWordMode
;; FindSubWordsInCamelCase

(with-library 'syntax-subword
  (global-syntax-subword-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diminish
;; Hide some minor mode indicators in the modeline.
(with-library 'diminish
  (with-eval-after-load "abbrev" (diminish 'abbrev-mode))
  (with-eval-after-load "auto-complete" (diminish 'auto-complete-mode))
  (with-eval-after-load "doxymacs" (diminish 'doxymacs-mode))
  (with-eval-after-load "drag-stuff" (diminish 'drag-stuff-mode))
  (with-eval-after-load "eldoc" (diminish 'eldoc-mode))
  (with-eval-after-load "face-remap" (diminish 'buffer-face-mode))
  (with-eval-after-load "fixme-mode" (diminish 'fixme-mode))
  (with-eval-after-load "flyspell" (diminish 'flyspell-mode))
  (with-eval-after-load "projectile" (diminish 'projectile-mode "proj"))
  (with-eval-after-load "workgroups" (diminish 'workgroups-mode))
  (with-eval-after-load "wrap-region" (diminish 'wrap-region-mode))
  (with-eval-after-load "yasnippet" (diminish 'yas-minor-mode))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help

(setq message-log-max 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers and strings

;; make it a bit easier to do hex sequences
;; could replace string-to-number in most cases
(defun str2num (str &optional base)
  "Like string-to-number, but with 'normal' (C-style) prefixes
for non-decimal strings."
  (let ((case-fold-search t))
    (cond
     ((string-match "0x[0-9A-F]+" str)
      (string-to-number (replace-regexp-in-string "0x" "" str)
                        16))
     ((string-match "0o[0-7]+" str)
      (string-to-number (replace-regexp-in-string "0o" "" str)
                        8))
     ((string-match "0d[0-9]+" str)
      (string-to-number (replace-regexp-in-string "0d" "" str)
                        10))
     ((string-match "0b[01]+" str)
      (string-to-number (replace-regexp-in-string "0b" "" str)
                        2))
     (t
      (string-to-number str base)))))

(with-library 'evil-numbers
  (global-set-key (kbd "C-c =") (make-repeatable-command 'evil-numbers/inc-at-pt))
  (global-set-key (kbd "C-c -") (make-repeatable-command 'evil-numbers/dec-at-pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CUA mode

;; FIXME: Emacs 24.4 has a reasonable rectangle-mark-mode.  Add
;; cua-rect functionality to it, and get rid of cua-mode.

(global-set-key (kbd "C-<return>") 'cua-rectangle-mark-mode)

(with-eval-after-load "cua-rect"
  (defun cua-sequence-rectangle (first incr format)
    "Resequence each line of CUA rectangle starting from FIRST.
The numbers are formatted according to the FORMAT string."
    (interactive
     (list (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (str2num
              (read-string "Start value: (0) " nil nil "0")))
           (str2num
            (read-string "Increment: (1) " nil nil "1"))
           (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))
    (if (= (length format) 0)
        (setq format cua--rectangle-seq-format)
      (setq cua--rectangle-seq-format format))
    (cua--rectangle-operation 'clear nil t 1 nil
                              '(lambda (s e l r)
                                 (delete-region s e)
                                 (insert (format format first))
                                 (setq first (+ first incr)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(global-set-key (kbd "<f5>") 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard

(setq x-select-enable-clipboard t
      x-select-enable-primary nil
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      select-active-regions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode

;; tip - use insert-char to insert unicode characters by name

;; from Jonathan Arkell (http://stackoverflow.com/questions/154097/whats-in-your-emacs/154980#154980)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(defun list-unicode-display (&optional regexp)
  "Display a list of unicode characters and their names in a buffer."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
         (case-fold-search t)
         (cmp (lambda (x y) (< (cdr x) (cdr y))))
         ;; alist like ("name" . code-point)
         (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
                                             (ucs-names))
                           cmp)))
    (with-help-window "*Unicode characters*"
      (with-current-buffer standard-output
        (dolist (c char-alist)
          (insert (format "0x%06X\t" (cdr c)))
          (insert (cdr c))
          (insert (format "\t%s\n" (car c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Daemon/Server

(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; (defun server-edit-or-save-buffers-kill-terminal (arg)
;;   "Runs `server-edit', and if it did nothing, then runs `save-buffers-kill-terminal'."
;;   (interactive "P")
;;   (if (if (boundp 'server-edit) (server-edit) t) ;; returns nil if it marked the buffer as "done"
;;     (save-buffers-kill-terminal arg)))
;; (global-set-key (kbd "C-x C-c") 'server-edit-or-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(when (daemonp)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Browser

(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag t)

(setq url-configuration-directory (concat emacs-persistence-directory "url/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; abbrev

(setq abbrev-file-name (concat emacs-persistence-directory "abbrev_defs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;; TAB completion with a nice UI

(defun jpk/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'jpk/dabbrev-friend-buffer)

(add-to-list 'load-path (concat user-emacs-directory "ac-sources"))
(with-library 'auto-complete-config
  (setq ac-source-yasnippet nil) ;; broken for latest yasnippet
  (ac-config-default))

(with-eval-after-load "auto-complete"

  (setq ac-auto-start 3
        ac-auto-show-menu t
        ac-use-quick-help nil
        ;;ac-quick-help-delay 0.5
        ac-ignore-case nil
        ac-comphist-file (concat emacs-persistence-directory "ac-comphist.dat"))

  (add-to-list 'ac-dictionary-files "~/.ispell_american")
  (add-to-list 'ac-dictionary-files "~/.aspell.en.pws")
  
  (add-to-list 'ac-modes 'latex-mode)

  (with-library 'ac-dabbrev
    (add-to-list 'ac-sources 'ac-source-dabbrev))

  ;; keybinds
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "M-k") 'ac-next)
  (define-key ac-completing-map (kbd "M-i") 'ac-previous)
  (define-key ac-completing-map (kbd "<backtab>") 'ac-expand-previous)
  (define-key ac-completing-map (kbd "S-TAB") 'ac-expand-previous)
  (define-key ac-completing-map (kbd "C-s") 'ac-isearch)

  ;; workaround for flyspell-mode
  (ac-flyspell-workaround)

  (defun ac-prefix-default ()
    "Same as `ac-prefix-symbol' but ignore a number prefix."
    (let ((start (ac-prefix-symbol)))
      (when (and start
               (not (string-match "^\\(?:0[xX][0-9A-Fa-f]+\\|[0-9]+\\)$"
                                  (buffer-substring-no-properties start (point)))))
        start)))
  )

;; hippie-expand is like dabbrev-expand, but more
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flyspell

;; TODO: wrap flyspell-auto-correct-word with ido

;; TODO: http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(setq ispell-program-name "aspell")

(with-eval-after-load "flyspell"
  (setq flyspell-use-meta-tab nil)
  (define-key flyspell-mode-map (kbd "M-TAB") nil)

  ;; hack to keep ispell from constantly restarting.  See
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2010-05/msg00248.html.
  (defadvice ispell-init-process (around ispell-root-default-directory activate)
    (let (default-directory)
      (cd "/")
      ad-do-it))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookmarks

(setq bookmark-default-file (concat emacs-persistence-directory "bookmarks")
      bookmark-save-flag nil)

;; bm.el is a visible bookmark package.  Bookmarks are indicated with
;; highlighting in the text and/or the fringe.  They are optionally
;; (per-buffer) persistent.

(setq bm-marker 'bm-marker-right
      bm-repository-file (concat emacs-persistence-directory "bm-repository")
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
      (define-key map (kbd "n") (make-repeatable-command 'bm-next))
      (define-key map (kbd "p") (make-repeatable-command 'bm-previous))
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

(ido-mode 'both) ;; both buffers and files
(ido-everywhere t)

(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)
(setq ido-enable-flex-matching t)
(setq ido-ignore-buffers '("\\` " "^\\*Completion" "^\\*Ido"))
(setq ido-max-work-file-list 50)
(setq ido-rotate-file-list-default nil)
(setq ido-save-directory-list-file (concat emacs-persistence-directory
                                           "ido-last"))
(setq ido-show-dot-for-dired t)
(setq ido-work-directory-match-only nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

;; from http://stackoverflow.com/a/1732081/245173
(defadvice completion--file-name-table
  (after ignoring-backups-f-n-completion activate)
  "Filter out results when they match `completion-ignored-extensions'."
  (let ((res ad-return-value))
    (if (and (listp res)
           (stringp (car res))
           (cdr res)) ;; length > 1, don't ignore sole match
        (setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

(defun ido-initiate-auto-merge-this-buffer ()
  "Calls `ido-initiate-auto-merge' on the current buffer.

ido's automerge is not particularly well documented.  Basically,
if no matches are found, it starts looking in subdirectories for
matches.  This can be annoying if you didn't really want it, so
it's probably better to explicitly request a merge."
  (interactive)
  (ido-initiate-auto-merge (current-buffer)))

(defun jpk/ido-setup-hook ()
  (define-key ido-completion-map [remap backward-kill-word] nil)
  (define-key ido-completion-map (kbd "<C-backspace>") 'backward-kill-word)
  (define-key ido-completion-map (kbd "C-c C-s") 'ido-initiate-auto-merge-this-buffer))
(add-hook 'ido-setup-hook 'jpk/ido-setup-hook)

(defun jpk/ido-minibuffer-setup-hook ()
  ;; disallow wrapping of the minibuffer
  (make-local-variable 'truncate-lines)
  (setq truncate-lines t))
(add-hook 'ido-minibuffer-setup-hook 'jpk/ido-minibuffer-setup-hook)

(defun insert-filename-or-buffername (&optional arg)
  "If the buffer has a file, insert the base name of that file.
  Otherwise insert the buffer name.  With prefix argument, insert
  the full file name."
  (interactive "P")
  (let* ((buffer (window-buffer (minibuffer-selected-window)))
         (file-path-maybe (buffer-file-name buffer)))
    (insert (if file-path-maybe
                (if arg
                    file-path-maybe
                  (file-name-nondirectory file-path-maybe))
              (buffer-name buffer)))))

(define-key minibuffer-local-map (kbd "C-c f") 'insert-filename-or-buffername)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use ido (almost) everywhere
(setq ido-ubiquitous-function-exceptions
      '(ido-write-file ido-find-file ido-list-directory ffap-menu-ask
        ffap-read-file-or-url grep-read-files write-file
        elp-instrument-package))

(with-library 'ido-ubiquitous
  (ido-ubiquitous-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido for M-x
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))

(setq smex-save-file (concat emacs-persistence-directory "smex-items"))
(with-library 'smex
  (add-hook 'after-init-hook 'smex-initialize)
  (add-hook 'after-load-functions 'smex-update-after-load)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TAGS

(with-library 'etags

  (setq ctags-executable "ctags-exuberant")

  (defun create-tags (dir)
    "Create a tags file in directory DIR."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -e -R %s"
             ctags-executable dir (directory-file-name dir))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup

(setq backup-dir (concat emacs-persistence-directory "backup/")
      backup-directory-alist `(("." . ,backup-dir)))
(if (not (file-exists-p backup-dir))
    (make-directory backup-dir t))

(setq make-backup-files t
      vc-make-backup-files t
      version-control t
      kept-new-versions 256
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autosave

(setq auto-save-dir (concat emacs-persistence-directory "auto-save/")
      auto-save-list-dir (concat emacs-persistence-directory "auto-save-list/")
      auto-save-list-file-prefix auto-save-list-dir
      auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
(dolist (d (list auto-save-dir auto-save-list-dir))
  (if (not (file-exists-p d))
      (make-directory d t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; save-visited-files
;; Saves a list of the open files (via auto-save-hook), which can be
;; restored with save-visited-files-restore.

(setq save-visited-files-location (concat emacs-persistence-directory
                                          "save-visited-files")
      save-visited-files-auto-restore t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recentf (recently visited files)

(setq recentf-save-file (concat emacs-persistence-directory "recentf")
      recentf-max-saved-items 256)
(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; savehist

(setq savehist-file (concat emacs-persistence-directory "history"))
(savehist-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP

(setq tramp-persistency-file-name (concat emacs-persistence-directory
                                          "tramp")
      tramp-backup-directory (concat emacs-persistence-directory
                                     "tramp-backup/")
      tramp-backup-directory-alist `(("." . ,tramp-backup-directory))
      tramp-auto-save-directory (concat emacs-persistence-directory
                                        "tramp-auto-save/")
      )

(dolist (d (list tramp-auto-save-directory tramp-backup-directory))
  (if (not (file-exists-p d))
    (make-directory d t)))

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
           (space (+ 6 (- (window-width) (length warning))))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face 'find-file-root-header-face)))))

(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((bname (expand-file-name (or buffer-file-name
                                     default-directory)))
        (pt (point)))
    (setq bname (or (file-remote-p bname 'localname)
                    (concat "/sudo::" bname)))
    (cl-flet ((server-buffer-done
               (buffer &optional for-killing)
               nil))
      (find-alternate-file bname))
    (goto-char pt)))

;; normally this is bound to find-file-read-only
;; use M-x read-only-mode instead
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

(add-hook 'find-file-hook 'find-file-root-header-warning)
(add-hook 'dired-mode-hook 'find-file-root-header-warning)

;; Multihop: /ssh:gwuser@gateway|ssh:user@remote:/path/to/file

;; In Windows, use the sshx method (it's a cygwinized version of ssh).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse
(with-library 'mouse+
  (global-set-key (kbd "<down-mouse-2>") 'mouse-flash-position)
  (global-set-key (kbd "S-<down-mouse-2>") 'mouse-scan-lines))

;; move mouse pointer away when the cursor gets near
(mouse-avoidance-mode 'cat-and-mouse)

;; it's really annoying when the frame raises by itself
(defadvice mouse-avoidance-set-mouse-position (around disable-raise activate)
  (cl-flet ((raise-frame (&optional frame) t))
    ad-do-it))

(global-set-key (kbd "<mouse-2>") 'mouse-yank-primary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows

(defun split-windows-in-quarters (arg)
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

;; default bindings are too long, make single keystrokes
(defun other-window-prev ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-1") 'other-window-prev)
(global-set-key (kbd "C-x O") 'other-window-prev)
(global-set-key (kbd "M-2") 'other-window)
;; default is the unwieldy C-x 4 C-o
(global-set-key (kbd "<f6>") 'display-buffer)

;; default is C-x <left> and C-x <right>
(global-set-key (kbd "M-<up>") 'previous-buffer)
(global-set-key (kbd "M-<down>") 'next-buffer)
(global-set-key (kbd "C-M-1") 'previous-buffer)
(global-set-key (kbd "C-M-2") 'next-buffer)

(dolist (func '(buf-move-up
                buf-move-down
                buf-move-left
                buf-move-right))
  (autoload func "buffer-move.el"
    "Move buffers to different windows." t))

;; (defvar buffer-move-mode-map
;;   (let ((map (make-keymap)))
;;     (dolist (e '(("i" . buf-move-up)
;;                  ("j" . buf-move-left)
;;                  ("k" . buf-move-down)
;;                  ("l" . buf-move-right)
;;                  ("q" . turn-off-buffer-move-mode)))
;;       (define-key map
;;         (read-kbd-macro (car e))
;;         (cdr e)))
;;     map)
;;   "Keymap for buffer-move-mode.")

;; (define-minor-mode buffer-move-mode
;;   ""
;;   :global t
;;   :lighter ""
;;   :keymap buffer-move-mode-map)

;; (defun turn-off-buffer-move-mode ()
;;   (interactive)
;;   (buffer-move-mode -1))

;; (defun turn-on-buffer-move-mode ()
;;   (interactive)
;;   (buffer-move-mode 1))

;; Save point position per-window instead of per-buffer.
(with-library 'winpoint
  (winpoint-mode 1))

;; I hate it when my windows get deleted.
(defvar confirm-delete-window nil
  "Ask the user before deleting a window.  This is used in
  around-advice for delete-window.")
(defvar never-delete-window nil
  "Never allow windows to be deleted.  This is used in
  around-advice for delete-window.")

(defadvice delete-window (around confirm activate)
  (if (and (not never-delete-window)
           (if confirm-delete-window
               (y-or-n-p "Delete window? ")
             t))
      ad-do-it
    ;; delete-window raises an error if the window shouldn't be
    ;; deleted
    (error "Not deleting window")))

(defadvice delete-windows-on (around confirm activate)
  (if (and (not never-delete-window)
           (if confirm-delete-window
               (y-or-n-p "Delete window? ")
             t))
      ad-do-it
    ;; delete-windows-on switches to other-buffer if the window
    ;; shouldn't be deleted
    (switch-to-buffer (other-buffer))))

;; prevent these functions from killing the selected window
(defadvice-list
  '(finder-exit View-quit log-edit-done
                vc-revert vc-rollback)
  'before 'no-window-delete
  '(lambda ()
     (let ((never-delete-window t))
       'ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workgroups for windows

(setq wg-prefix-key (kbd "C-c z")
      wg-no-confirm t
      wg-file (concat emacs-persistence-directory "workgroups")
      wg-use-faces nil
      wg-switch-on-load nil)

(defun wg-load-default ()
  "Run `wg-load' on `wg-file'."
  (interactive)
  (wg-load wg-file))

(defun wg-save-default ()
  "Run `wg-save' on `wg-file'."
  (interactive)
  (when wg-list
    (with-temp-message ""
      (wg-save wg-file))))

(with-library 'workgroups
  (define-key wg-map (kbd "C-l") 'wg-load-default)
  (define-key wg-map (kbd "C-s") 'wg-save-default)
  (workgroups-mode 1)
  (add-hook 'auto-save-hook 'wg-save-default)
  (add-hook 'kill-emacs-hook 'wg-save-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC mode

(with-eval-after-load "vc-hg"
  (require 'vc-hg-fixes)
  )

(defadvice vc-log-internal-common (around no-shrink-window activate)
  (cl-letf (((symbol-function 'shrink-window-if-larger-than-buffer) (lambda () nil)))
    (ignore-errors
      ad-do-it)))

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
(defadvice toggle-read-only (around suppress-vc-message activate)
  (with-temp-message ""
    ad-do-it))

;; TODO

;; vc-revert bug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff

(require 'ediff-tweak)

;; put the ediff control window in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Skip over whitespace-only differences in ediff mode.  Still finds
;; such regions, only changes navigation.  Toggle with # # in ediff
;; mode.
(setq-default ediff-ignore-similar-regions t)

(autoload 'commit-patch-buffer "commit-patch-buffer.el"
  "Use diff-mode buffers as commits for VC." t)

(defun jpk/diff-mode-hook ()
  (local-set-key (kbd "C-c C-k") 'diff-hunk-kill)
  (local-set-key (kbd "C-c C-S-k") 'diff-file-kill)
  (local-unset-key (kbd "K")) ;; diff-file-kill
  (local-unset-key (kbd "M-K")) ;; diff-file-kill
  (local-set-key (kbd "C-c C-c") 'commit-patch-buffer)
  (local-set-key (kbd "C-c C-m") 'diff-add-trailing-CR-in-hunk)
  (local-set-key (kbd "C-c C-j") 'diff-remove-trailing-CR-in-hunk)
  (local-set-key (kbd "C-c C-o") 'diff-goto-source)
  ;; FIXME why? special-mode-map suppress-keymap
  (local-set-key (kbd "M-1") nil)
  (local-set-key (kbd "M-2") nil)

  (setq adaptive-wrap-extra-indent 1)
  (visual-line-mode 1)
  )

(add-hook 'diff-mode-hook 'jpk/diff-mode-hook)

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
  (interactive "r")
  (save-restriction 
    (narrow-to-region b e)
    (while (re-search-forward "\\([^]\\)$" nil t)
      (replace-match "\\1" nil nil))))

(defun remove-CR-eol (b e)
  (interactive "r")
  (save-restriction 
    (narrow-to-region b e)
    (while (re-search-forward "$" nil t)
      (replace-match "" nil nil))))

(defun diff-add-or-remove-trailing-CR-in-hunk (add-not-remove)
  "Add trailing carriage returns in the current hunk."
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
  (interactive)
  (diff-add-or-remove-trailing-CR-in-hunk t))

(defun diff-remove-trailing-CR-in-hunk ()
  (interactive)
  (diff-add-or-remove-trailing-CR-in-hunk nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-hl

(with-library 'diff-hl
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (global-diff-hl-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint - command interpreter

(defvar comint-eob-on-send t
  "Like comint-eol-on-send, but moves to the end of buffer.")
(defadvice comint-send-input
  (before move-to-end-of-buffer activate)
  (when comint-eob-on-send
    (goto-char (point-max))))

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

(with-eval-after-load "multi-term"

  (dolist
      (bind '(("C-<right>"     . term-send-forward-word)
              ("C-<left>"      . term-send-backward-word)
              ("C-<backspace>" . term-send-backward-kill-word)
              ("C-<delete>"    . term-send-forward-kill-word)
              ("C-k"           . term-send-raw)
              ("C-y"           . term-send-raw)
              ("C-c C-z"       . term-stop-subjob)
              ("C-z"           . term-stop-subjob)
              ;; work like urxvt tabbed
              ("<S-down>"      . multi-term)
              ("<S-left>"      . multi-term-prev)
              ("<S-right>"     . multi-term-next)
              ("C-v"           . term-paste)
              ))
    (add-to-list 'term-bind-key-alist bind))
  )

(defun jpk/term-mode-hook ()
  (setq cua--ena-cua-keys-keymap nil)
  (with-library 'yasnippet
    (yas-minor-mode 0)))
(add-hook 'term-mode-hook 'jpk/term-mode-hook)

(global-set-key (kbd "C-c t") 'multi-term-prev)

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
(setq woman-fill-column 72)
(setq woman-cache-filename (concat emacs-persistence-directory "woman.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file functions

(defun rename-file-and-buffer (new-name)
  "Rename the current buffer and file it is visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((cur-name (buffer-file-name)))
    (if (not (and cur-name (file-exists-p cur-name)))
        (message "Buffer is not visiting a file!")
      (cond
       ((vc-backend cur-name)
        (vc-rename-file cur-name new-name))
       (t
        (rename-file cur-name new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name t t)
        (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

(defun copy-buffer-file-name-as-kill(choice)
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
                     (if suffixes
                         (lambda (x) (string-match (concat (regexp-opt suffixes t) "$") x)))
                     nil nil nil def)))

(defun find-file-in-path (dirs suffixes)
  "Find a file with a certain extension in a list of directories.
  See `get-file-name-in-path' for more info."
  (let* ((library (get-file-name-in-path dirs suffixes))
         (buf (find-file-noselect (locate-file library dirs))))
    (condition-case nil (switch-to-buffer buf) (error (pop-to-buffer buf)))))

(defun find-user-init-file ()
  "Finds ~/.emacs, or equivalent"
  (interactive)
  (find-file user-init-file))

(defun create-directory-if-necessary ()
  "Create the directory tree above `buffer-file-name', if it
  doesn't completely exist yet.  For use in `find-file-hook'."
  (interactive)
  (let ((dirname (file-name-directory (buffer-file-name))))
    (when (and dirname
             (not (file-directory-p dirname))
             (y-or-n-p (format "Directory %s does not exist.  Create it? " dirname)))
      (make-directory dirname t))))

(add-hook 'find-file-hook 'create-directory-if-necessary)
(add-hook 'before-save-hook 'create-directory-if-necessary)

(defun jpk/save-buffer-maybe (&optional args)
  "Like `save-buffer' but just prints a message if the buffer has no file."
  (interactive "p")
  (if (buffer-file-name)
      (save-buffer args)
    (message "Buffer is not associated with a file.  Use `write-file' instead.")))
(global-set-key [remap save-buffer] 'jpk/save-buffer-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired

(with-eval-after-load "dired"
  (setq image-dired-dir (concat emacs-persistence-directory "image-dired/"))
  
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-filenames-regexp)

  (with-library 'dired+
    (toggle-diredp-find-file-reuse-dir 1)
    (define-key dired-mode-map (kbd "<mouse-2>")
      'diredp-mouse-find-file-reuse-dir-buffer)
    )

  (with-library 'dired-x
    (setq dired-omit-verbose nil)
    (define-key dired-mode-map (kbd "C-c C-o") 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-omit-mode))

  (define-key dired-mode-map (kbd "S-<return>")
    'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "S-<down-mouse-2>")
    'dired-mouse-find-file-other-window)

  (setq dired-deletion-confirmer 'y-or-n-p
        dired-dwim-target t)

  (defun dired-do-move (&optional arg)
    "Similar to `dired-do-rename', but move the file or files to a
  new directory, whether there's one or many.  Also, when doing
  completion on the destination, treat it as a directory."
    (interactive "P")
    (cl-flet ((dired-mark-read-file-name
               (prompt dir op-symbol arg files &optional default)
               (dired-mark-pop-up
                nil op-symbol files
                (function read-directory-name)
                (format prompt (dired-mark-prompt arg files)) dir default)))
      (if (boundp 'dired-do-move-files)
          (dired-do-move-files 'move (function dired-rename-file)
                               "Move" arg dired-keep-marker-rename "Move")
        ;; seems like this got renamed in emacs 24.
        (dired-do-create-files 'move (function dired-rename-file)
                               "Move" arg dired-keep-marker-rename "Move"))))

  (define-key dired-mode-map (kbd "V") 'dired-do-move)

  (defun wdired-create-parentdirs (file-new)
    (let ((dir (file-name-directory file-new)))
      (unless (file-directory-p dir)
        (message "Creating dir for file %s" file-new)
        (make-directory dir t))))

  (defun wdired-do-renames (renames)
    "Perform RENAMES in parallel."
    (let ((residue ())
          (progress nil)
          (errors 0)
          (overwrite (or (not wdired-confirm-overwrite) 1)))
      (while (or renames
                ;; We've done one round through the renames, we have found
                ;; some residue, but we also made some progress, so maybe
                ;; some of the residue were resolved: try again.
                (prog1 (setq renames residue)
                  (setq progress nil)
                  (setq residue nil)))
        (let* ((rename (pop renames))
               (file-new (cdr rename)))
          (cond
           ((rassoc file-new renames)
            (error "Trying to rename 2 files to the same name"))
           ((assoc file-new renames)
            ;; Renaming to a file name that already exists but will itself be
            ;; renamed as well.  Let's wait until that one gets renamed.
            (push rename residue))
           ((and (assoc file-new residue)
               ;; Make sure the file really exists: if it doesn't it's
               ;; not really a conflict.  It might be a temp-file generated
               ;; specifically to break a circular renaming.
               (file-exists-p file-new))
            ;; Renaming to a file name that already exists, needed to be renamed,
            ;; but whose renaming could not be performed right away.
            (if (or progress renames)
                ;; There's still a chance the conflict will be resolved.
                (push rename residue)
              ;; We have not made any progress and we've reached the end of
              ;; the renames, so we really have a circular conflict, and we
              ;; have to forcefully break the cycle.
              (message "Circular renaming: using temporary file name")
              (let ((tmp (make-temp-name file-new)))
                (push (cons (car rename) tmp) renames)
                (push (cons tmp file-new) residue))))
           (t
            (setq progress t)
            (let ((file-ori (car rename)))
              (if wdired-use-interactive-rename
                  (wdired-search-and-rename file-ori file-new)
                ;; If dired-rename-file autoloads dired-aux while
                ;; dired-backup-overwrite is locally bound,
                ;; dired-backup-overwrite won't be initialized.
                ;; So we must ensure dired-aux is loaded.
                (require 'dired-aux)
                (condition-case err
                    (let ((dired-backup-overwrite nil))
                      (wdired-create-parentdirs file-new)
                      (dired-rename-file file-ori file-new
                                         overwrite))
                  (error
                   (setq errors (1+ errors))
                   (dired-log (concat "Rename `" file-ori "' to `"
                                      file-new "' failed:\n%s\n")
                              err)))))))))
      errors))

  (require 'wuxch-dired-copy-paste nil 'noerror)
  )

(with-eval-after-load 'view-mode
  (defun dired-view-next ()
    "Move to next dired line and view ."
    (interactive)
    (quit-window)
    (dired-next-line 1)
    (dired-view-file))

  (defun dired-view-prev ()
    "Move to next dired line and view ."
    (interactive)
    (quit-window)
    (dired-next-line -1)
    (dired-view-file))

  (define-key view-mode-map (kbd "n") 'dired-view-next)
  (define-key view-mode-map (kbd "p") 'dired-view-prev)

  )
  
;; word wrap looks terrible in dired buffers
(defun jpk/dired-before-readin-hook ()
  (visual-line-mode 0)
  )
(add-hook 'dired-before-readin-hook 'jpk/dired-before-readin-hook)

(setq dired-listing-switches (concat dired-listing-switches " --group-directories-first"))

(with-library 'openwith
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
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
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         '("\\.lyx" "lyx" (file))
         '("\\.chm" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "okular"
               '(file))
         ))
  (openwith-mode 1))

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

(with-eval-after-load "ibuffer"
  (require 'ibuf-ext)
  (require 'ibuf-macs)

  (define-ibuffer-sorter pathname
    "Sort by pathname"
    (:description "path")
    (cl-flet ((get-pathname
               (data)
               (with-current-buffer (car data)
                 (or buffer-file-name
                    (if (eq major-mode 'dired-mode)
                        (expand-file-name dired-directory))
                    ;; so that all non pathnames are at the end
                    "~"))))
      (string-lessp (get-pathname a) (get-pathname b))))

  (define-key ibuffer-mode-map
    (kbd "s p") 'ibuffer-do-sort-by-pathname)

  (defun get-all-buffer-directories ()
    "Return a list of all directories that have at least one
       file being visited."
    (interactive)
    (let (l)
      (dolist (e (sort (mapcar 'file-name-directory
                               (remove-if-not 'identity
                                              (mapcar 'buffer-file-name
                                                      (buffer-list))))
                       'string<))
        (unless (string= (car l) e)
          (setq l (cons e l))))
      l))

  (define-ibuffer-filter dirname
      "Toggle current view to buffers with in a directory DIRNAME."
    (:description "directory name"
                  :reader
                  (intern
                   (completing-read "Filter by directory: "
                                    (get-all-buffer-directories)
                                    'identity
                                    t nil nil nil nil)))
    (string= qualifier
             (and (buffer-file-name buf)
                (file-name-directory (buffer-file-name buf)))))

  (defun ibuffer-set-filter-groups-by-directory ()
    "Set the current filter groups to filter by directory."
    (interactive)
    (setq ibuffer-filter-groups
          (mapcar (lambda (dir)
                    (cons (format "%s" dir) `((dirname . ,dir))))
                  (get-all-buffer-directories)))
    (ibuffer-update nil t))

  (define-key ibuffer-mode-map
    (kbd "/ D") 'ibuffer-set-filter-groups-by-directory)
  (define-key ibuffer-mode-map
    (kbd "/ d") 'ibuffer-filter-by-dirname)

  (define-key ibuffer-mode-map
    (kbd "/ M") 'ibuffer-set-filter-groups-by-mode)
  (define-key ibuffer-mode-map
    (kbd "/ m") 'ibuffer-filter-by-used-mode)

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
    (when (= (point) (point-max))
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
    (when (and ibuffer-movement-cycle
             (= (point) (point-min)))
      (goto-char (point-max))
      (ibuffer-backward-filter-group 1))
    (ibuffer-forward-line 0))

  (define-key ibuffer-mode-map
    (kbd "C-<down>") 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map
    (kbd "C-<up>") 'ibuffer-backward-filter-group)

  
  (defadvice-list '(ibuffer-forward-filter-group
                    ibuffer-backward-filter-group
                    ibuffer-mark-interactive
                    ibuffer-toggle-filter-group-1)
    'after
    'recenter
    (lambda () (recenter-no-redraw)))
  
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
(defadvice ibuffer (around ibuffer-point-to-most-recent activate)
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (unless (string-match-p "*Ibuffer*" recent-buffer-name)
      (ibuffer-jump-to-buffer recent-buffer-name))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *scratch*
;; if *scratch* is killed, recreate it

(defun make-scratch-buffer ()
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (with-temp-message ""
      (lisp-interaction-mode))))

(defun scratch-respawns-when-killed ()
  (interactive)
  (if (not (string= (buffer-name (current-buffer)) "*scratch*"))
      t ;; return t so the caller may kill it
    (let ((kill-buffer-query-functions kill-buffer-query-functions))
      (remove-hook 'kill-buffer-query-functions 'scratch-respawns-when-killed)
      (set-buffer (get-buffer-create "*scratch*"))
      (kill-buffer (current-buffer)))
    (make-scratch-buffer)
    nil)) ;; return nil so the caller doesn't try to kill it

(add-hook 'kill-buffer-query-functions 'scratch-respawns-when-killed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; figlet
(dolist
    (func '(figlet
            figlet-comment
            figlet-figletify-region
            figlet-figletify-region-comment))
  (autoload func "figlet.el"
    "Figlet interface for Emacs." t))
(with-eval-after-load "figlet.el"
  (add-to-list 'figlet-options "-k") ;; kerning
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hi Lock

;; Hi Lock provides on-demand keyword highlighting.  It uses
;; font-lock-mode to do the work.  Unfortunately, this is overridden
;; by hl-line mode, so it looks bad when the point is on the line that
;; has hi locked words.  This forces it to use overlays too, which is
;; a performance hit, but it works with hl-line.
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify-symbols

;; prettify-symbols-mode is enabled in major mode hooks.
;; Major modes can append more symbols before enabling prettify-symbols-mode.
(setq prettify-symbols-alist
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
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
(global-set-key (kbd "C-c B") 'doxygen-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fixme-mode

;; TODO: improve this to only highlight in comments like fic-mode
;; TODO: don't use a list of modes, just a hook should be sufficient
(with-library 'fixme-mode
  (setq fixme-highlighted-words '("FIXME" "TODO" "BUG" "XXX" "DEBUG"))
  (dolist (m '(matlab-mode verilog-mode lisp-interaction-mode lisp-mode))
    (add-to-list 'fixme-modes m))
  (fixme-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Programming

(defun previous-defun (&optional arg)
  "Like `beginning-of-defun', and also recenters."
  (interactive "^p")
  (beginning-of-defun arg)
  (recenter-no-redraw))

(defun next-defun (&optional arg)
  "Like `beginning-of-defun' with a negative arg, and also recenters."
  (interactive "^p")
  (beginning-of-defun (- arg))
  (recenter-no-redraw))

(defun insert-comment-bar (arg)
  (interactive "*p")
  (let ((num (if (> arg 1) arg 72))
        (to-insert ""))
    (cond
     ((memq major-mode '(c-mode cc-mode c++-mode java-mode php-mode))
      (setq to-insert (concat "/*"
                              (make-string (- num 4) ?*)
                              "*/")))
     ((memq major-mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
      (setq to-insert (make-string num ?\;)))
     ((memq major-mode '(latex-mode))
      (setq to-insert (make-string num ?%)))
     (t
      (setq to-insert (make-string num ?#))))

    (beginning-of-line)
    (if (looking-at "^[[:space:]]*$")
        (end-of-line)
      (setq to-insert (concat to-insert "\n")))
    (insert to-insert)))

(defun jpk/prog-mode-hook ()
  ;;(smart-tabs-mode 0) ;; default to using spaces

  ;; for doxygen
  (with-library 'doxymacs
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

  ;(add-hook 'after-save-hook 'imenu-force-rescan 'append 'local)

  (local-set-key (kbd "C-M-;") 'insert-comment-bar)
  (local-set-key (kbd "C-m") 'newline-and-indent)
  (local-set-key (kbd "C-a") 'beginning-of-line-or-text)
  (local-set-key (kbd "C-e") 'end-of-line-or-code)
  (local-set-key (kbd "C-M-a") 'previous-defun)
  (local-set-key (kbd "C-M-e") 'next-defun)
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

(setq ctypes-file-name (concat emacs-persistence-directory "ctypes"))

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

(defun setup-simple-compile ()
  "Sets compile-command to something like `gcc -o foo foo.c' if
  there is no Makefile in the directory"
  (interactive)
  (when (and buffer-file-name (file-name-nondirectory buffer-file-name))
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (let* ((file (file-name-nondirectory buffer-file-name))
                  (compiler (if (string-equal "c" (file-name-extension file))
                                "gcc" "g++")))
             (format "%s -o %s %s %s %s" ;; "%s -c -o %s.o %s %s %s"
                     (or (getenv "CC") compiler)
                     (file-name-sans-extension file)
                     (or (getenv "CPPFLAGS") "-DDEBUG=9")
                     (or (getenv "CFLAGS") (if (string= compiler "gcc")
                                              "-ansi -Wall -g3 -std=c99"
                                            "-ansi -Wall -g3"))
                     file))))))

(setq prettify-symbols-c-alist
      '(("!=" . ?≠)
        ("NULL" . ?∅)
        ("&&" . ?⋀)
        ("||" . ?⋁)
        ("!" . ?¬)
        ("HUGE_VAL" . ?∞)
        ("->" . ?→)
        ("M_PI" . ?π)
        ))

(defun jpk/c-mode-hook ()
  (smart-tabs-mode 1)
  (setq tab-width 4)
  (setup-simple-compile)
  (imenu-add-to-menubar "IMenu")
  (setq comment-start "// "
        comment-end "")
  (prettify-symbols-mode -1)
  (dolist (x prettify-symbols-c-alist)
    (add-to-list 'prettify-symbols-alist x))
  (prettify-symbols-mode 1)
  ;;(cpp-highlight-if-0/1)
  ;;(add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  )

(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUD (Grand Unified Debugger)

(with-eval-after-load 'gud
  (define-key gud-minor-mode-map (kbd "C-c C-n") (make-repeatable-command 'gud-next))
  (define-key gud-minor-mode-map (kbd "C-c C-s") (make-repeatable-command 'gud-step))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make
(add-to-list 'auto-mode-alist '("Makefile" . makefile-gmake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl

(defalias 'perl-mode 'cperl-mode)

(setq cperl-invalid-face nil)

(setq prettify-symbols-perl-alist
      '(("!=" . ?≠)
        ("->" . ?→)
        ("=>" . ?⇒)
        ))

(defun jpk/cperl-mode-hook ()
  (prettify-symbols-mode -1)
  (dolist (x prettify-symbols-perl-alist)
    (add-to-list 'prettify-symbols-alist x))
  (prettify-symbols-mode 1)
  (cperl-set-style "BSD")
  )

(add-hook 'cperl-mode-hook 'jpk/cperl-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

;; (defun jpk/python-eval-insert-region (beg end)
;;   (interactive "r")
;;   (let ((str (if (region-active-p)
;;                  (buffer-substring-no-properties
;;                   (region-beginning) (region-end))
;;                (buffer-substring-no-properties
;;                 (line-beginning-position) (line-end-position)))))
;;     (save-excursion
;;       (beginning-of-line)
;;       (insert (shell-command-to-string
;;                (concat "python -c " (shell-quote-argument
;;                                      (concat "print(repr(" str "))"))))))))

(with-eval-after-load "python"

  (with-library 'pylint
    ;; N.B. you probably want to set shell-file-name to something like
    ;; "C:/cygwin/bin/bash.exe" on Windows, because the default shell
    ;; will fuck up the command line.
    (setq pylint-options '("--rcfile=./.pylintrc"
                           "--reports=n"
                           "--msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}]\n  {msg}'"
                           "--disable=C,R,locally-disabled"
                           ))

    (defun pylint2 ()
      (interactive)
      (let ((pylint-command "pylint2"))
        (pylint)))

    (defun pylint3 ()
      (interactive)
      (let ((pylint-command "pylint"))
        (pylint)))

    (define-key python-mode-map (kbd "C-c C-v") 'pylint2)
    )

  (define-key python-mode-map (kbd "S-<return>") 'python-send-region-or-line)
  (define-key python-mode-map (kbd "<backtab>") 'delete-indentation)
  (define-key python-mode-map (kbd "S-TAB") 'delete-indentation)
  (define-key python-mode-map (kbd "C-c C-3") 'python-2to3)

  (setq python-indent-offset 4)

  )

(when (executable-find "ipython2")
  (setq python-shell-interpreter "ipython2"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(defun python-send-region-or-line (beg en)
  "Like `python-send-region', but automatically send the current
  line if region is not active."
  (interactive "r")
  (if (use-region-p)
      (python-send-region beg en)
    (python-send-region (line-beginning-position)
                        (line-end-position))))

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

(setq prettify-symbols-python-alist
      '(("!=" . ?≠)
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

(defun jpk/python-mode-hook ()
  ;; which-func-mode causes huge performance problems in python-mode
  (when (boundp 'which-function-mode)
    (which-function-mode -1))
  (prettify-symbols-mode -1)
  (dolist (x prettify-symbols-python-alist)
    (add-to-list 'prettify-symbols-alist x))
  (prettify-symbols-mode 1)
  )
(add-hook 'python-mode-hook 'jpk/python-mode-hook)
(add-hook 'python-mode-hook 'jpk/prog-mode-hook)

(defun python-path ()
  "Returns a list of strings containing all the directories in Python's path."
  (split-string (shell-command-to-string
                 "python -c 'import sys; sys.stdout.write(\"+++\".join(sys.path))'")
                "+++"))

(defun find-python-library ()
  (interactive)
  (find-file-in-path (python-path) '(".py")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Octave/Matlab

;; ;; hack for octave mode "end" keyword
;; (with-eval-after-load "octave-mod"
;;   (add-to-list 'octave-end-keywords "end[[:space:]]*\\([%#].*\\|$\\)")
;;   (setq octave-block-end-regexp
;;         (concat "\\<\\("
;;                 (mapconcat 'identity octave-end-keywords "\\|")
;;                 "\\)\\>"))
;;   )

(defun octave-send-buffer ()
  (interactive)
  (octave-send-region (point-min) (point-max)))

(defun jpk/octave-mode-hook ()
  (interactive)
  (local-set-key (kbd "C-c C-s") 'octave-send-buffer)
  (local-set-key (kbd "C-c C-l") 'octave-send-line)
  (local-set-key (kbd "C-c C-r") 'octave-send-region)
  (with-library 'ac-octave
    (add-to-list 'ac-sources 'ac-source-octave)))

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

(setq prettify-symbols-verilog-alist
      '(("begin" . ?{)
        ("end" . ?})
        ("<=" . ?⇐)
        ))

(defun jpk/verilog-mode-hook ()
  (prettify-symbols-mode -1)
  (dolist (x prettify-symbols-verilog-alist)
    (add-to-list 'prettify-symbols-alist x))
  (prettify-symbols-mode 1)
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

(global-set-key (kbd "C-c C-e") 'eval-and-replace-last-sexp)
(global-set-key (kbd "C-c e") 'eval-print-last-sexp)

(setq eval-expression-print-length nil) ;; unlimited

(with-library 'morlock
  (font-lock-add-keywords 'emacs-lisp-mode morlock-font-lock-keywords)
  (font-lock-add-keywords 'lisp-interaction-mode morlock-font-lock-keywords))

(setq prettify-symbols-lisp-alist
      '(("/=" . ?≠)
        ("nil" . ?∅)
        ("and" . ?⋀)
        ("or" . ?⋁)
        ("not" . ?¬)))

(defun jpk/lisp-modes-hook ()
  (eldoc-mode 1)
  (local-set-key (kbd "C-M-S-x") 'eval-region)
  (with-library 'highlight-quoted
    (highlight-quoted-mode 1))
  (with-library 'highlight-operators
    (highlight-operators-mode -1))
  (prettify-symbols-mode -1)
  (dolist (x prettify-symbols-lisp-alist)
    (add-to-list 'prettify-symbols-alist x))
  (prettify-symbols-mode 1)
  )

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
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
  (with-library 'auto-complete
    (setq ac-sources '(ac-source-functions
                       ac-source-variables
                       ac-source-features
                       ac-source-symbols
                       ac-source-words-in-same-mode-buffers))
    (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
    (auto-complete-mode 1)))
(add-hook 'ielm-mode-hook 'jpk/ielm-mode-hook)

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

(setq aurel-download-directory (concat "/tmp/pacaurtmp-" user-real-login-name))

(dolist (x '("PKGBUILD" "\.install\\'"))
  (add-to-list 'auto-mode-alist `(,x . pkgbuild-mode)))

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
;; fix a bug in htmlize from emacs-goodies-el
(with-eval-after-load "htmlize"
  (defun htmlize-face-size (face)
    ;; The size (height) of FACE, taking inheritance into account.
    ;; Only works in Emacs 21 and later.
    (let ((size-list
           (loop
            for f = face then (face-attribute f :inherit)
            until (or (not f) (eq f 'unspecified))
            for h = (face-attribute f :height)
            collect (if (eq h 'unspecified) nil h))))
      (reduce 'htmlize-merge-size (cons nil size-list))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config files

(dolist (re '("\\.list\\'" ;; apt sources files
              "\\.hgignore" "\\.?hgrc" ;; mercurial files
              "Doxyfile" ;; Doxygen
              "\\.rules" ;; udev
              "\\.service" "\\.target" "\\.socket" "\\.mount" ;; systemd
              ))
  (add-to-list 'auto-mode-alist `(,re . conf-mode)))

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
    (add-to-list 'ac-sources '(ac-source-math-unicode
                               ac-source-math-latex
                               ac-source-latex-commands)))
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
  (interactive)
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
  
  (defadvice sql-sqlite (around complete-and-process-file activate)
    (let (sql-user
          sql-server
          insert-default-directory)
      (setq sql-database (if (and (stringp sql-database)
                                (file-exists-p sql-database))
                             sql-database
                           default-directory))
      (setq sql-database (read-file-name "SQLite file: "
                                         (file-name-directory sql-database)
                                         sql-database))
      ad-do-it))

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
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo))

;; C-x u starts the undo-tree visualizer
(defvar undo-tree-visualize-window-state nil
  "Stores the window state before undo-tree-visualize is called,
  so that it can be restored by undo-tree-visualizer-quit")
(defadvice undo-tree-visualize
  (before restore-window-layout activate)
  (setq undo-tree-visualize-window-state (current-window-configuration)))
(defadvice undo-tree-visualizer-quit
  (after restore-window-layout activate)
  (when undo-tree-visualize-window-state
    (set-window-configuration undo-tree-visualize-window-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch

(defun recenter-no-redraw (&optional arg)
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

;; minor improvements to isearch
(require 'isearch+ nil 'noerror)

;; Allow page up/down, C-l, and anything else that doesn't move the
;; point during isearch.
(setq isearch-allow-scroll t)
(put 'recenter-top-bottom 'isearch-scroll t)

;; normally M-<tab> which is obviously not going to work
;; get a literal tab with C-q <tab>
(define-key isearch-mode-map (kbd "<tab>") 'isearch-complete)

;; recenter the cursor after finding a match
(defadvice isearch-search (after isearch-recenter activate)
  (when isearch-success
    (recenter-no-redraw)))

;; make backspace behave in a more intuitive way
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(defun copy-isearch-match ()
    (interactive)
    (copy-region-as-kill isearch-other-end (point)))

(define-key isearch-mode-map (kbd "M-w") 'copy-isearch-match)

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

(with-library 'ergo-movement-mode
  (define-key isearch-mode-map (kbd "M-k") nil))

(with-library 'flex-isearch
  (setq flex-isearch-auto 'on-failed)
  (global-flex-isearch-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep

;; editable grep results
(with-library 'wgrep
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

;; recenter after running next-error
(setq next-error-recenter '(4))

;; previous-error just calls next-error
(defadvice next-error (after recenter-error-buffer activate)
  (let ((win (get-buffer-window next-error-last-buffer)))
    (when win
      (with-selected-window win
        (recenter-no-redraw)))))

;; compilation-previous-error just calls compilation-next-error
(defadvice compilation-next-error (after recenter-error-buffer activate)
  (recenter-no-redraw))

(global-set-key [remap next-error] (make-repeatable-command 'next-error))
(global-set-key [remap previous-error] (make-repeatable-command 'previous-error))

(defvar grep-context-lines 2
  "Default number of context lines (non-matching lines before and
  after the matching line) for `rgrep-context'.")

(defun rgrep-context (arg)
  (interactive "p")
  (setq arg (or arg grep-context-lines))
  (let ((grep-find-template
         (concat "find <D> <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -C "
                 (number-to-string arg) " -e <R>"))
        grep-host-defaults-alist
        current-prefix-arg)
    (call-interactively 'rgrep)))

(defun jpk/grep-mode-hook ()
  (setq adaptive-wrap-extra-indent 4)
  (visual-line-mode 1)
  (with-library 'hide-lines
    (run-with-timer
     0.01 nil
     (lambda ()
       (with-current-buffer "*grep*"
         (hide-lines-matching "^find"))))))

(add-hook 'grep-mode-hook 'jpk/grep-mode-hook)

(defun jpk/grep-setup-hook ()
  (setenv "GREP_OPTIONS") ;; workaround until emacs 25
  )

(add-hook 'grep-setup-hook 'jpk/grep-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and replace

(define-key query-replace-map (kbd "b") 'backup)
(define-key query-replace-map (kbd "B") 'backup)
(define-key query-replace-map (kbd "RET") 'act-and-show)

;; Findr - find and operate on files recursively
(autoload 'findr-query-replace "findr.el" "Replace text in files." t)

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
    (recenter-no-redraw)
    ret))
(setq replace-search-function 'search-forward-and-center)

(defun re-search-forward-and-center (regexp &optional bound noerror count)
  "Just like `re-search-forward', but centers the point in the window."
  (let ((ret (re-search-forward regexp bound noerror count)))
    (recenter-no-redraw)
    ret))
(setq replace-re-search-function 're-search-forward-and-center)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ack is a better grep

;; TODO look at easy-mmode-define-navigation

(setq ack-context 1
      ack-executable (executable-find "ack-grep")
      ack-prompt-for-directory t)

(autoload 'ack "full-ack.el"
  "Emacs frontend for ack" t)
(autoload 'ack-same "full-ack.el"
  "Emacs frontend for ack" t)

(defun anchor-regexp (re)
  "Simply wrap RE with `^' and `$', like so: `foo' -> `^foo$'"
  (when (stringp re) (concat "^" re "$")))

(with-eval-after-load "full-ack.el"

  (defsubst ack-read (regexp)
    (read-from-minibuffer
     (if regexp "ack pattern: " "ack literal search: ")
     (if (featurep 'thingatpt)
         (let ((str (thing-at-point 'symbol)))
           (set-text-properties 0 (length str) nil str)
           str))
     nil nil
     (if regexp 'ack-regexp-history 'ack-literal-history)))

  (defadvice ack-next-match (after recenter activate)
    (recenter-no-redraw))

  (defadvice ack-previous-match (after recenter activate)
    (recenter-no-redraw))

  (defun ack-next-file (pos arg)
    (interactive "d\np")
    (setq arg (* 2 arg))
    (unless (get-text-property pos 'ack-file)
      (setq arg (1- arg)))
    (assert (> arg 0))
    (dotimes (i arg)
      (setq pos (next-single-property-change pos 'ack-file))
      (unless pos
        (error "Moved past last file")))
    (goto-char pos)
    (recenter-no-redraw)
    pos)

  (defun ack-previous-file (pos arg)
    (interactive "d\np")
    (assert (> arg 0))
    (dotimes (i (* 2 arg))
      (setq pos (previous-single-property-change pos 'ack-file))
      (unless pos
        (error "Moved back before first file")))
    (goto-char pos)
    (recenter-no-redraw)
    pos)

  (add-to-list 'debug-ignored-errors
               (anchor-regexp (regexp-opt '("Moved back before first file"
                                            "Moved past last file"))))

  (define-key ack-mode-map (kbd "TAB") 'ack-next-match)
  (define-key ack-mode-map (kbd "<backtab>") 'ack-previous-match)
  (define-key ack-mode-map (kbd "S-TAB") 'ack-previous-match)
  (define-key ack-mode-map (kbd "N") 'ack-next-file)
  (define-key ack-mode-map (kbd "P") 'ack-previous-file)
  (define-key ack-mode-map (kbd "q") 'quit-window)
  )

;; for grep-mode
(add-to-list 'debug-ignored-errors
             (anchor-regexp (regexp-opt '("Moved past last grep hit"
                                          "Moved back before first grep hit"))))

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
;; (put 'goto-match-paren 'CUA 'move)

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
  (interactive)
  (if (> (length (thing-at-point 'string)) 0)
      (bounce-thing-boundary 'string)
    (goto-match-paren)))
(global-set-key (kbd "C-S-<iso-lefttab>") 'bounce-string-or-list)
(global-set-key (kbd "C-%") 'bounce-string-or-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region

(with-library 'expand-region
  (defun isearch-yank-selection ()
    "Put selection from buffer into search string."
    (interactive)
    (when (region-active-p)
      (deactivate-mark))  ;;fully optional, but I don't like unnecessary highlighting
    (let ((isearch-case-fold-search nil))
      (isearch-yank-internal (lambda () (mark)))))

  (global-set-key (kbd "C-=") 'er/expand-region)
  (define-key isearch-mode-map (kbd "C-S-y") 'isearch-yank-selection)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet (Yet Another Template Mode)

(with-library 'yasnippet
  (setq yas-prompt-functions (cons 'yas-ido-prompt
                                   (remove 'yas-ido-prompt
                                           yas-prompt-functions)))
  (yas-global-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous keybindings
;; the best way to find the symbol for a key is to run C-h k <key>

;; use M-{ijkl} and C-M-{ijkl} like arrows
;; integrates with CUA mode so shifted keys extend the region
(with-library 'ergo-movement-mode
  (ergo-movement-mode 1))

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
  (interactive)
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
  (interactive)
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
  (interactive)
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
;; Most of these keys kill by default; I think it's more intuitive to delete.
(require 'delete-things)

(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "C-S-k") 'kill-line)

(global-set-key (kbd "M-SPC") 'delete-horizontal-space)
(global-set-key (kbd "M-S-SPC") 'delete-blank-lines)

(global-set-key (kbd "<C-S-delete>") 'delete-syntax)
(global-set-key (kbd "<C-S-backspace>") 'backward-delete-syntax)
(global-set-key (kbd "C-<delete>") 'delete-word)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

(with-library 'ergo-movement-mode
  (global-set-key (kbd "C-S-d") (make-run-keybind-func "DEL" 'move))
  (global-set-key (kbd "M-D") (make-run-keybind-func "C-<backspace>" 'move))
  (global-set-key (kbd "M-d") (make-run-keybind-func "C-<delete>" 'move)))

(defalias 'delete-vertical-space 'delete-blank-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

(electric-indent-mode nil)

(defun indent-relative-dwim ()
  "Indent the current line to either: the beginning of last
 preceding line with text, the next tab stop (as determined by
 tab-width, not tab-stop-list) after the previous indent, or the
 beginning of the line.  Repeatedly calling this function cycles
 between these 3 actions."
  (interactive)

  (let ((last-line-indent 0)
        (next-indent 0))

    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (re-search-backward "[^\t ]" nil t)
          (beginning-of-line)
          (re-search-forward "[^\t ]" nil t)
          (goto-char (match-beginning 0))
          (setq last-line-indent (current-column)))))
    (setq next-indent (* (1+ (/ last-line-indent tab-width)) tab-width))

    (cond
     ((= (current-column) last-line-indent)
      (indent-line-to next-indent))
     ((= (current-column) next-indent)
      (indent-line-to 0))
     (t
      (indent-line-to last-line-indent)))))

(setq-default indent-line-function 'indent-relative-dwim)

(defun set-tab-width (arg)
  (interactive "nSet tab width: ")
  (set-variable 'tab-width arg))

;; new buffers default to using 4 spaces for indent
(setq-default tab-width 4
              indent-tabs-mode nil)

;; smart-tabs-mode will indent with tabs, align with spaces
(with-library 'smart-tabs-mode
  (smart-tabs-advice cperl-indent-line cperl-indent-level)
  (smart-tabs-advice c-indent-line     c-basic-offset)
  (smart-tabs-advice c-indent-region   c-basic-offset)
  (with-library 'php-mode
    (smart-tabs-advice php-cautious-indent-region c-basic-offset)
    (smart-tabs-advice php-cautious-indent-line c-basic-offset))
  )

;; shift-tab should undo what tab does
(global-set-key (kbd "<backtab>") 'delete-indentation)
(global-set-key (kbd "S-TAB") 'delete-indentation)

;; rigidly indent
;; see EmacsWiki://MovingRegionHorizontally
(defun move-horizontally (beg en arg)
  "Move the region defined by (beg en) by arg characters.
Positive arg means right; negative means left"
  (save-excursion
    (let ((deactivate-mark nil)
          beg-bol)
      (goto-char beg)
      (setq beg-bol (line-beginning-position))
      (goto-char en)
      ;;(move-beginning-of-line nil)
      (indent-rigidly beg-bol en arg)
      (push-mark beg t t))))

(defun move-horizontally-dwim (beg en arg)
  "If there is an active region, move the whole region arg
columns.  Otherwise, move the cursor line arg columns."
  (interactive "r")
  (if (use-region-p)
      (move-horizontally beg en arg)
    (move-horizontally (line-beginning-position) (line-end-position) arg)))

(global-set-key (kbd "C-9")
                (lambda (beg en)
                  "move region left by one"
                  (interactive "r")
                  (move-horizontally-dwim beg en -1)))
(global-set-key (kbd "C-0")
                (lambda (beg en)
                  "move region right by one"
                  (interactive "r")
                  (move-horizontally-dwim beg en 1)))
(global-set-key (kbd "C-(")
                (lambda (beg en)
                  "move region left by four"
                  (interactive "r")
                  (move-horizontally-dwim beg en -4)))
(global-set-key (kbd "C-)")
                (lambda (beg en)
                  "move region right by four"
                  (interactive "r")
                  (move-horizontally-dwim beg en 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; drag-stuff

;; drag things (words, regions, lines) with arrow keys
(with-library 'drag-stuff
  (setq drag-stuff-modifier '(meta shift))
  (drag-stuff-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fill

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(defadvice fill-paragraph (after scroll-to-left activate)
  (scroll-right (window-hscroll)))

(defun align-regexp-repeated (start stop regexp)
  "Like align-regexp, but repeated for multiple columns. See
http://www.emacswiki.org/emacs/AlignCommands"
  (interactive "r\nsAlign regexp: ")
  (let ((spacing 1)
        (old-buffer-size (buffer-size)))
    ;; If our align regexp is just spaces, then we don't need any
    ;; extra spacing.
    (when (string-match regexp " ")
      (setq spacing 0))
    (align-regexp start stop
                  ;; add space at beginning of regexp
                  (concat "\\([[:space:]]*\\)" regexp)
                  1 spacing t)
    ;; modify stop because align-regexp will add/remove characters
    (align-regexp start (+ stop (- (buffer-size) old-buffer-size))
                  ;; add space at end of regexp
                  (concat regexp "\\([[:space:]]*\\)")
                  1 spacing t)))

;; see also fill-individual-paragraphs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adaptive word wrap

(with-library 'adaptive-wrap
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

(defun beginning-of-line-or-text (arg)
  "Move to BOL, or if already there, to the first non-whitespace character."
  (interactive "^p")
  (if (bolp)
      (beginning-of-line-text arg)
    (move-beginning-of-line arg)))
(put 'beginning-of-line-or-text 'CUA 'move)

(defun end-of-line-code ()
  (interactive "^")
  (require 'newcomment)
  (if comment-start-skip
      (save-match-data
        (let* ((bolpos (line-beginning-position)))
          (end-of-line)
          (if (comment-search-backward bolpos 'noerror)
              (search-backward-regexp comment-start-skip bolpos 'noerror))
          (skip-syntax-backward " " bolpos)))
    (end-of-line)))

(defun end-of-line-or-code ()
  "Move to EOL, or if already there, to EOL sans comments."
  (interactive "^")
  (if (eolp) ;; test me here
      (end-of-line-code)
    (end-of-line)))
(put 'end-of-line-or-code 'CUA 'move)

;; scroll the other buffer
(global-set-key (kbd "S-<next>") 'scroll-other-window)
(global-set-key (kbd "S-<prior>") 'scroll-other-window-down)

(with-library 'smart-hscroll
  (smart-hscroll-mode 1)
  (mouse-hscroll-mode 1)
  (global-set-key (kbd "C->") 'scroll-left-8)
  (global-set-key (kbd "C-<") 'scroll-right-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert between different EOL modes

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
;;; host-specific stuff
(load (concat user-emacs-directory "host.el") 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "jpkotta's init.el loaded")
(message "init.el loaded in %0.3f s." (toc init-el-time))

;;; init.el ends here
