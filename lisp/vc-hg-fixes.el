;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fix super slow `hg stat -A`

(defun vc-hg-state (file)
  "Hg-specific version of `vc-state'."
  (let*
      ((status nil)
       (default-directory (file-name-directory file))
       (out
        (with-output-to-string
          (with-current-buffer
              standard-output
            (setq status
                  (condition-case nil
                      ;; Ignore all errors.
		      (let ((process-environment
			     ;; Avoid localization of messages so we
			     ;; can parse the output.  Disable pager.
			     (append
			      (list "TERM=dumb" "LANGUAGE=C" "HGPLAIN=1")
			      process-environment)))
			(if (file-remote-p file)
			    (process-file
			     "env" nil t nil
			     "HGPLAIN=1" vc-hg-program
			     "--config" "alias.status=status"
			     "--config" "defaults.status="
			     "status" (file-relative-name file))
			  (process-file
			   vc-hg-program nil t nil
			   "--config" "alias.status=status"
			   "--config" "defaults.status="
			   "status" (file-relative-name file))))
                    ;; Some problem happened.  E.g. We can't find an `hg'
                    ;; executable.
                    (error nil)))))))
    (when (eq 0 status)
        (when (and (null (string-match ".*: No such file or directory$" out))
                 (not (string= "" out)))
          (let ((state (aref out 0)))
            (cond
             ((eq state ?=) 'up-to-date)
             ((eq state ?A) 'added)
             ((eq state ?M) 'edited)
             ((eq state ?I) 'ignored)
             ((eq state ?R) 'removed)
             ((eq state ?!) 'missing)
             ((eq state ??) 'unregistered)
             ((eq state ?C) 'up-to-date) ;; Older mercurial versions use this.
             (t 'up-to-date)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphlog stuff

(setq vc-log-short-style nil)

(defvar hg-graphlog-re (concat "^"
                               (regexp-opt '("\\" "|" "/" "o" "@" " " "+" "-"))
                               "*")
  "Matches the junk hg log -G puts at the beginning of the line")

(add-to-list 'vc-hg-log-switches "-G")

(font-lock-add-keywords
 'vc-hg-log-view-mode
 (mapcar (lambda (x) (cons
                 (replace-regexp-in-string "^\\^" hg-graphlog-re (car x) nil t)
                 (cdr x)))
         ;; Here's (part of) the default font-lock-keywords for
         ;; vc-hg-log-view-mode.  I can't figure out how to get this
         ;; programmatically, so a manual way to get this is:
         ;; 
         ;;     emacs -q
         ;;     open a file/dir under hg version control
         ;;     C-x v L
         ;;     C-h v font-lock-keywords from the log buffer
         '(("^user:[ 	]+\\([^<(]+?\\)[ 	]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
            (1 'change-log-name)
            (2 'change-log-email))
           ("^user:[ 	]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)"
            (1 'change-log-email))
           ("^date: \\(.+\\)"
            (1 'change-log-date))
           ("^tag: +\\([^ ]+\\)$"
            (1 'highlight))
           ("^summary:[ 	]+\\(.+\\)"
            (1 'log-view-message))
           ("^changeset:[ 	]*\\([0-9]+\\):\\(.+\\)"
            (0 'log-view-message))
           ("^user:[ 	]+\\([^<(]+?\\)[ 	]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
            (1 'change-log-name)
            (2 'change-log-email))
           ("^user:[ 	]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)"
            (1 'change-log-email))
           ("^date: \\(.+\\)"
            (1 'change-log-date))
           ("^tag: +\\([^ ]+\\)$"
            (1 'highlight))
           ("^summary:[ 	]+\\(.+\\)"
            (1 'log-view-message))))
 'set)

(defun jpk/fix-log-view-re ()
  (setq log-view-message-re
        (replace-regexp-in-string "^\\^" hg-graphlog-re log-view-message-re nil t))
  (setq log-view-file-re 
        (replace-regexp-in-string "^\\^" hg-graphlog-re log-view-file-re nil t))
  )

(add-hook 'vc-hg-log-view-mode-hook 'jpk/fix-log-view-re)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotate (blame) customizations: I don't like the default (the date
;; is too verbose and there's no user name).

(setq vc-hg-annotate-re
      "^[ \t]*[[:alnum:]]+[ \t]+\\([0-9]+\\)[ \t]+\\(.\\{10\\}\\)\\(?:\\(: \\)\\|\\(?: +\\(.+\\): \\)\\)")

(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-q" "-u" "-n"
                 (when revision (concat "-r" revision))))

(defun vc-hg-annotate-time ()
  (when (looking-at vc-hg-annotate-re)
    (goto-char (match-end 0))
    (vc-annotate-convert-time
     (date-to-time (concat (match-string-no-properties 2) " 00:00:00")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other stuff

;; this version works better when viewing explicit ranges of revs
(defun vc-hg-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log associated with FILES."
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    (with-current-buffer
        buffer
      (apply 'vc-hg-command buffer 0 files "log"
             (append
              (when start-revision (list (format "-r%s" start-revision)))
              (when limit (list "-l" (format "%s" limit)))
              (when shortlog '("--style" "compact"))
              vc-hg-log-switches)))))

;; TODO submit bug report
;; this version works better with branchy changelogs
(defun log-view-diff (beg end)
  "Get the diff between two revisions.
If the mark is not active or the mark is on the revision at point,
get the diff between the revision at point and its previous revision.
Otherwise, get the diff between the revisions where the region starts
and ends.
Contrary to `log-view-diff-changeset', it will only show the part of the
changeset that affected the currently considered file(s)."
  (interactive
   (list (if (use-region-p) (region-beginning) (point))
         (if (use-region-p) (region-end) (point))))
  (let ((fr (log-view-current-tag beg))
        (to (log-view-current-tag end)))
    (when (and (string-equal fr to)
             (not (string-equal log-view-vc-backend "Hg")))
      (save-excursion
        (goto-char end)
        (log-view-msg-next)
        (setq to (log-view-current-tag))))
    (vc-diff-internal
     t (list log-view-vc-backend
             (if log-view-per-file-logs
                 (list (log-view-current-file))
               log-view-vc-fileset))
     to fr)))

;; this version works better for a single rev's diff
(defun vc-hg-diff (files &optional oldvers newvers buffer)
  "Get a difference report using hg between two revisions of FILES."
  (let* ((firstfile (car files))
         (working (and firstfile (vc-working-revision firstfile))))
    (when (and (equal oldvers working) (not newvers))
      (setq oldvers nil))
    (when (and (not oldvers) newvers)
      (setq oldvers working))
    (apply #'vc-hg-command (or buffer "*vc-diff*") nil files "diff"
           (append
            (vc-switches 'hg 'diff)
            (when oldvers
              (if newvers
                  (if (string-equal oldvers newvers)
                      (list "-c" oldvers)
                    (list "-r" oldvers "-r" newvers))
                (list "-r" oldvers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(defun jpk/vc-hg-log-view-mode-hook ()
  (setq adaptive-wrap-extra-indent 0)
  (visual-line-mode 1)
  (goto-char (point-min))
  )

(add-hook 'vc-hg-log-view-mode-hook 'jpk/vc-hg-log-view-mode-hook)

(when (boundp 'vc-hg-log-view-mode-map)
  (define-key vc-hg-log-view-mode-map (kbd "q") 'kill-this-buffer))

(global-set-key (kbd "C-x v o") 'vc-version-diff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vc-hg-fixes)
