;; TODO
;;
;; docs

(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

(defvar smart-hscroll-auto-hscroll-mode-saved nil
  "Saved value of `auto-hscroll-mode' to restore.")

;;(make-local-variable 'auto-hscroll-mode) ;; why did I need this?

(defvar smart-hscroll-point-previous 0
  "Saved location of the point when scrolling was started.")
(make-local-variable 'smart-hscroll-point-previous)

(defun smart-hscroll-detect-cursor-movement ()
  (with-temp-message (current-message)
    (unless (= (point) smart-hscroll-point-previous)
      (setq smart-hscroll-point-previous (point))
      (setq auto-hscroll-mode smart-hscroll-auto-hscroll-mode-saved)
      ;; a hack to make it apply on the first movement
      (message nil)
      )))

(defvar smart-hscroll-cursor-movement-detector-timer nil
  "Periodic timer that checks for cursor movement.")

(define-minor-mode smart-hscroll-mode
  "Disables `auto-hscroll-mode' while scrolling, and automatically
  restores it when the cursor moves."

  :init-value nil
  :lighter nil
  :global t
  
  (when smart-hscroll-mode
    (setq smart-hscroll-auto-hscroll-mode-saved auto-hscroll-mode)
    (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (setq hscroll-point-previous (point))))
    (setq smart-hscroll-cursor-movement-detector-timer 
          (run-with-idle-timer 0.1 t 'smart-hscroll-detect-cursor-movement)))
  (unless smart-hscroll-mode
    (setq auto-hscroll-mode smart-hscroll-auto-hscroll-mode-saved)
    (when smart-hscroll-cursor-movement-detector-timer 
      (cancel-timer smart-hscroll-cursor-movement-detector-timer))))

(dolist (func '(scroll-left scroll-right))
  (advice-add func
              :before
              (lambda (&rest args)
                "smart horizontal scrolling"
                (when smart-hscroll-mode
                  (setq auto-hscroll-mode nil)))))

(defun scroll-left-8 ()
  "Like `scroll-left' with an argument of 8."
  (interactive)
  (scroll-left 8))
(put 'scroll-left-8 'isearch-scroll t)

(defun scroll-right-8 ()
  "Like `scroll-right' with an argument of 8."
  (interactive)
  (scroll-right 8))
(put 'scroll-right-8 'isearch-scroll t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mwheel)

(defvar mouse-hscroll-amount
  '(4 ((shift) . 1) ((control) . 0.25))
  "Amount to scroll windows by when spinning the mouse wheel.
Similar to `mouse-wheel-scroll-amount'.  This is an alist mapping
the modifier key to the amount to scroll when the wheel is moved
with the modifier key depressed.  Elements of the list have the
form (MODIFIERS . AMOUNT) or just AMOUNT if MODIFIERS is nil.
Meta has no effect.")

(defun mouse-hscroll-mwheel-scroll (event)
  "Scroll left or right according to the EVENT.
This should only be bound to mouse buttons 4 and 5."
  (interactive (list last-input-event))
  (let* ((mousewin (mwheel-event-window event))
         (curwin (when mouse-wheel-follow-mouse
                   (prog1 (selected-window)
                     (select-window mousewin))))
         (buffer (window-buffer curwin))
         (opoint (with-current-buffer buffer
                   (when (eq (car-safe transient-mark-mode) 'only)
                     (point))))
         (mods
          (delq 'meta (delq 'click (delq 'double (delq 'triple (event-modifiers event))))))
         (amt (assoc mods mouse-hscroll-amount)))
    ;; Extract the actual amount or find the element that has no modifiers.
    (if amt (setq amt (cdr amt))
      (let ((list-elt mouse-hscroll-amount))
        (while (consp (setq amt (pop list-elt))))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-width))))))
    (unwind-protect
        (let ((button (mwheel-event-button event)))
          (cond ((eq button mouse-wheel-down-event)
                 (condition-case nil (scroll-left (- amt))))
                ((eq button mouse-wheel-up-event)
                 (condition-case nil (scroll-left amt)))
                (t (error "Bad binding in mouse-hscroll-mwheel-scroll"))))
      (if curwin (select-window curwin)))
    ;; If there is a temporarily active region, deactivate it iff
    ;; scrolling moves point.
    (when opoint
      (with-current-buffer buffer
        (when (/= opoint (point))
          (deactivate-mark)))))
  (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
    (if mwheel-inhibit-click-event-timer
        (cancel-timer mwheel-inhibit-click-event-timer)
      (add-hook 'pre-command-hook 'mwheel-filter-click-events))
    (setq mwheel-inhibit-click-event-timer
          (run-with-timer mouse-wheel-inhibit-click-time nil
                          'mwheel-inhibit-click-timeout))))

(defvar mouse-hscroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<M-mouse-4>") 'mouse-hscroll-mwheel-scroll)
    (define-key map (kbd "<M-mouse-5>") 'mouse-hscroll-mwheel-scroll)
    (define-key map (kbd "<C-M-mouse-4>") 'mouse-hscroll-mwheel-scroll)
    (define-key map (kbd "<C-M-mouse-5>") 'mouse-hscroll-mwheel-scroll)
    (define-key map (kbd "<S-M-mouse-4>") 'mouse-hscroll-mwheel-scroll)
    (define-key map (kbd "<S-M-mouse-5>") 'mouse-hscroll-mwheel-scroll)

    ;; these are generally the horizontal nudges on a scroll wheel
    (define-key map (kbd "<mouse-7>") 'scroll-left-8)
    (define-key map (kbd "<mouse-6>") 'scroll-right-8)

    (define-key map (kbd "C-<next>") 'scroll-left-8)
    (define-key map (kbd "C-<prior>") 'scroll-right-8)
    map)
  
  "Keymap for `mouse-hscroll-mode'.")

(define-minor-mode mouse-hscroll-mode
  "Scroll left and right with the mouse wheel."
  :global t
  :lighter nil
  :init-value nil
  :keymap mouse-hscroll-mode-map
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'smart-hscroll)
