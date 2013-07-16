;;; calm-forest-theme.el --- A soothing dark color theme for Emacs.

;; Copyright (C) 2011-2013 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; URL: FIXME
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the calm forest color theme from the older color-theme.el
;; package to Emacs 24's deftheme.
;;

;;; Credits
;;
;; Based on the calm forest theme from color-theme, by Artur Hefczyc,
;; created 2003-04-18.
;;
;; zenburn-theme.el (http://github.com/bbatsov/zenburn-emacs) by
;; Bozhidar Batsov was used as a template.

;;; Code:
(deftheme calm-forest "The calm-forest color theme")

(let ((class '((class color) (min-colors 89)))
      ;; calm-forest palette
      (fg-2 "dark green")
      (fg-1 "green3")
      (fg   "green2")
      (fg+1 "green")
      (bg-2 "gray6")
      (bg-1 "gray9")
      (bg   "gray12")
      (bg+1 "gray16")
      (bg+2 "gray20")
      (bg+3 "gray24"))
  
  (custom-theme-set-faces
   'calm-forest

   ;; used to find uncustomized faces
   `(uncustomized ((t (:underline (:color foreground-color :style wave)))))

   ;;; basic coloring
   `(default ((t (:foreground ,fg :background ,bg))))
   `(escape-glyph ((t (:foreground "sky blue" :bold t))))
   ;;`(header-line ((t (:foreground ,calm-forest-yellow :background ,bg-2 :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,bg-1))))
   `(success ((t (:foreground "green2" :weight bold))))
   `(warning ((t (:foreground "red3" :weight bold))))

   `(fixed-pitch ((t (:family "DejaVu Sans Mono"))))

   ;; ;; misc
   ;; `(button ((t (:underline t))))
   ;; `(link ((t (:underline t :weight bold))))
   ;; `(link-visited ((t (:foreground ,fg-1 :underline t :weight normal))))
   `(eldoc-highlight-function-argument ((t (:inherit font-lock-variable-name-face))))

   ;; custom
   ;; `(custom-button ((((type x w32 mac) (class color)) (:underline t :weight bold))))
   ;; `(custom-button-mouse ((((type x w32 mac) (class color)) (:inherit (custom-button highlight)))))
   ;; `(custom-button-pressed ((((type x w32 mac) (class color)) (:inherit custom-button :underline nil))))
   ;; `(custom-button-unraised ((t (:inherit custom-button :underline nil))))
   `(custom-button-face ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed-face ((t (:inherit custom-button :box (:line-width 2 :style pressed-button)))))
   `(custom-changed-face ((t (:background "blue" :foreground "white"))))
   `(custom-comment-face ((t (:background "dim gray"))))
   `(custom-comment-tag-face ((t (:foreground "gray80"))))
   `(custom-documentation-face ((t (nil))))
   `(custom-face-tag-face ((t (:bold t :family "helv" :weight bold :height 1.2))))
   `(custom-group-tag-face ((t (:bold t :foreground "light blue" :weight bold :height 1.2))))
   `(custom-group-tag-face-1 ((t (:bold t :family "helv" :foreground "pink" :weight bold :height 1.2))))
   `(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
   `(custom-modified-face ((t (:background "blue" :foreground "white"))))
   `(custom-rogue-face ((t (:background "black" :foreground "pink"))))
   `(custom-saved-face ((t (:underline t))))
   `(custom-set-face ((t (:background "white" :foreground "blue"))))
   `(custom-state-face ((t (:foreground "lime green"))))
   `(custom-variable-button-face ((t (:bold t :underline t :weight bold))))
   `(custom-variable-tag-face ((t (:bold t :family "helv" :foreground "light blue" :weight bold :height 1.2))))

   ;; ;;; compilation
   ;; `(compilation-column-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(compilation-enter-directory-face ((t (:foreground ,calm-forest-green))))
   ;; `(compilation-error-face ((t (:foreground ,calm-forest-red-1 :weight bold :underline t))))
   ;; `(compilation-face ((t (:foreground ,fg))))
   ;; `(compilation-info-face ((t (:foreground ,calm-forest-blue))))
   ;; `(compilation-info ((t (:foreground ,calm-forest-green+4 :underline t))))
   ;; `(compilation-leave-directory-face ((t (:foreground ,calm-forest-green))))
   ;; `(compilation-line-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(compilation-line-number ((t (:foreground ,calm-forest-yellow))))
   ;; `(compilation-message-face ((t (:foreground ,calm-forest-blue))))
   ;; `(compilation-warning-face ((t (:inherit warning))))
   ;; `(compilation-mode-line-exit ((t (:foreground ,calm-forest-green+2 :weight bold))))
   ;; `(compilation-mode-line-fail ((t (:foreground ,calm-forest-red :weight bold))))
   ;; `(compilation-mode-line-run ((t (:foreground ,calm-forest-yellow :weight bold))))

   ;; ;;; grep
   ;; `(grep-context-face ((t (:foreground ,fg))))
   ;; `(grep-error-face ((t (:foreground ,calm-forest-red-1 :weight bold :underline t))))
   ;; `(grep-hit-face ((t (:foreground ,calm-forest-blue))))
   ;; `(grep-match-face ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(match ((t (:background ,bg-2 :foreground ,calm-forest-orange :weight bold))))

   ;; ;; faces used by isearch
   ;; `(isearch ((t (:foreground ,calm-forest-yellow-2 :weight bold :background ,bg-2))))
   ;; `(isearch-fail ((t (:foreground ,fg :background ,calm-forest-red-4))))
   ;; `(lazy-highlight ((t (:foreground ,calm-forest-yellow-2 :weight bold :background ,bg-1))))

   ;; UI

   `(mode-line ((t (:foreground "black" :background "gray75"))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground "gray80" :background "gray30"))))
   `(mode-line-highlight ((t (:box (:line-width 1 :color ,bg :style nil)))))
   `(modelinepos-column-warning ((t (:inherit font-lock-warning-face))))
   `(modelinepos-region ((t (:inverse-video t))))

   `(region ((t (:background "#400060"))))
   `(secondary-selection ((t (:background "#600040"))))

   `(trailing-whitespace ((t (:background "red3"))))
   `(vertical-border ((t (:foreground "gray80" :background "gray30"))))
   
   `(menu ((t (:foreground "gray80" :background "gray30"))))
   `(scroll-bar ((t (:foreground "gray80" :background "gray30"))))

   `(minibuffer-prompt ((t (:foreground "cyan"))))

   `(fringe ((t (:background ,bg-1 :foreground ,fg-2))))

   `(mouse ((t (:background "orange"))))
   `(mouse-flash-position ((t (:background "grey75"))))
   `(mouse-scan-lines ((t (:background "grey33"))))

   `(cursor ((t (:foreground ,fg :background "orange"))))

   ;; highlight
   `(hi-blue ((((background dark)) (:foreground "light blue" :inverse-video t))))
   `(hi-green ((((min-colors 88) (background dark)) (:foreground "green1" :inverse-video t))))
   `(hi-pink ((((background dark)) (:foreground "pink" :inverse-video t))))
   `(hi-yellow ((((min-colors 88) (background dark)) (:foreground "yellow1" :inverse-video t))))

   `(hl-line ((t (:background ,bg-1))))

   ;; ;; hl-sexp
   ;; `(hl-sexp-face ((,class (:background ,bg+1))
   ;;                 (t :weight bold)))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
   `(font-lock-comment-face ((t (:foreground "chocolate1"))))
   ;;`(font-lock-comment-delimiter-face ((t (:foreground ,calm-forest-green))))
   `(font-lock-constant-face ((t (:foreground "aquamarine"))))
   `(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
   `(font-lock-keyword-face ((t (:foreground "cyan"))))
   `(font-lock-string-face ((t (:foreground "LightSalmon"))))
   `(font-lock-type-face ((t (:foreground "PaleGreen"))))
   `(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
   `(font-lock-warning-face ((t (:bold t :foreground "pink" :weight bold))))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "sandy brown"))))
   `(font-lock-fixme-face ((t (:foreground "red" :weight bold))))
   `(font-lock-warning-face ((t (:foreground "red3" :weight bold))))
   ;;`(font-lock-negation-char-face ((t (:foreground ,fg))))
   ;;`(font-lock-preprocessor-face ((t (:foreground ,calm-forest-blue+1))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;; ;;; newsticker
   ;; `(newsticker-date-face ((t (:foreground ,fg))))
   ;; `(newsticker-default-face ((t (:foreground ,fg))))
   ;; `(newsticker-enclosure-face ((t (:foreground ,calm-forest-green+3))))
   ;; `(newsticker-extra-face ((t (:foreground ,bg+2 :height 0.8))))
   ;; `(newsticker-feed-face ((t (:foreground ,fg))))
   ;; `(newsticker-immortal-item-face ((t (:foreground ,calm-forest-green))))
   ;; `(newsticker-new-item-face ((t (:foreground ,calm-forest-blue))))
   ;; `(newsticker-obsolete-item-face ((t (:foreground ,calm-forest-red))))
   ;; `(newsticker-old-item-face ((t (:foreground ,bg+3))))
   ;; `(newsticker-statistics-face ((t (:foreground ,fg))))
   ;; `(newsticker-treeview-face ((t (:foreground ,fg))))
   ;; `(newsticker-treeview-immortal-face ((t (:foreground ,calm-forest-green))))
   ;; `(newsticker-treeview-listwindow-face ((t (:foreground ,fg))))
   ;; `(newsticker-treeview-new-face ((t (:foreground ,calm-forest-blue :weight bold))))
   ;; `(newsticker-treeview-obsolete-face ((t (:foreground ,calm-forest-red))))
   ;; `(newsticker-treeview-old-face ((t (:foreground ,bg+3))))
   ;; `(newsticker-treeview-selection-face ((t (:foreground ,calm-forest-yellow))))

   ;; ;;; external
   ;; `(ace-jump-face-background
   ;;   ((t (:foreground ,fg-1 :background ,bg :inverse-video nil))))
   ;; `(ace-jump-face-foreground
   ;;   ((t (:foreground ,calm-forest-green+2 :background ,bg :inverse-video nil))))

   ;; full-ack
   `(ack-separator ((t (:foreground ,fg))))
   `(ack-file ((t (:foreground "red2"))))
   `(ack-line ((t (:foreground ,fg-1))))
   `(ack-match ((t (:foreground "orange" :background ,bg-2 :weight bold))))

   ;; ;; auctex
   ;; `(font-latex-bold ((t (:inherit bold))))
   ;; `(font-latex-warning ((t (:inherit font-lock-warning))))
   ;; `(font-latex-sedate ((t (:foreground ,calm-forest-yellow :weight bold ))))
   ;; `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((t (:background ,bg+1 :foreground "lavender"))))
   `(ac-completion-face ((t (:foreground ,fg-1))))
   `(ac-selection-face ((t (:background ,bg-1 :foreground "magenta"))))

   ;; `(popup-tip-face ((t (:background ,calm-forest-yellow-2 :foreground "black"))))
   ;; `(popup-scroll-bar-foreground-face ((t (:background ,calm-forest-blue-5))))
   ;; `(popup-scroll-bar-background-face ((t (:background ,bg-2))))
   ;; `(popup-isearch-match ((t (:background ,bg :foreground ,fg))))

   ;; ;; android mode
   ;; `(android-mode-debug-face ((t (:foreground ,calm-forest-green+1))))
   ;; `(android-mode-error-face ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(android-mode-info-face ((t (:foreground ,fg))))
   ;; `(android-mode-verbose-face ((t (:foreground ,calm-forest-green))))
   ;; `(android-mode-warning-face ((t (:foreground ,calm-forest-yellow))))

   ;; ;; bm
   ;; `(bm-face ((t (:background ,calm-forest-yellow-1 :foreground ,bg))))
   ;; `(bm-fringe-face ((t (:background ,calm-forest-yellow-1 :foreground ,bg))))
   ;; `(bm-fringe-persistent-face ((t (:background ,calm-forest-green-1 :foreground ,bg))))
   ;; `(bm-persistent-face ((t (:background ,calm-forest-green-1 :foreground ,bg))))

   ;; ;; clojure-test-mode
   ;; `(clojure-test-failure-face ((t (:foreground ,calm-forest-orange :weight bold :underline t))))
   ;; `(clojure-test-error-face ((t (:foreground ,calm-forest-red :weight bold :underline t))))
   ;; `(clojure-test-success-face ((t (:foreground ,calm-forest-green+1 :weight bold :underline t))))

   ;; ;; ctable
   ;; `(ctbl:face-cell-select ((t (:background ,calm-forest-blue :foreground ,bg))))
   ;; `(ctbl:face-continue-bar ((t (:background ,bg-1 :foreground ,bg))))
   ;; `(ctbl:face-row-select ((t (:background ,calm-forest-cyan :foreground ,bg))))

   ;; diff
   `(diff-added ((t (:inherit diff-changed :foreground "green2"))))
   `(diff-refine-added ((t (:inherit diff-refine-change :background ,bg+2))))
   `(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background ,bg+2))))
   `(diff-refine-removed ((t (:inherit diff-refine-change :background ,bg+2))))
   `(diff-removed ((t (:inherit diff-changed :foreground "red3"))))

   ;; `(diff-added ((,class (:foreground ,calm-forest-green+4 :background nil))
   ;;               (t (:foreground ,calm-forest-green-1 :background nil))))
   ;; `(diff-changed ((t (:foreground ,calm-forest-yellow))))
   ;; `(diff-removed ((,class (:foreground ,calm-forest-red :background nil))
   ;;                 (t (:foreground ,calm-forest-red-3 :background nil))))
   ;; `(diff-refine-added ((t :inherit diff-added :weight bold)))
   ;; `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   ;; `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   ;; `(diff-header ((,class (:background ,bg+2))
   ;;                (t (:background ,fg :foreground ,bg))))
   ;; `(diff-file-header
   ;;   ((,class (:background ,bg+2 :foreground ,fg :bold t))
   ;;    (t (:background ,fg :foreground ,bg :bold t))))

   ;; dired+
   ;;`(diredp-compressed-file-suffix ((t (:foreground ,calm-forest-orange))));
   ;;`(diredp-date-time ((t (:foreground ,calm-forest-magenta))));
   ;;`(diredp-deletion ((t (:foreground ,calm-forest-yellow))));
   ;;`(diredp-deletion-file-name ((t (:foreground ,calm-forest-red))));
   `(diredp-dir-heading ((t (:background "grey10" :foreground "magenta"))))
   `(diredp-dir-priv ((t (:foreground "CornFlowerBlue"))))
   `(diredp-display-msg ((t (:foreground "LightBlue"))))
   `(diredp-exec-priv ((t (:foreground "red2"))))
   ;;`(diredp-executable-tag ((t (:foreground ,calm-forest-green+1))));
   `(diredp-file-name ((t nil)))
   ;;`(diredp-file-suffix ((t (:foreground ,calm-forest-green))));
   ;;`(diredp-flag-mark ((t (:foreground ,calm-forest-yellow))));
   `(diredp-flag-mark-line ((t (:background "grey10"))))
   ;;`(diredp-ignored-file-name ((t (:foreground ,calm-forest-red))));
   `(diredp-inode+size ((t (:foreground "LightBlue4"))))
   `(diredp-link-priv ((t (:foreground "cyan"))))
   ;;`(diredp-mode-line-flagged ((t (:foreground ,calm-forest-yellow))));
   ;;`(diredp-mode-line-marked ((t (:foreground ,calm-forest-orange))));
   `(diredp-no-priv ((t nil)))
   ;;`(diredp-number ((t (:foreground ,calm-forest-green+1))));
   `(diredp-other-priv ((t (:foreground "violet"))))
   `(diredp-rare-priv ((t (:foreground "magenta"))))
   `(diredp-read-priv ((t (:foreground "light green"))))
   `(diredp-symlink ((t (:foreground "cyan"))))
   `(diredp-write-priv ((t (:foreground "yellow2"))))

   ;; ;; ert
   ;; `(ert-test-result-expected ((t (:foreground ,calm-forest-green+4 :background ,bg))))
   ;; `(ert-test-result-unexpected ((t (:foreground ,calm-forest-red :background ,bg))))

   ;; ;; eshell
   ;; `(eshell-prompt ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(eshell-ls-archive ((t (:foreground ,calm-forest-red-1 :weight bold))))
   ;; `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   ;; `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   ;; `(eshell-ls-directory ((t (:foreground ,calm-forest-blue+1 :weight bold))))
   ;; `(eshell-ls-executable ((t (:foreground ,calm-forest-red+1 :weight bold))))
   ;; `(eshell-ls-unreadable ((t (:foreground ,fg))))
   ;; `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   ;; `(eshell-ls-product ((t (:inherit font-lock-doc))))
   ;; `(eshell-ls-special ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(eshell-ls-symlink ((t (:foreground ,calm-forest-cyan :weight bold))))

   ;; ;; flycheck
   ;; `(flycheck-error-face ((t (:foreground ,calm-forest-red-1 :weight bold :underline t))))
   ;; `(flycheck-warning-face ((t (:foreground ,calm-forest-orange :weight bold :underline t))))

   ;; ;; flymake
   ;; `(flymake-errline ((t (:foreground ,calm-forest-red-1 :weight bold :underline t))))
   ;; `(flymake-warnline ((t (:foreground ,calm-forest-orange :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color "orange" :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color "red" :style wave)))))

   ;; ;; erc
   ;; `(erc-action-face ((t (:inherit erc-default-face))))
   ;; `(erc-bold-face ((t (:weight bold))))
   ;; `(erc-current-nick-face ((t (:foreground ,calm-forest-blue :weight bold))))
   ;; `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   ;; `(erc-default-face ((t (:foreground ,fg))))
   ;; `(erc-direct-msg-face ((t (:inherit erc-default))))
   ;; `(erc-error-face ((t (:inherit font-lock-warning))))
   ;; `(erc-fool-face ((t (:inherit erc-default))))
   ;; `(erc-highlight-face ((t (:inherit hover-highlight))))
   ;; `(erc-input-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(erc-keyword-face ((t (:foreground ,calm-forest-blue :weight bold))))
   ;; `(erc-nick-default-face ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(erc-my-nick-face ((t (:foreground ,calm-forest-red :weight bold))))
   ;; `(erc-nick-msg-face ((t (:inherit erc-default))))
   ;; `(erc-notice-face ((t (:foreground ,calm-forest-green))))
   ;; `(erc-pal-face ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(erc-prompt-face ((t (:foreground ,calm-forest-orange :background ,bg :weight bold))))
   ;; `(erc-timestamp-face ((t (:foreground ,calm-forest-green+1))))
   ;; `(erc-underline-face ((t (:underline t))))

   ;; ;; git-gutter
   ;; `(git-gutter:added ((t (:foreground ,calm-forest-green :weight bold :inverse-video t))))
   ;; `(git-gutter:deleted ((t (:foreground ,calm-forest-red :weight bold :inverse-video t))))
   ;; `(git-gutter:modified ((t (:foreground ,calm-forest-magenta :weight bold :inverse-video t))))
   ;; `(git-gutter:unchanged ((t (:foreground ,fg :weight bold :inverse-video t))))

   ;; ;; git-gutter-fr
   ;; `(git-gutter-fr:added ((t (:foreground ,calm-forest-green  :weight bold))))
   ;; `(git-gutter-fr:deleted ((t (:foreground ,calm-forest-red :weight bold))))
   ;; `(git-gutter-fr:modified ((t (:foreground ,calm-forest-magenta :weight bold))))

   ;; ;; gnus
   ;; `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   ;; `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   ;; `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   ;; `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   ;; `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   ;; `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   ;; `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   ;; `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   ;; `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   ;; `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   ;; `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   ;; `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   ;; `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   ;; `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   ;; `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   ;; `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   ;; `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   ;; `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   ;; `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   ;; `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   ;; `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   ;; `(gnus-header-content ((t (:inherit message-header-other))))
   ;; `(gnus-header-from ((t (:inherit message-header-from))))
   ;; `(gnus-header-name ((t (:inherit message-header-name))))
   ;; `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   ;; `(gnus-header-subject ((t (:inherit message-header-subject))))
   ;; `(gnus-summary-cancelled ((t (:foreground ,calm-forest-orange))))
   ;; `(gnus-summary-high-ancient ((t (:foreground ,calm-forest-blue))))
   ;; `(gnus-summary-high-read ((t (:foreground ,calm-forest-green :weight bold))))
   ;; `(gnus-summary-high-ticked ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(gnus-summary-high-unread ((t (:foreground ,fg :weight bold))))
   ;; `(gnus-summary-low-ancient ((t (:foreground ,calm-forest-blue))))
   ;; `(gnus-summary-low-read ((t (:foreground ,calm-forest-green))))
   ;; `(gnus-summary-low-ticked ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(gnus-summary-low-unread ((t (:foreground ,fg))))
   ;; `(gnus-summary-normal-ancient ((t (:foreground ,calm-forest-blue))))
   ;; `(gnus-summary-normal-read ((t (:foreground ,calm-forest-green))))
   ;; `(gnus-summary-normal-ticked ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(gnus-summary-normal-unread ((t (:foreground ,fg))))
   ;; `(gnus-summary-selected ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(gnus-cite-1 ((t (:foreground ,calm-forest-blue))))
   ;; `(gnus-cite-10 ((t (:foreground ,calm-forest-yellow-1))))
   ;; `(gnus-cite-11 ((t (:foreground ,calm-forest-yellow))))
   ;; `(gnus-cite-2 ((t (:foreground ,calm-forest-blue-1))))
   ;; `(gnus-cite-3 ((t (:foreground ,calm-forest-blue-2))))
   ;; `(gnus-cite-4 ((t (:foreground ,calm-forest-green+2))))
   ;; `(gnus-cite-5 ((t (:foreground ,calm-forest-green+1))))
   ;; `(gnus-cite-6 ((t (:foreground ,calm-forest-green))))
   ;; `(gnus-cite-7 ((t (:foreground ,calm-forest-red))))
   ;; `(gnus-cite-8 ((t (:foreground ,calm-forest-red-1))))
   ;; `(gnus-cite-9 ((t (:foreground ,calm-forest-red-2))))
   ;; `(gnus-group-news-1-empty ((t (:foreground ,calm-forest-yellow))))
   ;; `(gnus-group-news-2-empty ((t (:foreground ,calm-forest-green+3))))
   ;; `(gnus-group-news-3-empty ((t (:foreground ,calm-forest-green+1))))
   ;; `(gnus-group-news-4-empty ((t (:foreground ,calm-forest-blue-2))))
   ;; `(gnus-group-news-5-empty ((t (:foreground ,calm-forest-blue-3))))
   ;; `(gnus-group-news-6-empty ((t (:foreground ,bg+2))))
   ;; `(gnus-group-news-low-empty ((t (:foreground ,bg+2))))
   ;; `(gnus-signature ((t (:foreground ,calm-forest-yellow))))
   ;; `(gnus-x ((t (:background ,fg :foreground ,bg))))

   ;; ;; guide-key
   ;; `(guide-key/highlight-command-face ((t (:foreground ,calm-forest-blue))))
   ;; `(guide-key/key-face ((t (:foreground ,calm-forest-green))))
   ;; `(guide-key/prefix-command-face ((t (:foreground ,calm-forest-green+1))))

   ;; ;; helm
   ;; `(helm-header
   ;;   ((t (:foreground ,calm-forest-green
   ;;                    :background ,bg
   ;;                    :underline nil
   ;;                    :box nil))))
   ;; `(helm-source-header
   ;;   ((t (:foreground ,calm-forest-yellow
   ;;                    :background ,bg-2
   ;;                    :underline nil
   ;;                    :weight bold
   ;;                    :box (:line-width -1 :style released-button)))))
   ;; `(helm-selection ((t (:background ,bg+1 :underline nil))))
   ;; `(helm-selection-line ((t (:background ,bg+1))))
   ;; `(helm-visible-mark ((t (:foreground ,bg :background ,calm-forest-yellow-2))))
   ;; `(helm-candidate-number ((t (:foreground ,calm-forest-green+4 :background ,bg-2))))
   ;; `(helm-ff-directory ((t (:foreground ,calm-forest-magenta))))

   ;; ido-mode
   `(ido-first-match ((t (:foreground "magenta"))))
   `(ido-only-match ((t (:foreground "magenta" :slant italic))))
   `(ido-subdir ((((min-colors 88) (class color)) (:foreground "cyan"))))

   ;; ;; js2-mode
   ;; `(js2-warning-face ((t (:underline ,calm-forest-orange))))
   ;; `(js2-error-face ((t (:foreground ,calm-forest-red :weight bold))))
   ;; `(js2-jsdoc-tag-face ((t (:foreground ,calm-forest-green-1))))
   ;; `(js2-jsdoc-type-face ((t (:foreground ,calm-forest-green+2))))
   ;; `(js2-jsdoc-value-face ((t (:foreground ,calm-forest-green+3))))
   ;; `(js2-function-param-face ((t (:foreground, calm-forest-green+3))))
   ;; `(js2-external-variable-face ((t (:foreground ,calm-forest-orange))))

   ;; ;; jabber-mode
   ;; `(jabber-roster-user-away ((t (:foreground ,calm-forest-green+2))))
   ;; `(jabber-roster-user-online ((t (:foreground ,calm-forest-blue-1))))
   ;; `(jabber-roster-user-dnd ((t (:foreground ,calm-forest-red+1))))
   ;; `(jabber-rare-time-face ((t (:foreground ,calm-forest-green+1))))
   ;; `(jabber-chat-prompt-local ((t (:foreground ,calm-forest-blue-1))))
   ;; `(jabber-chat-prompt-foreign ((t (:foreground ,calm-forest-red+1))))
   ;; `(jabber-activity-face((t (:foreground ,calm-forest-red+1))))
   ;; `(jabber-activity-personal-face ((t (:foreground ,calm-forest-blue+1))))
   ;; `(jabber-title-small ((t (:height 1.1 :weight bold))))
   ;; `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   ;; `(jabber-title-large ((t (:height 1.3 :weight bold))))

   ;; ;; linum-mode
   ;; `(linum ((t (:foreground ,calm-forest-green+2 :background ,bg))))

   ;; ;; macrostep
   ;; `(macrostep-gensym-1
   ;;   ((t (:foreground ,calm-forest-green+2 :background ,bg-2))))
   ;; `(macrostep-gensym-2
   ;;   ((t (:foreground ,calm-forest-red+1 :background ,bg-2))))
   ;; `(macrostep-gensym-3
   ;;   ((t (:foreground ,calm-forest-blue+1 :background ,bg-2))))
   ;; `(macrostep-gensym-4
   ;;   ((t (:foreground ,calm-forest-magenta :background ,bg-2))))
   ;; `(macrostep-gensym-5
   ;;   ((t (:foreground ,calm-forest-yellow :background ,bg-2))))
   ;; `(macrostep-expansion-highlight-face
   ;;   ((t (:inherit highlight))))
   ;; `(macrostep-macro-face
   ;;   ((t (:underline t))))

   ;; ;; magit
   ;; `(magit-section-title ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(magit-branch ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(magit-item-highlight ((t (:background ,bg+1))))

   ;; ;; egg
   ;; `(egg-text-base ((t (:foreground ,fg))))
   ;; `(egg-help-header-1 ((t (:foreground ,calm-forest-yellow))))
   ;; `(egg-help-header-2 ((t (:foreground ,calm-forest-green+3))))
   ;; `(egg-branch ((t (:foreground ,calm-forest-yellow))))
   ;; `(egg-branch-mono ((t (:foreground ,calm-forest-yellow))))
   ;; `(egg-term ((t (:foreground ,calm-forest-yellow))))
   ;; `(egg-diff-add ((t (:foreground ,calm-forest-green+4))))
   ;; `(egg-diff-del ((t (:foreground ,calm-forest-red+1))))
   ;; `(egg-diff-file-header ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(egg-section-title ((t (:foreground ,calm-forest-yellow))))
   ;; `(egg-stash-mono ((t (:foreground ,calm-forest-green+4))))

   ;; ;; message-mode
   ;; `(message-cited-text ((t (:inherit font-lock-comment))))
   ;; `(message-header-name ((t (:foreground ,calm-forest-green+1))))
   ;; `(message-header-other ((t (:foreground ,calm-forest-green))))
   ;; `(message-header-to ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(message-header-from ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(message-header-cc ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(message-header-newsgroups ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(message-header-subject ((t (:foreground ,calm-forest-orange :weight bold))))
   ;; `(message-header-xheader ((t (:foreground ,calm-forest-green))))
   ;; `(message-mml ((t (:foreground ,calm-forest-yellow :weight bold))))
   ;; `(message-separator ((t (:inherit font-lock-comment))))

   ;; ;; mew
   ;; `(mew-face-header-subject ((t (:foreground ,calm-forest-orange))))
   ;; `(mew-face-header-from ((t (:foreground ,calm-forest-yellow))))
   ;; `(mew-face-header-date ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-header-to ((t (:foreground ,calm-forest-red))))
   ;; `(mew-face-header-key ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-header-private ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-header-important ((t (:foreground ,calm-forest-blue))))
   ;; `(mew-face-header-marginal ((t (:foreground ,fg :weight bold))))
   ;; `(mew-face-header-warning ((t (:foreground ,calm-forest-red))))
   ;; `(mew-face-header-xmew ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-header-xmew-bad ((t (:foreground ,calm-forest-red))))
   ;; `(mew-face-body-url ((t (:foreground ,calm-forest-orange))))
   ;; `(mew-face-body-comment ((t (:foreground ,fg :slant italic))))
   ;; `(mew-face-body-cite1 ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-body-cite2 ((t (:foreground ,calm-forest-blue))))
   ;; `(mew-face-body-cite3 ((t (:foreground ,calm-forest-orange))))
   ;; `(mew-face-body-cite4 ((t (:foreground ,calm-forest-yellow))))
   ;; `(mew-face-body-cite5 ((t (:foreground ,calm-forest-red))))
   ;; `(mew-face-mark-review ((t (:foreground ,calm-forest-blue))))
   ;; `(mew-face-mark-escape ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-mark-delete ((t (:foreground ,calm-forest-red))))
   ;; `(mew-face-mark-unlink ((t (:foreground ,calm-forest-yellow))))
   ;; `(mew-face-mark-refile ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-mark-unread ((t (:foreground ,calm-forest-red-2))))
   ;; `(mew-face-eof-message ((t (:foreground ,calm-forest-green))))
   ;; `(mew-face-eof-part ((t (:foreground ,calm-forest-yellow))))

   ;; ;; mic-paren
   ;; `(paren-face-match ((t (:foreground ,calm-forest-cyan :background ,bg :weight bold))))
   ;; `(paren-face-mismatch ((t (:foreground ,bg :background ,calm-forest-magenta :weight bold))))
   ;; `(paren-face-no-match ((t (:foreground ,bg :background ,calm-forest-red :weight bold))))

   ;; ;; mingus
   ;; `(mingus-directory-face ((t (:foreground ,calm-forest-blue))))
   ;; `(mingus-pausing-face ((t (:foreground ,calm-forest-magenta))))
   ;; `(mingus-playing-face ((t (:foreground ,calm-forest-cyan))))
   ;; `(mingus-playlist-face ((t (:foreground ,calm-forest-cyan ))))
   ;; `(mingus-song-file-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(mingus-stopped-face ((t (:foreground ,calm-forest-red))))

   ;; ;; nav
   ;; `(nav-face-heading ((t (:foreground ,calm-forest-yellow))))
   ;; `(nav-face-button-num ((t (:foreground ,calm-forest-cyan))))
   ;; `(nav-face-dir ((t (:foreground ,calm-forest-green))))
   ;; `(nav-face-hdir ((t (:foreground ,calm-forest-red))))
   ;; `(nav-face-file ((t (:foreground ,fg))))
   ;; `(nav-face-hfile ((t (:foreground ,calm-forest-red-4))))

   ;; ;; mu4e
   ;; `(mu4e-cited-1-face ((t (:foreground ,calm-forest-blue    :slant italic))))
   ;; `(mu4e-cited-2-face ((t (:foreground ,calm-forest-green+2 :slant italic))))
   ;; `(mu4e-cited-3-face ((t (:foreground ,calm-forest-blue-2  :slant italic))))
   ;; `(mu4e-cited-4-face ((t (:foreground ,calm-forest-green   :slant italic))))
   ;; `(mu4e-cited-5-face ((t (:foreground ,calm-forest-blue-4  :slant italic))))
   ;; `(mu4e-cited-6-face ((t (:foreground ,calm-forest-green-1 :slant italic))))
   ;; `(mu4e-cited-7-face ((t (:foreground ,calm-forest-blue    :slant italic))))
   ;; `(mu4e-replied-face ((t (:foreground ,bg+3))))
   ;; `(mu4e-trashed-face ((t (:foreground ,bg+3 :strike-through t))))

   ;; ;; mumamo
   `(mumamo-background-chunk-major ((t nil)))
   `(mumamo-background-chunk-submode1 ((t (:background ,bg-2))))
   `(mumamo-background-chunk-submode2 ((t (:background ,bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,bg+1))))

   ;; ;; org-mode
   ;; `(org-agenda-date-today
   ;;   ((t (:foreground "white" :slant italic :weight bold))) t)
   ;; `(org-agenda-structure
   ;;   ((t (:inherit font-lock-comment-face))))
   ;; `(org-archived ((t (:foreground ,fg :weight bold))))
   ;; `(org-checkbox ((t (:background ,bg+2 :foreground "white"
   ;;                                 :box (:line-width 1 :style released-button)))))
   ;; `(org-date ((t (:foreground ,calm-forest-blue :underline t))))
   ;; `(org-deadline-announce ((t (:foreground ,calm-forest-red-1))))
   ;; `(org-done ((t (:bold t :weight bold :foreground ,calm-forest-green+3))))
   ;; `(org-formula ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(org-headline-done ((t (:foreground ,calm-forest-green+3))))
   ;; `(org-hide ((t (:foreground ,bg-2))))
   ;; `(org-level-1 ((t (:foreground ,calm-forest-orange))))
   ;; `(org-level-2 ((t (:foreground ,calm-forest-green+4))))
   ;; `(org-level-3 ((t (:foreground ,calm-forest-blue-1))))
   ;; `(org-level-4 ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(org-level-5 ((t (:foreground ,calm-forest-cyan))))
   ;; `(org-level-6 ((t (:foreground ,calm-forest-green+2))))
   ;; `(org-level-7 ((t (:foreground ,calm-forest-red-4))))
   ;; `(org-level-8 ((t (:foreground ,calm-forest-blue-4))))
   ;; `(org-link ((t (:foreground ,calm-forest-yellow-2 :underline t))))
   ;; `(org-scheduled ((t (:foreground ,calm-forest-green+4))))
   ;; `(org-scheduled-previously ((t (:foreground ,calm-forest-red-4))))
   ;; `(org-scheduled-today ((t (:foreground ,calm-forest-blue+1))))
   ;; `(org-special-keyword ((t (:foreground ,fg-1 :weight normal))))
   ;; `(org-table ((t (:foreground ,calm-forest-green+2))))
   ;; `(org-tag ((t (:bold t :weight bold))))
   ;; `(org-time-grid ((t (:foreground ,calm-forest-orange))))
   ;; `(org-todo ((t (:bold t :foreground ,calm-forest-red :weight bold))))
   ;; `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((t (:bold t :foreground ,calm-forest-red :weight bold :underline nil))))
   ;; `(org-column ((t (:background ,bg-2))))
   ;; `(org-column-title ((t (:background ,bg-2 :underline t :weight bold))))

   ;; ;; outline
   ;; `(outline-1 ((t (:foreground ,calm-forest-orange))))
   ;; `(outline-2 ((t (:foreground ,calm-forest-green+4))))
   ;; `(outline-3 ((t (:foreground ,calm-forest-blue-1))))
   ;; `(outline-4 ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(outline-5 ((t (:foreground ,calm-forest-cyan))))
   ;; `(outline-6 ((t (:foreground ,calm-forest-green+2))))
   ;; `(outline-7 ((t (:foreground ,calm-forest-red-4))))
   ;; `(outline-8 ((t (:foreground ,calm-forest-blue-4))))

   ;; ;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
   ;; `(rainbow-delimiters-depth-2-face ((t (:foreground ,calm-forest-green+2))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(rainbow-delimiters-depth-4-face ((t (:foreground ,calm-forest-cyan))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,calm-forest-green-1))))
   ;; `(rainbow-delimiters-depth-6-face ((t (:foreground ,calm-forest-blue+1))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,calm-forest-yellow-1))))
   ;; `(rainbow-delimiters-depth-8-face ((t (:foreground ,calm-forest-green+1))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,calm-forest-blue-2))))
   ;; `(rainbow-delimiters-depth-10-face ((t (:foreground ,calm-forest-orange))))
   ;; `(rainbow-delimiters-depth-11-face ((t (:foreground ,calm-forest-green))))
   ;; `( rainbow-delimiters-depth-12-face ((t (:foreground ,calm-forest-blue-5))))

   ;; ;;rcirc
   ;; `(rcirc-my-nick ((t (:foreground ,calm-forest-blue))))
   ;; `(rcirc-other-nick ((t (:foreground ,calm-forest-orange))))
   ;; `(rcirc-bright-nick ((t (:foreground ,calm-forest-blue+1))))
   ;; `(rcirc-dim-nick ((t (:foreground ,calm-forest-blue-2))))
   ;; `(rcirc-server ((t (:foreground ,calm-forest-green))))
   ;; `(rcirc-server-prefix ((t (:foreground ,calm-forest-green+1))))
   ;; `(rcirc-timestamp ((t (:foreground ,calm-forest-green+2))))
   ;; `(rcirc-nick-in-message ((t (:foreground ,calm-forest-yellow))))
   ;; `(rcirc-nick-in-message-full-line ((t (:bold t))))
   ;; `(rcirc-prompt ((t (:foreground ,calm-forest-yellow :bold t))))
   ;; `(rcirc-track-nick ((t (:inverse-video t))))
   ;; `(rcirc-track-keyword ((t (:bold t))))
   ;; `(rcirc-url ((t (:bold t))))
   ;; `(rcirc-keyword ((t (:foreground ,calm-forest-yellow :bold t))))

   ;; ;; rpm-mode
   ;; `(rpm-spec-dir-face ((t (:foreground ,calm-forest-green))))
   ;; `(rpm-spec-doc-face ((t (:foreground ,calm-forest-green))))
   ;; `(rpm-spec-ghost-face ((t (:foreground ,calm-forest-red))))
   ;; `(rpm-spec-macro-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(rpm-spec-obsolete-tag-face ((t (:foreground ,calm-forest-red))))
   ;; `(rpm-spec-package-face ((t (:foreground ,calm-forest-red))))
   ;; `(rpm-spec-section-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(rpm-spec-tag-face ((t (:foreground ,calm-forest-blue))))
   ;; `(rpm-spec-var-face ((t (:foreground ,calm-forest-red))))

   ;; ;; rst-mode
   ;; `(rst-level-1-face ((t (:foreground ,calm-forest-orange))))
   ;; `(rst-level-2-face ((t (:foreground ,calm-forest-green+1))))
   ;; `(rst-level-3-face ((t (:foreground ,calm-forest-blue-1))))
   ;; `(rst-level-4-face ((t (:foreground ,calm-forest-yellow-2))))
   ;; `(rst-level-5-face ((t (:foreground ,calm-forest-cyan))))
   ;; `(rst-level-6-face ((t (:foreground ,calm-forest-green-1))))

   ;; show-paren
   `(show-paren-match ((t (:background "turquoise"))))
   `(show-paren-mismatch ((t (:background "purple" :foreground "white"))))

   ;; `(show-paren-mismatch ((t (:inherit warning))))
   ;; `(show-paren-match ((t (:background ,bg :inverse-video t))))

   ;; ;; sml-mode-line
   ;; '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; ;; SLIME
   ;; `(slime-repl-inputed-output-face ((t (:foreground ,calm-forest-red))))

   ;; ;; tabbar
   ;; `(tabbar-button ((t (:foreground ,fg
   ;;                                  :background ,bg))))
   ;; `(tabbar-selected ((t (:foreground ,fg
   ;;                                    :background ,bg
   ;;                                    :box (:line-width -1 :style pressed-button)))))
   ;; `(tabbar-unselected ((t (:foreground ,fg
   ;;                                      :background ,bg+1
   ;;                                      :box (:line-width -1 :style released-button)))))

   ;; term
   `(term ((t (:foreground "lavender" :background ,bg))))
   `(term-bold ((t (:inherit term :weight bold))))
   `(term-underline ((t (:inherit term :underline t))))

   `(term-color-black ((t (:foreground ,bg :background ,bg-2))))
   `(term-color-blue ((t (:foreground "#1020FF" :background "blue4"))))
   `(term-color-cyan ((t (:foreground "cyan1" :background "cyan3"))))
   `(term-color-green ((t (:foreground "green3" :background "green4"))))
   `(term-color-magenta ((t (:foreground "magenta2" :background "magenta4"))))
   `(term-color-red ((t (:foreground "red3" :background "red4"))))
   `(term-color-white ((t (:foreground "white" :background "gray"))))
   `(term-color-yellow ((t (:foreground "yellow2" :background "yellow3"))))

   ;; ;; volatile-highlights
   ;; `(vhl/default-face ((t (:background ,bg-1))))

   ;; ;; emacs-w3m
   ;; `(w3m-anchor ((t (:foreground ,calm-forest-yellow :underline t
   ;;                               :weight bold))))
   ;; `(w3m-arrived-anchor ((t (:foreground ,calm-forest-yellow-2
   ;;                                       :underline t :weight normal))))
   ;; `(w3m-form ((t (:foreground ,calm-forest-red-1 :underline t))))
   ;; `(w3m-header-line-location-title ((t (:foreground ,calm-forest-yellow
   ;;                                                   :underline t :weight bold))))
   ;; '(w3m-history-current-url ((t (:inherit match))))
   ;; `(w3m-lnum ((t (:foreground ,calm-forest-green+2 :background ,bg))))
   ;; `(w3m-lnum-match ((t (:background ,bg-2
   ;;                                   :foreground ,calm-forest-orange
   ;;                                   :weight bold))))
   ;; `(w3m-lnum-minibuffer-prompt ((t (:foreground ,calm-forest-yellow))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:foreground "firebrick"))))
   `(whitespace-hspace ((((class color) (background dark)) (:foreground "aquamarine3"))))
   `(whitespace-indentation ((t (:foreground "firebrick"))))
   `(whitespace-line ((t (:foreground "violet"))))
   `(whitespace-space ((((class color) (background dark)) (:foreground "aquamarine3"))))
   `(whitespace-space-after-tab ((t (:foreground "firebrick"))))
   `(whitespace-space-before-tab ((t (:foreground "firebrick"))))
   `(whitespace-tab ((((class color) (background dark)) (:foreground "aquamarine3"))))
   `(whitespace-trailing ((t (:foreground "yellow" :weight bold))))
   ;;`(whitespace-newline ((t (:foreground ,bg+1))))

   ;; ;; wanderlust
   ;; `(wl-highlight-folder-few-face ((t (:foreground ,calm-forest-red-2))))
   ;; `(wl-highlight-folder-many-face ((t (:foreground ,calm-forest-red-1))))
   ;; `(wl-highlight-folder-path-face ((t (:foreground ,calm-forest-orange))))
   ;; `(wl-highlight-folder-unread-face ((t (:foreground ,calm-forest-blue))))
   ;; `(wl-highlight-folder-zero-face ((t (:foreground ,fg))))
   ;; `(wl-highlight-folder-unknown-face ((t (:foreground ,calm-forest-blue))))
   ;; `(wl-highlight-message-citation-header ((t (:foreground ,calm-forest-red-1))))
   ;; `(wl-highlight-message-cited-text-1 ((t (:foreground ,calm-forest-red))))
   ;; `(wl-highlight-message-cited-text-2 ((t (:foreground ,calm-forest-green+2))))
   ;; `(wl-highlight-message-cited-text-3 ((t (:foreground ,calm-forest-blue))))
   ;; `(wl-highlight-message-cited-text-4 ((t (:foreground ,calm-forest-blue+1))))
   ;; `(wl-highlight-message-header-contents-face ((t (:foreground ,calm-forest-green))))
   ;; `(wl-highlight-message-headers-face ((t (:foreground ,calm-forest-red+1))))
   ;; `(wl-highlight-message-important-header-contents ((t (:foreground ,calm-forest-green+2))))
   ;; `(wl-highlight-message-header-contents ((t (:foreground ,calm-forest-green+1))))
   ;; `(wl-highlight-message-important-header-contents2 ((t (:foreground ,calm-forest-green+2))))
   ;; `(wl-highlight-message-signature ((t (:foreground ,calm-forest-green))))
   ;; `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,fg))))
   ;; `(wl-highlight-summary-answered-face ((t (:foreground ,calm-forest-blue))))
   ;; `(wl-highlight-summary-disposed-face ((t (:foreground ,fg
   ;;                                                       :slant italic))))
   ;; `(wl-highlight-summary-new-face ((t (:foreground ,calm-forest-blue))))
   ;; `(wl-highlight-summary-normal-face ((t (:foreground ,fg))))
   ;; `(wl-highlight-summary-thread-top-face ((t (:foreground ,calm-forest-yellow))))
   ;; `(wl-highlight-thread-indent-face ((t (:foreground ,calm-forest-magenta))))
   ;; `(wl-highlight-summary-refiled-face ((t (:foreground ,fg))))
   ;; `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

   ;; ;; which-func-mode
   ;; `(which-func ((t (:foreground ,calm-forest-green+4))))

   ;; ;; yascroll
   ;; `(yascroll:thumb-text-area ((t (:background ,bg-2))))
   ;; `(yascroll:thumb-fringe ((t (:background ,bg-2 :foreground ,bg-2)))))

  ;; ;;; custom theme variables
  ;; (custom-theme-set-variables
  ;;  'calm-forest
  ;;  `(ansi-color-names-vector [,bg ,calm-forest-red ,calm-forest-green ,calm-forest-yellow
  ;;                                         ,calm-forest-blue ,calm-forest-magenta ,calm-forest-cyan ,fg])

  ;;  ;; fill-column-indicator
  ;;  `(fci-rule-color ,bg-1)

   ;; ;; vc-annotate
   ;; `(vc-annotate-color-map
   ;;   '(( 20. . ,calm-forest-red-1)
   ;;     ( 40. . ,calm-forest-red)
   ;;     ( 60. . ,calm-forest-orange)
   ;;     ( 80. . ,calm-forest-yellow-2)
   ;;     (100. . ,calm-forest-yellow-1)
   ;;     (120. . ,calm-forest-yellow)
   ;;     (140. . ,calm-forest-green-1)
   ;;     (160. . ,calm-forest-green)
   ;;     (180. . ,calm-forest-green+1)
   ;;     (200. . ,calm-forest-green+2)
   ;;     (220. . ,calm-forest-green+3)
   ;;     (240. . ,calm-forest-green+4)
   ;;     (260. . ,calm-forest-cyan)
   ;;     (280. . ,calm-forest-blue-2)
   ;;     (300. . ,calm-forest-blue-1)
   ;;     (320. . ,calm-forest-blue)
   ;;     (340. . ,calm-forest-blue+1)
   ;;     (360. . ,calm-forest-magenta)))
   ;; `(vc-annotate-very-old-color ,calm-forest-magenta)
   ;; `(vc-annotate-background ,bg-2)
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'calm-forest)

;;; calm-forest-theme.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           _                 _    _
;;  __  ___ | | ___  _ _  ___ | |_ | |_   ___  _ __   ___  ___
;; / _|/ _ \| |/ _ \| '_||___||  _|| ' \ / -_)| '  \ / -_)|___|
;; \__|\___/|_|\___/|_|        \__||_||_|\___||_|_|_|\___|
;;            _              __                     _
;;  __  __ _ | | _ __  ___  / _| ___  _ _  ___  ___| |_
;; / _|/ _` || || '  \|___||  _|/ _ \| '_|/ -_)(_-<|  _|
;; \__|\__,_||_||_|_|_|    |_|  \___/|_|  \___|/__/ \__|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun color-theme-calm-forest ()
;;   "Color theme by Artur Hefczyc, created 2003-04-18."
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-calm-forest
;;      ((background-color . "gray12")
;;       (background-mode . dark)
;;       (border-color . "black")
;;       (cursor-color . "orange")
;;       (foreground-color . "green")
;;       (mouse-color . "yellow"))
;;      ((help-highlight-face . underline)
;;       (list-matching-lines-face . bold)
;;       (senator-eldoc-use-color . t)
;;       (view-highlight-face . highlight)
;;       (widget-mouse-face . highlight))
;;      `(default ((t (:stipple nil :background "gray12" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "outline-courier new"))))
;;      `(Info-title-1-face ((t (:bold t :weight bold :family "helv" :height 1.728))))
;;      `(Info-title-2-face ((t (:bold t :family "helv" :weight bold :height 1.44))))
;;      `(Info-title-3-face ((t (:bold t :weight bold :family "helv" :height 1.2))))
;;      `(Info-title-4-face ((t (:bold t :family "helv" :weight bold))))
;;      `(bold ((t (:bold t :weight bold))))
;;      `(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
;;      `(border ((t (:background "black"))))
;;      `(comint-highlight-input ((t (:bold t :weight bold))))
;;      `(comint-highlight-prompt ((t (:foreground "cyan"))))
;;      `(cparen-around-andor-face ((t (:bold t :foreground "maroon" :weight bold))))
;;      `(cparen-around-begin-face ((t (:foreground "maroon"))))
;;      `(cparen-around-conditional-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))
;;      `(cparen-around-define-face ((t (:bold t :foreground "Blue" :weight bold))))
;;      `(cparen-around-lambda-face ((t (:foreground "LightSeaGreen"))))
;;      `(cparen-around-letdo-face ((t (:bold t :foreground "LightSeaGreen" :weight bold))))
;;      `(cparen-around-quote-face ((t (:foreground "SaddleBrown"))))
;;      `(cparen-around-set!-face ((t (:foreground "OrangeRed"))))
;;      `(cparen-around-syntax-rules-face ((t (:foreground "Magenta"))))
;;      `(cparen-around-vector-face ((t (:foreground "chocolate"))))
;;      `(cparen-binding-face ((t (:foreground "ForestGreen"))))
;;      `(cparen-binding-list-face ((t (:bold t :foreground "ForestGreen" :weight bold))))
;;      `(cparen-conditional-clause-face ((t (:foreground "RoyalBlue"))))
;;      `(cparen-normal-paren-face ((t (:foreground "grey50"))))
;;      `(cursor ((t (:background "orange"))))
;;      `(eieio-custom-slot-tag-face ((t (:foreground "light blue"))))
;;      `(extra-whitespace-face ((t (:background "pale green"))))
;;      `(fixed-pitch ((t (:family "courier"))))
;;      `(font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))
;;      `(font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))
;;      `(font-latex-math-face ((t (:foreground "burlywood"))))
;;      `(font-latex-sedate-face ((t (:foreground "LightGray"))))
;;      `(font-latex-string-face ((t (:foreground "RosyBrown"))))
;;      `(font-latex-warning-face ((t (:bold t :foreground "Red" :weight bold))))
;;      `(header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
;;      `(highlight ((t (:background "darkolivegreen"))))
;;      `(info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
;;      `(info-header-xref ((t (:bold t :weight bold :foreground "cyan"))))
;;      `(info-menu-5 ((t (:foreground "red1"))))
;;      `(info-menu-header ((t (:bold t :family "helv" :weight bold))))
;;      `(info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
;;      `(info-xref ((t (:bold t :foreground "cyan" :weight bold))))
;;      `(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
;;      `(isearch-lazy-highlight-face ((t (:background "paleturquoise4"))))
;;      `(italic ((t (:italic t :slant italic))))
;;      `(jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))
;;      `(jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))
;;      `(jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))
;;      `(jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))
;;      `(jde-java-font-lock-api-face ((t (:foreground "light goldenrod"))))
;;      `(jde-java-font-lock-bold-face ((t (:bold t :weight bold))))
;;      `(jde-java-font-lock-code-face ((t (nil))))
;;      `(jde-java-font-lock-constant-face ((t (:foreground "Aquamarine"))))
;;      `(jde-java-font-lock-doc-tag-face ((t (:foreground "light coral"))))
;;      `(jde-java-font-lock-italic-face ((t (:italic t :slant italic))))
;;      `(jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))
;;      `(jde-java-font-lock-modifier-face ((t (:foreground "LightSteelBlue"))))
;;      `(jde-java-font-lock-number-face ((t (:foreground "LightSalmon"))))
;;      `(jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))
;;      `(jde-java-font-lock-package-face ((t (:foreground "steelblue1"))))
;;      `(jde-java-font-lock-pre-face ((t (nil))))
;;      `(jde-java-font-lock-underline-face ((t (:underline t))))
;;      `(semantic-dirty-token-face ((t (:background "gray10"))))
;;      `(semantic-unmatched-syntax-face ((t (:underline "red"))))
;;      `(senator-intangible-face ((t (:foreground "gray75"))))
;;      `(senator-momentary-highlight-face ((t (:background "gray30"))))
;;      `(senator-read-only-face ((t (:background "#664444"))))
;;      `(speedbar-button-face ((t (:foreground "green3"))))
;;      `(speedbar-directory-face ((t (:foreground "light blue"))))
;;      `(speedbar-file-face ((t (:foreground "cyan"))))
;;      `(speedbar-highlight-face ((t (:background "sea green"))))
;;      `(speedbar-selected-face ((t (:foreground "red" :underline t))))
;;      `(speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
;;      `(speedbar-tag-face ((t (:foreground "yellow"))))
;;      `(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
;;      `(trailing-whitespace ((t (:background "red"))))
;;      `(underline ((t (:underline t))))
;;      `(variable-pitch ((t (:family "helv"))))
;;      `(widget-button-face ((t (:bold t :weight bold))))
;;      `(widget-button-pressed-face ((t (:foreground "red"))))
;;      `(widget-documentation-face ((t (:foreground "lime green"))))
;;      `(widget-field-face ((t (:background "dim gray"))))
;;      `(widget-inactive-face ((t (:foreground "light gray"))))
;;      `(widget-single-line-field-face ((t (:background "dim gray")))))))


