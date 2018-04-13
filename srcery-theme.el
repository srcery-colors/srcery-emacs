;;; srcery-theme.el --- Dark color theme.
;; Copyright (C) 2018 Daniel Berg

;; Author: Daniel Berg
;; URL: https://github.com/roosta/emacs-srcery

;; Version: 0.1.0
;; Keywords: faces
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; Port of vim-srcery, a dark color theme. <https://github.com/roosta/emacs-srcery>
;; Should work well in a 256 color terminal.
;; The port is based on https://github.com/nashamri/spacemacs-theme

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The monokai theme requires Emacs 24 or later!"))

(deftheme srcery "Srcery color theme")

(defmacro srcery-dyn-let (varlist fn setfaces setvars)
  "Macro to bind color variables.
Argument VARLIST list of color variables
Argument FN Override function
Argument SETFACES ‘create-theme-set-faces’ with bound colors
Argument SETVARS ‘custom-theme-set-variables’ with bound colors"
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup srcery nil
  "Srcery options."
  :group 'faces)

;; (defcustom srcery-comment-bg nil
;;   "Use a background for comment lines."
;;   :type 'boolean
;;   :group 'srcery)

(defcustom srcery-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'srcery)

(defcustom srcery-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'srcery)

(defcustom srcery-invert-matches nil
  "Use inverse video for search matches."
  :type 'boolean
  :group 'srcery)

(defun srcery-true-color-p ()
  "Check if in tty or gui."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun srcery-custom-colors-override ()
  "Overrides base colors with values from ‘srcery-custom-colors’."
  (mapcar (lambda (x) (list (car x) (cdr x)))
          srcery-custom-colors))

(defun srcery-create-theme ()
  "Create srcery theme."
  (srcery-dyn-let ((class '((class color) (min-colors 89)))

            (black          (if (srcery-true-color-p) "#1C1B19" "black"))
            (red            (if (srcery-true-color-p) "#EF2F27" "red"))
            (green          (if (srcery-true-color-p) "#519F50" "green"))
            (yellow         (if (srcery-true-color-p) "#FBB829" "yellow"))
            (blue           (if (srcery-true-color-p) "#2C78BF" "blue"))
            (magenta        (if (srcery-true-color-p) "#E02C6D" "magenta"))
            (cyan           (if (srcery-true-color-p) "#0AAEB3" "cyan"))
            (white          (if (srcery-true-color-p) "#918175" "white"))
            (bright-black   (if (srcery-true-color-p) "#2D2B28" "brightblack"))
            (bright-red     (if (srcery-true-color-p) "#F75341" "brightred"))
            (bright-green   (if (srcery-true-color-p) "#98BC37" "brightgreen"))
            (bright-yellow  (if (srcery-true-color-p) "#FED06E" "brightyellow"))
            (bright-blue    (if (srcery-true-color-p) "#68A8E4" "brightblue"))
            (bright-magenta (if (srcery-true-color-p) "#FF5C8F" "brightmagenta"))
            (bright-cyan    (if (srcery-true-color-p) "#53FDE9" "brightcyan"))
            (bright-white   (if (srcery-true-color-p) "#FCE8C3" "brightwhite"))

            ;; xterm colors
            (orange         (if (srcery-true-color-p) "#D75F00" "color-166"))
            (bright-orange  (if (srcery-true-color-p) "#FF8700" "color-208"))
            (hard-black     (if (srcery-true-color-p) "#121212" "color-233"))
            (xgray1         (if (srcery-true-color-p) "#262626" "color-235"))
            (xgray2         (if (srcery-true-color-p) "#303030" "color-236"))
            (xgray3         (if (srcery-true-color-p) "#3A3A3A" "color-237"))
            (xgray4         (if (srcery-true-color-p) "#444444" "color-238"))
            (xgray5         (if (srcery-true-color-p) "#4E4E4E" "color-239")))

           srcery-custom-colors-override

           (custom-theme-set-faces
            'srcery

;;;;; basics
            `(cursor                       ((,class ,(if srcery-invert-matches
                                                        `(:inverse-video t)
                                                        `(:background ,bright-white :foreground ,black)))))
            `(custom-button                ((,class :background ,black :foreground ,bright-white :box (:line-width 2 :style released-button))))
            `(default                      ((,class (:background ,black :foreground ,bright-white))))
            `(default-italic               ((,class (:italic t))))
            `(error                        ((,class (:foreground ,red :weight bold))))
            `(eval-sexp-fu-flash           ((,class (:background ,green))))
            `(eval-sexp-fu-flash-error     ((,class (:background ,red))))
            `(font-lock-builtin-face       ((,class (:foreground ,blue))))
            `(font-lock-comment-face       ((,class (:foreground ,white :italic t))))
            `(font-lock-constant-face      ((,class (:foreground ,bright-magenta))))
            `(font-lock-reference-face     ((,class (:foreground ,bright-blue))))
            `(font-lock-doc-face           ((,class (:foreground ,green))))
            `(font-lock-function-name-face ((,class (:foreground ,yellow))))
            `(font-lock-keyword-face       ((,class (:foreground ,red))))
            `(font-lock-negation-char-face ((,class (:foreground ,bright-magenta))))
            `(font-lock-preprocessor-face  ((,class (:foreground ,yellow))))
            `(font-lock-string-face        ((,class (:foreground ,bright-green))))
            `(font-lock-type-face          ((,class (:foreground ,bright-blue))))
            `(font-lock-variable-name-face ((,class (:foreground ,bright-magenta))))
            `(font-lock-warning-face       ((,class (:foreground ,bright-orange :background ,black))))
            `(fringe                       ((,class (:foreground ,bright-white))))
            `(header-line                  ((,class (:background ,black))))
            `(highlight                    ((,class ,(if srcery-invert-matches
                                                         `(:inverse-video t)
                                                       `(:background ,magenta :foreground ,bright-white)))))
            `(hl-line                      ((,class (:background ,bright-black))))
            `(isearch                      ((,class ,(if srcery-invert-matches
                                                         `(:inverse-video t)
                                                       `(:background ,magenta :foreground ,bright-white)))))
            `(lazy-highlight               ((,class ,(if srcery-invert-matches
                                                         `(:inverse-video t)
                                                       `(:background ,magenta :foreground ,bright-white)))))
            `(link                         ((,class (:inherit font-lock-comment-face :underline t))))
            `(link-visited                 ((,class (:inherit font-lock-comment-face :underline t))))
            `(match                        ((,class ,(if srcery-invert-matches
                                                         `(:inverse-video t)
                                                       `(:background ,magenta :foreground ,bright-white)))))
            `(minibuffer-prompt            ((,class (:weight bold :foreground ,yellow))))
            `(page-break-lines             ((,class (:foreground ,xgray3))))
            `(region                       ((,class (:inverse-video ,t))))
            `(secondary-selection          ((,class (:background ,xgray2))))
            `(success                      ((,class (:foreground ,green))))
            `(tooltip                      ((,class (:background ,bright-blue :foreground ,bright-white :bold nil :italic nil :underline nil))))
            `(vertical-border              ((,class (:foreground ,magenta))))
            `(warning                      ((,class (:foreground ,bright-orange))))
            `(tool-bar                     ((,class (:foreground ,bright-white))))

;;;;; ahs
            `(ahs-face                     ((,class (:background ,magenta))))
            `(ahs-plugin-whole-buffer-face ((,class (:background ,yellow :foreground ,black))))

;;;;; anzu-mode
            `(anzu-mode-line ((,class (:foreground ,yellow :weight bold))))

;;;;; auto-complete
            `(ac-completion-face ((,class (:background ,bright-black :foreground ,bright-white))))

;;;;; avy
            `(avy-lead-face   ((,class (:background ,bright-black :foreground ,bright-magenta))))
            `(avy-lead-face-0 ((,class (:background ,bright-black :foreground ,bright-yellow))))
            `(avy-lead-face-1 ((,class (:background ,bright-black :foreground ,bright-green))))
            `(avy-lead-face-2 ((,class (:background ,bright-black :foreground ,bright-blue))))

;;;;; cider
            `(cider-enlightened                       ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
            `(cider-enlightened-face                  ((,class (:background nil :box (:color ,white :line-width -1 :style nil) :foreground ,blue))))
            `(cider-enlightened-local                 ((,class (:foreground ,bright-yellow))))
            `(cider-instrumented-face                 ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
            `(cider-result-overlay-face               ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
            `(cider-test-error-face                   ((,class (:background ,bright-orange :foreground ,black))))
            `(cider-test-failure-face                 ((,class (:background ,red :foreground ,bright-white))))
            `(cider-test-success-face                 ((,class (:background ,green :foreground ,black))))
            `(cider-traced-face                       ((,class :box (:color ,cyan :line-width -1 :style nil))))
            `(cider-fringe-good-face                  ((,class :foreground ,green)))
            `(cider-fragile-button-face               ((,class :foreground ,orange :box (:style released-button))))
            `(cider-stacktrace-promoted-button-face   ((,class :foreground ,red :box (:style released-button))))
            `(cider-stacktrace-suppressed-button-face ((,class :foreground ,white :box (:style pressed-button))))
            `(cider-enlightened-local-face            ((,class :foreground ,yellow :weight bold)))

;;;;; clojure
            `(clojure-keyword-face ((,class (:foreground ,blue))))

;;;;; company
            `(company-echo-common              ((,class (:background ,bright-white :foreground ,black))))
            `(company-preview                  ((,class (:background ,xgray1 :foreground ,bright-white))))
            `(company-preview-common           ((,class (:background ,xgray1 :foreground ,bright-white))))
            `(company-preview-search           ((,class (:inherit match))))
            `(company-scrollbar-bg             ((,class (:background ,xgray1))))
            `(company-scrollbar-fg             ((,class (:background ,white))))
            `(company-template-field           ((,class (:inherit region))))
            `(company-tooltip                  ((,class (:background ,xgray1 :foreground ,white))))
            `(company-tooltip-annotation       ((,class (:foreground ,red))))
            `(company-tooltip-common           ((,class (:background ,xgray1 :foreground ,bright-white))))
            `(company-tooltip-common-selection ((,class (:foreground ,bright-magenta))))
            `(company-tooltip-mouse            ((,class (:inherit highlight))))
            `(company-tooltip-search           ((,class (:inherit match))))
            `(company-tooltip-selection        ((,class (:foreground ,magenta))))

;;;;; racer
            `(racer-tooltip           ((,class (:foreground ,bright-white :background ,xgray1))))
            `(racer-help-heading-face ((,class (:foreground ,bright-white :weight bold))))

;;;;; rust

            `(rust-builtin-formatting-macro-face ((,class (:foreground ,blue))))
            `(rust-question-mark-face            ((,class (:foreground ,blue :weight bold))))
            `(rust-string-interpolation-face     ((,class (:foreground ,bright-green :italic t))))
            `(rust-unsafe-face                   ((,class (:foreground ,bright-orange))))

;;;;; diff
            `(diff-added             ((,class :background nil :foreground ,green)))
            `(diff-changed           ((,class :background nil :foreground ,red)))
            `(diff-header            ((,class :background ,bright-black :foreground ,yellow)))
            `(diff-indicator-added   ((,class :background nil :foreground ,green)))
            `(diff-indicator-changed ((,class :background nil :foreground ,red)))
            `(diff-indicator-removed ((,class :background nil :foreground ,red)))
            `(diff-refine-added      ((,class :background ,green :foreground ,black)))
            `(diff-refine-changed    ((,class :background ,blue :foreground ,bright-white)))
            `(diff-refine-removed    ((,class :background ,red :foreground ,bright-white)))
            `(diff-removed           ((,class :background nil :foreground ,red)))

;;;;; diff-hl
            `(diff-hl-change ((,class :foreground ,blue)))
            `(diff-hl-delete ((,class :foreground ,red)))
            `(diff-hl-insert ((,class :foreground ,green)))

;;;;; dired
            `(dired-directory  ((,class (:foreground ,blue :background ,black))))
            `(dired-flagged    ((,class (:foreground ,red))))
            `(dired-header     ((,class (:foreground ,green :weight bold))))
            `(dired-ignored    ((,class (:inherit shadow))))
            `(dired-mark       ((,class (:foreground ,green :weight bold))))
            `(dired-marked     ((,class (:foreground ,magenta :weight bold))))
            `(dired-perm-write ((,class (:foreground ,bright-white :underline t))))
            `(dired-symlink    ((,class (:foreground ,cyan :background ,black :weight bold))))
            `(dired-warning    ((,class (:foreground ,bright-orange))))

;;;;; ediff
            `(ediff-current-diff-A        ((,class(:background ,xgray1 :foreground ,red))))
            `(ediff-current-diff-Ancestor ((,class(:background ,black :foreground ,cyan))))
            `(ediff-current-diff-B        ((,class(:foreground ,green))))
            `(ediff-current-diff-C        ((,class(:background ,blue :foreground ,blue))))
            `(ediff-even-diff-A           ((,class(:background ,bright-black))))
            `(ediff-even-diff-Ancestor    ((,class(:background ,bright-black))))
            `(ediff-even-diff-B           ((,class(:background ,bright-black))))
            `(ediff-even-diff-C           ((,class(:background ,bright-black))))
            `(ediff-fine-diff-A           ((,class(:background nil :weight bold))))
            `(ediff-fine-diff-Ancestor    ((,class(:background nil :weight bold))))
            `(ediff-fine-diff-B           ((,class(:background nil :weight bold))))
            `(ediff-fine-diff-C           ((,class(:background nil :weight bold))))
            `(ediff-odd-diff-A            ((,class(:background ,black))))
            `(ediff-odd-diff-Ancestor     ((,class(:background ,black))))
            `(ediff-odd-diff-B            ((,class(:background ,black))))
            `(ediff-odd-diff-C            ((,class(:background ,black))))

;;;;; ein
            `(ein:cell-input-area           ((,class (:background ,bright-black))))
            `(ein:cell-input-prompt         ((,class (:foreground ,green))))
            `(ein:cell-output-prompt        ((,class (:foreground ,red))))
            `(ein:notification-tab-normal   ((,class (:foreground ,red))))
            `(ein:notification-tab-selected ((,class (:foreground ,green :weight bold))))

;;;;; eldoc
            `(eldoc-highlight-function-argument ((,class (:foreground ,yellow :weight bold))))

;;;;; enh-ruby
            `(enh-ruby-string-delimiter-face ((,class (:foreground ,bright-green))))
            `(enh-ruby-op-face               ((,class (:background ,black :foreground ,bright-white))))

;;;;; erc
            `(erc-input-face        ((,class (:foreground ,yellow))))
            `(erc-my-nick-face      ((,class (:foreground ,red))))
            `(erc-nick-default-face ((,class (:foreground ,red))))
            `(erc-nick-prefix-face  ((,class (:foreground ,yellow))))
            `(erc-notice-face       ((,class (:foreground ,bright-green))))
            `(erc-prompt-face       ((,class (:foreground ,yellow :weight bold))))
            `(erc-timestamp-face    ((,class (:foreground ,red))))

;;;;; eshell
            `(eshell-ls-archive    ((,class (:foreground ,red :weight bold))))
            `(eshell-ls-backup     ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-clutter    ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-directory  ((,class (:foreground ,blue))))
            `(eshell-ls-executable ((,class (:foreground ,orange :weight bold))))
            `(eshell-ls-missing    ((,class (:inherit font-lock-warning-face))))
            `(eshell-ls-product    ((,class (:inherit font-lock-doc-face))))
            `(eshell-ls-special    ((,class (:foreground ,magenta :weight bold))))
            `(eshell-ls-symlink    ((,class (:foreground ,cyan :weight bold))))
            `(eshell-ls-unreadable ((,class (:foreground ,bright-white))))
            `(eshell-prompt        ((,class (:foreground ,magenta :weight bold))))

;;;;; evil
            `(evil-ex-substitute-matches ((,class (:background ,red :foreground ,bright-white))))
            `(evil-ex-substitute-replacement ((,class (:background ,bright-green :foreground ,black))))

;;;;; flycheck
            `(flycheck-error
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,red)))
               (,class (:foreground ,bright-white :background ,red :weight bold :underline t))))
            `(flycheck-error-list-checker-name ((,class (:foreground ,red))))
            `(flycheck-fringe-error ((,class (:foreground ,red :weight bold))))
            `(flycheck-fringe-info ((,class (:foreground ,red :weight bold))))
            `(flycheck-fringe-warning ((,class (:foreground ,bright-orange :weight bold))))
            `(flycheck-info
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,red)))
               (,class (:foreground ,bright-white :background ,red :weight bold :underline t))))
            `(flycheck-warning
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,bright-orange)))
               (,class (:foreground ,bright-white :background ,bright-orange :weight bold :underline t))))

;;;;; jabber
            `(jabber-activity-face          ((,class (:weight bold :foreground ,red))))
            `(jabber-activity-personal-face ((,class (:weight bold :foreground ,blue))))
            `(jabber-chat-error             ((,class (:weight bold :foreground ,red))))
            `(jabber-chat-prompt-foreign    ((,class (:weight bold :foreground ,red))))
            `(jabber-chat-prompt-local      ((,class (:weight bold :foreground ,blue))))
            `(jabber-chat-prompt-system     ((,class (:weight bold :foreground ,green))))
            `(jabber-chat-text-foreign      ((,class (:foreground ,bright-white))))
            `(jabber-chat-text-local        ((,class (:foreground ,bright-white))))
            `(jabber-rare-time-face         ((,class (:foreground ,green))))
            `(jabber-roster-user-away       ((,class (:foreground ,yellow))))
            `(jabber-roster-user-chatty     ((,class (:weight bold :foreground ,green))))
            `(jabber-roster-user-dnd        ((,class (:foreground ,red))))
            `(jabber-roster-user-error      ((,class (:foreground ,red))))
            `(jabber-roster-user-offline    ((,class (:foreground ,bright-white))))
            `(jabber-roster-user-online     ((,class (:weight bold :foreground ,green))))
            `(jabber-roster-user-xa         ((,class (:foreground ,cyan))))
;;;;; git
            `(git-commit-summary              ((,class (:foreground ,red))))
            `(git-commit-nonempty-second-line ((,class (:foreground ,yellow))))
            `(diff-file-header                ((,class (:foreground ,bright-white))))
            `(diff-hunk-header                ((,class (:foreground ,yellow))))
            `(diff-function                   ((,class (:foreground ,yellow))))
            `(diff-header                     ((,class (:foreground ,bright-white))))


;;;;; git-gutter-fr
            `(git-gutter-fr:added    ((,class (:foreground ,green :weight bold))))
            `(git-gutter-fr:deleted  ((,class (:foreground ,red :weight bold))))
            `(git-gutter-fr:modified ((,class (:foreground ,blue :weight bold))))
            `(git-gutter+-added      ((,class (:foreground ,green))))
            `(git-gutter+-deleted    ((,class (:foreground ,red))))
            `(git-gutter+-separator  ((,class (:foreground ,cyan))))
            `(git-gutter+-modified   ((,class (:foreground ,magenta))))
            `(git-gutter+-unchanged  ((,class (:foreground ,bright-white))))
            `(git-gutter:added       ((,class (:foreground ,green))))
            `(git-gutter:modified    ((,class (:foreground ,magenta))))
            `(git-gutter:unchanged   ((,class (:foreground ,bright-white))))

;;;;; git-timemachine
            `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :weight bold :background ,blue))))

;;;;; gnus
            `(gnus-emphasis-highlight-words ((,class (:background ,green :foreground ,black))))
            `(gnus-header-content           ((,class (:foreground ,red))))
            `(gnus-header-from              ((,class (:foreground ,blue))))
            `(gnus-header-name              ((,class (:foreground ,green))))
            `(gnus-header-subject           ((,class (:foreground ,yellow :weight bold))))
            `(gnus-summary-cancelled        ((,class (:background ,bright-orange :foreground ,black))))

;;;;; guide-key
            `(guide-key/highlight-command-face ((,class (:foreground ,bright-white))))
            `(guide-key/key-face               ((,class (:foreground ,red))))
            `(guide-key/prefix-command-face    ((,class (:foreground ,red :weight bold))))

;;;;; helm
            `(helm-bookmark-directory          ((,class (:inherit helm-ff-directory))))
            `(helm-bookmark-file               ((,class (:foreground ,bright-white))))
            `(helm-bookmark-gnus               ((,class (:foreground ,green))))
            `(helm-bookmark-info               ((,class (:foreground ,green))))
            `(helm-bookmark-man                ((,class (:foreground ,green))))
            `(helm-bookmark-w3m                ((,class (:foreground ,green))))
            `(helm-buffer-directory            ((,class (:foreground ,blue))))
            `(helm-buffer-file                 ((,class (:foreground ,bright-white :background ,black))))
            `(helm-buffer-not-saved            ((,class (:foreground ,green :background ,black))))
            `(helm-buffer-process              ((,class (:foreground ,red :background ,black))))
            `(helm-buffer-saved-out            ((,class (:foreground ,bright-white :background ,black))))
            `(helm-buffer-size                 ((,class (:foreground ,bright-white :background ,black))))
            `(helm-candidate-number            ((,class (:background ,black :foreground ,red :weight bold))))
            `(helm-ff-directory                ((,class (:foreground ,blue))))
            `(helm-ff-dotted-directory         ((,class (:foreground ,blue))))
            `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan))))
            `(helm-ff-executable               ((,class (:foreground ,green :background ,black :weight normal))))
            `(helm-ff-file                     ((,class (:foreground ,bright-white :background ,black :weight normal))))
            `(helm-ff-invalid-symlink          ((,class (:foreground ,red :background ,black :weight bold))))
            `(helm-ff-prefix                   ((,class (:foreground ,black :background ,red :weight normal))))
            `(helm-ff-symlink                  ((,class (:foreground ,cyan :background ,black :weight bold))))
            `(helm-grep-cmd-line               ((,class (:foreground ,bright-white :background ,black))))
            `(helm-grep-file                   ((,class (:foreground ,bright-white :background ,black))))
            `(helm-grep-finish                 ((,class (:foreground ,bright-white :background ,black))))
            `(helm-grep-lineno                 ((,class (:foreground ,bright-blue :background ,black :weight bold))))
            `(helm-grep-match                  ((,class (:foreground nil :background nil :inherit helm-match))))
            `(helm-header                      ((,class (:foreground ,bright-white :background ,black :underline nil :box nil))))
            `(helm-header-line-left-margin     ((,class (:foreground ,red :background ,nil))))
            `(helm-match                       ((,class (:foreground ,magenta))))
            `(helm-match-item                  ((,class (:foreground ,magenta))))
            `(helm-moccur-buffer               ((,class (:foreground ,blue :background ,black))))
            `(helm-selection                   ((,class (:background ,bright-black :weight bold))))
            `(helm-selection-line              ((,class (:background ,bright-black :weight bold))))
            `(helm-separator                   ((,class (:foreground ,green :background ,black))))
            `(helm-source-header               ((,class (:background ,black :foreground ,green :underline t))))
            `(helm-time-zone-current           ((,class (:foreground ,red :background ,black))))
            `(helm-time-zone-home              ((,class (:foreground ,green :background ,black))))
            `(helm-visible-mark                ((,class (:foreground ,red :background ,black))))

;;;;; helm-swoop
            `(helm-swoop-target-line-block-face ((,class (:foreground ,yellow :background ,black))))
            `(helm-swoop-target-line-face       ((,class (:background ,bright-black :weight bold))))
            `(helm-swoop-target-word-face       ((,class (:foreground ,magenta :weight bold))))

;;;;; highlights
            `(hi-yellow ((,class (:foreground ,yellow))))
            `(hi-green  ((,class (:foreground ,green))))

;;;;; highlight-indentation
            `(highlight-indentation-face ((,class (:background ,white))))

;;;;; highlight-symbol
            `(highlight-symbol-face ((,class (:background ,bright-black))))

;;;;; hydra
            `(hydra-face-blue ((,class (:foreground ,blue))))
            `(hydra-face-red  ((,class (:foreground ,red))))

;;;;; ido
            `(ido-first-match         ((,class (:foreground ,green :weight bold))))
            `(ido-only-match          ((,class (:foreground ,yellow :weight bold))))
            `(ido-subdir              ((,class (:foreground ,red))))
            `(ido-indicator           ((,class (:background ,red :foreground ,bright-white))))
            `(ido-vertical-match-face ((,class (:foreground ,green :underline nil))))

;;;;; info
            `(info-header-xref    ((,class (:foreground ,yellow :underline t))))
            `(info-menu           ((,class (:foreground ,green))))
            `(info-node           ((,class (:foreground ,yellow :weight bold))))
            `(info-quoted-name    ((,class (:foreground ,red))))
            `(info-reference-item ((,class (:background nil :underline t :weight bold))))
            `(info-string         ((,class (:foreground ,bright-green))))
            `(info-title-1        ((,class (:height 1.4 :weight bold))))
            `(info-title-2        ((,class (:height 1.3 :weight bold))))
            `(info-title-3        ((,class (:height 1.3))))
            `(info-title-4        ((,class (:height 1.2))))

;;;;; ivy
            `(ivy-current-match           ((,class (:background ,magenta :weight bold))))
            `(ivy-minibuffer-match-face-1 ((,class (:weight bold))))
            `(ivy-minibuffer-match-face-2 ((,class (:foreground ,blue :underline t))))
            `(ivy-minibuffer-match-face-3 ((,class (:foreground ,yellow :underline t))))
            `(ivy-minibuffer-match-face-4 ((,class (:foreground ,bright-green :underline t))))
            `(ivy-remote                  ((,class (:foreground ,cyan))))

;;;;; latex
            `(font-latex-bold-face                ((,class (:foreground ,green))))
            `(font-latex-italic-face              ((,class (:foreground ,red :italic t))))
            `(font-latex-match-reference-keywords ((,class (:foreground ,bright-magenta))))
            `(font-latex-match-variable-keywords  ((,class (:foreground ,blue))))
            `(font-latex-sectioning-0-face        ((,class (:weight bold :foreground ,bright-green :height ,(if srcery-org-height 1.3 1.0)))))
            `(font-latex-sectioning-1-face        ((,class (:weight bold :foreground ,bright-yellow :height ,(if srcery-org-height 1.3 1.0)))))
            `(font-latex-sectioning-2-face        ((,class (:weight bold :foreground ,blue :height ,(if srcery-org-height 1.3 1.0)))))
            `(font-latex-sectioning-3-face        ((,class (:weight bold :foreground ,cyan :height ,(if srcery-org-height 1.2 1.0)))))
            `(font-latex-sectioning-4-face        ((,class (:bold nil :foreground ,bright-green :height ,(if srcery-org-height 1.1 1.0)))))
            `(font-latex-sectioning-5-face        ((,class (:bold nil :foreground ,yellow))))
            `(font-latex-string-face              ((,class (:foreground ,bright-green))))

;;;;; linum-mode
            `(linum ((,class (:foreground ,white :background ,black))))

;;;;; linum-relative
            `(linum-relative-current-face ((,class (:foreground ,yellow))))

;;;;; magit
            `(magit-blame-culprit               ((,class :foreground ,yellow)))
            `(magit-blame-header                ((,class :foreground ,green)))
            `(magit-blame-sha1                  ((,class :foreground ,yellow)))
            `(magit-blame-subject               ((,class :foreground ,yellow)))
            `(magit-blame-time                  ((,class :foreground ,green)))
            `(magit-blame-name                  ((,class :foreground ,yellow)))
            `(magit-blame-heading               ((,class :foreground ,green)))
            `(magit-blame-hash                  ((,class :foreground ,yellow)))
            `(magit-blame-summary               ((,class :foreground ,yellow)))
            `(magit-blame-date                  ((,class :foreground ,green)))
            `(magit-log-date                    ((,class :foreground ,bright-white)))
            `(magit-log-graph                   ((,class :foreground ,bright-white)))
            `(magit-reflog-amend                ((,class :foreground ,magenta)))
            `(magit-reflog-other                ((,class :foreground ,cyan)))
            `(magit-reflog-rebase               ((,class :foreground ,magenta)))
            `(magit-reflog-remote               ((,class :foreground ,cyan)))
            `(magit-reflog-reset                ((,class :foreground ,red)))
            `(magit-branch                      ((,class (:foreground ,bright-magenta :weight bold))))
            `(magit-branch-current              ((,class (:background ,black :foreground ,blue :weight bold :box t))))
            `(magit-branch-local                ((,class (:background ,black :foreground ,blue :weight bold))))
            `(magit-branch-remote               ((,class (:background ,black :foreground ,orange :weight bold))))

            `(magit-diff-file-header            ((,class (:foreground ,yellow))))

            `(magit-diff-file-heading           ((,class (:foreground ,blue :weight light))))
            `(magit-diff-file-heading-highlight ((,class (:foreground ,blue :weight bold))))
            `(magit-diff-file-heading-selection ((,class (:foreground ,blue :weight bold :background ,bright-black))))

            `(magit-diff-hunk-heading           ((,class (:foreground ,yellow :weight light))))
            `(magit-diff-hunk-heading-highlight ((,class (:foreground ,yellow :weight bold))))
            `(magit-diff-hunk-heading-selection ((,class (:foreground ,black :background ,white :weight bold))))

            `(magit-diff-added                  ((,class (:foreground ,green :weight light))))
            `(magit-diff-removed                ((,class (:foreground ,red :weight light))))
            `(magit-diff-context                ((,class (:foreground ,white :weight light))))
            `(magit-diff-added-highlight        ((,class (:foreground ,green :weight bold))))
            `(magit-diff-removed-highlight      ((,class (:foreground ,red :weight bold))))
            `(magit-diff-context-highlight      ((,class (:foreground ,white :weight bold))))
            `(magit-diff-base                   ((,class (:foreground ,white :weight light))))
            `(magit-diff-base-highlight         ((,class (:foreground ,white :weight bold))))
            `(magit-diff-lines-boundary         ((,class (:background ,white :foreground ,black))))
            `(magit-diff-lines-heading          ((,class (:background ,white :foreground ,black))))

            `(magit-hash                        ((,class (:foreground ,yellow))))
            `(magit-item-highlight              ((,class :background ,bright-black)))
            `(magit-log-author                  ((,class (:foreground ,yellow))))
            `(magit-log-head-label-head         ((,class (:background ,yellow :foreground ,black :weight bold))))
            `(magit-log-head-label-local        ((,class (:background ,red :foreground ,black :weight bold))))
            `(magit-log-head-label-remote       ((,class (:background ,green :foreground ,black :weight bold))))
            `(magit-log-head-label-tags         ((,class (:background ,magenta :foreground ,black :weight bold))))
            `(magit-log-head-label-wip          ((,class (:background ,cyan :foreground ,black :weight bold))))
            `(magit-log-sha1                    ((,class (:foreground ,bright-green))))
            `(magit-process-ng                  ((,class (:foreground ,bright-orange :weight bold))))
            `(magit-process-ok                  ((,class (:foreground ,yellow :weight bold))))

            `(magit-section-heading             ((,class (:foreground ,red))))
            `(magit-section-highlight           ((,class (:weight bold))))
            `(section-heading-selection         ((,class (:foreground ,red :weight bold))))
            `(magit-section-title               ((,class (:background ,black :foreground ,red :weight bold))))

            `(magit-cherry-equivalent           ((,class (:foreground ,magenta))))
            `(magit-cherry-unmatched            ((,class (:foreground ,cyan))))

            `(magit-reflog-checkout             ((,class (:foreground ,blue))))
            `(magit-reflog-cherry-pick          ((,class (:foreground ,bright-green))))
            `(magit-bisect-bad                  ((,class (:foreground ,red))))
            `(magit-bisect-good                 ((,class (:foreground ,green))))
            `(magit-bisect-skip                 ((,class (:foreground ,bright-white))))
            `(magit-diff-conflict-heading       ((,class (:foreground ,bright-white))))

;;;;; man
            `(Man-overstrike ((,class (:foreground ,blue :weight bold))))
            `(Man-reverse    ((,class (:foreground ,magenta))))
            `(Man-underline  ((,class (:foreground ,green :underline t))))

;;;;; markdown
            `(markdown-header-face-1 ((,class (:weight bold :foreground ,blue :height ,(if srcery-org-height 1.3 1.0)))))
            `(markdown-header-face-2 ((,class (:weight bold :foreground ,bright-cyan :height ,(if srcery-org-height 1.2 1.0)))))
            `(markdown-header-face-3 ((,class (:bold nil :foreground ,bright-green :height ,(if srcery-org-height 1.1 1.0)))))
            `(markdown-header-face-4 ((,class (:bold nil :foreground ,yellow))))
            `(markdown-header-face-5 ((,class (:bold nil :foreground ,blue))))
            `(markdown-header-face-6 ((,class (:bold nil :foreground ,cyan))))

;;;;; mu4e
            `(mu4e-cited-1-face         ((,class (:foreground ,bright-white))))
            `(mu4e-cited-7-face         ((,class (:foreground ,bright-white))))
            `(mu4e-header-marks-face    ((,class (:foreground ,green))))
            `(mu4e-header-key-face      ((,class (:foreground ,cyan :weight bold))))
            `(mu4e-view-url-number-face ((,class (:foreground ,green))))
            `(mu4e-unread-face          ((,class (:foreground ,yellow :weight bold))))

;;;;; neotree
            `(neo-dir-link-face   ((,class (:foreground ,red :weight bold))))
            `(neo-expand-btn-face ((,class (:foreground ,bright-white))))
            `(neo-file-link-face  ((,class (:foreground ,bright-white))))
            `(neo-root-dir-face   ((,class (:foreground ,yellow :weight bold))))

;;;;; org
            `(org-agenda-clocking         ((,class (:background ,magenta :foreground ,green))))
            `(org-agenda-date             ((,class (:foreground ,blue :height ,(if srcery-org-height 1.1 1.0)))))
            `(org-agenda-date-today       ((,class (:foreground ,red :slant italic :weight bold :height ,(if srcery-org-height 1.3 1.0)))))
            `(org-agenda-date-weekend     ((,class (:weight bold :foreground ,blue))))
            `(org-agenda-done             ((,class (:foreground ,green :height ,(if srcery-org-height 1.2 1.0)))))
            `(org-agenda-structure        ((,class (:weight bold :foreground ,green))))
            `(org-block                   ((,class (:foreground ,bright-white))))
            `(org-block-begin-line        ((,class (:background ,bright-black :foreground ,green))))
            `(org-block-end-line          ((,class (:background ,bright-black :foreground ,green))))
            `(org-clock-overlay           ((,class (:foreground ,green))))
            `(org-code                    ((,class (:foreground ,cyan))))
            `(org-column                  ((,class (:background ,magenta))))
            `(org-column-title            ((,class (:background ,magenta))))
            `(org-date                    ((,class (:underline t :foreground ,blue))))
            `(org-date-selected           ((,class (:background ,yellow :foreground ,black))))
            `(org-document-info-keyword   ((,class (:foreground ,white))))
            `(org-document-info           ((,class (:foreground ,bright-magenta))))
            `(org-document-title          ((,class (:foreground ,yellow :weight bold :height ,(if srcery-org-height 1.4 1.0)))))
            `(org-done                    ((,class (:foreground ,green :weight bold))))
            `(org-ellipsis                ((,class (:foreground ,red))))
            `(org-footnote                ((,class (:underline t :foreground ,bright-white))))
            `(org-hide                    ((,class (:foreground ,bright-white))))
            `(org-kbd                     ((,class (:inherit region :foreground ,bright-white :box (:line-width 1 :style released-button)))))
            `(org-level-1                 ((,class (:weight bold :foreground ,blue :height ,(if srcery-org-height 1.3 1.0)))))
            `(org-level-2                 ((,class (:weight bold :foreground ,green :height ,(if srcery-org-height 1.2 1.0)))))
            `(org-level-3                 ((,class (:bold nil :foreground ,bright-blue :height ,(if srcery-org-height 1.1 1.0)))))
            `(org-level-4                 ((,class (:bold nil :foreground ,bright-green))))
            `(org-level-5                 ((,class (:bold nil :foreground ,yellow))))
            `(org-level-6                 ((,class (:bold nil :foreground ,bright-magenta))))
            `(org-level-7                 ((,class (:bold nil :foreground ,yellow))))
            `(org-level-8                 ((,class (:bold nil :foreground ,cyan))))
            `(org-link                    ((,class (:foreground ,white :underline t))))
            `(org-meta-line               ((,class (:foreground ,white))))
            `(org-mode-line-clock-overrun ((,class (:foreground ,red))))
            `(org-mode-line-clock         ((,class (:foreground ,bright-green))))
            `(org-priority                ((,class (:foreground ,bright-orange :weight bold))))
            `(org-quote                   ((,class (:inherit org-block :slant italic))))
            `(org-scheduled               ((,class (:foreground ,green))))
            `(org-scheduled-today         ((,class (:foreground ,yellow :height ,(if srcery-org-height 1.2 1.0)))))
            `(org-sexp-date               ((,class (:foreground ,bright-white))))
            `(org-special-keyword         ((,class (:foreground ,yellow))))
            `(org-table                   ((,class (:foreground ,bright-white :background ,xgray1))))
            `(org-time-grid               ((,class (:foreground ,bright-green))))
            `(org-todo                    ((,class (:foreground ,yellow :weight bold))))
            `(org-verbatim                ((,class (:foreground ,bright-orange))))
            `(org-verse                   ((,class (:inherit org-block :slant italic))))
            `(org-warning                 ((,class (:foreground ,red))))

;;;;; perspective
            `(persp-selected-face ((,class (:weight bold :foreground ,yellow))))

;;;;; popup
            `(popup-face                       ((,class (:background ,bright-black :foreground ,bright-white))))
            `(popup-tip-face                   ((,class (:background ,bright-blue :foreground ,bright-white :bold nil :italic nil :underline nil))))
            `(popup-menu-face                  ((,class (:background ,bright-black :foreground ,bright-white))))
            `(popup-enu-selection-face         ((,class (:background ,bright-blue :foreground ,bright-white))))
            `(popup-menu-mouse-face            ((,class (:inherit highlight))))
            `(popup-isearch-match              ((,class (:inherit match))))
            `(popup-scroll-bar-foreground-face ((,class (:background ,xgray4))))
            `(popup-scroll-bar-background-face ((,class (:background ,bright-black))))

;;;;; mode-line
            `(powerline-active1   ((,class (:background ,xgray3 :foreground ,bright-white))))
            `(powerline-active2   ((,class (:foreground ,bright-white))))
            `(powerline-inactive1 ((,class (:background ,bright-black :foreground ,white))))
            `(powerline-inactive2 ((,class (:background ,bright-black :foreground ,white))))

            `(mode-line                    ((,class (:foreground ,white :background ,bright-black))))
            `(mode-line-inactive           ((,class (:foreground ,white :background ,bright-black))))
            `(mode-line-buffer-id          ((,class (:foreground ,yellow))))
            `(mode-line-highlight          ((,class (:background ,bright-black :box (:color ,magenta :line-width 1)))))
            `(mode-line-buffer-id-inactive ((,class (:foreground ,bright-white))))
            `(magit-mode-line-process      ((,class (:foreground ,blue))))
            ;; `(mode-line-emphasis           ((,class (:weight bold :foreground ,yellow))))

            `(spaceline-python-venv      ((,class (:foreground ,green))))
            `(spaceline-flycheck-error   ((,class (:foreground ,red))))
            `(spaceline-flycheck-info    ((,class (:foreground ,bright-white))))
            `(spaceline-flycheck-warning ((,class (:foreground ,bright-orange))))
            `(spaceline-evil-normal      ((,class (:background ,xgray4 :foreground ,bright-white))))
            `(spaceline-evil-insert      ((,class (:background ,bright-white :foreground ,black))))
            `(spaceline-evil-replace     ((,class (:background ,bright-red :foreground ,bright-white))))
            `(spaceline-evil-visual      ((,class (:background ,cyan :foreground ,black))))
            `(spaceline-evil-motion      ((,class (:background ,bright-magenta :foreground ,black))))
            `(spaceline-evil-emacs       ((,class (:background ,orange :foreground ,bright-white))))
            `(spaceline-unmodified       ((,class (:foreground ,bright-white))))
            `(spaceline-modified         ((,class (:background ,bright-orange :foreground ,black))))
            `(spaceline-read-only        ((,class (:background ,xgray1 :foreground ,orange))))
            `(spaceline-highlight-face   ((,class (:background ,yellow :foreground ,black))))

;;;;; rainbow-delimiters
            `(rainbow-delimiters-depth-1-face    ((,class :foreground ,bright-white)))
            `(rainbow-delimiters-depth-2-face    ((,class :foreground ,bright-blue)))
            `(rainbow-delimiters-depth-3-face    ((,class :foreground ,bright-white)))
            `(rainbow-delimiters-depth-4-face    ((,class :foreground ,bright-cyan)))
            `(rainbow-delimiters-depth-5-face    ((,class :foreground ,bright-green)))
            `(rainbow-delimiters-depth-6-face    ((,class :foreground ,blue)))
            `(rainbow-delimiters-depth-7-face    ((,class :foreground ,green)))
            `(rainbow-delimiters-depth-8-face    ((,class :foreground ,yellow)))
            `(rainbow-delimiters-unmatched-face  ((,class :foreground ,red)))
            `(rainbow-delimiters-mismatched-face ((,class :foreground ,bright-red)))
            ;; `(rainbow-delimiters-unmatched-face  ((,class :foreground ,red :overline t :inhert bold)))
            ;; `(rainbow-delimiters-mismatched-face ((,class :foreground ,red :overline t :weight bold)))


;;;;; sh
            `(sh-heredoc ((,class (:foreground ,green :weight bold))))
            `(sh-quoted-exec ((,class (:foreground ,yellow :weight bold))))

;;;;; shm
            `(shm-current-face    ((,class (:background ,green, :foreground ,black))))
            `(shm-quarantine-face ((,class (:background ,xgray1))))

;;;;; show-paren
            `(show-paren-match    ((,class (:foreground ,bright-magenta :weight bold))))
            `(show-paren-mismatch ((,class (:background ,red :foreground ,bright-white))))

;;;;; paren-face
            `(parenthesis ((,class (:foreground ,xgray5))))

;;;;; smartparens
            `(sp-pair-overlay-face         ((,class (:background ,magenta :foreground nil))))
            `(sp-show-pair-match-face      ((,class (:foreground ,bright-magenta :weight bold))))
            `(sp-wrap-overlay-closing-pair ((,class (:background ,magenta :foreground, bright-yellow))))

;;;;; evil-snipe
            `(evil-snipe-first-match-face ((,class (:foreground ,magenta :weight bold))))
            `(evil-snipe-matches-face     ((,class (:foreground ,magenta :weight bold))))

;;;;; spacemacs
            `(spacemacs-normal-face                ((,class (:background ,xgray4 :foreground ,bright-white))))
            `(spacemacs-insert-face                ((,class (:background ,bright-white :foreground ,black))))
            `(spacemacs-replace-face               ((,class (:background ,bright-red :foreground ,bright-white))))
            `(spacemacs-visual-face                ((,class (:background ,bright-cyan :foreground ,black))))
            `(spacemacs-motion-face                ((,class (:background ,magenta :foreground ,bright-white))))
            `(spacemacs-emacs-face                 ((,class (:background ,orange :foreground ,bright-white))))
            `(spacemacs-hybrid-face                ((,class (:background ,bright-blue :foreground ,bright-black))))
            `(spacemacs-lisp-face                  ((,class (:background ,green :foreground ,black))))
            `(spacemacs-evilified-face             ((,class (:background ,bright-yellow :foreground ,black))))
            `(spacemacs-helm-navigation-ms-face    ((,class (:foreground ,bright-white))))
            `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,green :box nil :weight bold))))
            `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,green :box nil :weight bold))))
            `(spacemacs-ido-navigation-ts-face     ((,class (:foreground ,yellow))))
            `(spacemacs-iedit-face                 ((,class (:background ,bright-orange :foreground ,black))))
            `(spacemacs-iedit-insert-face          ((,class (:background ,bright-orange :foreground ,black))))
            `(spacemacs-micro-state-binding-face   ((,class (:foreground ,yellow :weight bold))))
            ;; spacemacs-ido-navigation-ts-face
;;;;; swiper
            `(swiper-line-face    ((,class (:background ,magenta :weight bold))))
            `(swiper-match-face-1 ((,class (:weight bold))))
            `(swiper-match-face-2 ((,class (:foreground ,blue :underline t))))
            `(swiper-match-face-3 ((,class (:foreground ,yellow :underline t))))
            `(swiper-match-face-4 ((,class (:foreground ,bright-green :underline t))))

;;;;; term
            `(term               ((,class (:foreground ,bright-white :background ,black))))
            `(term-color-black   ((,class (:foreground ,black))))
            `(term-color-blue    ((,class (:foreground ,blue))))
            `(term-color-cyan    ((,class (:foreground ,cyan))))
            `(term-color-green   ((,class (:foreground ,green))))
            `(term-color-magenta ((,class (:foreground ,magenta))))
            `(term-color-red     ((,class (:foreground ,red))))
            `(term-color-white   ((,class (:foreground ,white))))
            `(term-color-yellow  ((,class (:foreground ,yellow))))

;;;;; web-mode
            `(web-mode-builtin-face         ((,class (:inherit ,font-lock-builtin-face))))
            `(web-mode-comment-face         ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-constant-face        ((,class (:inherit ,font-lock-constant-face))))
            `(web-mode-doctype-face         ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-function-name-face   ((,class (:inherit ,font-lock-function-name-face))))
            `(web-mode-html-attr-name-face  ((,class (:foreground ,yellow))))
            `(web-mode-html-attr-value-face ((,class (:foreground ,red))))
            `(web-mode-html-tag-face        ((,class (:foreground ,red))))
            `(web-mode-keyword-face         ((,class (:foreground ,red))))
            `(web-mode-string-face          ((,class (:foreground ,bright-green))))
            `(web-mode-symbol-face          ((,class (:foreground ,bright-blue))))
            `(web-mode-type-face            ((,class (:inherit ,font-lock-type-face))))
            `(web-mode-warning-face         ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
            `(which-key-command-description-face ((,class (:foreground ,bright-white))))
            `(which-key-group-description-face   ((,class (:foreground ,red))))
            `(which-key-key-face                 ((,class (:foreground ,yellow :weight bold))))
            `(which-key-separator-face           ((,class (:background nil :foreground ,bright-green))))
            `(which-key-special-key-face         ((,class (:background ,yellow :foreground ,black))))

;;;;; which-function-mode
            `(which-func ((,class (:foreground ,yellow))))

;;;;; whitespace-mode
            `(whitespace-empty            ((,class (:background nil :foreground ,yellow))))
            `(whitespace-indentation      ((,class (:background nil :foreground ,bright-orange))))
            `(whitespace-line             ((,class (:background nil :foreground ,green))))
            `(whitespace-newline          ((,class (:background nil :foreground ,green))))
            `(whitespace-space            ((,class (:background nil :foreground ,xgray4))))
            `(whitespace-space-after-tab  ((,class (:background nil :foreground ,yellow))))
            `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
            `(whitespace-tab              ((,class (:background nil))))
            `(whitespace-trailing         ((,class (:background ,red :foreground ,bright-orange))))

;;;;; other, need more work
            `(ac-completion-face                      ((,class (:underline t :foreground ,red))))
            `(ffap                                    ((,class (:foreground ,bright-white))))
            `(flx-highlight-face                      ((,class (:foreground ,green :underline nil))))
            `(icompletep-determined                   ((,class :foreground ,red)))
            `(js2-external-variable                   ((,class (:foreground ,green))))
            `(js2-function-param                      ((,class (:foreground ,bright-magenta))))
            `(js2-jsdoc-html-tag-delimiter            ((,class (:foreground ,bright-green))))
            `(js2-jsdoc-html-tag-name                 ((,class (:foreground ,red))))
            `(js2-jsdoc-value                         ((,class (:foreground ,bright-green))))
            `(js2-private-function-call               ((,class (:foreground ,bright-magenta))))
            `(js2-private-member                      ((,class (:foreground ,bright-white))))
            `(js3-error-face                          ((,class (:underline ,bright-orange))))
            `(js3-external-variable-face              ((,class (:foreground ,blue))))
            `(js3-function-param-face                 ((,class (:foreground ,red))))
            `(js3-instance-member-face                ((,class (:foreground ,bright-magenta))))
            `(js3-jsdoc-tag-face                      ((,class (:foreground ,red))))
            `(js3-warning-face                        ((,class (:underline ,red))))
            `(slime-repl-inputed-output-face          ((,class (:foreground ,green))))
            `(trailing-whitespace                     ((,class :foreground nil :background ,red)))
            `(undo-tree-visualizer-current-face       ((,class :foreground ,red)))
            `(undo-tree-visualizer-default-face       ((,class :foreground ,bright-white)))
            `(undo-tree-visualizer-register-face      ((,class :foreground ,green)))
            `(undo-tree-visualizer-unmodified-face    ((,class :foreground ,blue)))
            `(undo-tree-visualizer-active-branch-face ((,class :foreground ,bright-magenta)))
            `(persp-face-lighter-buffer-not-in-persp  ((,class :background ,red :foreground ,bright-white)))
            `(pulse-highlight-face                    ((,class :background ,green :foreground ,black)))
            `(pulse-highlight-start-face              ((,class :background ,bright-green :foreground ,black)))
            `(custom-invalid                          ((,class :background ,bright-red :foreground ,bright-white)))
            )

           (custom-theme-set-variables
            'srcery
            `(ansi-color-names-vector [,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white])
            ;; `(hl-paren-colors '(,bright-white ,green ,blue ,white))
            )))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(srcery-create-theme)

(provide-theme 'srcery)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; srcery-theme.el ends here
