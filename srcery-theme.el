;;; srcery-theme.el --- Colorscheme that focus ease of use,
;;; and clearly defined contrasting colors with a slightly earthy tone.

;; Original source:
;; URL: <https://github.com/nashamri/spacemacs-theme>
;;
;; Author: Daniel Berg (Roosta)
;; URL <https://github.com/roosta/emacs-srcery>
;;
;; Version: 0.1
;; Keywords: color, theme
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

;; This is a color theme for spacemacs <https://github.com/syl20bnr/spacemacs>.
;; Should work well in a 256 color terminal.

;;; Code:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup srcery-theme nil
  "Srcery-theme options."
  :group 'faces)

;; (defcustom srcery-theme-comment-bg nil
;;   "Use a background for comment lines."
;;   :type 'boolean
;;   :group 'srcery-theme)

(defcustom srcery-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'srcery-theme)

(defcustom srcery-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'srcery-theme)

(defcustom srcery-theme-custom-colors nil
  "Specify a list of custom colors"
  :type 'alist
  :group 'srcery-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          srcery-theme-custom-colors))

(defun create-srcery-theme (theme-name)
  (dyn-let ((class '((class color) (min-colors 89)))

            ;; generic
            ;; (act1          (if (true-color-p) "#2D2B28" "brightblack"))
            (act2          (if (true-color-p) "#4E4E4E" "color-239"))
            (base          (if (true-color-p) "#FCE8C3" "brightwhite"))
            (base-dim      (if (true-color-p) "#545557" "#585858"))
            (bg1           (if (true-color-p) "#1C1B19" "black"))
            (bg2           (if (true-color-p) "#2D2B28" "brightblack"))
            (bg3           (if (true-color-p) "#4E4E4E" "color-239"))
            (bg4           (if (true-color-p) "#080808" "color-232"))
            (border        (if (true-color-p) "#2D2B28" "brightblack"))
            (cblk          (if (true-color-p) "#FCE8C3" "brightwhite"))
            (cblk-bg       (if (true-color-p) "#2D2B28" "brightblack"))
            (cblk-ln       (if (true-color-p) "#E35682" "brightmagenta"))
            (cblk-ln-bg    (if (true-color-p) "#2D2B28" "brightblack"))
            (cursor        (if (true-color-p) "#FCE8C3" "brightwhite"))
            (const         (if (true-color-p) "#5573A3" "blue"))
            (comment       (if (true-color-p) "#918175" "white"))
            (doc           (if (true-color-p)  "#519F50" "green"))
            ;; (comment-bg    (if (true-color-p) "#2D2B28" "brightblack"))
            (comp          (if (true-color-p) "#519F50" "green"))
            (err           (if (true-color-p) "#FF3128" "red"))
            (func          (if (true-color-p) "#FBB829" "yellow"))
            (head1         (if (true-color-p) "#4f97d7" "#268bd2"))
            (head1-bg      (if (true-color-p) "#293239" "#262626"))
            (head2         (if (true-color-p) "#2d9574" "#2aa198"))
            (head2-bg      (if (true-color-p) "#293235" "#262626"))
            (head3         (if (true-color-p) "#67b11d" "#67b11d"))
            (head3-bg      (if (true-color-p) "#293235" "#262626"))
            (head4         (if (true-color-p) "#b1951d" "#875f00"))
            (head4-bg      (if (true-color-p) "#32322c" "#262626"))
            (highlight     (if (true-color-p) "#4E4E4E" "color-239"))
            (keyword       (if (true-color-p) "#D75F00" "color-166"))
            ;; (lnum          (if (true-color-p) "#44505c" "#444444"))
            (mat           (if (true-color-p) "#FBB829" "yellow"))
            (meta          (if (true-color-p) "#9f8766" "#af875f"))
            (str           (if (true-color-p) "#98BC37" "brightgreen"))
            (suc           (if (true-color-p) "#519F50" "green"))
            (ttip          (if (true-color-p) "#FCE8C3" "brightwhite"))
            (ttip-sl       (if (true-color-p) "#8EB2F7" "brightblue"))
            (ttip-bg       (if (true-color-p) "#2D2B28" "brightblack"))
            (type          (if (true-color-p) "#E35682" "brightmagenta"))
            (var           (if (true-color-p) "#8EB2F7" "brightblue"))
            (war           (if (true-color-p) "#dc752f" "#dc752f"))


            (red-bg        (if (true-color-p) "#3c2a2c" "#262626"))
            (red-bg-s      (if (true-color-p) "#512e31" "#262626"))
            (blue-bg       (if (true-color-p) "#293239" "#262626"))
            (magenta       (if (true-color-p) "#a31db1" "#af00df"))
            (yellow-bg     (if (true-color-p) "#32322c" "#262626"))

            (black          (if (true-color-p) "#1C1B19" "black"))
            (red            (if (true-color-p) "#EF2F27" "red"))
            (green          (if (true-color-p) "#519F50" "green"))
            (yellow         (if (true-color-p) "#FBB829" "yellow"))
            (blue           (if (true-color-p) "#5573A3" "blue"))
            (magenta        (if (true-color-p) "#E02C6D" "magenta"))
            (cyan           (if (true-color-p) "#0AAEB3" "cyan"))
            (white          (if (true-color-p) "#918175" "white"))
            (bright-black   (if (true-color-p) "#2D2B28" "brightblack"))
            (bright-red     (if (true-color-p) "#F75341" "brightred"))
            (bright-green   (if (true-color-p) "#98BC37" "brightgreen"))
            (bright-yellow  (if (true-color-p) "#FED06E" "brightyellow"))
            (bright-blue    (if (true-color-p) "#2C78BF" "brightblue"))
            (bright-magenta (if (true-color-p) "#FF5C8F" "brightmagenta"))
            (bright-cyan    (if (true-color-p) "#53FDE9" "brightcyan"))
            (bright-white   (if (true-color-p) "#FCE8C3" "brightwhite"))

            ;; X colors
            (orange         (if (true-color-p) "#D75F00" "color-166"))
            (bright-orange  (if (true-color-p) "#FF8700" "color-208"))
            (hard-black     (if (true-color-p) "#121212" "color-233"))
            (xgrey1         (if (true-color-p) "#262626" "color-235"))
            (xgrey2         (if (true-color-p) "#303030" "color-236"))
            (xgrey3         (if (true-color-p) "#3A3A3A" "color-237"))
            (xgrey4         (if (true-color-p) "#444444" "color-238"))
            (xgrey5         (if (true-color-p) "#4E4E4E" "color-239"))
            )

           custom-colors-override

           (custom-theme-set-faces
            theme-name

;;;;; basics
            `(cursor ((,class (:background ,bright-white))))
            `(custom-button ((,class :background ,black :foreground ,bright-white :box (:line-width 2 :style released-button))))
            `(default ((,class (:background ,black :foreground ,bright-white))))
            `(default-italic ((,class (:italic t))))
            `(error ((,class (:foreground ,red :inherit bold))))
            `(eval-sexp-fu-flash ((,class (:background ,green :foreground ,bright-white))))
            `(eval-sexp-fu-flash-error ((,class (:background ,bright-red :foreground ,bright-white))))
            `(font-lock-builtin-face ((,class (:foreground ,blue))))
            `(font-lock-comment-face ((,class (:foreground ,white :italic t))))
            `(font-lock-constant-face ((,class (:foreground ,bright-magenta))))
            `(font-lock-doc-face ((,class (:foreground ,green))))
            `(font-lock-function-name-face ((,class (:foreground ,yellow))))
            `(font-lock-keyword-face ((,class (:foreground ,red))))
            `(font-lock-negation-char-face ((,class (:foreground ,bright-magenta))))
            `(font-lock-preprocessor-face ((,class (:foreground ,yellow))))
            `(font-lock-reference-face ((,class (:foreground ,bright-magenta))))
            `(font-lock-string-face ((,class (:foreground ,bright-green))))
            `(font-lock-type-face ((,class (:foreground ,bright-blue))))
            `(font-lock-variable-name-face ((,class (:foreground ,blue))))
            `(font-lock-warning-face ((,class (:foreground ,bright-orange :background ,bg1))))
            `(fringe ((,class (:foreground ,bright-white))))
            `(header-line ((,class :background ,bg4)))
            `(highlight ((,class (:foreground ,bright-white :background ,bright-magenta))))
            `(hl-line ((,class (:background ,bg2))))
            `(isearch ((,class (:foreground ,bright-white :background ,magenta))))
            `(lazy-highlight ((,class (:background ,magenta :foreground ,bright-white :weight normal))))
            `(link ((,class (:foreground ,comment :underline t))))
            `(link-visited ((,class (:foreground ,comp :underline t))))
            `(match ((,class (:background ,highlight :foreground ,mat))))
            `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
            `(page-break-lines ((,class (:foreground ,act2))))
            `(region ((,class (:inverse-video ,t))))
            `(secondary-selection ((,class (:background ,bg3))))
            `(success ((,class (:foreground ,suc))))
            `(tooltip ((,class (:background ,ttip-sl :foreground ,bright-white :bold nil :italic nil :underline nil))))
            `(vertical-border ((,class (:foreground ,bg4))))
            `(warning ((,class (:foreground ,war))))

;;;;; ahs
            `(ahs-face ((,class (:background ,highlight))))
            `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))

;;;;; anzu-mode
            `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
            `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
            `(avy-lead-face   ((,class (:background ,blue-bg :foreground ,magenta))))
            `(avy-lead-face-0 ((,class (:background ,blue-bg :foreground ,blue))))
            `(avy-lead-face-1 ((,class (:background ,blue-bg :foreground ,magenta))))
            `(avy-lead-face-2 ((,class (:background ,blue-bg :foreground ,blue))))

;;;;; cider
            `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
            `(cider-enlightened-local ((,class (:foreground ,yellow))))
            `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
            `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
            `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
            `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
            `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
            `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
            `(company-echo-common ((,class (:background ,bright-white :foreground ,bg1))))
            `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-preview-common ((,class (:background ,ttip-bg :foreground ,bright-white))))
            `(company-preview-search ((,class (:inherit match))))
            `(company-scrollbar-bg ((,class (:background ,bg2))))
            `(company-scrollbar-fg ((,class (:background ,act2))))
            `(company-template-field ((,class (:inherit region))))
            `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-tooltip-annotation ((,class (:foreground ,keyword))))
            `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,bright-white))))
            `(company-tooltip-common-selection ((,class (:foreground ,black))))
            `(company-tooltip-mouse ((,class (:inherit highlight))))
            `(company-tooltip-search ((,class (:inherit match))))
            `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,black))))

;;;;; diff
            `(diff-added             ((,class :background nil :foreground ,green)))
            `(diff-changed           ((,class :background nil :foreground ,keyword)))
            `(diff-header            ((,class :background ,cblk-ln-bg :foreground ,yellow)))
            `(diff-indicator-added   ((,class :background nil :foreground ,green)))
            `(diff-indicator-changed ((,class :background nil :foreground ,keyword)))
            `(diff-indicator-removed ((,class :background nil :foreground ,red)))
            `(diff-refine-added      ((,class :background ,green :foreground ,bg4)))
            `(diff-refine-changed    ((,class :background ,keyword :foreground ,bg4)))
            `(diff-refine-removed    ((,class :background ,red :foreground ,bg4)))
            `(diff-removed           ((,class :background nil :foreground ,red)))

;;;;; diff-hl
            `(diff-hl-change ((,class :foreground ,blue)))
            `(diff-hl-delete ((,class :foreground ,red)))
            `(diff-hl-insert ((,class :foreground ,green)))

;;;;; dired
            `(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(dired-flagged ((,class (:foreground ,red))))
            `(dired-header ((,class (:foreground ,comp :inherit bold))))
            `(dired-ignored ((,class (:inherit shadow))))
            `(dired-mark ((,class (:foreground ,comp :inherit bold))))
            `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
            `(dired-perm-write ((,class (:foreground ,bright-white :underline t))))
            `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(dired-warning ((,class (:foreground ,war))))

;;;;; ediff
            `(ediff-current-diff-A ((,class(:background ,red-bg-s :foreground ,red))))
            `(ediff-current-diff-Ancestor ((,class(:background ,black :foreground ,cyan))))
            `(ediff-current-diff-B ((,class(:foreground ,green))))
            `(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue))))
            `(ediff-even-diff-A ((,class(:background ,bg3))))
            `(ediff-even-diff-Ancestor ((,class(:background ,bg3))))
            `(ediff-even-diff-B ((,class(:background ,bg3))))
            `(ediff-even-diff-C ((,class(:background ,bg3))))
            `(ediff-fine-diff-A ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-B ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-C ((,class(:background nil :inherit bold))))
            `(ediff-odd-diff-A ((,class(:background ,bg4))))
            `(ediff-odd-diff-Ancestor ((,class(:background ,bg4))))
            `(ediff-odd-diff-B ((,class(:background ,bg4))))
            `(ediff-odd-diff-C ((,class(:background ,bg4))))

;;;;; ein
            `(ein:cell-input-area((,class (:background ,bg2))))
            `(ein:cell-input-prompt ((,class (:foreground ,suc))))
            `(ein:cell-output-prompt ((,class (:foreground ,err))))
            `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
            `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
            `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; enh-ruby
            `(enh-ruby-string-delimiter-face ((,class (:foreground ,bright-green))))
            `(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,bright-white))))

;;;;; erc
            `(erc-input-face ((,class (:foreground ,yellow))))
            `(erc-my-nick-face ((,class (:foreground ,keyword))))
            `(erc-nick-default-face ((,class (:foreground ,keyword))))
            `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
            `(erc-notice-face ((,class (:foreground ,bright-green))))
            `(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
            `(erc-timestamp-face ((,class (:foreground ,keyword))))

;;;;; eshell
            `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
            `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
            `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
            `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
            `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
            `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
            `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
            `(eshell-ls-unreadable ((,class (:foreground ,bright-white))))
            `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; evil
            `(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
            `(evil-ex-substitute-replacement ((,class (:foreground ,green))))

;;;;; flycheck
            `(flycheck-error
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,err)))
               (,class (:foreground ,bright-white :background ,err :inherit bold :underline t))))
            `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
            `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
            `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
            `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
            `(flycheck-info
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,keyword)))
               (,class (:foreground ,bright-white :background ,keyword :inherit bold :underline t))))
            `(flycheck-warning
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,war)))
               (,class (:foreground ,bright-white :background ,war :inherit bold :underline t))))

;;;;; jabber
            `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
            `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
            `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
            `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
            `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
            `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
            `(jabber-chat-text-foreign ((,class (:foreground ,bright-white))))
            `(jabber-chat-text-local ((,class (:foreground ,bright-white))))
            `(jabber-rare-time-face ((,class (:foreground ,green))))
            `(jabber-roster-user-away ((,class (:foreground ,yellow))))
            `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
            `(jabber-roster-user-dnd ((,class (:foreground ,red))))
            `(jabber-roster-user-error ((,class (:foreground ,err))))
            `(jabber-roster-user-offline ((,class (:foreground ,bright-white))))
            `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
            `(jabber-roster-user-xa ((,class (:foreground ,cyan))))

;;;;; git-gutter-fr
            `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
            `(git-gutter-fr:deleted ((,class (:foreground ,war :inherit bold))))
            `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

;;;;; git-timemachine
            `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
            `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
            `(gnus-header-content ((,class (:foreground ,keyword))))
            `(gnus-header-from ((,class (:foreground ,blue))))
            `(gnus-header-name ((,class (:foreground ,comp))))
            `(gnus-header-subject ((,class (:foreground ,yellow :inherit bold))))
            `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
            `(guide-key/highlight-command-face ((,class (:foreground ,bright-white))))
            `(guide-key/key-face ((,class (:foreground ,keyword))))
            `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
            `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
            `(helm-bookmark-file ((,class (:foreground ,bright-white))))
            `(helm-bookmark-gnus ((,class (:foreground ,comp))))
            `(helm-bookmark-info ((,class (:foreground ,comp))))
            `(helm-bookmark-man ((,class (:foreground ,comp))))
            `(helm-bookmark-w3m ((,class (:foreground ,comp))))
            `(helm-buffer-directory ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-buffer-file ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
            `(helm-buffer-process ((,class (:foreground ,keyword :background ,bg1))))
            `(helm-buffer-saved-out ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-buffer-size ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-candidate-number ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
            `(helm-ff-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
            `(helm-ff-file ((,class (:foreground ,bright-white :background ,bg1 :weight normal))))
            `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :inherit bold))))
            `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
            `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(helm-grep-cmd-line ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-grep-file ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-grep-finish ((,class (:foreground ,bright-white :background ,bg1))))
            `(helm-grep-lineno ((,class (:foreground ,bright-blue :background ,bg1 :inherit bold))))
            `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
            `(helm-header ((,class (:foreground ,bright-white :background ,bg1 :underline nil :box nil))))
            `(helm-header-line-left-margin ((,class (:foreground ,keyword :background ,nil))))
            `(helm-match ((,class (:background ,head1-bg :foreground ,head1))))
            `(helm-match-item ((,class (:background ,head1-bg :foreground ,head1))))
            `(helm-moccur-buffer ((,class (:foreground ,blue :background ,bg1))))
            `(helm-selection ((,class (:background ,highlight))))
            `(helm-selection-line ((,class (:background ,bg2))))
            `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
            `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :inherit bold))))
            `(helm-time-zone-current ((,class (:foreground ,keyword :background ,bg1))))
            `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
            `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg3))))

;;;;; helm-swoop
            `(helm-swoop-target-line-block-face ((,class (:foreground ,bright-white :background ,highlight))))
            `(helm-swoop-target-line-face ((,class (:background ,highlight))))
            `(helm-swoop-target-word-face ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
            `(hi-yellow ((,class (:foreground ,yellow))))
            `(hi-green  ((,class (:foreground ,green))))

;;;;; highlight-indentation
            `(highlight-indentation-face ((,class (:background ,white))))

;;;;; highlight-symbol
            `(highlight-symbol-face ((,class (:background ,bg2))))

;;;;; hydra
            `(hydra-face-blue ((,class (:foreground ,blue))))
            `(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
            `(ido-first-match ((,class (:foreground ,comp :inherit bold))))
            `(ido-only-match ((,class (:foreground ,mat :inherit bold))))
            `(ido-subdir ((,class (:foreground ,keyword))))
            `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
            `(info-header-xref ((,class (:foreground ,yellow :underline t))))
            `(info-menu ((,class (:foreground ,suc))))
            `(info-node ((,class (:foreground ,yellow :inherit bold))))
            `(info-quoted-name ((,class (:foreground ,keyword))))
            `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
            `(info-string ((,class (:foreground ,bright-green))))
            `(info-title-1 ((,class (:height 1.4 :inherit bold))))
            `(info-title-2 ((,class (:height 1.3 :inherit bold))))
            `(info-title-3 ((,class (:height 1.3))))
            `(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
            `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
            `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
            `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
            `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
            `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
            `(ivy-remote ((,class (:foreground ,cyan))))

;;;;; latex
            `(font-latex-bold-face ((,class (:foreground ,comp))))
            `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
            `(font-latex-match-reference-keywords ((,class (:foreground ,bright-magenta))))
            `(font-latex-match-variable-keywords ((,class (:foreground ,blue))))
            `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if srcery-theme-org-height 1.3 1.0) :background ,(when srcery-theme-org-highlight head3-bg)))))
            `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if srcery-theme-org-height 1.3 1.0) :background ,(when srcery-theme-org-highlight head4-bg)))))
            `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if srcery-theme-org-height 1.3 1.0) :background ,(when srcery-theme-org-highlight head1-bg)))))
            `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if srcery-theme-org-height 1.2 1.0) :background ,(when srcery-theme-org-highlight head2-bg)))))
            `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if srcery-theme-org-height 1.1 1.0) :background ,(when srcery-theme-org-highlight head3-bg)))))
            `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when srcery-theme-org-highlight head4-bg)))))
            `(font-latex-string-face ((,class (:foreground ,bright-green))))

;;;;; linum-mode
            `(linum ((,class (:foreground ,white :background ,black))))

;;;;; linum-relative
            `(linum-relative-current-face ((,class (:foreground ,yellow))))

;;;;; magit
            `(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-branch ((,class (:foreground ,bright-magenta :inherit bold))))
            `(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
            `(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
            `(magit-branch-remote ((,class (:background ,black :foreground ,cyan :inherit bold))))
            `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,bright-white))))
            `(magit-diff-file-header ((,class (:foreground ,comment))))
            `(magit-diff-file-heading ((,class (:foreground ,comment))))
            `(magit-diff-file-heading-highlight ((,class (:foreground ,comment))))
            `(magit-diff-hunk-header ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-hash ((,class (:foreground ,blue))))
            `(magit-hunk-heading           ((,class (:background ,bg3))))
            `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
            `(magit-item-highlight ((,class :background ,bg2)))
            `(magit-log-author ((,class (:foreground ,yellow))))
            `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
            `(magit-log-sha1 ((,class (:foreground ,bright-green))))
            `(magit-process-ng ((,class (:foreground ,war :inherit bold))))
            `(magit-process-ok ((,class (:foreground ,yellow :inherit bold))))
            `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold))))
            `(magit-section-highlight      ((,class (:background ,bg2))))
            `(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))

;;;;; man
            `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
            `(Man-reverse ((,class (:foreground ,highlight))))
            `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
            `(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if srcery-theme-org-height 1.3 1.0) :background ,(when srcery-theme-org-highlight head1-bg)))))
            `(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if srcery-theme-org-height 1.2 1.0) :background ,(when srcery-theme-org-highlight head2-bg)))))
            `(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if srcery-theme-org-height 1.1 1.0) :background ,(when srcery-theme-org-highlight head3-bg)))))
            `(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when srcery-theme-org-highlight head4-bg)))))
            `(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
            `(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))

;;;;; mode-line
            `(mode-line           ((,class (:foreground ,bright-white :background ,bright-black :box (:color ,border :line-width 1)))))
            `(mode-line-inactive  ((,class (:foreground ,bright-white :background ,bg1  :box (:color ,border :line-width 1)))))
            `(mode-line-buffer-id ((,class (:inherit bold :foreground ,yellow))))

;;;;; mu4e
            `(mu4e-cited-1-face ((,class (:foreground ,bright-white))))
            `(mu4e-cited-7-face ((,class (:foreground ,bright-white))))
            `(mu4e-header-marks-face ((,class (:foreground ,comp))))
            `(mu4e-header-key-face ((,class (:foreground ,head2 :inherit bold))))
            `(mu4e-view-url-number-face ((,class (:foreground ,comp))))
            `(mu4e-unread-face ((,class (:foreground ,yellow :inherit bold))))

;;;;; neotree
            `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
            `(neo-expand-btn-face ((,class (:foreground ,bright-white))))
            `(neo-file-link-face ((,class (:foreground ,bright-white))))
            `(neo-root-dir-face ((,class (:foreground ,yellow :inherit bold))))

;;;;; org
            `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
            `(org-agenda-date ((,class (:foreground ,blue :height ,(if srcery-theme-org-height 1.1 1.0)))))
            `(org-agenda-date-today ((,class (:foreground ,keyword :slant italic :inherit bold :height ,(if srcery-theme-org-height 1.3 1.0)))))
            `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,blue))))
            `(org-agenda-done ((,class (:foreground ,suc :height ,(if srcery-theme-org-height 1.2 1.0)))))
            `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
            `(org-block ((,class (:background ,cblk-bg :foreground ,cblk))))
            `(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
            `(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
            `(org-clock-overlay ((,class (:foreground ,comp))))
            `(org-code ((,class (:foreground ,cyan))))
            `(org-column ((,class (:background ,highlight))))
            `(org-column-title ((,class (:background ,highlight))))
            `(org-date ((,class (:underline t :foreground ,blue))))
            `(org-date-selected ((,class (:background ,yellow :foreground ,bg1))))
            `(org-document-info-keyword ((,class (:foreground ,meta))))
            `(org-document-title ((,class (:foreground ,yellow :inherit bold :height ,(if srcery-theme-org-height 1.4 1.0) :underline t))))
            `(org-done ((,class (:foreground ,green :inherit bold))))
            `(org-ellipsis ((,class (:foreground ,keyword))))
            `(org-footnote  ((,class (:underline t :foreground ,bright-white))))
            `(org-hide ((,class (:foreground ,bright-white))))
            `(org-kbd ((,class (:inherit region :foreground ,bright-white :box (:line-width 1 :style released-button)))))
            `(org-level-1 ((,class (:inherit bold :foreground ,head1 :height ,(if srcery-theme-org-height 1.3 1.0) :background ,(when srcery-theme-org-highlight head1-bg)))))
            `(org-level-2 ((,class (:inherit bold :foreground ,head2 :height ,(if srcery-theme-org-height 1.2 1.0) :background ,(when srcery-theme-org-highlight head2-bg)))))
            `(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if srcery-theme-org-height 1.1 1.0) :background ,(when srcery-theme-org-highlight head3-bg)))))
            `(org-level-4 ((,class (:bold nil :foreground ,head4 :background ,(when srcery-theme-org-highlight head4-bg)))))
            `(org-level-5 ((,class (:bold nil :foreground ,head1))))
            `(org-level-6 ((,class (:bold nil :foreground ,head2))))
            `(org-level-7 ((,class (:bold nil :foreground ,head3))))
            `(org-level-8 ((,class (:bold nil :foreground ,head4))))
            `(org-link ((,class (:underline t :foreground ,comment))))
            `(org-meta-line ((,class (:foreground ,meta))))
            `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
            `(org-priority ((,class (:foreground ,war :inherit bold))))
            `(org-quote ((,class (:inherit org-block :slant italic))))
            `(org-scheduled ((,class (:foreground ,comp))))
            `(org-scheduled-today ((,class (:foreground ,yellow :height ,(if srcery-theme-org-height 1.2 1.0)))))
            `(org-sexp-date ((,class (:foreground ,bright-white))))
            `(org-special-keyword ((,class (:foreground ,yellow))))
            `(org-table ((,class (:foreground ,bright-white :background ,head1-bg))))
            `(org-time-grid ((,class (:foreground ,bright-green))))
            `(org-todo ((,class (:foreground ,war :inherit bold :background ,yellow-bg))))
            `(org-verbatim ((,class (:foreground ,keyword))))
            `(org-verse ((,class (:inherit org-block :slant italic))))
            `(org-warning ((,class (:foreground ,err))))

;;;;; perspective
            `(persp-selected-face ((,class (:inherit bold :foreground ,yellow))))

;;;;; popup
            `(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(popup-tip-face ((,class (:background ,ttip-sl :foreground ,bright-white :bold nil :italic nil :underline nil))))
            `(popup-menu-face ((,class (:background ,ttip-bg :foreground ,bright-white))))
            `(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,bright-white))))
            `(popup-menu-mouse-face ((,class (:inherit highlight))))
            `(popup-isearch-match ((,class (:inherit match))))
            `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
            `(popup-scroll-bar-background-face ((,class (:background ,bg2))))

;;;;; powerline
            `(powerline-active1 ((,class (:background ,act2 :foreground ,bright-white))))
            `(powerline-active2 ((,class (:background ,act2 :foreground ,bright-white))))
            `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,bright-white))))
            `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,bright-white))))

;;;;; rainbow-delimiters
            `(rainbow-delimiters-depth-1-face ((,class :foreground ,keyword)))
            `(rainbow-delimiters-depth-2-face ((,class :foreground ,yellow)))
            `(rainbow-delimiters-depth-3-face ((,class :foreground ,bright-green)))
            `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
            `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
            `(rainbow-delimiters-depth-6-face ((,class :foreground ,keyword)))
            `(rainbow-delimiters-depth-7-face ((,class :foreground ,yellow)))
            `(rainbow-delimiters-depth-8-face ((,class :foreground ,bright-green)))
            `(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))
            `(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))

;;;;; shm
            `(shm-current-face ((,class (:background ,green, :foreground ,black))))
            `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
            `(show-paren-match ((,class (:background ,green :foreground ,black))))
            `(show-paren-mismatch ((,class (:background ,red-bg-s))))

;;;;; smartparens
            `(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
            `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold :underline t))))

;;;;; spaceline
            `(spaceline-python-venv ((,class (:foreground ,comp))))
            `(spaceline-flycheck-error  ((,class (:foreground ,err))))
            `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
            `(spaceline-flycheck-warning((,class (:foreground ,war))))

;;;;; spacemacs-specific
            `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
            `(swiper-line-face ((,class (:background ,highlight :inherit bold))))
            `(swiper-match-face-1 ((,class (:inherit bold))))
            `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
            `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
            `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; term
            `(term ((,class (:foreground ,bright-white :background ,bg1))))
            `(term-color-black ((,class (:foreground ,bg4))))
            `(term-color-blue ((,class (:foreground ,keyword))))
            `(term-color-cyan ((,class (:foreground ,cyan))))
            `(term-color-green ((,class (:foreground ,green))))
            `(term-color-magenta ((,class (:foreground ,magenta))))
            `(term-color-red ((,class (:foreground ,red))))
            `(term-color-white ((,class (:foreground ,bright-white))))
            `(term-color-yellow ((,class (:foreground ,yellow))))

;;;;; web-mode
            `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
            `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
            `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
            `(web-mode-html-attr-name-face ((,class (:foreground ,yellow))))
            `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
            `(web-mode-html-tag-face ((,class (:foreground ,keyword))))
            `(web-mode-keyword-face ((,class (:foreground ,keyword))))
            `(web-mode-string-face ((,class (:foreground ,bright-green))))
            `(web-mode-symbol-face ((,class (:foreground ,bright-blue))))
            `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
            `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
            `(which-key-command-description-face ((,class (:foreground ,bright-white))))
            `(which-key-group-description-face ((,class (:foreground ,keyword))))
            `(which-key-key-face ((,class (:foreground ,yellow :inherit bold))))
            `(which-key-separator-face ((,class (:background nil :foreground ,bright-green))))
            `(which-key-special-key-face ((,class (:background ,yellow :foreground ,bg1))))

;;;;; which-function-mode
            `(which-func ((,class (:foreground ,yellow))))

;;;;; whitespace-mode
            `(whitespace-empty ((,class (:background nil :foreground ,yellow))))
            `(whitespace-indentation ((,class (:background nil :foreground ,war))))
            `(whitespace-line ((,class (:background nil :foreground ,comp))))
            `(whitespace-newline ((,class (:background nil :foreground ,comp))))
            `(whitespace-space ((,class (:background nil :foreground ,act2))))
            `(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
            `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
            `(whitespace-tab ((,class (:background nil))))
            `(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; other, need more work
            `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
            `(ffap ((,class (:foreground ,bright-white))))
            `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
            `(icompletep-determined ((,class :foreground ,keyword)))
            `(js2-external-variable ((,class (:foreground ,comp))))
            `(js2-function-param ((,class (:foreground ,bright-magenta))))
            `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,bright-green))))
            `(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
            `(js2-jsdoc-value ((,class (:foreground ,bright-green))))
            `(js2-private-function-call ((,class (:foreground ,bright-magenta))))
            `(js2-private-member ((,class (:foreground ,bright-white))))
            `(js3-error-face ((,class (:underline ,war))))
            `(js3-external-variable-face ((,class (:foreground ,blue))))
            `(js3-function-param-face ((,class (:foreground ,keyword))))
            `(js3-instance-member-face ((,class (:foreground ,bright-magenta))))
            `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
            `(js3-warning-face ((,class (:underline ,keyword))))
            `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
            `(trailing-whitespace ((,class :foreground nil :background ,err)))
            `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
            `(undo-tree-visualizer-default-face ((,class :foreground ,bright-white)))
            `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
            `(undo-tree-visualizer-unmodified-face ((,class :foreground ,blue))))

           (custom-theme-set-variables
            theme-name
            `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,bright-white]))

           ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(deftheme srcery "Srcery theme")

(create-srcery-theme 'srcery)

(provide-theme 'srcery)
