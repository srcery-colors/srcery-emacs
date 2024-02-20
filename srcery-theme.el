;;; srcery-theme.el --- Dark color theme
;; Copyright (C) 2019 Daniel Berg

;; Author: Daniel Berg
;; URL: https://github.com/srcery-colors/srcery-emacs

;; Version: 0.2.0
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

;; Port of vim-srcery: https://github.com/srcery-colors/srcery-vim, a dark color
;; theme with with focus on clearly defined contrasting colors and relative ease
;; of use.  Srcery should play well with a 256 color terminal, provided the
;; terminal colors are set to srcery colors.  See here for sample terminal
;; configs: https://github.com/srcery-colors/srcery-terminal

;;; Credits

;; Nasser Alshammari
;; https://github.com/nashamri/spacemacs-theme
;; I used this theme as a base for how to make an Emacs color theme

;; Kelvin Smith
;; https://github.com/oneKelvinSmith/monokai-emacs
;; Used as a reference for 256 color handling

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Srcery theme requires Emacs 24 or later!"))

(deftheme srcery "Srcery color theme")

(defgroup srcery nil
  "Srcery options."
  :group 'faces)

(defcustom srcery-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'srcery)

(defcustom srcery-invert-matches nil
  "Use inverse video for search matches."
  :type 'boolean
  :group 'srcery)

(defcustom srcery-invert-region t
  "Use inverse video for region."
  :type 'boolean
  :group 'srcery)

(defcustom srcery-transparent-background nil
  "Sets black background color to nil in terminal."
  :type 'boolean
  :group 'srcery)

(defcustom srcery-black "#1C1B19"
  "Palette - black."
  :type 'string
  :group 'srcery)

(defcustom srcery-red "#EF2F27"
  "Palette - red."
  :type 'string
  :group 'srcery)

(defcustom srcery-green "#519F50"
  "Palette - green."
  :type 'string
  :group 'srcery)

(defcustom srcery-yellow "#FBB829"
  "Palette - yellow."
  :type 'string
  :group 'srcery)

(defcustom srcery-blue "#2C78BF"
  "Palette - blue."
  :type 'string
  :group 'srcery)

(defcustom srcery-magenta "#E02C6D"
  "Palette - magenta."
  :type 'string
  :group 'srcery)

(defcustom srcery-cyan "#0AAEB3"
  "Palette - cyan."
  :type 'string
  :group 'srcery)

(defcustom srcery-white "#BAA67F"
  "Palette - white."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-black "#918175"
  "Palette - bright black."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-red "#F75341"
  "Palette - bright red."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-green "#98BC37"
  "Palette - bright green."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-yellow "#FED06E"
  "Palette - bright yellow."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-blue "#68A8E4"
  "Palette - bright blue."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-magenta "#FF5C8F"
  "Palette - bright magenta."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-cyan "#2BE4D0"
  "Palette - bright cyan."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-white "#FCE8C3"
  "Palette - bright white."
  :type 'string
  :group 'srcery)

(defcustom srcery-orange "#FF5F00"
  "Palette xterm 202 - orange."
  :type 'string
  :group 'srcery)

(defcustom srcery-bright-orange "#FF8700"
  "Palette xterm 208 - bright orange."
  :type 'string
  :group 'srcery)

(defcustom srcery-hard-black "#121212"
  "Palette xterm 233 - hard black."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-1 "#262626"
  "Palette xterm 235 - gray 1."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-2 "#303030"
  "Palette xterm 236 - gray 2."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-3 "#3A3A3A"
  "Palette xterm 237 - gray 3."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-4 "#444444"
  "Palette xterm 238 - gray 4."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-5 "#4E4E4E"
  "Palette xterm 239 - gray 5."
  :type 'string
  :group 'srcery)

(defcustom srcery-gray-6 "#585858"
  "Palette xterm 240 - gray 6."
  :type 'string
  :group 'srcery)

(defcustom srcery-dark-red "#5f0000"
  "Palette xterm 52."
  :type 'string
  :group 'srcery)

(defcustom srcery-dark-green "#005f00"
  "Palette xterm 22."
  :type 'string
  :group 'srcery)

(defcustom srcery-dark-cyan "#005f5f"
  "Palette xterm 23."
  :type 'string
  :group 'srcery)

(defcustom srcery-dark-blue "#00005f"
  "Palette xterm 17."
  :type 'string
  :group 'srcery)

(defcustom srcery-teal "#008080"
  "Palette xterm 30 - Teal."
  :type 'string
  :group 'srcery)

(defcustom srcery-green3 "#00d700"
  "Palette xterm 40 - Green3."
  :type 'string
  :group 'srcery)

(let* ((srcery-class '((class color) (min-colors 257)))

       (srcery-256-class '((class color) (min-colors 89)))

       (srcery-256-black          "black")
       (srcery-256-red            "red")
       (srcery-256-green          "green")
       (srcery-256-yellow         "yellow")
       (srcery-256-blue           "blue")
       (srcery-256-magenta        "magenta")
       (srcery-256-cyan           "cyan")
       (srcery-256-white          "white")
       (srcery-256-bright-black   "brightblack")
       (srcery-256-bright-red     "brightred")
       (srcery-256-bright-green   "brightgreen")
       (srcery-256-bright-yellow  "brightyellow")
       (srcery-256-bright-blue    "brightblue")
       (srcery-256-bright-magenta "brightmagenta")
       (srcery-256-bright-cyan    "brightcyan")
       (srcery-256-bright-white   "brightwhite")

       (srcery-256-orange          "color-202")
       (srcery-256-bright-orange   "color-208")
       (srcery-256-hard-black      "color-233")
       (srcery-256-gray-1          "color-235")
       (srcery-256-gray-2          "color-236")
       (srcery-256-gray-3          "color-237")
       (srcery-256-gray-4          "color-238")
       (srcery-256-gray-5          "color-239")
       (srcery-256-gray-6          "color-240")

       (srcery-256-dark-red       "color-52")
       (srcery-256-dark-green     "color-22")
       (srcery-256-dark-cyan      "color-23")
       (srcery-256-dark-blue      "color-17")

       (srcery-256-teal           "color-30")
       (srcery-256-green3         "color-40"))

  (custom-theme-set-faces
   'srcery

   ;;----------------------------------------------------------------------------
   ;; basics
   ;;----------------------------------------------------------------------------
   `(cursor
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))

   `(custom-button
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-bright-white :box (:line-width 2 :style released-button)))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-bright-white :box (:line-width 2 :style released-button)))))

   `(default
      ((,srcery-class (:background ,srcery-black :foreground ,srcery-bright-white))
       (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-bright-white))))

   `(default-italic
      ((,srcery-class (:italic t))
       (,srcery-256-class (:italic t))))

   `(error
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(eval-sexp-fu-flash
     ((,srcery-class (:background ,srcery-green))
      (,srcery-256-class (:background ,srcery-256-green))))

   `(eval-sexp-fu-flash-error
     ((,srcery-class (:background ,srcery-red))
      (,srcery-256-class (:background ,srcery-256-red))))

   `(font-lock-builtin-face
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(font-lock-comment-face
     ((,srcery-class (:foreground ,srcery-bright-black :italic t))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :italic t))))

   `(font-lock-constant-face
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(font-lock-reference-face
     ((,srcery-class (:foreground ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-bright-blue))))

   `(font-lock-doc-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(font-lock-function-name-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(font-lock-keyword-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(font-lock-negation-char-face
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(font-lock-preprocessor-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(font-lock-string-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(font-lock-type-face
     ((,srcery-class (:foreground ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-bright-blue))))

   `(font-lock-variable-name-face
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(font-lock-warning-face
     ((,srcery-class (:foreground ,srcery-bright-orange :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(fringe
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(header-line
     ((,srcery-class (:background ,srcery-black))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(highlight
     ((,srcery-class ,(if srcery-invert-matches
                          `(:inverse-video t)
                        `(:background ,srcery-gray-5 :weight bold)))
      (,srcery-256-class ,(if srcery-invert-matches
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-5 :weight bold)))))

   `(hl-line
     ((,srcery-class (:background ,srcery-gray-2))
      (,srcery-256-class (:background ,srcery-256-gray-2))))

   `(isearch
     ((,srcery-class ,(if srcery-invert-matches
                          `(:inverse-video t)
                        `(:underline t :background ,srcery-gray-5 :weight bold)))
      (,srcery-256-class ,(if srcery-invert-matches
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-5 :weight bold)))))
   `(isearch-fail
    ((,srcery-class (:foreground ,srcery-red))
     (,srcery-256-class (:foreground ,srcery-256-red))))

   `(lazy-highlight
     ((,srcery-class ,(if srcery-invert-matches
                          `(:inverse-video t)
                        `(:background ,srcery-gray-5 :weight bold)))
      (,srcery-256-class ,(if srcery-invert-matches
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-5 :weight bold)))))

   `(link
     ((,srcery-class (:inherit font-lock-comment-face :underline t))
      (,srcery-256-class (:inherit font-lock-comment-face :underline t))))

   `(link-visited
     ((,srcery-class (:inherit font-lock-comment-face :underline t))
      (,srcery-256-class (:inherit font-lock-comment-face :underline t))))

   `(match
     ((,srcery-class ,(if srcery-invert-matches
                          `(:inverse-video t)
                        `(:background ,srcery-gray-5 :weight bold)))
      (,srcery-256-class ,(if srcery-invert-matches
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-5 :weight bold)))))

   `(minibuffer-prompt
     ((,srcery-class (:weight bold :foreground ,srcery-yellow))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-yellow))))

   `(page-break-lines
     ((,srcery-class (:foreground ,srcery-gray-4))
      (,srcery-256-class (:foreground ,srcery-256-gray-4))))

   `(region
     ((,srcery-class ,(if srcery-invert-region
                          `(:inverse-video t)
                        `(:background ,srcery-gray-3)))
      (,srcery-256-class ,(if srcery-invert-region
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-3)))))

   `(secondary-selection
     ((,srcery-class (:background ,srcery-gray-3))
      (,srcery-256-class (:background ,srcery-256-gray-3))))

   `(success
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(tooltip
     ((,srcery-class (:background ,srcery-bright-blue :foreground ,srcery-bright-white :bold nil :italic nil :underline nil))
      (,srcery-256-class (:background ,srcery-256-bright-blue :foreground ,srcery-256-bright-white :bold nil :italic nil :underline nil))))

   `(vertical-border
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(warning
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   `(tool-bar
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))


   ;;----------------------------------------------------------------------------
   ;; ahs
   ;;----------------------------------------------------------------------------
   `(ahs-face
     ((,srcery-class (:background ,srcery-magenta))
      (,srcery-256-class (:background ,srcery-256-magenta))))

   `(ahs-plugin-whole-buffer-face
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))

   `(ahs-edit-mode-face
     ((,srcery-class (:background ,srcery-bright-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-bright-red :foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; anzu-mode
   ;;----------------------------------------------------------------------------
   `(anzu-mode-line
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; auto-complete
   ;;----------------------------------------------------------------------------
   `(ac-completion-face
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; avy
   ;;----------------------------------------------------------------------------
   `(avy-lead-face
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-magenta))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-magenta))))

   `(avy-lead-face-0
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-yellow))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-yellow))))

   `(avy-lead-face-1
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-green))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-green))))

   `(avy-lead-face-2
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-blue))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-blue))))


   ;;----------------------------------------------------------------------------
   ;; cider
   ;;----------------------------------------------------------------------------
   `(cider-enlightened
     ((,srcery-class (:background unspecified :box (:color ,srcery-yellow :line-width -1 :style nil) :foreground ,srcery-yellow))
      (,srcery-256-class (:background unspecified :box (:color ,srcery-256-yellow :line-width -1 :style nil) :foreground ,srcery-256-yellow))))

   `(cider-enlightened-face
     ((,srcery-class (:background unspecified :box (:color ,srcery-bright-black :line-width -1 :style nil) :foreground ,srcery-blue))
      (,srcery-256-class (:background unspecified :box (:color ,srcery-256-bright-black :line-width -1 :style nil) :foreground ,srcery-256-blue))))

   `(cider-enlightened-local
     ((,srcery-class (:foreground ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-bright-yellow))))

   `(cider-instrumented-face
     ((,srcery-class (:background unspecified :box (:color ,srcery-red :line-width -1 :style nil) :foreground ,srcery-red))
      (,srcery-256-class (:background unspecified :box (:color ,srcery-256-red :line-width -1 :style nil) :foreground ,srcery-256-red))))

   `(cider-result-overlay-face
     ((,srcery-class (:background unspecified :box (:color ,srcery-blue :line-width -1 :style nil) :foreground ,srcery-blue))
      (,srcery-256-class (:background unspecified :box (:color ,srcery-256-blue :line-width -1 :style nil) :foreground ,srcery-256-blue))))

   `(cider-test-error-face
     ((,srcery-class (:background ,srcery-bright-orange :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-orange :foreground ,srcery-256-black))))

   `(cider-test-failure-face
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-white))))

   `(cider-test-success-face
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black))))

   `(cider-traced-face
     ((,srcery-class :box (:color ,srcery-cyan :line-width -1 :style nil))
      (,srcery-256-class :box (:color ,srcery-256-cyan :line-width -1 :style nil))))

   `(cider-fringe-good-face
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(cider-fragile-button-face
     ((,srcery-class :foreground ,srcery-orange :box (:style released-button))
      (,srcery-256-class :foreground ,srcery-256-orange :box (:style released-button))))

   `(cider-stacktrace-promoted-button-face
     ((,srcery-class :foreground ,srcery-red :box (:style released-button))
      (,srcery-256-class :foreground ,srcery-256-red :box (:style released-button))))

   `(cider-stacktrace-suppressed-button-face
     ((,srcery-class :foreground ,srcery-bright-black :box (:style pressed-button))
      (,srcery-256-class :foreground ,srcery-256-bright-black :box (:style pressed-button))))

   `(cider-enlightened-local-face
     ((,srcery-class :foreground ,srcery-yellow :weight bold)
      (,srcery-256-class :foreground ,srcery-256-yellow :weight bold)))

   `(cider-deprecated-face
     ((,srcery-class  :foreground ,srcery-bright-yellow :underline t)
      (,srcery-256-class :foreground ,srcery-256-bright-yellow :underline t)))

   `(cider-debug-code-overlay-face
     ((,srcery-class :background ,srcery-bright-blue :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-bright-blue :foreground ,srcery-256-black)))

   `(cider-docview-table-border-face
     ((,srcery-class :foreground ,srcery-bright-black)
      (,srcery-256-class :foreground ,srcery-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; clojure
   ;;----------------------------------------------------------------------------
   `(clojure-keyword-face
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   ;;----------------------------------------------------------------------------
   ;; company
   ;;----------------------------------------------------------------------------
   `(company-echo-common
     ((,srcery-class (:background ,srcery-bright-white :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-white :foreground ,srcery-256-black))))

   `(company-preview
     ((,srcery-class (:background ,srcery-hard-black :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-hard-black :foreground ,srcery-256-bright-white))))

   `(company-preview-common
     ((,srcery-class (:background ,srcery-hard-black :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-hard-black :foreground ,srcery-256-bright-white))))

   `(company-preview-search
     ((,srcery-class (:inherit match))
      (,srcery-256-class (:inherit match))))

   `(company-scrollbar-bg
     ((,srcery-class (:background ,srcery-hard-black))
      (,srcery-256-class (:background ,srcery-256-hard-black))))

   `(company-scrollbar-fg
     ((,srcery-class (:background ,srcery-bright-black))
      (,srcery-256-class (:background ,srcery-256-bright-black))))

   `(company-template-field
     ((,srcery-class (:inherit region))
      (,srcery-256-class (:inherit region))))

   `(company-tooltip
     ((,srcery-class (:background ,srcery-hard-black :foreground ,srcery-bright-black))
      (,srcery-256-class (:background ,srcery-256-hard-black :foreground ,srcery-256-bright-black))))

   `(company-tooltip-annotation
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(company-tooltip-common
     ((,srcery-class (:background ,srcery-hard-black :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-hard-black :foreground ,srcery-256-bright-white))))

   `(company-tooltip-common-selection
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(company-tooltip-mouse
     ((,srcery-class (:inherit highlight))
      (,srcery-256-class (:inherit highlight))))

   `(company-tooltip-search
     ((,srcery-class (:inherit match))
      (,srcery-256-class (:inherit match))))

   `(company-tooltip-selection
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   ;;----------------------------------------------------------------------------
   ;; racer
   ;;----------------------------------------------------------------------------
   `(racer-tooltip
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-hard-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,srcery-256-hard-black))))

   `(racer-help-heading-face
     ((,srcery-class (:foreground ,srcery-bright-white :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; rust
   ;;----------------------------------------------------------------------------
   `(rust-builtin-formatting-macro-face
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(rust-question-mark-face
     ((,srcery-class (:foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold))))

   `(rust-string-interpolation-face
     ((,srcery-class (:foreground ,srcery-bright-green :italic t))
      (,srcery-256-class (:foreground ,srcery-256-bright-green :italic t))))

   `(rust-unsafe-face
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   ;;----------------------------------------------------------------------------
   ;; diff
   ;;----------------------------------------------------------------------------
   `(diff-added
     ((,srcery-class :background unspecified :foreground ,srcery-green)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-green)))

   `(diff-changed
     ((,srcery-class :background unspecified :foreground ,srcery-red)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-red)))

   `(diff-header
     ((,srcery-class :background ,srcery-gray-2 :foreground ,srcery-yellow)
      (,srcery-256-class :background ,srcery-256-gray-2 :foreground ,srcery-256-yellow)))

   `(diff-indicator-added
     ((,srcery-class :background unspecified :foreground ,srcery-green)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-green)))

   `(diff-indicator-changed
     ((,srcery-class :background unspecified :foreground ,srcery-red)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-red)))

   `(diff-indicator-removed
     ((,srcery-class :background unspecified :foreground ,srcery-red)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-red)))

   `(diff-refine-added
     ((,srcery-class :background ,srcery-green :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-green :foreground ,srcery-256-black)))

   `(diff-refine-changed
     ((,srcery-class :background ,srcery-blue :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-blue :foreground ,srcery-256-bright-white)))

   `(diff-refine-removed
     ((,srcery-class :background ,srcery-red :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-red :foreground ,srcery-256-bright-white)))

   `(diff-removed
     ((,srcery-class :background unspecified :foreground ,srcery-red)
      (,srcery-256-class :background unspecified :foreground ,srcery-256-red)))

   ;;----------------------------------------------------------------------------
   ;; diff-hl
   ;;----------------------------------------------------------------------------
   `(diff-hl-change
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(diff-hl-delete
     ((,srcery-class :foreground ,srcery-red)
      (,srcery-256-class :foreground ,srcery-256-red)))

   `(diff-hl-insert
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   ;;----------------------------------------------------------------------------
   ;; dired
   ;;----------------------------------------------------------------------------
   `(dired-directory
     ((,srcery-class (:foreground ,srcery-blue :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-blue :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(dired-flagged
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(dired-header
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(dired-ignored
     ((,srcery-class (:inherit shadow))
      (,srcery-256-class (:inherit shadow))))

   `(dired-mark
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(dired-marked
     ((,srcery-class (:foreground ,srcery-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-magenta :weight bold))))

   `(dired-perm-write
     ((,srcery-class (:foreground ,srcery-bright-white :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :underline t))))

   `(dired-symlink
     ((,srcery-class (:foreground ,srcery-cyan :background ,srcery-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-cyan :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight bold))))

   `(dired-warning
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   ;;----------------------------------------------------------------------------
   ;; Dired Plus
   ;;----------------------------------------------------------------------------
   `(diredp-date-time
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(diredp-number
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(diredp-file-name
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredp-file-suffix
     ((,srcery-class (:foreground ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-bright-blue))))

   `(diredp-dir-heading
     ((,srcery-class (:foreground ,srcery-bright-white :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :underline t))))

   `(diredp-dir-heading
     ((,srcery-class (:foreground ,srcery-bright-white :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :underline t))))

   `(diredp-dir-priv
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredp-read-priv
     ((,srcery-class (:foreground ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-bright-yellow))))

   `(diredp-write-priv
     ((,srcery-class (:foreground ,srcery-bright-red))
      (,srcery-256-class (:foreground ,srcery-256-bright-red))))

   `(diredp-write-priv
     ((,srcery-class (:foreground ,srcery-bright-red))
      (,srcery-256-class (:foreground ,srcery-256-bright-red))))

   `(diredp-dir-name
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredp-exec-priv
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(diredp-symlink
     ((,srcery-class (:foreground ,srcery-bright-cyan))
      (,srcery-256-class (:foreground ,srcery-256-bright-cyan))))

   `(diredp-tagged-autofile-name
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,srcery-256-magenta))))

   `(diredp-no-priv
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(diredp-flag-mark
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black))))

   `(diredp-flag-mark-line
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black))))

   `(diredp-autofile-name
     ((,srcery-class (:background ,srcery-blue :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-blue :foreground ,srcery-256-bright-white))))

   `(diredp-deletion
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-white))))

   `(diredp-ignored-file-name
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(diredp-link-priv
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(diredp-mode-line-marked
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(diredp-other-priv
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredp-rare-priv
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; diredfl
   ;;----------------------------------------------------------------------------
   `(diredfl-autofile-name
     ((,srcery-class (:background ,srcery-blue))
      (,srcery-256-class (:background ,srcery-256-blue))))

   `(diredfl-compressed-file-name
     ((,srcery-class (:foreground ,srcery-green3))
      (,srcery-256-class (:foreground ,srcery-256-green3))))

   `(diredfl-compressed-file-suffix
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(diredfl-date-time
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(diredfl-dir-heading
     ((,srcery-class (:foreground ,srcery-teal))
      (,srcery-256-class (:foreground ,srcery-256-teal))))

   `(diredfl-dir-name
     ((,srcery-class (:foreground ,srcery-teal))
      (,srcery-256-class (:foreground ,srcery-256-teal))))

   `(diredfl-dir-priv
     ((,srcery-class (:foreground ,srcery-teal))
      (,srcery-256-class (:foreground ,srcery-256-teal))))

   `(diredfl-exec-priv
     ((,srcery-class (:foreground ,srcery-bright-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-green :weight bold))))

   `(diredfl-executable-tag
     ((,srcery-class (:foreground ,srcery-bright-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-green :weight bold))))

   `(diredfl-file-name
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredfl-file-suffix
     ((,srcery-class (:foreground ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-bright-yellow))))

   `(diredfl-flag-mark
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))

   `(diredfl-flag-mark-line
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(diredfl-ignored-file-name
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(diredfl-link-priv
     ((,srcery-class (:foreground ,srcery-orange))
      (,srcery-256-class (:foreground ,srcery-256-orange))))

   `(diredfl-no-priv
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(diredfl-number
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(diredfl-other-priv
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(diredfl-rare-priv
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diredfl-read-priv
     ((,srcery-class (:foreground ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-bright-yellow))))

   `(diredfl-symlink
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   `(diredfl-tagged-autofile-name
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(diredfl-write-priv
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   ;;----------------------------------------------------------------------------
   ;; ediff
   ;;----------------------------------------------------------------------------
   `(ediff-current-diff-A
     ((,srcery-class(:background ,srcery-dark-red))
      (,srcery-256-class(:background ,srcery-256-dark-red))))

   `(ediff-current-diff-Ancestor
     ((,srcery-class(:background ,srcery-dark-cyan))
      (,srcery-256-class(:background ,srcery-256-dark-cyan))))

   `(ediff-current-diff-B
     ((,srcery-class(:background ,srcery-dark-green))
      (,srcery-256-class(:background ,srcery-256-dark-green))))

   `(ediff-current-diff-C
     ((,srcery-class(:background ,srcery-dark-blue))
      (,srcery-256-class(:background ,srcery-256-dark-blue))))

   `(ediff-even-diff-A
     ((,srcery-class(:background ,srcery-gray-1))
      (,srcery-256-class(:background ,srcery-256-gray-1))))

   `(ediff-even-diff-Ancestor
     ((,srcery-class(:background ,srcery-gray-1))
      (,srcery-256-class(:background ,srcery-256-gray-1))))

   `(ediff-even-diff-B
     ((,srcery-class(:background ,srcery-gray-1))
      (,srcery-256-class(:background ,srcery-256-gray-1))))

   `(ediff-even-diff-C
     ((,srcery-class(:background ,srcery-gray-1))
      (,srcery-256-class(:background ,srcery-256-gray-1))))

   `(ediff-fine-diff-A
     ((,srcery-class(:background ,srcery-red :weight bold))
      (,srcery-256-class(:background ,srcery-256-red :weight bold))))

   `(ediff-fine-diff-Ancestor
     ((,srcery-class(:background ,srcery-cyan :weight bold))
      (,srcery-256-class(:background ,srcery-256-cyan :weight bold))))

   `(ediff-fine-diff-B
     ((,srcery-class(:background ,srcery-green :weight bold))
      (,srcery-256-class(:background ,srcery-256-green :weight bold))))

   `(ediff-fine-diff-C
     ((,srcery-class(:background ,srcery-blue :weight bold))
      (,srcery-256-class(:background ,srcery-256-blue :weight bold))))

   `(ediff-odd-diff-A
     ((,srcery-class(:background ,srcery-gray-3))
      (,srcery-256-class(:background ,srcery-256-gray-3))))

   `(ediff-odd-diff-Ancestor
     ((,srcery-class(:background ,srcery-gray-3))
      (,srcery-256-class(:background ,srcery-256-gray-3))))

   `(ediff-odd-diff-B
     ((,srcery-class(:background ,srcery-gray-3))
      (,srcery-256-class(:background ,srcery-256-gray-3))))

   `(ediff-odd-diff-C
     ((,srcery-class(:background ,srcery-gray-3))
      (,srcery-256-class(:background ,srcery-256-gray-3))))

   ;;----------------------------------------------------------------------------
   ;; ein
   ;;----------------------------------------------------------------------------
   `(ein:cell-input-area
     ((,srcery-class (:background ,srcery-hard-black))
      (,srcery-256-class (:background ,srcery-256-hard-black))))

   `(ein:cell-input-prompt
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(ein:cell-output-prompt
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(ein:notification-tab-normal
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(ein:notification-tab-selected
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   ;;----------------------------------------------------------------------------
   ;;eldoc
   ;;----------------------------------------------------------------------------
   `(eldoc-highlight-function-argument
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))


   ;;----------------------------------------------------------------------------
   ;; enh-ruby
   ;;----------------------------------------------------------------------------
   `(enh-ruby-string-delimiter-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(enh-ruby-op-face
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; erc
   ;;----------------------------------------------------------------------------
   `(erc-input-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(erc-my-nick-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(erc-nick-default-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(erc-nick-prefix-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(erc-notice-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(erc-prompt-face
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(erc-timestamp-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))


   ;;----------------------------------------------------------------------------
   ;; eshell
   ;;----------------------------------------------------------------------------
   `(eshell-ls-archive
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(eshell-ls-backup
     ((,srcery-class (:inherit font-lock-comment-face))
      (,srcery-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,srcery-class (:inherit font-lock-comment-face))
      (,srcery-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(eshell-ls-executable
     ((,srcery-class (:foreground ,srcery-orange :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-orange :weight bold))))

   `(eshell-ls-missing
     ((,srcery-class (:inherit font-lock-warning-face))
      (,srcery-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,srcery-class (:inherit font-lock-doc-face))
      (,srcery-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,srcery-class (:foreground ,srcery-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-magenta :weight bold))))

   `(eshell-ls-symlink
     ((,srcery-class (:foreground ,srcery-cyan :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-cyan :weight bold))))

   `(eshell-ls-unreadable
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(eshell-prompt
     ((,srcery-class (:foreground ,srcery-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-magenta :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; evil
   ;;----------------------------------------------------------------------------
   `(evil-ex-substitute-matches
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-white))))

   `(evil-ex-substitute-replacement
     ((,srcery-class (:background ,srcery-bright-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-green :foreground ,srcery-256-black))))

   `(evil-search-highlight-persist-highlight-face
     ((,srcery-class ,(if srcery-invert-matches
                          `(:inverse-video t)
                        `(:background ,srcery-gray-5 :weight bold)))
      (,srcery-256-class ,(if srcery-invert-matches
                              `(:inverse-video t)
                            `(:background ,srcery-256-gray-5 :weight bold)))))

   ;;----------------------------------------------------------------------------
   ;; Flymake
   ;;----------------------------------------------------------------------------
   `(flymake-error
     ((,srcery-class (:foreground ,srcery-red :underline t))
      (,srcery-256-class (:foreground ,srcery-256-red :underline t))))

   `(flymake-info
     ((,srcery-class (:foreground ,srcery-bright-white :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :underline t))))

   `(flymake-warning
     ((,srcery-class (:foreground ,srcery-bright-orange :underline t))
      (,srcery-256-class (:foreground ,srcery-bright-orange :underline t))))

   ;;----------------------------------------------------------------------------
   ;; Flycheck
   ;;----------------------------------------------------------------------------
   `(flycheck-error
     ((,srcery-class (:inherit flymake-error))
      (,srcery-256-class (:inherit flymake-error))))

   `(flycheck-note
     ((,srcery-class (:inherit flymake-info))
      (,srcery-256-class (:inherit flymake-info))))

   `(flycheck-warning
     ((,srcery-class (:inherit flymake-warning))
      (,srcery-256-class (:inherit flymake-warning))))

   `(flycheck-error-list-checker-name
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(flycheck-fringe-error
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(flycheck-fringe-info
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(flycheck-fringe-warning
     ((,srcery-class (:foreground ,srcery-bright-orange :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; Flyspell
   ;;----------------------------------------------------------------------------
   `(flyspell-duplicate
     ((,srcery-class (:foreground ,srcery-bright-white :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :underline t))))

   `(flyspell-incorrect
     ((,srcery-class (:foreground ,srcery-red :underline t))
      (,srcery-256-class (:foreground ,srcery-256-red :underline t))))

   ;;----------------------------------------------------------------------------
   ;; jabber
   ;;----------------------------------------------------------------------------
   `(jabber-activity-face
     ((,srcery-class (:weight bold :foreground ,srcery-red))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-red))))

   `(jabber-activity-personal-face
     ((,srcery-class (:weight bold :foreground ,srcery-blue))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-blue))))

   `(jabber-chat-error
     ((,srcery-class (:weight bold :foreground ,srcery-red))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-red))))

   `(jabber-chat-prompt-foreign
     ((,srcery-class (:weight bold :foreground ,srcery-red))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-red))))

   `(jabber-chat-prompt-local
     ((,srcery-class (:weight bold :foreground ,srcery-blue))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-blue))))

   `(jabber-chat-prompt-system
     ((,srcery-class (:weight bold :foreground ,srcery-green))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-green))))

   `(jabber-chat-text-foreign
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(jabber-chat-text-local
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(jabber-rare-time-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(jabber-roster-user-away
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(jabber-roster-user-chatty
     ((,srcery-class (:weight bold :foreground ,srcery-green))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-green))))

   `(jabber-roster-user-dnd
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(jabber-roster-user-error
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(jabber-roster-user-offline
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(jabber-roster-user-online
     ((,srcery-class (:weight bold :foreground ,srcery-green))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-green))))

   `(jabber-roster-user-xa
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   ;;----------------------------------------------------------------------------
   ;; git
   ;;----------------------------------------------------------------------------
   `(git-commit-summary
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(git-commit-note
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(git-commit-nonempty-second-line
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(diff-file-header
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(diff-hunk-header
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(diff-function
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(diff-header
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; git-gutter-fr
   ;;----------------------------------------------------------------------------
   `(git-gutter-fr:added
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(git-gutter-fr:deleted
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(git-gutter-fr:modified
     ((,srcery-class (:foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold))))

   `(git-gutter+-added
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(git-gutter+-deleted
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(git-gutter+-separator
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(git-gutter+-modified
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(git-gutter+-unchanged
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(git-gutter:added
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(git-gutter:modified
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(git-gutter:unchanged
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; git-timemachine
   ;;----------------------------------------------------------------------------
   `(git-timemachine-minibuffer-detail-face
     ((,srcery-class (:foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; gnus
   ;;----------------------------------------------------------------------------
   `(gnus-emphasis-highlight-words
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black))))

   `(gnus-header-content
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(gnus-header-from
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(gnus-header-name
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(gnus-header-subject
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(gnus-summary-cancelled
     ((,srcery-class (:background ,srcery-bright-orange :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-orange :foreground ,srcery-256-black))))

   ;;----------------------------------------------------------------------------
   ;; guide-key
   ;;----------------------------------------------------------------------------
   `(guide-key/highlight-command-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(guide-key/key-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(guide-key/prefix-command-face
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; helm
   ;;----------------------------------------------------------------------------
   `(helm-bookmark-directory
     ((,srcery-class (:inherit helm-ff-directory))
      (,srcery-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(helm-bookmark-gnus
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(helm-bookmark-info
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(helm-bookmark-man
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(helm-bookmark-w3m
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(helm-buffer-directory
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(helm-buffer-file
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-buffer-not-saved
     ((,srcery-class (:foreground ,srcery-green :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-green :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-buffer-process
     ((,srcery-class (:foreground ,srcery-red :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-red :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-buffer-saved-out
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-buffer-size
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-candidate-number
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-red :weight bold))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-red :weight bold))))

   `(helm-ff-directory
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(helm-ff-dotted-directory
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(helm-ff-dotted-symlink-directory
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(helm-ff-executable
     ((,srcery-class (:foreground ,srcery-green :background ,srcery-black :weight normal))
      (,srcery-256-class (:foreground ,srcery-256-green :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight normal))))

   `(helm-ff-file
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black :weight normal))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight normal))))

   `(helm-ff-invalid-symlink
     ((,srcery-class (:foreground ,srcery-red :background ,srcery-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight bold))))

   `(helm-ff-prefix
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-red :weight normal))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-red :weight normal))))

   `(helm-ff-symlink
     ((,srcery-class (:foreground ,srcery-cyan :background ,srcery-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-cyan :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight bold))))

   `(helm-grep-cmd-line
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-grep-file
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-grep-finish
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-grep-lineno
     ((,srcery-class (:foreground ,srcery-bright-blue :background ,srcery-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-blue :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :weight bold))))

   `(helm-grep-match
     ((,srcery-class (:foreground unspecified :background unspecified :inherit helm-match))
      (,srcery-256-class (:foreground unspecified :background unspecified :inherit helm-match))))

   `(helm-header
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black :underline nil :box nil))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black) :underline nil :box nil))))

   `(helm-header-line-left-margin
     ((,srcery-class (:foreground ,srcery-red :background unspecified))
      (,srcery-256-class (:foreground ,srcery-256-red :background unspecified))))

   `(helm-match
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(helm-match-item
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(helm-moccur-buffer
     ((,srcery-class (:foreground ,srcery-blue :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-blue :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-selection
     ((,srcery-class (:background ,srcery-gray-2 :weight bold))
      (,srcery-256-class (:background ,srcery-256-gray-2 :weight bold))))

   `(helm-selection-line
     ((,srcery-class (:background ,srcery-gray-2 :weight bold))
      (,srcery-256-class (:background ,srcery-256-gray-2 :weight bold))))

   `(helm-separator
     ((,srcery-class (:foreground ,srcery-green :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-green :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-source-header
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-green :underline t))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-green :underline t))))

   `(helm-time-zone-current
     ((,srcery-class (:foreground ,srcery-red :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-red :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-time-zone-home
     ((,srcery-class (:foreground ,srcery-green :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-green :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-visible-mark
     ((,srcery-class (:foreground ,srcery-red :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-red :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))


   ;;----------------------------------------------------------------------------
   ;; helm-swoop
   ;;----------------------------------------------------------------------------
   `(helm-swoop-target-line-block-face
     ((,srcery-class (:foreground ,srcery-yellow :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-yellow :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(helm-swoop-target-line-face
     ((,srcery-class (:background ,srcery-gray-2 :weight bold))
      (,srcery-256-class (:background ,srcery-256-gray-2 :weight bold))))

   `(helm-swoop-target-word-face
     ((,srcery-class (:foreground ,srcery-magenta :underline t))
      (,srcery-256-class (:foreground ,srcery-256-magenta :underline t))))

   ;;----------------------------------------------------------------------------
   ;; highlights
   ;;----------------------------------------------------------------------------
   `(hi-yellow
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(hi-green
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   ;;----------------------------------------------------------------------------
   ;; highlight-indentation
   ;;----------------------------------------------------------------------------
   `(highlight-indentation-face
     ((,srcery-class (:background ,srcery-bright-black))
      (,srcery-256-class (:background ,srcery-256-bright-black))))

   ;;----------------------------------------------------------------------------
   ;; highlight-symbol
   ;;----------------------------------------------------------------------------
   `(highlight-symbol-face
     ((,srcery-class (:background ,srcery-gray-2))
      (,srcery-256-class (:background ,srcery-256-gray-2))))

   ;;----------------------------------------------------------------------------
   ;; hydra
   ;;----------------------------------------------------------------------------
   `(hydra-face-blue
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(hydra-face-red
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   ;;----------------------------------------------------------------------------
   ;; ido
   ;;----------------------------------------------------------------------------
   `(ido-first-match
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(ido-only-match
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(ido-subdir
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(ido-indicator
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-white))))

   `(ido-vertical-match-face
     ((,srcery-class (:foreground ,srcery-green :underline nil))
      (,srcery-256-class (:foreground ,srcery-256-green :underline nil))))

   ;;----------------------------------------------------------------------------
   ;; info
   ;;----------------------------------------------------------------------------
   `(info-header-xref
     ((,srcery-class (:foreground ,srcery-yellow :underline t))
      (,srcery-256-class (:foreground ,srcery-256-yellow :underline t))))

   `(info-menu
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(info-node
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(info-quoted-name
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(info-reference-item
     ((,srcery-class (:background nil :underline t :weight bold))
      (,srcery-256-class (:background nil :underline t :weight bold))))

   `(info-string
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(info-title-1
     ((,srcery-class (:height 1.4 :weight bold))
      (,srcery-256-class (:height 1.4 :weight bold))))

   `(info-title-2
     ((,srcery-class (:height 1.3 :weight bold))
      (,srcery-256-class (:height 1.3 :weight bold))))

   `(info-title-3
     ((,srcery-class (:height 1.3))
      (,srcery-256-class (:height 1.3))))

   `(info-title-4
     ((,srcery-class (:height 1.2))
      (,srcery-256-class (:height 1.2))))

   ;;----------------------------------------------------------------------------
   ;; ivy
   ;;----------------------------------------------------------------------------
   `(ivy-current-match
     ((,srcery-class (:background ,srcery-gray-2 :weight bold))
      (,srcery-256-class (:background ,srcery-256-gray-2 :weight bold))))

   `(ivy-minibuffer-match-face-1
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(ivy-minibuffer-match-face-2
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(ivy-minibuffer-match-face-3
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(ivy-minibuffer-match-face-4
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(ivy-remote
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(ivy-highlight-face
     ((,srcery-class (:underline t))
      (,srcery-256-class (:underline t))))

   ;;----------------------------------------------------------------------------
   ;; latex
   ;;----------------------------------------------------------------------------
   `(font-latex-bold-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(font-latex-italic-face
     ((,srcery-class (:foreground ,srcery-red :italic t))
      (,srcery-256-class (:foreground ,srcery-256-red :italic t))))

   `(font-latex-match-reference-keywords
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(font-latex-match-variable-keywords
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(font-latex-sectioning-0-face
     ((,srcery-class (:weight bold :foreground ,srcery-bright-green :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-bright-green :height ,(if srcery-org-height 1.3 1.0)))))

   `(font-latex-sectioning-1-face
     ((,srcery-class (:weight bold :foreground ,srcery-bright-yellow :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-bright-yellow :height ,(if srcery-org-height 1.3 1.0)))))

   `(font-latex-sectioning-2-face
     ((,srcery-class (:weight bold :foreground ,srcery-blue :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-blue :height ,(if srcery-org-height 1.3 1.0)))))

   `(font-latex-sectioning-3-face
     ((,srcery-class (:weight bold :foreground ,srcery-cyan :height ,(if srcery-org-height 1.2 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-cyan :height ,(if srcery-org-height 1.2 1.0)))))

   `(font-latex-sectioning-4-face
     ((,srcery-class (:bold nil :foreground ,srcery-bright-green :height ,(if srcery-org-height 1.1 1.0)))
      (,srcery-class (:bold nil :foreground ,srcery-256-bright-green :height ,(if srcery-org-height 1.1 1.0)))))

   `(font-latex-sectioning-5-face
     ((,srcery-class (:bold nil :foreground ,srcery-yellow))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-yellow))))

   `(font-latex-string-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   ;;----------------------------------------------------------------------------
   ;; Line numbers
   ;;----------------------------------------------------------------------------
   `(linum
     ((,srcery-class (:foreground ,srcery-bright-black :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(linum-relative-current-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(nlinum-current-line
     ((,srcery-class (:foreground ,srcery-bright-black :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(nlinum-relative-current-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(line-number
     ((,srcery-class (:foreground ,srcery-bright-black :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(line-number-current-line
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; Git
   ;;----------------------------------------------------------------------------
   `(diff-context
     ((,srcery-class :foreground ,srcery-bright-black)
      (,srcery-256-class :foreground ,srcery-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; magit
   ;;----------------------------------------------------------------------------
   `(magit-blame-culprit
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-header
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(magit-blame-sha1
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-subject
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-time
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(magit-blame-name
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-heading
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(magit-blame-hash
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-summary
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(magit-blame-date
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(magit-log-date
     ((,srcery-class :foreground ,srcery-bright-white)
      (,srcery-256-class :foreground ,srcery-256-bright-white)))

   `(magit-log-graph
     ((,srcery-class :foreground ,srcery-bright-white)
      (,srcery-256-class :foreground ,srcery-256-bright-white)))

   `(magit-reflog-amend
     ((,srcery-class :foreground ,srcery-magenta)
      (,srcery-256-class :foreground ,srcery-256-magenta)))

   `(magit-reflog-other
     ((,srcery-class :foreground ,srcery-cyan)
      (,srcery-256-class :foreground ,srcery-256-cyan)))

   `(magit-reflog-rebase
     ((,srcery-class :foreground ,srcery-magenta)
      (,srcery-256-class :foreground ,srcery-256-magenta)))

   `(magit-reflog-remote
     ((,srcery-class :foreground ,srcery-cyan)
      (,srcery-256-class :foreground ,srcery-256-cyan)))

   `(magit-reflog-reset
     ((,srcery-class :foreground ,srcery-red)
      (,srcery-256-class :foreground ,srcery-256-red)))

   `(magit-branch
     ((,srcery-class (:foreground ,srcery-bright-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta :weight bold))))

   `(magit-branch-current
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-blue :weight bold :box t))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-blue :weight bold :box t))))

   `(magit-branch-local
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-blue :weight bold))))

   `(magit-branch-remote
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-orange :weight bold))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-orange :weight bold))))

   `(magit-diff-file-header
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(magit-diff-file-heading
     ((,srcery-class (:foreground ,srcery-blue :weight light))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight light))))

   `(magit-diff-file-heading-highlight
     ((,srcery-class (:foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold))))

   `(magit-diff-file-heading-selection
     ((,srcery-class (:foreground ,srcery-blue :weight bold :background ,srcery-gray-2))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold :background ,srcery-256-gray-2))))


   `(magit-diff-hunk-heading
     ((,srcery-class (:foreground ,srcery-yellow :weight light))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight light))))

   `(magit-diff-hunk-heading-highlight
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(magit-diff-hunk-heading-selection
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-black :weight bold))))


   `(magit-diff-added
     ((,srcery-class (:foreground ,srcery-green :weight light))
      (,srcery-256-class (:foreground ,srcery-256-green :weight light))))

   `(magit-diff-removed
     ((,srcery-class (:foreground ,srcery-red :weight light))
      (,srcery-256-class (:foreground ,srcery-256-red :weight light))))

   `(magit-diff-context
     ((,srcery-class (:foreground ,srcery-bright-black :weight light))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :weight light))))

   `(magit-diff-added-highlight
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(magit-diff-removed-highlight
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(magit-diff-context-highlight
     ((,srcery-class (:foreground ,srcery-bright-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :weight bold))))

   `(magit-diff-base
     ((,srcery-class (:foreground ,srcery-bright-black :weight light))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :weight light))))

   `(magit-diff-base-highlight
     ((,srcery-class (:foreground ,srcery-bright-black :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :weight bold))))

   `(magit-diff-lines-boundary
     ((,srcery-class (:background ,srcery-bright-black :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-black :foreground ,srcery-256-black))))

   `(magit-diff-lines-heading
     ((,srcery-class (:background ,srcery-bright-black :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-black :foreground ,srcery-256-black))))

   `(magit-hash
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(magit-item-highlight
     ((,srcery-class :background ,srcery-gray-2)
      (,srcery-256-class :background ,srcery-256-gray-2)))

   `(magit-log-author
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(magit-log-head-label-head
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black :weight bold))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black :weight bold))))

   `(magit-log-head-label-local
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-black :weight bold))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-black :weight bold))))

   `(magit-log-head-label-remote
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black :weight bold))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black :weight bold))))

   `(magit-log-head-label-tags
     ((,srcery-class (:background ,srcery-magenta :foreground ,srcery-black :weight bold))
      (,srcery-256-class (:background ,srcery-256-magenta :foreground ,srcery-256-black :weight bold))))

   `(magit-log-head-label-wip
     ((,srcery-class (:background ,srcery-cyan :foreground ,srcery-black :weight bold))
      (,srcery-256-class (:background ,srcery-256-cyan :foreground ,srcery-256-black :weight bold))))

   `(magit-log-sha1
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(magit-process-ng
     ((,srcery-class (:foreground ,srcery-bright-orange :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange :weight bold))))

   `(magit-process-ok
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(magit-section-heading
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(magit-section-highlight
     ((,srcery-class (:weight bold))
      (,srcery-256-class (:weight bold))))

   `(section-heading-selection
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(magit-section-title
     ((,srcery-class (:background ,srcery-black :foreground ,srcery-red :weight bold))
      (,srcery-256-class (:background ,(if srcery-transparent-background 'unspecified srcery-256-black) :foreground ,srcery-256-red :weight bold))))

   `(magit-cherry-equivalent
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(magit-cherry-unmatched
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(magit-reflog-checkout
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(magit-reflog-cherry-pick
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(magit-bisect-bad
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(magit-bisect-good
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(magit-bisect-skip
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(magit-diff-conflict-heading
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(magit-dimmed
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(magithub-ci-no-status
     ((,srcery-class (:foreground ,srcery-gray-6))
      (,srcery-256-class (:foreground ,srcery-256-gray-6))))

   `(magithub-issue-number
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(magithub-notification-reason
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   ;;----------------------------------------------------------------------------
   ;; smerge
   ;;----------------------------------------------------------------------------
   `(smerge-base
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(smerge-markers
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(smerge-mine
     ((,srcery-class (:foreground unspecified))
      (,srcery-256-class (:foreground unspecified))))

   `(smerge-other
     ((,srcery-class (:background ,srcery-gray-2))
      (,srcery-256-class (:background ,srcery-256-gray-2))))

   `(smerge-refined-added
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(smerge-refined-changed
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(smerge-refined-removed
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(smerge-upper
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(smerge-lower
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   ;;----------------------------------------------------------------------------
   ;; man
   ;;----------------------------------------------------------------------------
   `(Man-overstrike
     ((,srcery-class (:foreground ,srcery-blue :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-blue :weight bold))))

   `(Man-reverse
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(Man-underline
     ((,srcery-class (:foreground ,srcery-green :underline t))
      (,srcery-256-class (:foreground ,srcery-256-green :underline t))))


   ;;----------------------------------------------------------------------------
   ;; markdown
   ;;----------------------------------------------------------------------------
   `(markdown-header-face-1
     ((,srcery-class (:weight bold :foreground ,srcery-blue :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-blue :height ,(if srcery-org-height 1.3 1.0)))))

   `(markdown-header-face-2
     ((,srcery-class (:weight bold :foreground ,srcery-bright-cyan :height ,(if srcery-org-height 1.2 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-bright-cyan :height ,(if srcery-org-height 1.2 1.0)))))

   `(markdown-header-face-3
     ((,srcery-class (:bold nil :foreground ,srcery-bright-green :height ,(if srcery-org-height 1.1 1.0)))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-bright-green :height ,(if srcery-org-height 1.1 1.0)))))

   `(markdown-header-face-4
     ((,srcery-class (:bold nil :foreground ,srcery-yellow))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-yellow))))

   `(markdown-header-face-5
     ((,srcery-class (:bold nil :foreground ,srcery-blue))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-blue))))

   `(markdown-header-face-6
     ((,srcery-class (:bold nil :foreground ,srcery-cyan))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-cyan))))

   `(markdown-html-tag-delimiter-face
     ((,srcery-class (:bold nil :foreground ,srcery-gray-6))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-gray-6))))

   `(markdown-list-face
     ((,srcery-class (:bold nil :foreground ,srcery-gray-6))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-gray-6))))

   `(markdown-markup-face
     ((,srcery-class (:bold nil :foreground ,srcery-gray-6))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-gray-6))))

   ;;----------------------------------------------------------------------------
   ;; mu4e
   ;;----------------------------------------------------------------------------
   `(mu4e-cited-1-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(mu4e-cited-7-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(mu4e-header-marks-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(mu4e-header-key-face
     ((,srcery-class (:foreground ,srcery-cyan :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-cyan :weight bold))))

   `(mu4e-view-url-number-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(mu4e-unread-face
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; neotree
   ;;----------------------------------------------------------------------------
   `(neo-dir-link-face
     ((,srcery-class (:foreground ,srcery-red :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-red :weight bold))))

   `(neo-expand-btn-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(neo-file-link-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(neo-root-dir-face
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; org
   ;;----------------------------------------------------------------------------
   `(org-agenda-clocking
     ((,srcery-class (:background ,srcery-magenta :foreground ,srcery-green))
      (,srcery-256-class (:background ,srcery-256-magenta :foreground ,srcery-256-green))))

   `(org-agenda-date
     ((,srcery-class (:foreground ,srcery-blue :height ,(if srcery-org-height 1.1 1.0)))
      (,srcery-256-class (:foreground ,srcery-256-blue :height ,(if srcery-org-height 1.1 1.0)))))

   `(org-agenda-date-today
     ((,srcery-class (:foreground ,srcery-red :slant italic :weight bold :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:foreground ,srcery-256-red :slant italic :weight bold :height ,(if srcery-org-height 1.3 1.0)))))

   `(org-agenda-date-weekend
     ((,srcery-class (:weight bold :foreground ,srcery-blue))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-blue))))

   `(org-agenda-done
     ((,srcery-class (:foreground ,srcery-green :height ,(if srcery-org-height 1.2 1.0)))
      (,srcery-256-class (:foreground ,srcery-256-green :height ,(if srcery-org-height 1.2 1.0)))))

   `(org-agenda-structure
     ((,srcery-class (:weight bold :foreground ,srcery-green))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-green))))

   `(org-block
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(org-block-begin-line
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(org-block-end-line
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(org-clock-overlay
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(org-code
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(org-column
     ((,srcery-class (:background ,srcery-magenta))
      (,srcery-256-class (:background ,srcery-256-magenta))))

   `(org-column-title
     ((,srcery-class (:background ,srcery-magenta))
      (,srcery-256-class (:background ,srcery-256-magenta))))

   `(org-date
     ((,srcery-class (:underline t :foreground ,srcery-blue))
      (,srcery-256-class (:underline t :foreground ,srcery-256-blue))))

   `(org-date-selected
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))

   `(org-document-info-keyword
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(org-document-info
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(org-document-title
     ((,srcery-class (:foreground ,srcery-yellow :weight bold :height ,(if srcery-org-height 1.4 1.0)))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold :height ,(if srcery-org-height 1.4 1.0)))))

   `(org-done
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(org-ellipsis
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(org-footnote
     ((,srcery-class (:underline t :foreground ,srcery-bright-white))
      (,srcery-256-class (:underline t :foreground ,srcery-256-bright-white))))

   `(org-hide
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(org-kbd
     ((,srcery-class (:inherit region :foreground ,srcery-bright-white :box (:line-width 1 :style released-button)))
      (,srcery-256-class (:inherit region :foreground ,srcery-256-bright-white :box (:line-width 1 :style released-button)))))

   `(org-level-1
     ((,srcery-class (:weight bold :foreground ,srcery-bright-blue :height ,(if srcery-org-height 1.3 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-bright-blue :height ,(if srcery-org-height 1.3 1.0)))))

   `(org-level-2
     ((,srcery-class (:weight bold :foreground ,srcery-green :height ,(if srcery-org-height 1.2 1.0)))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-green :height ,(if srcery-org-height 1.2 1.0)))))

   `(org-level-3
     ((,srcery-class (:bold nil :foreground ,srcery-yellow :height ,(if srcery-org-height 1.1 1.0)))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-yellow :height ,(if srcery-org-height 1.1 1.0)))))

   `(org-level-4
     ((,srcery-class (:bold nil :foreground ,srcery-blue))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-blue))))

   `(org-level-5
     ((,srcery-class (:bold nil :foreground ,srcery-cyan))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-cyan))))

   `(org-level-6
     ((,srcery-class (:bold nil :foreground ,srcery-green))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-green))))

   `(org-level-7
     ((,srcery-class (:bold nil :foreground ,srcery-bright-orange))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-orange))))

   `(org-level-8
     ((,srcery-class (:bold nil :foreground ,srcery-bright-magenta))
      (,srcery-256-class (:bold nil :foreground ,srcery-256-bright-magenta))))

   `(org-link
     ((,srcery-class (:foreground ,srcery-bright-black :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-black :underline t))))

   `(org-meta-line
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(org-mode-line-clock-overrun
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(org-mode-line-clock
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(org-priority
     ((,srcery-class (:foreground ,srcery-bright-orange :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange :weight bold))))

   `(org-quote
     ((,srcery-class (:inherit org-block :slant italic))
      (,srcery-256-class (:inherit org-block :slant italic))))

   `(org-scheduled
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(org-scheduled-today
     ((,srcery-class (:foreground ,srcery-yellow :height ,(if srcery-org-height 1.2 1.0)))
      (,srcery-256-class (:foreground ,srcery-256-yellow :height ,(if srcery-org-height 1.2 1.0)))))

   `(org-sexp-date
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(org-special-keyword
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(org-drawer
      ((,srcery-class (:foreground ,srcery-yellow))
       (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(org-table
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-gray-1))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,srcery-256-gray-1))))

   `(org-time-grid
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(org-todo
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(org-verbatim
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   `(org-verse
     ((,srcery-class (:inherit org-block :slant italic))
      (,srcery-256-class (:inherit org-block :slant italic))))

   `(org-warning
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(org-archived
    ((,srcery-class (:foreground ,srcery-gray-5))
     (,srcery-256-class (:foreground ,srcery-256-gray-5))))


   ;;----------------------------------------------------------------------------
   ;; perspective
   ;;----------------------------------------------------------------------------
   `(persp-selected-face
     ((,srcery-class (:weight bold :foreground ,srcery-yellow))
      (,srcery-256-class (:weight bold :foreground ,srcery-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; popup
   ;;----------------------------------------------------------------------------
   `(popup-face
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-white))))

   `(popup-tip-face
     ((,srcery-class (:background ,srcery-bright-blue :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-blue :foreground ,srcery-256-black))))

   `(popup-menu-face
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-white))))

   `(popup-menu-selection-face
     ((,srcery-class (:background ,srcery-bright-blue :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-blue :foreground ,srcery-256-black))))

   `(popup-menu-mouse-face
     ((,srcery-class (:inherit highlight))
      (,srcery-256-class (:inherit highlight))))

   `(popup-isearch-match
     ((,srcery-class (:inherit match))
      (,srcery-256-class (:inherit match))))

   `(popup-scroll-bar-foreground-face
     ((,srcery-class (:background ,srcery-gray-5))
      (,srcery-256-class (:background ,srcery-256-gray-5))))

   `(popup-scroll-bar-background-face
     ((,srcery-class (:background ,srcery-gray-2))
      (,srcery-256-class (:background ,srcery-256-gray-2))))


   ;;----------------------------------------------------------------------------
   ;; mode-line
   ;;----------------------------------------------------------------------------
   `(powerline-active1
     ((,srcery-class (:background ,srcery-gray-4 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-4 :foreground ,srcery-256-bright-white))))

   `(powerline-active2
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-gray-2))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,srcery-256-gray-2))))

   `(powerline-inactive1
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-black))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-black))))

   `(powerline-inactive2
     ((,srcery-class (:background ,srcery-gray-2 :foreground ,srcery-bright-black))
      (,srcery-256-class (:background ,srcery-256-gray-2 :foreground ,srcery-256-bright-black))))

   `(mode-line
     ((,srcery-class (:foreground ,srcery-white :background ,srcery-gray-2))
      (,srcery-256-class (:foreground ,srcery-256-white :background ,srcery-256-gray-2))))

   `(mode-line-inactive
     ((,srcery-class (:foreground ,srcery-gray-6 :background ,srcery-gray-2))
      (,srcery-256-class (:foreground ,srcery-256-gray-6 :background ,srcery-256-gray-2))))

   `(mode-line-buffer-id
     ((,srcery-class (:weight bold))
      (,srcery-256-class (:weight bold))))

   `(mode-line-highlight
     ((,srcery-class (:background ,srcery-gray-2 :box (:color ,srcery-magenta :line-width 1)))
      (,srcery-256-class (:background ,srcery-256-gray-2 :box (:color ,srcery-256-magenta :line-width 1)))))

   `(mode-line-buffer-id-inactive
     ((,srcery-class (:weight bold))
      (,srcery-256-class (:weight bold))))

   `(magit-mode-line-process
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   ;; `(mode-line-emphasis
   ;;   ((,srcery-class (:weight bold :foreground ,srcery-yellow))
   ;;    (,srcery-256-class (:weight bold :foreground ,srcery-256-yellow))))

   `(spaceline-python-venv
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(spaceline-flycheck-error
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(spaceline-flycheck-info
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(spaceline-flycheck-warning
     ((,srcery-class (:foreground ,srcery-bright-orange))
      (,srcery-256-class (:foreground ,srcery-256-bright-orange))))

   `(spaceline-evil-normal
     ((,srcery-class (:background ,srcery-gray-5 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-5 :foreground ,srcery-256-bright-white))))

   `(spaceline-evil-insert
     ((,srcery-class (:background ,srcery-bright-white :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-white :foreground ,srcery-256-black))))

   `(spaceline-evil-replace
     ((,srcery-class (:background ,srcery-bright-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-bright-red :foreground ,srcery-256-bright-white))))

   `(spaceline-evil-visual
     ((,srcery-class (:background ,srcery-cyan :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-cyan :foreground ,srcery-256-black))))

   `(spaceline-evil-motion
     ((,srcery-class (:background ,srcery-bright-magenta :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-magenta :foreground ,srcery-256-black))))

   `(spaceline-evil-emacs
     ((,srcery-class (:background ,srcery-orange :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-orange :foreground ,srcery-256-bright-white))))

   `(spaceline-unmodified
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(spaceline-modified
     ((,srcery-class (:background ,srcery-bright-orange :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-orange :foreground ,srcery-256-black))))

   `(spaceline-read-only
     ((,srcery-class (:background ,srcery-hard-black :foreground ,srcery-orange))
      (,srcery-256-class (:background ,srcery-256-hard-black :foreground ,srcery-256-orange))))

   `(spaceline-highlight-face
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))


   ;;----------------------------------------------------------------------------
   ;; rainbow-delimiters
   ;;----------------------------------------------------------------------------
   `(rainbow-delimiters-depth-1-face
     ((,srcery-class :foreground ,srcery-bright-white)
      (,srcery-256-class :foreground ,srcery-256-bright-white)))

   `(rainbow-delimiters-depth-2-face
     ((,srcery-class :foreground ,srcery-bright-blue)
      (,srcery-256-class :foreground ,srcery-256-bright-blue)))

   `(rainbow-delimiters-depth-3-face
     ((,srcery-class :foreground ,srcery-bright-white)
      (,srcery-256-class :foreground ,srcery-256-bright-white)))

   `(rainbow-delimiters-depth-4-face
     ((,srcery-class :foreground ,srcery-bright-cyan)
      (,srcery-256-class :foreground ,srcery-256-bright-cyan)))

   `(rainbow-delimiters-depth-5-face
     ((,srcery-class :foreground ,srcery-bright-green)
      (,srcery-256-class :foreground ,srcery-256-bright-green)))

   `(rainbow-delimiters-depth-6-face
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(rainbow-delimiters-depth-7-face
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(rainbow-delimiters-depth-8-face
     ((,srcery-class :foreground ,srcery-yellow)
      (,srcery-256-class :foreground ,srcery-256-yellow)))

   `(rainbow-delimiters-unmatched-face
     ((,srcery-class :foreground ,srcery-red)
      (,srcery-256-class :foreground ,srcery-256-red)))

   `(rainbow-delimiters-mismatched-face
     ((,srcery-class :foreground ,srcery-bright-red)
      (,srcery-256-class :foreground ,srcery-256-bright-red)))

   ;; `(rainbow-delimiters-unmatched-face
   ;;   ((,srcery-class :foreground ,srcery-red :overline t :inhert bold)
   ;;    (,srcery-256-class :foreground ,srcery-256-red :overline t :inhert bold)))

   ;; `(rainbow-delimiters-mismatched-face
   ;;   ((,srcery-class :foreground ,srcery-red :overline t :weight bold)
   ;;    (,srcery-256-class :foreground ,srcery-256-red :overline t :weight bold)))


   ;;----------------------------------------------------------------------------
   ;; sh
   ;;----------------------------------------------------------------------------
   `(sh-heredoc
     ((,srcery-class (:foreground ,srcery-green :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-green :weight bold))))

   `(sh-quoted-exec
     ((,srcery-class (:foreground ,srcery-orange))
      (,srcery-256-class (:foreground ,srcery-256-orange))))

   ;;----------------------------------------------------------------------------
   ;; shm
   ;;----------------------------------------------------------------------------
   `(shm-current-face
     ((,srcery-class (:background ,srcery-green, :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green, :foreground ,srcery-256-black))))

   `(shm-quarantine-face
     ((,srcery-class (:background ,srcery-hard-black))
      (,srcery-256-class (:background ,srcery-256-hard-black))))


   ;;----------------------------------------------------------------------------
   ;; show-paren
   ;;----------------------------------------------------------------------------
   `(show-paren-match
     ((,srcery-class (:background ,srcery-magenta :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-magenta :foreground ,srcery-256-bright-white))))

   `(show-paren-mismatch
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-white))))


   ;;----------------------------------------------------------------------------
   ;; paren-face
   ;;----------------------------------------------------------------------------
   `(parenthesis
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))


   ;;----------------------------------------------------------------------------
   ;; smartparens
   ;;----------------------------------------------------------------------------
   `(sp-pair-overlay-face
     ((,srcery-class (:background ,srcery-gray-5 :foreground unspecified))
      (,srcery-256-class (:background ,srcery-256-gray-5 :foreground unspecified))))

   `(sp-show-pair-match-face
     ((,srcery-class (:background ,srcery-magenta :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-magenta :foreground ,srcery-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; evil-snipe
   ;;----------------------------------------------------------------------------
   `(evil-snipe-first-match-face
     ((,srcery-class (:foreground ,srcery-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-magenta :weight bold))))

   `(evil-snipe-matches-face
     ((,srcery-class (:foreground ,srcery-magenta :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-magenta :weight bold))))


   ;;----------------------------------------------------------------------------
   ;; spacemacs
   ;;----------------------------------------------------------------------------
   `(spacemacs-normal-face
     ((,srcery-class (:background ,srcery-gray-5 :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-gray-5 :foreground ,srcery-256-bright-white))))

   `(spacemacs-insert-face
     ((,srcery-class (:background ,srcery-bright-white :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-white :foreground ,srcery-256-black))))

   `(spacemacs-replace-face
     ((,srcery-class (:background ,srcery-bright-red :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-bright-red :foreground ,srcery-256-bright-white))))

   `(spacemacs-visual-face
     ((,srcery-class (:background ,srcery-bright-cyan :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-cyan :foreground ,srcery-256-black))))

   `(spacemacs-motion-face
     ((,srcery-class (:background ,srcery-magenta :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-magenta :foreground ,srcery-256-bright-white))))

   `(spacemacs-emacs-face
     ((,srcery-class (:background ,srcery-orange :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-orange :foreground ,srcery-256-bright-white))))

   `(spacemacs-hybrid-face
     ((,srcery-class (:background ,srcery-bright-orange :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-orange :foreground ,srcery-256-black))))

   `(spacemacs-lisp-face
     ((,srcery-class (:background ,srcery-green :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-green :foreground ,srcery-256-black))))

   `(spacemacs-evilified-face
     ((,srcery-class (:background ,srcery-bright-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-yellow :foreground ,srcery-256-black))))

   `(spacemacs-helm-navigation-ms-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(spacemacs-transient-state-title-face
     ((,srcery-class (:background unspecified :foreground ,srcery-green :box nil :weight bold))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-green :box nil :weight bold))))

   `(spacemacs-ido-navigation-ts-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(spacemacs-iedit-face
     ((,srcery-class (:background ,srcery-blue :foreground ,srcery-bright-white))
      (,srcery-256-class (:background ,srcery-256-blue :foreground ,srcery-256-bright-white))))

   `(spacemacs-iedit-insert-face
     ((,srcery-class (:background ,srcery-bright-blue :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-bright-blue :foreground ,srcery-256-black))))

   `(spacemacs-micro-state-binding-face
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   ;; spacemacs-ido-navigation-ts-face

   ;;----------------------------------------------------------------------------
   ;; swiper
   ;;----------------------------------------------------------------------------
   `(swiper-line-face
     ((,srcery-class (:background ,srcery-gray-2 :weight bold))
      (,srcery-256-class (:background ,srcery-gray-2 :weight bold))))

   `(swiper-match-face-1
     ((,srcery-class (:weight bold))
      (,srcery-256-class (:weight bold))))

   `(swiper-match-face-2
     ((,srcery-class (:foreground ,srcery-magenta :underline t))
      (,srcery-256-class (:foreground ,srcery-256-magenta :underline t))))

   `(swiper-match-face-3
     ((,srcery-class (:foreground ,srcery-yellow :underline t))
      (,srcery-256-class (:foreground ,srcery-256-yellow :underline t))))

   `(swiper-match-face-4
     ((,srcery-class (:foreground ,srcery-bright-green :underline t))
      (,srcery-256-class (:foreground ,srcery-256-bright-green :underline t))))


   ;;----------------------------------------------------------------------------
   ;; term
   ;;----------------------------------------------------------------------------
   `(term
     ((,srcery-class (:foreground ,srcery-bright-white :background ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-white :background ,(if srcery-transparent-background 'unspecified srcery-256-black)))))

   `(term-color-black
     ((,srcery-class (:foreground ,srcery-black))
      (,srcery-256-class (:foreground ,srcery-256-black))))

   `(term-color-blue
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(term-color-cyan
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(term-color-green
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(term-color-magenta
     ((,srcery-class (:foreground ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-magenta))))

   `(term-color-red
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(term-color-white
     ((,srcery-class (:foreground ,srcery-white))
      (,srcery-256-class (:foreground ,srcery-256-white))))

   `(term-color-yellow
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))


   ;;----------------------------------------------------------------------------
   ;; web-mode
   ;;----------------------------------------------------------------------------
   `(web-mode-builtin-face
     ((,srcery-class (:inherit ,font-lock-builtin-face))
      (,srcery-256-class (:inherit ,font-lock-builtin-face))))

   `(web-mode-comment-face
     ((,srcery-class (:inherit ,font-lock-comment-face))
      (,srcery-256-class (:inherit ,font-lock-comment-face))))

   `(web-mode-constant-face
     ((,srcery-class (:inherit ,font-lock-constant-face))
      (,srcery-256-class (:inherit ,font-lock-constant-face))))

   `(web-mode-doctype-face
     ((,srcery-class (:inherit ,font-lock-comment-face))
      (,srcery-256-class (:inherit ,font-lock-comment-face))))

   `(web-mode-function-name-face
     ((,srcery-class (:inherit ,font-lock-function-name-face))
      (,srcery-256-class (:inherit ,font-lock-function-name-face))))

   `(web-mode-html-attr-name-face
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(web-mode-html-attr-value-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(web-mode-html-tag-face
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(web-mode-html-tag-bracket-face
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-bright-black))))

   `(web-mode-keyword-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(web-mode-string-face
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(web-mode-symbol-face
     ((,srcery-class (:foreground ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-bright-blue))))

   `(web-mode-type-face
     ((,srcery-class (:inherit ,font-lock-type-face))
      (,srcery-256-class (:inherit ,font-lock-type-face))))

   `(web-mode-warning-face
     ((,srcery-class (:inherit ,font-lock-warning-face))
      (,srcery-256-class (:inherit ,font-lock-warning-face))))

   ;;----------------------------------------------------------------------------
   ;; CSS
   ;;----------------------------------------------------------------------------
   `(css-selector
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(css-property
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; XML
   ;;----------------------------------------------------------------------------
   `(nxml-element-local-name
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(nxml-attribute-local-name
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; which-key
   ;;----------------------------------------------------------------------------
   `(which-key-command-description-face
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(which-key-group-description-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(which-key-key-face
     ((,srcery-class (:foreground ,srcery-yellow :weight bold))
      (,srcery-256-class (:foreground ,srcery-256-yellow :weight bold))))

   `(which-key-separator-face
     ((,srcery-class (:background unspecified :foreground ,srcery-bright-green))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-bright-green))))

   `(which-key-special-key-face
     ((,srcery-class (:background ,srcery-yellow :foreground ,srcery-black))
      (,srcery-256-class (:background ,srcery-256-yellow :foreground ,srcery-256-black))))


   ;;----------------------------------------------------------------------------
   ;; which-function-mode
   ;;----------------------------------------------------------------------------
   `(which-func
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))


   ;;----------------------------------------------------------------------------
   ;; whitespace-mode
   ;;----------------------------------------------------------------------------
   `(whitespace-empty
     ((,srcery-class (:background unspecified :foreground ,srcery-yellow))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-yellow))))

   `(whitespace-indentation
     ((,srcery-class (:background unspecified :foreground ,srcery-bright-orange))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-bright-orange))))

   `(whitespace-line
     ((,srcery-class (:background unspecified :foreground ,srcery-green))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-green))))

   `(whitespace-newline
     ((,srcery-class (:background unspecified :foreground ,srcery-green))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-green))))

   `(whitespace-space
     ((,srcery-class (:background unspecified :foreground ,srcery-gray-5))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-gray-5))))

   `(whitespace-space-after-tab
     ((,srcery-class (:background unspecified :foreground ,srcery-yellow))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-yellow))))

   `(whitespace-space-before-tab
     ((,srcery-class (:background unspecified :foreground ,srcery-yellow))
      (,srcery-256-class (:background unspecified :foreground ,srcery-256-yellow))))

   `(whitespace-tab
     ((,srcery-class (:background unspecified))
      (,srcery-256-class (:background unspecified))))

   `(whitespace-trailing
     ((,srcery-class (:background ,srcery-red :foreground ,srcery-bright-orange))
      (,srcery-256-class (:background ,srcery-256-red :foreground ,srcery-256-bright-orange))))


   ;;----------------------------------------------------------------------------
   ;; ctbl
   ;;----------------------------------------------------------------------------
   `(ctbl:face-cell-select
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-magenta))))

   `(ctbl:face-continue-bar
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-yellow))))

   `(ctbl:face-row-select
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-blue))))

   ;;----------------------------------------------------------------------------
   ;; hlt
   ;;----------------------------------------------------------------------------
   `(hlt-property-highlight
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-yellow))))

   `(hlt-regexp-level-1
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-magenta))))

   `(hlt-regexp-level-2
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-green))))

   `(hlt-regexp-level-3
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-magenta))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-magenta))))

   `(hlt-regexp-level-4
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-yellow))))

   `(hlt-regexp-level-5
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-green))))

   `(hlt-regexp-level-6
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-blue))))

   `(hlt-regexp-level-7
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-cyan))))

   `(hlt-regexp-level-8
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-blue))))

   ;;----------------------------------------------------------------------------
   ;; reb
   ;;----------------------------------------------------------------------------
   `(reb-match-0
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-blue))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-blue))))

   `(reb-match-1
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-bright-cyan))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-bright-cyan))))

   `(reb-match-2
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-green))))

   `(reb-match-3
     ((,srcery-class (:foreground ,srcery-black :background ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-black :background ,srcery-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; other, need more work
   ;;----------------------------------------------------------------------------
   `(ac-completion-face
     ((,srcery-class (:underline t :foreground ,srcery-red))
      (,srcery-256-class (:underline t :foreground ,srcery-256-red))))

   `(epc:face-title
     ((,srcery-class :foreground ,srcery-blue :weight bold)
      (,srcery-256-class :foreground ,srcery-256-blue :weight bold)))

   `(ffap
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(flx-highlight-face
     ((,srcery-class (:foreground ,srcery-green :underline nil))
      (,srcery-256-class (:foreground ,srcery-256-green :underline nil))))

   `(icompletep-determined
     ((,srcery-class :foreground ,srcery-red)
      (,srcery-256-class :foreground ,srcery-256-red)))

   `(js2-external-variable
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(js2-function-param
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(js2-jsdoc-html-tag-name
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(js2-jsdoc-value
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   `(js2-private-function-call
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(js2-private-member
     ((,srcery-class (:foreground ,srcery-bright-white))
      (,srcery-256-class (:foreground ,srcery-256-bright-white))))

   `(js2-object-property
     ((,srcery-class (:foreground ,srcery-cyan))
      (,srcery-256-class (:foreground ,srcery-256-cyan))))

   `(js3-error-face
     ((,srcery-class (:underline ,srcery-bright-orange))
      (,srcery-256-class (:underline ,srcery-256-bright-orange))))

   `(js3-external-variable-face
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(js3-function-param-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(js3-instance-member-face
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(js3-jsdoc-tag-face
     ((,srcery-class (:foreground ,srcery-red))
      (,srcery-256-class (:foreground ,srcery-256-red))))

   `(js3-warning-face
     ((,srcery-class (:underline ,srcery-red))
      (,srcery-256-class (:underline ,srcery-256-red))))

   `(slime-repl-inputed-output-face
     ((,srcery-class (:foreground ,srcery-green))
      (,srcery-256-class (:foreground ,srcery-256-green))))

   `(trailing-whitespace
     ((,srcery-class :foreground unspecified :background ,srcery-red)
      (,srcery-256-class :foreground unspecified :background ,srcery-256-red)))

   `(undo-tree-visualizer-current-face
     ((,srcery-class :foreground ,srcery-red)
      (,srcery-256-class :foreground ,srcery-256-red)))

   `(undo-tree-visualizer-default-face
     ((,srcery-class :foreground ,srcery-bright-white)
      (,srcery-256-class :foreground ,srcery-256-bright-white)))

   `(undo-tree-visualizer-register-face
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(undo-tree-visualizer-unmodified-face
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(undo-tree-visualizer-active-branch-face
     ((,srcery-class :foreground ,srcery-bright-magenta)
      (,srcery-256-class :foreground ,srcery-256-bright-magenta)))

   `(persp-face-lighter-buffer-not-in-persp
     ((,srcery-class :background ,srcery-red :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-red :foreground ,srcery-256-bright-white)))

   `(pulse-highlight-face
     ((,srcery-class :background ,srcery-green :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-green :foreground ,srcery-256-black)))

   `(pulse-highlight-start-face
     ((,srcery-class :background ,srcery-bright-green :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-bright-green :foreground ,srcery-256-black)))

   `(custom-invalid
     ((,srcery-class :background ,srcery-bright-red :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-bright-red :foreground ,srcery-256-bright-white)))

   `(holiday
     ((,srcery-class :background ,srcery-bright-magenta :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-bright-magenta :foreground ,srcery-256-bright-white)))

   `(whitespace-trailing
     ((,srcery-class :background ,srcery-red :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-red :foreground ,srcery-256-bright-white)))

   `(whitespace-big-indent
     ((,srcery-class :background ,srcery-bright-red :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-bright-red :foreground ,srcery-256-bright-white)))

   `(whitespace-hspace
     ((,srcery-class :background ,srcery-bright-blue :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-bright-blue :foreground ,srcery-256-bright-white)))

   ;;----------------------------------------------------------------------------
   ;; Slack
   ;;----------------------------------------------------------------------------
   `(lui-button-face
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(lui-highlight-face
     ((,srcery-class :foreground ,srcery-magenta)
      (,srcery-256-class :foreground ,srcery-256-magenta)))

   `(lui-time-stamp-face
     ((,srcery-class :foreground ,srcery-bright-black)
      (,srcery-256-class :foreground ,srcery-256-bright-black)))

   `(slack-profile-image-face
     ((,srcery-class :background ,srcery-bright-white :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-bright-white :foreground ,srcery-256-black)))

   `(slack-preview-face
     ((,srcery-class :foreground ,srcery-cyan)
      (,srcery-256-class :foreground ,srcery-256-cyan)))

   `(slack-message-output-header
     ((,srcery-class :foreground ,srcery-yellow :weight bold)
      (,srcery-256-class :foreground ,srcery-256-yellow :weight bold)))

   ;;----------------------------------------------------------------------------
   ;; Message
   ;;----------------------------------------------------------------------------
   `(message-header-cc
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(message-header-newsgroups
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(message-header-subject
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(message-header-to
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   ;;----------------------------------------------------------------------------
   ;; Alert
   ;;----------------------------------------------------------------------------
   `(alert-low-face
     ((,srcery-class :foreground ,srcery-blue :weight bold)
      (,srcery-256-class :foreground ,srcery-256-blue :weight bold)))

   `(alert-moderate-face
     ((,srcery-class :foreground ,srcery-yellow :weight bold)
      (,srcery-256-class :foreground ,srcery-256-yellow :weight bold)))

   ;;----------------------------------------------------------------------------
   ;; Custom
   ;;----------------------------------------------------------------------------
   `(custom-comment-tag
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(custom-face-tag
     ((,srcery-class :foreground ,srcery-blue :weight bold)
      (,srcery-256-class :foreground ,srcery-256-blue :weight bold)))

   `(custom-group-tag
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(custom-state
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(custom-set
     ((,srcery-class :background ,srcery-bright-blue :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-bright-blue :foreground ,srcery-256-black)))

   `(custom-modified
     ((,srcery-class :background ,srcery-blue :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-blue :foreground ,srcery-256-bright-white)))

   `(custom-themed
     ((,srcery-class :background ,srcery-blue :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-blue :foreground ,srcery-256-black)))

   `(custom-variable-tag
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(custom-changed
     ((,srcery-class :background ,srcery-blue :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-blue :foreground ,srcery-256-black)))

   `(custom-comment
     ((,srcery-class :background ,srcery-bright-black :foreground ,srcery-black)
      (,srcery-256-class :background ,srcery-256-bright-black :foreground ,srcery-256-black)))

   ;;----------------------------------------------------------------------------
   ;; widget
   ;;----------------------------------------------------------------------------
   `(widget-field
     ((,srcery-class :background ,srcery-gray-3 :foreground ,srcery-bright-white)
      (,srcery-256-class :background ,srcery-256-gray-3 :foreground ,srcery-256-bright-white)))

   `(widget-documentation
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   ;;----------------------------------------------------------------------------
   ;; Misc
   ;;----------------------------------------------------------------------------
   `(epa-string
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(imenu-list-entry-face-0
     ((,srcery-class :foreground ,srcery-magenta)
      (,srcery-256-class :foreground ,srcery-256-magenta)))

   `(imenu-list-entry-face-1
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(imenu-list-entry-face-2
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(imenu-list-entry-face-3
     ((,srcery-class :foreground ,srcery-blue)
      (,srcery-256-class :foreground ,srcery-256-blue)))

   `(lv-separator
     ((,srcery-class :background ,srcery-white :foreground ,srcery-bright-black)
      (,srcery-256-class :background ,srcery-256-white :foreground ,srcery-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; mmm
   ;;----------------------------------------------------------------------------
   `(mmm-default-submode-face
     ((,srcery-class :background ,(if srcery-transparent-background 'unspecified srcery-256-black))
      (,srcery-256-class :background unspecified)))

   ;;----------------------------------------------------------------------------
   ;; rst
   ;;----------------------------------------------------------------------------
   `(rst-level-1
     ((,srcery-class :inherit org-level-1)
      (,srcery-256-class :inherit org-level-1)))

   `(rst-level-2
     ((,srcery-class :inherit org-level-2)
      (,srcery-256-class :inherit org-level-2)))

   `(rst-level-3
     ((,srcery-class :inherit org-level-3)
      (,srcery-256-class :inherit org-level-3)))

   `(rst-level-4
     ((,srcery-class :inherit org-level-4)
      (,srcery-256-class :inherit org-level-4)))

   `(rst-level-5
     ((,srcery-class :inherit org-level-5)
      (,srcery-256-class :inherit org-level-5)))

   `(rst-level-6
     ((,srcery-class :inherit org-level-6)
      (,srcery-256-class :inherit org-level-6)))

   `(rst-adornment
     ((,srcery-class :foreground ,srcery-white)
      (,srcery-256-class :foreground ,srcery-256-white)))

   ;;----------------------------------------------------------------------------
   ;; lsp
   ;;----------------------------------------------------------------------------
   `(lsp-ui-doc-background
     ((,srcery-class :background ,srcery-gray-2)
      (,srcery-256-class :background ,srcery-256-gray-2)))

   `(lsp-ui-doc-header
     ((,srcery-class :foreground ,srcery-green)
      (,srcery-256-class :foreground ,srcery-256-green)))

   `(lsp-ui-peek-footer
     ((,srcery-class :foreground ,srcery-bright-white :background ,srcery-gray-3)
      (,srcery-256-class :foreground ,srcery-256-bright-white :background ,srcery-256-gray-3)))

   `(lsp-ui-peek-header
     ((,srcery-class :foreground ,srcery-bright-white :background ,srcery-gray-4)
      (,srcery-256-class :foreground ,srcery-256-bright-white :background ,srcery-256-gray-4)))

   `(lsp-ui-peek-highlight
     ((,srcery-class :foreground ,srcery-bright-white :background ,srcery-gray-5)
      (,srcery-256-class :foreground ,srcery-256-bright-white :background ,srcery-256-gray-5)))

   `(lsp-ui-peek-line-number
     ((,srcery-class (:foreground ,srcery-bright-black))
      (,srcery-256-class (:foreground ,srcery-256-bright-black))))

   `(lsp-ui-peek-list
     ((,srcery-class (:inherit lsp-ui-doc-background))
      (,srcery-256-class (:inherit lsp-ui-doc-background))))

   `(lsp-ui-peek-peek
     ((,srcery-class (:inherit lsp-ui-doc-background))
      (,srcery-256-class (:inherit lsp-ui-doc-background))))

   ;;----------------------------------------------------------------------------
   ;; orderless
   ;;----------------------------------------------------------------------------
   `(orderless-match-face-0
     ((,srcery-class (:foreground ,srcery-bright-magenta))
      (,srcery-256-class (:foreground ,srcery-256-bright-magenta))))

   `(orderless-match-face-1
     ((,srcery-class (:foreground ,srcery-blue))
      (,srcery-256-class (:foreground ,srcery-256-blue))))

   `(orderless-match-face-2
     ((,srcery-class (:foreground ,srcery-yellow))
      (,srcery-256-class (:foreground ,srcery-256-yellow))))

   `(orderless-match-face-3
     ((,srcery-class (:foreground ,srcery-bright-green))
      (,srcery-256-class (:foreground ,srcery-256-bright-green))))

   ;;----------------------------------------------------------------------------
   ;; OCaml (tuareg)
   ;;----------------------------------------------------------------------------
   `(tuareg-font-lock-governing-face
     ((,srcery-class (:foreground ,srcery-bright-yellow))
      (,srcery-256-class (:foreground ,srcery-256-bright-yellow))))
   )

  (custom-theme-set-variables
   'srcery
   `(ansi-color-names-vector [,srcery-black ,srcery-red ,srcery-green ,srcery-bright-yellow ,srcery-blue ,srcery-magenta ,srcery-cyan ,srcery-gray-6])
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'srcery)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; srcery-theme.el ends here
