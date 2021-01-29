;;; themes/doom-clapoto-light-theme.el -*- lexical-binding: t;no-byte-compile: t -*-

;;; Commentary:
(require 'doom-themes)
;;; Code:
;;
(defgroup doom-clapoto-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-clapoto-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-clapoto-light-theme
  :type 'boolean)

(defcustom doom-clapoto-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-clapoto-light-theme
  :type 'boolean)

(defcustom doom-clapoto-light-comment-bg doom-clapoto-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-clapoto-light-theme
  :type 'boolean)

(defcustom doom-clapoto-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-clapoto-light-theme
  :type '(choice integer boolean))

(def-doom-theme doom-clapoto-light
  "A sepia/green light theme"

  ((bg         '("#fbfffc" "color-231"))
   (bg-alt     '("#eff3f0" "color-194"))
   (base0      '("#e9fae6" "color-194"))
   (base1      '("#ddfad9" "color-188"))
   (base2      '("#d0dfd0" "color-151"))
   (base3      '("#a1bc9f" "color-145"))
   (base4      '("#b6cab6" "color-151"))
   (base5      '("#9eb79e" "color-108"))
   (base6      '("#4c5a4c" "color-240"))
   (base7      '("#345134" "color-65"))
   (base8      '("#1c261e" "color-235"))
   (fg         '("#0f280f" "color-16"))
   (fg-alt     '("#153915" "color-22"))

   (grey        '("#475A47" "color-240"))
   (red         '("#b12900" "color-124"))
   (orange      '("#976000" "color-94"))
   (green       '("#018513" "color-28"))
   (green-br    '("#50a05b" "color-71"))
   (teal        '("#6775cc" "color-68"))
   (yellow      '("#796F13" "color-100"))
   (dark-yellow '("#B4A700" "color-142"))
   (blue        '("#0c1e8a" "color-18"))
   (dark-blue   '("#6775cc" "color-62"))
   (magenta     '("#970855" "color-89"))
   (violet      '("#cc659d" "color-169"))
   (cyan        '("#147d68" "color-29"))
   (dark-cyan   '("#85cbbd" "color-116"))
   ;; face categories
   (highlight      yellow)
   (vertical-bar   base0)
   (selection      base4)
   (builtin        dark-yellow)
   (comments       (if doom-clapoto-light-brighter-comments green-br grey))
   (doc-comments   (if doom-clapoto-light-brighter-comments (doom-lighten green-br 0.15) (doom-darken grey 0.1)))
   (constants      base7)
   (functions      green-br)
   (keywords       dark-yellow)
   (methods        yellow)
   (operators      cyan)
   (type           dark-yellow)
   (strings        green)
   (variables      green-br)
   (numbers        green-br)
   (region         base3)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    teal)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when doom-clapoto-light-padded-modeline
      (if (integerp doom-clapoto-light-padded-modeline) doom-sourcerer-padded-modeline 4)))

   (modeline-fg base8)
   (modeline-fg-alt (doom-blend yellow grey (if doom-clapoto-light-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if doom-clapoto-light-brighter-modeline
        `(,(car base4) ,@(cdr base1))
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-l
    (if doom-clapoto-light-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background base8 :foreground base4)
   (cursor :background yellow)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-clapoto-light-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (mode-line-buffer-id :foreground blue :bold bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground teal :bold bold)

   (doom-modeline-bar :background (if doom-clapoto-light-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if doom-clapoto-light-brighter-modeline base8 blue) :bold bold)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-project-root :foreground teal :weight 'bold)

   (tab-line :background base3)
   (tab-line-tab :background base3 :foreground fg)
   (tab-line-tab-inactive :background base3 :foreground fg-alt :box `(:line-width 1 :color ,base1 :style released-button))
   (tab-line-tab-current :background base4 :foreground fg :box `(:line-width 1 :color ,base1 :style pressed-button))
   (tab-bar :background base2)
   (tab-bar-tab :background base4 :foreground highlight)
   (tab-bar-tab-inactive :background bg-alt :foreground fg-alt)

   (mode-line
    :background base3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
   (mode-line-inactive
    :background bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if doom-clapoto-light-brighter-modeline base8 highlight))
   (fringe :background base1)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (lazy-highlight :foreground fg :background base4)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground yellow)

;;;;; ivy-mode
   (ivy-current-match :background dark-yellow :distant-foreground base7 :weight 'normal)
   (ivy-posframe :background base0 :foreground fg)
   (internal-border :background base7)

;;;;; lsp-mode and lsp-ui-mode
   (lsp-face-highlight-textual :background dark-cyan :foreground base8 :distant-foreground base7 :weight 'bold)
   (lsp-face-highlight-read    :background dark-cyan :foreground base8 :distant-foreground base7 :weight 'bold)
   (lsp-face-highlight-write   :background dark-cyan :foreground base8 :distant-foreground base7 :weight 'bold)
   (lsp-ui-peek-header :foreground fg :background (doom-darken bg 0.1) :bold bold)
   (lsp-ui-peek-list :background (doom-lighten bg 0.1))
   (lsp-ui-peek-peek :background (doom-lighten bg 0.1))

   ;; tooltip and company
   (tooltip              :background bg-alt :foreground fg)
   (company-tooltip-selection     :background base3)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)
   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground dark-yellow)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground magenta)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground teal)

;;;;; org-mode
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt :slant 'italic)
   ((org-block-end-line &override) :background bg-alt :slant 'italic)
   ((org-document-title &override) :foreground yellow :height 1.953125)
   ((org-level-1 &override) :foreground blue :height 1.75)
   ((org-level-2 &override) :foreground green :height 1.5625)
   ((org-level-3 &override) :foreground orange :height 1.25)
   ((org-level-4 &override) :foreground red)
   ((org-level-5 &override) :foreground magenta)
   ((org-level-6 &override) :foreground yellow)
   ((org-level-7 &override) :foreground violet)
   ((org-level-8 &override) :foreground green)
   (org-hide :foreground hidden)
   ((org-quote &override) :background base1)
   (solaire-org-hide-face :foreground hidden-alt)

;;;;; rjsx-mode
   (rjsx-tag :foreground yellow)
   (rjsx-tag-bracket-face :foreground base8)
   (rjsx-attr :foreground magenta :slant 'italic :weight 'medium)

;;;;; selectrum
   (selectrum-current-candidate :background base3 :distant-foreground base7 :weight 'normal)

;;;;; treemacs
   (treemacs-root-face :foreground strings :weight 'bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground comments)
   )


  ;; --- extra variables --------------------
  ;; ()

  )

;;; doom-clapoto-light-theme.el ends here
