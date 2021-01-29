;;; themes/doom-papercolor-light-theme.el -*- lexical-binding: t;no-byte-compile: t -*-

;;; Commentary:
(require 'doom-themes)
;;; Code:
;;
(defgroup doom-papercolor-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-papercolor-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-papercolor-light-theme
  :type 'boolean)

(defcustom doom-papercolor-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-papercolor-light-theme
  :type 'boolean)

(defcustom doom-papercolor-light-comment-bg doom-papercolor-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-papercolor-light-theme
  :type 'boolean)

(defcustom doom-papercolor-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-papercolor-light-theme
  :type '(choice integer boolean))

(def-doom-theme doom-papercolor-light
  "A clone of PaperColor light theme. https://github.com/NLKNguyen/papercolor-theme"

  ((bg         '("#EEEEEE" "color-231"))
   (bg-alt     '("#DADADA" "color-194"))
   (base0      '("#E4E4E4" "color-194"))
   (base1      '("#D0D0D0" "color-188"))
   (base2      '("#C6C6C6" "color-151"))
   (base3      '("#B2B2B2" "color-145"))
   (base4      '("#9E9E9E" "color-151"))
   (base5      '("#808080" "color-108"))
   (base6      '("#6C6C6C" "color-240"))
   (base7      '("#585858" "color-65"))
   (base8      '("#1C1C1C" "color-235"))
   (fg         '("#303030" "color-16"))
   (fg-alt     '("#4E4E4E" "color-22"))

   (grey        '("#9E9E9E" "#9E9E9E"))
   (white       '("#FFFFFF" "#FFFFFF"))
   (red         '("#D7005F" "color-124"))
   (orange      '("#D75F00" "color-94"))
   (green       '("#008700" "color-28"))
   (green-br    '("#5F8700" "color-71"))
   (teal        '("#5F87AF" "color-68"))
   (yellow      '("#D7AF00" "color-100"))
   (dark-yellow '("#AF8700" "color-142"))
   (blue        '("#005FAF" "color-18"))
   (dark-blue    blue)
   (magenta     '("#875FAF" "color-89"))
   (violet      '("#875FAF" "color-169"))
   (cyan        '("#5FAFAF" "color-29"))
   (dark-cyan   '("#005F87" "color-116"))
   ;; face categories
   (highlight      base7)
   (vertical-bar   base0)
   (selection      base4)
   (builtin        dark-yellow)
   (comments       (if doom-papercolor-light-brighter-comments green-br grey))
   (doc-comments   (if doom-papercolor-light-brighter-comments (doom-lighten green-br 0.15) (doom-darken grey 0.1)))
   (constants      base7)
   (functions      green-br)
   (keywords       dark-yellow)
   (methods        blue)
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
    (when doom-papercolor-light-padded-modeline
      (if (integerp doom-papercolor-light-padded-modeline) doom-sourcerer-padded-modeline 4)))

   (modeline-fg base8)
   (modeline-fg-alt (doom-blend yellow grey (if doom-papercolor-light-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if doom-papercolor-light-brighter-modeline
        `(,(car base4) ,@(cdr base1))
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-l
    (if doom-papercolor-light-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background base8 :foreground base4)
   (cursor :background yellow)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-papercolor-light-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (mode-line-buffer-id :foreground blue :bold bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground teal :bold bold)

   (doom-modeline-bar :background (if doom-papercolor-light-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if doom-papercolor-light-brighter-modeline base8 blue) :bold bold)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-project-root :foreground teal :weight 'bold)

   (tab-line :background base3)
   (tab-line-tab :background base3 :foreground fg)
   (tab-line-tab-inactive :background base3 :foreground fg-alt :box `(:line-width 1 :color ,base1 :style released-button))
   (tab-line-tab-current :background base4 :foreground fg :box `(:line-width 1 :color ,base1 :style pressed-button))
   (tab-bar :background base2)
   (tab-bar-tab :background base4 :foreground highlight)
   (tab-bar-tab-inactive :background bg-alt :foreground fg-alt)

   ((show-paren-match &override) :foreground dark-cyan)
   (mode-line
    :background base3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
   (mode-line-inactive
    :background bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if doom-papercolor-light-brighter-modeline base8 highlight))
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
   (ivy-current-match :background white :distant-foreground base7 :weight 'normal)
   (ivy-posframe :background base0 :foreground fg)
   (internal-border :background base7)

;;;;; lsp-mode and lsp-ui-mode
   (lsp-face-highlight-textual :background white :foreground base8 :distant-foreground base7 :weight 'bold)
   (lsp-face-highlight-read    :background white :foreground base8 :distant-foreground base7 :weight 'bold)
   (lsp-face-highlight-write   :background white :foreground base8 :distant-foreground base7 :weight 'bold)
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

;;; doom-papercolor-light-theme.el ends here
