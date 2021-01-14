;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic behaviour and appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show trailing spaces
;(setq-default show-trailing-whitespace t)

;; Disable trailing whitespaces in the minibuffer
;(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
;  (setq-local show-trailing-whitespace nil))

;; Set tabs to indent as white spaces and set def. tab width to 4 white spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Minimalistic Emacs at startup
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode nil)

;; Maximize first frame
;(set-frame-parameter nil 'fullscreen 'maximized)

;; File names relative to project (not root)
;(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

;; Don't ask when killing emacs
(setq confirm-kill-emacs nil)

;; Resize windows interactively.
;(def-package! resize-window
;  :commands (resize-window))

;; Reuse dired buffers
;(put 'dired-find-alternate-file 'disabled nil)

;; Smooth mouse scrolling
;(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
;      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
;      mouse-wheel-follow-mouse t                    ; scroll window under mouse
;      scroll-step 1)

;; Do not automatically copy selected text.
(setq select-enable-primary nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall theme & visual behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Font setup
(setq doom-font (font-spec :family "Fira Code Medium" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Code Medium")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Medium" :size 16))

;; (setq doom-font (font-spec :family "Meslo LG M DZ for Powerline" :size 20)
;;       doom-variable-pitch-font (font-spec :family "Meslo LG M DZ for Powerline")
;;       doom-unicode-font (font-spec :family "DejaVu Sans Mono")
;;       doom-big-font (font-spec :family "Meslo LG M DZ for Powerline" :size 24))



;; All themes are safe to load
;(setq custom-safe-themes t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-nova)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-sourcerer)

;; User brighter comments for doom one, particularly
;; useful for reveal js presentations that inherits
;; code highlighting from one's emacs theme.
(setq doom-one-brighter-comments t)
(setq doom-one-comment-bg nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;(setq display-line-numbers-type t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(after! smartparens
;  (smartparens-global-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persp / workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq-default +workspaces-switch-project-function #'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq projectile-enable-caching nil)
;; (setq projectile-project-compilation-cmd "./run.py")
(setq projectile-project-search-path '("~/Projects/" "~/work/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PlantUML



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode and org-capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal variables?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select popup buffers by default
;(setq +popup-defaults
;  (list :side   'bottom
;        :height 0.16
;        :width  40
;        :quit   t
;        :select t
;        :ttl    5))

;; TODO(dfrib): See if I really feel that I need this customization?
;; Select the IList buffer when it is shown
;(after! imenu-list
;  (set-popup-rule! "^\\*Ilist"
;    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
;(after! undo-tree
;  (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'right :size 40 :modeline nil :select t :quit t))

;; Larger org src edit
;(after! org
;  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet file templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file templates
;; Just place them in ~/.doom.d/snippets - these will take precedence over the
;; default ones.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and caching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq make-backup-files t
;      backup-by-copying t
;      delete-old-versions t
;      kept-new-versions 6
;      kept-old-versions 2
;      version-control t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(after! flycheck
;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;  )

;; Let flycheck search for required files in the `load-path' and the current folder.
;(setq flycheck-emacs-lisp-load-path '("./"))

;; disable using hooks
;; (add-hook 'text-mode-hook (lambda ()
;;                             (flycheck-mode -1)))
;; (add-hook 'org-mode-hook (lambda ()
;; (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; (use-package! lsp-mode
;;   :hook (((rust-mode c-mode c++-mode java-mode elixir-mode) . lsp)
;;          (lsp-mode . yas-minor-mode))
;;   :custom-face
;;   (lsp-modeline-code-actions-face ((t (:inherit mode-line))))
;;   :custom
;;   (lsp-enable-links nil)
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-rust-clippy-preference "on")
;;   (lsp-prefer-capf t)
;;   (lsp-enable-indentation nil)
;;   (lsp-enable-symbol-highlighting t)
;;   (lsp-rust-server 'rust-analyzer)
;;   (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
;;   (lsp-headerline-breadcrumb-enable nil))

;; (defun aorst/escape ()
;;   "Quit in current context.

;; When there is an active minibuffer and we are not inside it close
;; it.  When we are inside the minibuffer use the regular
;; `minibuffer-keyboard-quit' which quits any active region before
;; exiting.  When there is no minibuffer `keyboard-quit' unless we
;; are defining or executing a macro."
;;   (interactive)
;;   (cond ((active-minibuffer-window)
;;          (if (minibufferp)
;;              (minibuffer-keyboard-quit)
;;            (abort-recursive-edit)))
;;         ((bound-and-true-p iedit-mode)
;;          (iedit-quit))
;;         (t
;;          (unless (or defining-kbd-macro
;;                      executing-kbd-macro)
;;            (keyboard-quit)))))
;; (global-set-key [remap keyboard-quit] #'aorst/escape)

;; (use-package! lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :bind (:map lsp-ui-mode-map
;;          ("M-." . lsp-ui-peek-find-definitions)
;;          ("M-/" . lsp-ui-peek-find-references))
;;   :custom
;;   (lsp-ui-doc-border (face-attribute 'mode-line-inactive :background))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-imenu-enable nil)
;;   (lsp-ui-doc-delay 1 "higher than eldoc delay")
;;   (lsp-ui-doc-position 'at-point)
;;   :config
;;   (when (fboundp #'aorst/escape)
;;     (define-advice lsp-ui-doc--make-request (:around (foo) :lsp-ui-doc-hide)
;;       (unless (eq this-command 'aorst/escape)
;;         (funcall foo))))
;;   (lsp-ui-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package! company
;;   :bind (:map company-active-map
;;          ("TAB" . company-complete-common-or-cycle)
;;          ("<tab>" . company-complete-common-or-cycle)
;;          ("<S-Tab>" . company-select-previous)
;;          ("<backtab>" . company-select-previous)
;;          ("C-n" . company-select-next)
;;          ("C-p" . company-select-previous))
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-require-match 'never)
;;   (company-minimum-prefix-length 2)
;;   (company-tooltip-align-annotations t)
;;   (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
;;                        company-preview-frontend
;;                        company-echo-metadata-frontend))
;;   (company-backends '(company-capf company-files))
;;   (company-tooltip-minimum-width 30)
;;   (company-tooltip-maximum-width 60))

;; (use-package! company-posframe
;;   :after company
;;   :custom
;;   (company-posframe-quickhelp-show-header nil)
;;   (company-posframe-show-indicator nil)
;;   (company-posframe-show-metadata nil)
;;   (company-posframe-quickhelp-show-params
;;    (list :poshandler #'company-posframe-quickhelp-right-poshandler
;;          :internal-border-width 1
;;          :timeout 60
;;          :internal-border-color (face-attribute 'mode-line :background)
;;          :no-properties nil))
;;   (company-posframe-show-params
;;    (list :poshandler #'company-posframe-quickhelp-right-poshandler
;;          :internal-border-width 1
;;          :timeout 60
;;          :internal-border-color (face-attribute 'mode-line :background)
;;          :no-properties nil))
;;   :config
;;   (company-posframe-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! c++-mode
  (set-formatter! 'c++-mode 'clang-format)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open debugging window style
;(setq gdb-many-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalized bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load! "+bindings")
