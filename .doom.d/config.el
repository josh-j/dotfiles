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
(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

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
;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-nova)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-sourcerer)
;(setq doom-theme 'doom-laserwave)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-laserwave t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


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
(setq projectile-project-search-path '("~/Projects/"))

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
;;    (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;    (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

;; (setq lsp-clients-clangd-args '("-j=4"
;;                                 "--background-index"
;;                                 "--clang-tidy"
;;                                 "--pch-storage=memory"
;;                                 "--completion-style=detailed"
;;                                 "--header-insertion=never"))
;; (after! lsp-clangd (set-lsp-priority! 'clangd 2))

(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        ))

(use-package! company-lsp
  :after lsp-mode
  :config
  (setq company-transformers nil company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp)
  )

(after! flycheck
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode -1)
  )

;; LSP-UI
;;https://github.com/MaskRay/Config
(use-package! lsp-ui
  ;:load-path "~/Dev/Emacs/lsp-ui"
  :commands lsp-ui-mode
  :config
  (setq
   ;; Disable sideline hints
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   ;; Disable imenu
   lsp-ui-imenu-enable nil
   ;; Disable ui-doc (already present in minibuffer)
   lsp-ui-doc-enable nil
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature nil
   lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg)
   ;; Enable ui-peek
   lsp-ui-peek-enable t
   ;lsp-ui-peek-fontify t
   lsp-ui-peek-always-show t
   lsp-ui-peek-force-fontify nil
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs))
   ;; Flycheck
   lsp-ui-flycheck-enable t
   )

  (custom-set-faces
   '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
   '(lsp-face-highlight-read ((t (:background "sea green"))))
   '(lsp-face-highlight-write ((t (:background "brown4"))))
   ;; '(lsp-ui-peek-peek ((t (:background "sea green"))))
   ;; '(lsp-ui-peek-list ((t (:background "deep sky blue"))))
   '(lsp-ui-peek-highlight ((t (:background "deep sky blue"))))
   '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
   '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil)))))

   ;; (map! :after lsp-ui-peek
   ;;       :map lsp-ui-peek-mode-map
   ;;       "h" #'lsp-ui-peek--select-prev-file
   ;;       "j" #'lsp-ui-peek--select-next
   ;;       "k" #'lsp-ui-peek--select-prev
   ;;       "l" #'lsp-ui-peek--select-next-file
   ;;       )

  ;; Slightly modified hydra version of original evil version from:
  ;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/config.el
  (defhydra +mr/lsp-traverse-hydra (:hint nil)
  "Traverse references"
  ("d" lsp-ui-peek-find-definitions "next" :bind nil)
  ("n" (-let [(i . n) (lsp-ui-find-next-reference)]
         (if (> n 0) (message "%d/%d" (+ i 1) n))) "next")
  ("p" (-let [(i . n) (lsp-ui-find-prev-reference)]
         (if (> n 0) (message "%d/%d" (+ i 1) n))) "prev")
  ("R" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 8))]
         (if (> n 0) (message "read %d/%d" (+ i 1) n))) "prev read" :bind nil)
  ("r" (-let [(i . n) (lsp-ui-find-next-reference '(:role 8))]
         (if (> n 0) (message "read %d/%d" (+ i 1) n))) "next read" :bind nil)
  ("W" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 16))]
         (if (> n 0) (message "write %d/%d" (+ i 1) n))) "prev write" :bind nil)
  ("w" (-let [(i . n) (lsp-ui-find-next-reference '(:role 16))]
         (if (> n 0) (message "write %d/%d" (+ i 1) n))) "next write" :bind nil)
  ("q" nil "stop")
  )
)

;; LSP-Mode
(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-auto-guess-root t lsp-eldoc-prefer-signature-help nil)
  (setq lsp-enable-links nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (add-to-list 'lsp-file-watch-ignored "build")
  ;; (setq lsp-project-blacklist '("/CC/"))
  )

;; LSP-Company
(use-package! company-lsp
  ;:load-path "~/Dev/Emacs/company-lsp"
  :after lsp-mode
  :config
  (setq company-transformers nil company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp)
  )

;(set-company-backend! '(c-mode c++-mode)
;  '(company-lsp company-files company-yasnippet))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/MaskRay/ccls/wiki/lsp-mode


;;;###autoload
(defvar +ccls-path-mappings [])

;;;###autoload
(defvar +ccls-initial-blacklist [])

;;;###autoload
(defvar +lsp-blacklist nil)

;;;###autoload
(defvar +my-use-eglot nil)

;;;###autoload
(defun +ccls|enable ()
  (when (and buffer-file-name (--all? (not (string-match-p it buffer-file-name)) +lsp-blacklist))
    (require 'ccls)
    (setq-local lsp-ui-sideline-show-symbol nil)
    (when (string-match-p "/llvm" buffer-file-name)
      (setq-local lsp-enable-file-watchers nil))
    (if +my-use-eglot (call-interactively #'eglot) (lsp))))

(defun +my|toggle-eglot ()
  (interactive)
  (setq +my-use-eglot (not +my-use-eglot))
  (message "use: %s" (if +my-use-eglot "eglot" "lsp-mode")))

(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))


;; References whose filenames are under this project
;(lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))

(use-package! ccls
  ;:load-path "~/Dev/Emacs/emacs-ccls"
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +ccls|enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq
   ccls-initialization-options
   `(:clang
     (:excludeArgs
      ;; Linux's gcc options. See ccls/wiki
      ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
       "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
       "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
      :extraArgs []
      :pathMappings ,+ccls-path-mappings)
     :completion
     (:include
      (:blacklist
       ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
        "^/usr/(local/)?include/c\\+\\+/v1/"
        ]))
     :index (:initialBlacklist ,+ccls-initial-blacklist :parametersInDeclarations :json-false :trackDependency 1)))

  (after! projectile
   (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! company-lsp
;;   :hook (company-mode . company-box-mode)
;;   ;:load-path "~/Dev/Emacs/company-lsp"
;;   :after lsp-mode
;;   :config
;;   (setq company-transformers nil company-lsp-cache-candidates nil)
;;   (set-company-backend! 'lsp-mode 'company-lsp)
;;  )

(use-package company-prescient
    :after company
    :hook (company-mode . company-prescient-mode))

;; (after! company
;;     (setq company-tooltip-limit 5
;;           company-tooltip-minimum-width 80
;;           company-tooltip-minimum 5
;;           company-transformers nil
;;           company-lsp-cache-candidates 1
;;           company-backends
;;           '(company-capf company-dabbrev company-files company-yasnippet)
;;           company-global-modes '(not comint-mode lsp-mode erc-mode message-mode help-mode gud-mode)))

(use-package! company
  :bind (:map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
  ;       ("<S-Tab>" . company-select-previous)
  ;       ("<backtab>" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-limit 5)
  (company-tooltip-minimum 5)
  (company-transformers nil)
  (company-lsp-cache-candidates 1)
  (company-require-match 'never)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
  (company-backends '(company-capf company-files))
  (company-tooltip-minimum-width 80)
  (company-tooltip-maximum-width 60))

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
