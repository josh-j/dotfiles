;;; ~/.doom.d/config.el --- Private Doom Emacs config -*- lexical-binding: t; -*-


;;; Set up load path and host specific configuration
(let ((default-directory (expand-file-name "lisp" doom-private-dir)))
  (normal-top-level-add-subdirs-to-load-path))
(setq load-path (append
                 (mapcar (lambda (folder) (expand-file-name folder doom-private-dir))
                         '("lisp"))
                 load-path))
(load! "local-config")


;;; Theme
(setq doom-theme 'doom-one
      doom-themes-treemacs-theme 'doom-colors
      doom-acario-dark-brighter-modeline t
      doom-acario-light-brighter-modeline t)

(setq current-theme doom-theme ; Use the dark theme first, better to flash dark->light than light->dark
      gagbo-light-theme 'doom-one
      gagbo-dark-theme 'doom-one
      gagbo-light-theme-begin '(00 00 9)
      gagbo-light-theme-end '(00 00 20))

(run-with-timer 0 900 #'gagbo-circadian-theme)

;;;; HL-TODO faces
;; (setq hl-todo-keyword-faces
;;       `(("TODO"  . ,(face-foreground 'warning))
;;         ("FIXME" . ,(face-foreground 'error))
;;         ("NOTE"  . ,(face-foreground 'success))))

;;;; Rainbow mode with overlays
(use-package ov-rainbow-mode
  :commands ov-rainbow-mode)

;;;; Org and Treemacs
(doom-themes-treemacs-config)
(doom-themes-org-config)


;;; Misc
;;;; Window splitting
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;;; Global substitute
(setq evil-ex-substitute-global t)

;;;; Remove iedit message
(setq iedit-toggle-key-default nil)

;;;; Uniquify
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-strip-common-suffix t)

;;;; Which key
(setq which-key-idle-delay 0.3)

;;;; Tabs and final EOL
(setq-default tab-width 8)
(setq require-final-newline t)

;;;; Ligatures
(setq +ligatures-in-modes (when IS-LINUX '(emacs-lisp-mode haskell-mode python-mode)))

;;;; Ophints
(setq evil-goggles-duration 0.2)

;;;; LSP
(set-eglot-client! 'cc-mode '("clangd"))
(setq lsp-headerline-breadcrumb-enable t)
(after! lsp
  (setq
   ;; obsolete, since https://github.com/emacs-lsp/lsp-mode/commit/4796140ef3b3f478725767e777c5af145602fd2e
   ;; but I add it because of module :lang cc
   lsp-enable-semantic-tokens nil
   lsp-progress-via-spinner nil
   lsp-idle-delay 0.5
   lsp-headerline-breadcrumb-enable nil
   lsp-print-performance nil
   lsp-enable-indentation t
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting nil
   lsp-log-io nil))

(setq +lsp-company-backends '(company-capf :with company-yasnippet))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-doc-enable nil
        lsp-ui-doc-max-width 50
        lsp-ui-doc-max-height 5
        lsp-ui-doc-include-signature t
        lsp-ui-doc-header t)

  (add-hook! 'lsp-ui-mode-hook
    (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

;;;;; Flycheck
(after! flycheck
  (setq flycheck-display-errors-delay 0.1))



;;; Non-Doom packages

;;;; Rainbow mode
;; Fontify html hex codes
(use-package rainbow-mode
  :diminish
  :commands rainbow-mode)

;;;; Key Quiz
(use-package! key-quiz
  :commands (key-quiz)
  :init
  (setq key-quiz-matching-regexp "^<?[MCSgz]"))

;;;; Aggressive Indent
(use-package! aggressive-indent
  :commands (aggressive-indent-mode))
;; Add hooks for messy code like (my) elisp and CSS
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)

;;;; Page Break Lines
(use-package! page-break-lines
  :init
  (setq page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode))
  :commands (global-page-break-lines-mode page-break-lines-mode))

(global-page-break-lines-mode)

;;;; Flymake
(after! flymake
  (setq flymake-fringe-indicator-position 'left-fringe
        flymake-suppress-zero-counters t
        flymake-start-on-flymake-mode t
        flymake-no-changes-timeout nil
        flymake-start-on-save-buffer t
        flymake-proc-compilation-prevents-syntax-check t
        flymake-wrap-around nil))

;;;; Fish
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

;;;; company
(after! company
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode org-mode)
        company-idle-delay 0.0
        company-minimum-prefix-length 1
                                        ;company-transformers nil
        company-lsp-async 1
        company-lsp-cache-candidates nil))

;;;; ivy
(after! ivy-posframe (setq ivy-posframe-display-functions-alist
                           '((swiper          . nil)
                             (complete-symbol . ivy-posframe-display-at-point)
                             (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
                             (counsel-rg      . ivy-posframe-display-at-frame-center)
                             (t               . ivy-posframe-display-at-frame-top-center))
                           ivy-posframe-border-width 1
                           ivy-posframe-parameters '((min-width . 90)
                                                     (min-height . 10)
                                                     (left-fringe . 8)
                                                     (right-fringe . 8))))

(after! swiper
  (setq swiper-goto-start-of-match t))

;;;; Helm
(after! helm
  (helm-adaptive-mode 1)
  (setq helm-display-header-line t
        helm-find-files-doc-header " (\\<helm-find-files-map>\\[helm-find-files-up-one-level]: Go up one level)"
        helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
        helm-posframe-border-width 4
        helm-posframe-parameters '((min-width . 90)
                                   (min-height . 15)
                                   (border-width . 4)
                                   (left-fringe . 8)
                                   (right-fringe . 8))
        helm-posframe-poshandler #'posframe-poshandler-frame-top-center)
  ;; (helm-posframe-setup) ; For the posframe goodness
  )

;;;; Info colors
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-mode))

;;;; asciidoc
;; (use-package! adoc-mode
;;   :mode (("\\.asciidoc\\'" . adoc-mode)
;;          ("\\.asciidoc.fr\\'" . adoc-mode)
;;          ("\\.adoc.fr\\'" . adoc-mode)))

;;;; Jinja2
;; (use-package! jinja2-mode
;;   :mode (("\\.j2\\'" . jinja2-mode)
;;          ("\\.jinja\\'" . jinja2-mode)))

;;;; djinni
;; (use-package! djinni-mode
;;   :mode (("\\.djinni\\'" . djinni-mode)))

;;;; elisp
(use-package! flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

;;;; go
(use-package! flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config (setenv "GO111MODULE" "on"))

(after! (go flycheck)
  (gagbo--go-flycheck-setup))

;;;; org
(load! "org-config")

;;;; python
(after! lsp
  (setq lsp-pyls-plugins-pycodestyle-enabled nil ;; Disable to ensure sanity
        lsp-pyls-plugins-pylint-enabled nil ;; Disable to ensure performance
        lsp-pyls-plugins-rope-completion-enabled nil ;; Disable to ensure jedi
        lsp-pyls-configuration-sources ["flake8"]))

(after! (python flycheck)
  (gagbo--python-flycheck-setup))

;;;; javascript (Flow)
                                        ;(use-package company-flow
                                        ;  :after company)
                                        ;(with-eval-after-load 'company
                                        ;  (add-to-list 'company-backends 'company-flow))

                                        ;(use-package flycheck-flow
                                        ;  :after flycheck)
                                        ;(with-eval-after-load 'flycheck
                                        ;  (gagbo--flow-flycheck-setup))

                                        ;(use-package flow-minor-mode
                                        ;  :hook (js2-mode . flow-minor-enable-automatically))

;;;  cc
;;;
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-clang))
;;;; rust
(after! rustic
  (set-formatter! 'rustic-mode #'rustic-cargo-fmt))

(setq rustic-lsp-server 'rust-analyzer
      rustic-format-on-save t
      lsp-rust-server 'rust-analyzer)

(set-popup-rule!
  "^\\*rust"
  :slot -2
  :size 0.45
  :side 'right
  :autosave t
  :quit 'current
  :ttl nil
  :modeline t)

(after! lsp-rust
  (setq lsp-rust-analyzer-lru-capacity 10
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-cargo-load-out-dirs-from-check t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy"))

;;;; imenu-list
(use-package! imenu-list
  :commands imenu-list-smart-toggle)

;;;; Treemacs
(after! treemacs
  (set-popup-rule! "^ \\*Treemacs"
    :side 'left
    :slot 1
    :size 0.30
    :quit nil
    :ttl 0)
  (set-popup-rule! "^\\*LSP Symbols List\\*$"
    :side 'left
    :slot 2
    :size 0.30
    :quit nil
    :ttl 0))

;;;; Debugging
(use-package! realgud-lldb
  :defer t
  :init (add-to-list '+debugger--realgud-alist
                     '(realgud:lldb :modes (c-mode c++-mode rust-mode)
                                    :package realgud-lldb)))
;;;; Pkgbuild
(use-package! pkgbuild-mode
  :defer t
  :init
  (setq pkgbuild-update-sums-on-save nil)
  :config
  (add-hook! 'pkgbuild-mode-hook
    (setq mode-name "PKGBUILD"
          mode-line-process nil)))



;;; Faces
(custom-set-faces!
  `(markdown-code-face
    :background ,(doom-color 'bg-alt))
  `(markdown-markup-face
    :foreground ,(doom-color 'blue))
  '(tree-sitter-hl-face:property :slant oblique))


;;; Miscellaneous popup rules

(set-popup-rules!
  '(("^\\*info\\*"
     :slot 2 :side left :width 83 :quit nil)
    ("^\\*\\(?:Wo\\)?Man "
     :vslot -6 :size 0.45 :select t :quit nil :ttl 0)
    ("^\\*ielm\\*$"
     :vslot 2 :size 0.4 :quit nil :ttl nil)
    ("^\\*Ilist\\*$"
     :slot 2 :side left :size 0.3 :quit nil :ttl nil)
    ;; `help-mode', `helpful-mode'
    ("^\\*[Hh]elp"
     :slot 2 :vslot -8 :size 0.45 :select t)
    ("^\\*Checkdoc Status\\*$"
     :vslot -2 :select ignore :quit t :ttl 0)
    ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
     :slot -2 :size 0.45 :side right :autosave t :quit current :ttl nil
     :modeline t)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*\\(?:Proced\\|timer-list\\|Process List\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :ignore t)
    ("^\\*Flycheck errors\\*$"
     :vslot -2 :select t :quit t :ttl 0)))


;;; Eagerly run first input hook
(run-with-idle-timer 0.2 nil #'doom-first-input-hook-h)


;;; Bindings overrides

(map!
;;;; Window movement
 :n "gsi" #'oi-jump

 (:map evil-treemacs-state-map
  "M-j" #'multi-next-line
  "M-k" #'multi-previous-line)

;;;; :completion selectrum
 :n "M-c" #'embark-act
 (:map minibuffer-local-map
  "C-SPC" #'embark-act-noexit)

 (:leader
;;;; :ui workspace rework
  "bb" #'bufler-switch-buffer
  "bi" #'bufler
  (:prefix-map ("j" . "Bookmarks")
   :desc "Create Bookmark" "c" 'bookmark-map
   :desc "Restore burly bookmark" "b" 'burly-open-bookmark)
  (:prefix-map ("TAB" . "Workspaces")
   :desc "Save the workspaces" "s" 'burly-bookmark-frames
   :desc "Restore workspaces" "g" 'burly-open-bookmark)

;;;; Quit
  (:prefix "q"
   :desc "Restart Emacs"                "r" #'doom/restart
   :desc "Restart & restore Emacs"      "R" #'doom/restart-and-restore
   :desc "Save buffers and kill server" "Q" #'save-buffers-kill-emacs)
;;;; Imenu
  (:prefix "o"
   :desc "Imenu list"                   "i" #'gagbo-imenu-toggle-maybe-lsp)
;;;; Toggles
  (:prefix "t"
   :desc "Read-only mode"               "r" #'read-only-mode
   :desc "Ghost (Transparency)" "G" #'gagbo/toggle-transparency)
;;;; Magit
  (:prefix "g"
   :desc "Magit file dispatch"          "d" #'magit-file-dispatch
   :desc "Create or checkout branch"    "b" #'magit-branch-or-checkout))

;;;; Hydra
 (:leader
  (:when (featurep! :ui hydra)
   (:prefix "w"
    :desc "Interactive menu"    "w" #'+hydra/window-nav/body)
   (:prefix ("z" . "zoom")
    :desc "Text"                "t" #'+hydra/text-zoom/body)))

;;;; Isearch
 (:after isearch
  (:map isearch-mode-map
   [return] #'+isearch-exit-start-of-match
   "RET"    #'+isearch-exit-start-of-match
   "C-RET"  #'isearch-exit))

;;;; Ivy
 (:after ivy
  (:map ivy-minibuffer-map
   [return] #'ivy-alt-done
   "RET"    #'ivy-alt-done))

;;;; Outshine
 (:after outshine
  (:map outshine-mode-map
   "TAB" #'outshine-cycle
   [tab] #'outshine-cycle
   [M-up] nil
   [M-left] nil
   [M-right] nil
   [M-down] nil))

;;;; Company
 (:after company
  (:map company-active-map
   [tab] nil
   "TAB" nil))

;;;; Yasnippet
 (:after yasnippet
  (:map yas-keymap
   [tab] #'yas-next-field
   "TAB" #'yas-next-field))

;;;; fzf
 ;; (map! :leader "SPC" #'fzf-projectile)

;;;; Flycheck
 (:after flycheck
  (:map flycheck-mode-map
   "M-n" #'flycheck-next-error
   "M-p" #'flycheck-previous-error))

;;;; org-jira
 ;; (:after org-jira
 ;;  (:map org-jira-entry-mode-map
 ;;   :localleader
 ;;   (:prefix ("j" . "Jira Integration")
 ;;    (:prefix ("c" . "Comment")
 ;;     :desc "Add comment" "c" #'org-jira-add-comment
 ;;     :desc "Update comment" "u" #'org-jira-update-comment)
 ;;    (:prefix ("i" . "Issue")
 ;;     :desc "Assign issue" "a" #'org-jira-assign-issue
 ;;     :desc "Browse issue" "b" #'org-jira-browse-issue
 ;;     :desc "Create issue" "c" #'org-jira-create-issue
 ;;     :desc "Get issues by fixversion" "f" #'org-jira-get-issues-by-fixversion
 ;;     :desc "Get issues" "g" #'org-jira-get-issues
 ;;     :desc "Get issue heads" "h" #'org-jira-get-issues-headonly
 ;;     :desc "Query issues custom jql" "j" #'org-jira-get-issues-from-custom-jql
 ;;     :desc "Copy issue key" "k" #'org-jira-copy-current-issue-key
 ;;     :desc "Refresh issue" "r" #'org-jira-refresh-issue
 ;;     :desc "Refresh all issues" "R" #'org-jira-refresh-issues-in-buffer
 ;;     :desc "Next step issue" "n" #'org-jira-progress-issue-next
 ;;     :desc "Update issue" "u" #'org-jira-update-issue
 ;;     :desc "Issue workflow" "w" #'org-jira-progress-issue)
 ;;    (:prefix ("s" . "Subtask")
 ;;     :desc "Create subtask" "c" #'org-jira-create-subtask
 ;;     :desc "Get Subtask" "g" #'org-jira-get-subtasks)
 ;;    (:prefix ("p" . "Project")
 ;;     :desc "Get projects" "g" #'org-jira-get-projects)
 ;;    :desc "Send TODO to jira" "t" #'org-jira-todo-to-jira
 ;;    :desc "Update logs from clock" "u" #'org-jira-update-worklogs-from-org-clocks)))

;;;; Languages
;;;;; Rust
 (:after rustic
  (:map rustic-mode-map
   :localleader
   (:prefix ("r" . "Rustic")
    :desc "Clippy pretty"     "C" #'rustic-cargo-clippy
    :desc "Popup"             "r" #'rustic-popup
    :desc "Format everything" "f" #'rustic-cargo-fmt
    :desc "Cargo-outdated"    "u" #'rustic-cargo-outdated)))

;;;;; Python
 (:after python
  (:map python-mode-map
   :localleader
   :desc "Blacken buffer" "cb" #'blacken-buffer)))
