;;; private/my-cc/config.el -*- lexical-binding: t; -*-

;;;;; Settings
;; GDB settings
(setq gdb-show-main t
      gdb-many-windows t)

;;;;; Flycheck
(after! (cc-mode flycheck)
  (gagbo--cc-flycheck-setup))

;;;;; Bindings
(map!
 (:after cc-mode
  :map (c-mode-map c++-mode-map)
  (:leader
   :n "=" #'clang-format-region))
 (:after ccls
  :map (c-mode-map c++-mode-map)
  :localleader
  :desc "breakpoint"
  :n "db" #'gagbo/cc-insert-breakpoint))



;;;;; LSP settings
(when (featurep! :tools lsp)
  (setq lsp-clients-clangd-args '("-j=8"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--pch-storage=memory"
                                  "--limit-results=20"
                                  "--cross-file-rename"
                                  "--suggest-missing-includes"
                                  "--completion-style=bundled"
                                  "--compile-commands-dir=build"
                                  "--header-insertion=never")))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;;;;; Packages

;; (use-package! ccls
;;   :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +ccls|enable)
;;   :init
;;   (after! projectile
;;     (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
;;     (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root"))
;;   ;; Avoid using `:after' because it ties the :config below to when `lsp-mode'
;;   ;; loads, rather than `ccls' loads.
;;   (after! lsp-mode (require 'ccls))
;;   :config
;;   (evil-set-initial-state 'ccls-tree-mode 'emacs)
;;   ;; overlay is slow
;;   ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
;;   (setq ccls-sem-highlight-method 'font-lock)
;;   (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
;;   (ccls-use-default-rainbow-sem-highlight)
;;   ;; https://github.com/maskray/ccls/blob/master/src/config.h
;;   (setq
;;    ccls-initialization-options
;;    `(:clang
;;      (:excludeArgs
;;       ;; Linux's gcc options. See ccls/wiki
;;       ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
;;        "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
;;        "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
;;       :extraArgs []
;;       :pathMappings ,+ccls-path-mappings)
;;      :completion
;;      (:include
;;       (:blacklist
;;        ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
;;         "^/usr/(local/)?include/c\\+\\+/v1/"
;;         ]))
;;      :index (:initialBlacklist ,+ccls-initial-blacklist :parametersInDeclarations :json-false :trackDependency 1)))

;;   )

(use-package! clang-format
  :commands (clang-format-region))

(use-package! google-c-style
  :after cc-mode
  :config
  (c-add-style "Google" google-c-style))

(after! cc-mode
  (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

  (font-lock-add-keywords
   'c++-mode '(("\\<\\(\\w+::\\)" . font-lock-constant-face)))

  (after! smartparens
    (sp-local-pair 'c++-mode "(" nil :post-handlers '(:rem ("||\n[i]" "RET")))
    (sp-local-pair 'c++-mode "{" nil :post-handlers '(:rem ("| "      "SPC"))))

  (advice-add 'c-electric-colon :after #'+cc-better-electric-colon-a)

  (setf (alist-get 'c++-mode c-default-style) "Google")

  (setq +cc-default-header-file-mode 'c++-mode))
