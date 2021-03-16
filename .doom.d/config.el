;;; ~/.doom.d/config.el --- Private Doom Emacs config -*- lexical-binding: t; -*-


;;; Set up load path and host specific configuration
;; (let ((default-directory (expand-file-name "lisp" doom-private-dir)))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (setq load-path (append
;;                  (mapcar (lambda (folder) (expand-file-name folder doom-private-dir))
;;                          '("lisp"))
;;                  load-path))
(load! "local-config")


;;; Theme
(setq doom-theme 'doom-one
      doom-themes-treemacs-theme 'doom-colors
      doom-acario-dark-brighter-modeline t
      doom-acario-light-brighter-modeline t
      subatomic-more-visible-comment-delimiters t)


;;;; Org and Treemacs
(doom-themes-visual-bell-config)
(doom-themes-treemacs-config)
(doom-themes-org-config)


;;;; Smooth-scrolling
(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      scroll-margin 0)


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
(setq-default tab-width 2)
(setq require-final-newline t)

;;;; Ligatures
(setq +ligatures-in-modes (when IS-LINUX '(emacs-lisp-mode haskell-mode python-mode)))

;;;; Ophints
(setq evil-goggles-duration 0.1)
(setq evil-goggles-blocking-duration 0.1)
(setq evil-goggles-async-duration 0.9)
(setq evil-goggles-enable-paste t)
(setq evil-goggles-enable-surround t)
(setq evil-goggles-enable-delete t)
(setq evil-goggles-enable-change t)
(setq evil-goggles-pulse t)
;;;; LSP
;; (after! lsp
;;   (setq
;;    ;; obsolete, since https://github.com/emacs-lsp/lsp-mode/commit/4796140ef3b3f478725767e777c5af145602fd2e
;;    ;; but I add it because of module :lang cc
;;    lsp-enable-semantic-tokens nil
;;    lsp-progress-via-spinner nil
;;    lsp-idle-delay 0.5
;;    lsp-headerline-breadcrumb-enable t
;;    lsp-enable-on-type-formatting t
;;    lsp-print-performance nil
;;    lsp-enable-indentation t
;;    lsp-enable-on-type-formatting nil
;;    lsp-enable-symbol-highlighting nil
;;    lsp-log-io nil))

(setq +lsp-company-backends '(company-capf))
;; ;; (setq +lsp-company-backends '(company-clang))

;; (after! lsp-ui
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-symbol nil
;;         lsp-ui-sideline-show-hover nil
;;         lsp-ui-sideline-show-diagnostics nil
;;         lsp-ui-doc-enable nil
;;         lsp-ui-doc-max-width 50
;;         lsp-ui-doc-max-height 5
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-header t)

;;   (add-hook! 'lsp-ui-mode-hook
;;     (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

;;;;; Flycheck
(after! flycheck
  (setq flycheck-display-errors-delay 0.1))

;;;; Compilation Buffer
(add-hook 'compilation-finish-functions
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

;;; Non-Doom packages

;;;; Page Break Lines
;; (use-package! page-break-lines
;;   :init
;;   (setq page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode))
;;   :commands (global-page-break-lines-mode page-break-lines-mode))

;; (global-page-break-lines-mode)

;; ;;;; Flymake
;; (after! flymake
;;   (setq flymake-fringe-indicator-position 'left-fringe
;;         flymake-suppress-zero-counters t
;;         flymake-start-on-flymake-mode t
;;         flymake-no-changes-timeout nil
;;         flymake-start-on-save-buffer t
;;         flymake-proc-compilation-prevents-syntax-check t
;;         flymake-wrap-around nil))

;;;; Google-C-Style
(use-package! google-c-style
  :after cc-mode
  :config
  (c-add-style "Google" google-c-style)
  (setf (alist-get 'c++-mode c-default-style) "Google"))

;;; company
(after! company
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode org-mode)
        company-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-echo-delay (if (display-graphic-p) nil 0)
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-minimum-prefix-length 2
        company-lsp-async 1
        ;company-transformers '(company-prescient-transformer company-sort-prefer-same-case-prefix company-sort-by-backend-importance)
        company-lsp-cache-candidates nil))


(use-package! company-prescient
  :init (company-prescient-mode 1))


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


;;;; Debugging
;; (use-package! realgud-lldb
;;   :defer t
;;   :init (add-to-list '+debugger--realgud-alist
;;                      '(realgud:lldb :modes (c-mode c++-mode rust-mode)
;;                                     :package realgud-lldb)))


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
     :slot -2 :size 0.15 :side bottom :autosave t :quit t :ttl nil
     :modeline t)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
     :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
    ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
    ("^\\*\\(?:Proced\\|timer-list\\|Process List\\|Abbrevs\\|Output\\|Occur\\|unsent mail\\)\\*" :ignore t)
    ("^\\*Flycheck errors\\*$"
     :vslot -2 :select t :quit t :ttl 0)))


;;; Bindings overrides

(map!
;;;; Window movement
 :n "gh" #'lsp-clangd-find-other-file
)
