;;; ~/.config/doom/+local_config_example.el -*- lexical-binding: t; -*-
;; Place host specific configuration

(setq user-full-name "Josh Johnson"
      user-mail-address "joshj.tx@gmail.com" ; Use the email address linked to the GPG Key on this host
      epa-file-encrypt-to user-mail-address
      conda-env-home-directory "/home/jdoe/.conda")


(setq calendar-latitude 48.85341
      calendar-longitude 2.3488)
;;; UI
;; Ignore errors if the fonts aren't found.
;;(ignore-errors
  (setq doom-font (font-spec :family "Iosevka" :weight 'normal :slant 'normal :width 'expanded :size 14)
        doom-big-font (font-spec :family "JetBrains Mono" :size 26)
        doom-variable-pitch-font (font-spec :family "JetBrains Mono" :height 0.5)
        doom-serif-font (font-spec :family "Noto Sans" :height 1.0))

(setq doom-modeline-height 1)
;;(set-face-attribute 'mode-line nil :family "Iosevka Term Regular" :size 10) ;;smaller font in modeling
;;(set-face-attribute 'mode-line-inactive nil :family "Iosevka Term Regular" :size 10)

(setq +pretty-code-fira-font-name "Fira Code Symbol"
      +pretty-code-hasklig-font-name "Hasklig"
      +pretty-code-iosevka-font-name "Iosevka")

;; (setq doom-line-numbers-style 'relative)

;;;; Frames/Windows
;;;;; Maximize window
(add-hook 'window-setup-hook #'toggle-frame-maximized)
;;;;; Fringe
(setq-default fringe-mode 4)
(set-fringe-mode 4)
;;;;; Title
(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " ● %s" project-name))))))
;; Mac stuff
(when IS-MAC
  ;;  (setq mac-command-modifier 'meta)
  ;;  (setq mac-option-modifier 'none)
  (setq mac-function-modifier 'control
        mac-control-modifier 'control ;;'AlT
        mac-command-modifier 'meta;;'control
        mac-option-modifier 'alt;;'alt
        mac-right-command-modifier 'meta
        mac-right-control-modifier 'control
        mac-right-option-modifier 'alt)
  ;; Make mouse wheel / trackpad scrolling less jerky
                                        ;(setq mouse-wheel-scroll-amount '(1
                                        ;                                  ((shift) . 5)
                                        ;                                  ((control))))
                                        ;(dolist (multiple '("" "double-" "triple-"))
                                        ;  (dolist (direction '("right" "left"))
                                        ;    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

;;; Org
(setq org-directory (file-name-as-directory "~/Projects/org/"))
(setq org-plantuml-jar-path "~/path/to/plantuml.jar")

;;; Garbage Collection
(setq garbage-collection-messages t)  ; For debugging
(setq gcmh-high-cons-threshold (* 1024 1024 3))

;;; LSP trickery
(setq read-process-output-max (* 1024 1024)) ; 1 MiB >> 4KB
(setq lsp-file-watch-threshold 50)
;; (setq +lua-lsp-dir "/home/jdoe/soft/lua-language-server/")
;; (setq lsp-clients-lua-language-server-install-dir "/home/jdoe/soft/lua-language-server")
