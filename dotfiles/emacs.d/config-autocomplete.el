;; auto-complete config
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories (concat config-dir "ac-dict"))
;;(ac-config-default)

;; ac-slime config
; (add-hook 'slime-mode-hook 'set-up-slime-ac)
; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

; (add-to-list 'load-path (concat myoptdir "AC"))
;(require 'auto-complete-config)
; (add-to-list 'ac-dictionary-directories (concat myoptdir "AC/ac-dict"))

;(require 'auto-complete-clang)
(require 'auto-complete-clang-async)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

; (defun my-ac-cc-mode-setup ()
;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(defun my-ac-cc-mode-setup  ()
  (setq ac-clang-complete-executable "~/.emacs.d/vendor/emacs-clang-complete-async-master/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(my-ac-config)