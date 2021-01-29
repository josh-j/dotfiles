;;; private/transient/config.el -*- lexical-binding: t; -*-


(use-package! hercules
  :config
  (hercules-def
   :toggle-funs #'my-buffer-hercules
   :keymap 'doom-leader-buffer-map
   :transient t)

  (hercules-def
   :toggle-funs #'my-window-hercules
   :keymap 'evil-window-map
   :transient t))

(define-key doom-leader-map (kbd "bb") #'my-buffer-hercules)
(define-key doom-leader-map (kbd "ww") #'my-window-hercules)
