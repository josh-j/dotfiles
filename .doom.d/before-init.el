;;; +init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(setq custom-file (expand-file-name "custom.el" doom-private-dir))
(unless (file-exists-p custom-file)
  (write-region ";;; custom.el -*- lexical-binding: t; -*-" nil custom-file t))
(load custom-file)

;; Fix for #2386 until further investigation
(when noninteractive
  (after! undo-tree
    (global-undo-tree-mode -1)))
