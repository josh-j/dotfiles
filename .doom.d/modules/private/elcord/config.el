;;; private/elcord/config.el -*- lexical-binding: t; -*-

(use-package! elcord
  :hook (after-init . elcord-mode)
  :custom (elcord-display-buffer-details nil))
