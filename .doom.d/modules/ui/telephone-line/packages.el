;; -*- no-byte-compile: t; -*-
;;; ui/telephone-line/packages.el

(when (featurep! +minions)
  (package! minions))
(when (featurep! +keycast)
  (package! keycast))
(package! anzu)
(package! evil-anzu)
(package! telephone-line)
(package! flycheck-indicator)
