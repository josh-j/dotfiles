;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum)
(package! consult)
(when (featurep! :checkers syntax)
  (package! consult-flycheck))
(package! marginalia)
(package! embark)
(package! ripgrep)
(when (featurep! +prescient)
  (package! selectrum-prescient))
(when (featurep! +orderless)
  (package! orderless))
