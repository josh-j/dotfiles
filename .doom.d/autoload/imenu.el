;;; ~/.config/doom/autoload/imenu.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo-imenu-toggle-maybe-lsp ()
  "Toggles an imenu list popup. It uses lsp-ui if it is enabled."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-imenu)
    (imenu-list-smart-toggle)))
