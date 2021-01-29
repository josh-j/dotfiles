;;; ~/.config/doom/autoload/flycheck.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo--go-flycheck-setup ()
  "Setup Flycheck checkers for Golang"
  (flycheck-golangci-lint-setup)
  (cond ((featurep! :tools lsp +eglot) (after! eglot (flycheck-add-next-checker 'eglot 'golangci-lint)))
        ((featurep! :tools lsp) (after! lsp (push 'golangci-lint flycheck-checkers)))))

;;;###autoload
(defun gagbo--python-flycheck-setup ()
  "Setup Flycheck checkers for Python"
  (cond ((featurep! :tools lsp +eglot)
         (after! eglot
           (progn
             (flycheck-add-next-checker 'eglot 'python-pylint)
             (flycheck-add-next-checker 'eglot 'python-mypy))))
        ((featurep! :tools lsp)
         (after! lsp
           (progn
             (push 'python-pylint flycheck-checkers)
             (push 'python-mypy flycheck-checkers))))))

;;;###autoload
(defun gagbo--flow-flycheck-setup ()
  "Setup Flycheck checkers for Flow (javascript)"
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
