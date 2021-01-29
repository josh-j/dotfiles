;;; ~/.config/doom/autoload/ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ivy-partial-or-complete ()
  "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, complete the minibuffer text with the
currently selected candidate."
  ;; FIXME restore candidate after ivy-partial
  (interactive)
  (cond ((ivy-partial))
        ((or (eq this-command last-command)
             (eq ivy--length 1))
         (ivy-insert-current))))
