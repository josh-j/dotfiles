;;; autoload/cc.el -*- lexical-binding: t; -*-

(defun gagbo/cc-insert-breakpoint ()
  (interactive)
  (evil-open-above 1)
  (insert "volatile static int z=0;while(!z)asm(\"pause\");")
  (evil-normal-state))
