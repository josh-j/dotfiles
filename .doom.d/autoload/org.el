;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo--log-todo-next-creation-date-h (&rest _)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
