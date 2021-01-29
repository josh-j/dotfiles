;;; ~/.config/doom/autoload/org-clock.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo/clock-in-to-next (current-state)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member current-state (list "TODO"))
           (gagbo/is-task-p))
      "NEXT")
     ((and (member current-state (list "NEXT"))
           (gagbo/is-project-p))
      "TODO"))))

;;;###autoload
(defun gagbo/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;;;###autoload
(defun gagbo/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq gagbo--keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (gagbo/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (gagbo/clock-in-organization-task-as-default)))))

;;;###autoload
(defun gagbo/punch-out ()
  (interactive)
  (setq gagbo--keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

;;;###autoload
(defun gagbo/clock-in-default-task ()
  "Clock in the default task defined in id `gagbo-organization-task-id."
  (interactive)
  (save-excursion
    (org-with-point-at (org-id-find gagbo-organization-task-id 'marker)
      (org-clock-in))))

;;;###autoload
(defun gagbo/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when gagbo--keep-clock-running
            (gagbo/clock-in-default-task)))))))

;;;###autoload
(defun gagbo/clock-in-organization-task-as-default ()
  "Clock in the task defined in id `gagbo-organization-task-id
and mark it as default task."
  (interactive)
  (org-with-point-at (org-id-find gagbo-organization-task-id 'marker)
    (org-clock-in '(16))))

;;;###autoload
(defun gagbo/clock-out-maybe ()
  (when (and gagbo--keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (gagbo/clock-in-parent-task)))
