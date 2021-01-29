;;; ~/.config/doom/autoload/themes.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo-circadian-theme ()
  "Sets the theme according to the hour in the current time.

If the hour is (both inclusive) in `gagbo-light-theme-hours' then
`gagbo-light-theme' is loaded, otherwise `gagbo-dark-theme' is loaded.
"
  (interactive)
  (let ((current-time (decode-time)))
    (if (gagbo-timestamp-between-p current-time gagbo-light-theme-begin gagbo-light-theme-end)
        (setq now gagbo-light-theme)
      (setq now gagbo-dark-theme))
    (unless (equal now current-theme)
      (setq current-theme now)
      (load-theme now t))))

;;;###autoload
(defun gagbo/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 80) '(100 . 100)))))
