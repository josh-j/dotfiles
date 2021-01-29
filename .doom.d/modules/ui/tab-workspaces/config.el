;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(load! "generic")
(load! "bindings")

(defvar +workspaces-switch-project-function #'doom-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

;; We need to find the correct hook to change the bufler workspace 
;; (which is frame-local and not tab-local) to the tab we are choosing.
(defun +workspaces-set-bufler-workspace-a (&rest _)
  "Advice to set the workspace of the frame on tab changes"
  (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
    (if (string-equal tab-name +workspaces-main)
        (bufler-workspace-frame-set)
      (bufler-workspace-frame-set (list tab-name)))))

(advice-add #'tab-bar-select-tab :after #'+workspaces-set-bufler-workspace-a)
(advice-add #'tab-bar-switch-to-tab :after #'+workspaces-set-bufler-workspace-a)

(defun +workspaces-switch-to-or-create (name)
  "Get or create a workspace with name NAME."
  (if-let ((index (tab-bar--tab-index-by-name name)))
      (tab-select (1+ index))
    (+workspaces-create-then-switch-to name)))

(defun +workspaces-create-then-switch-to (name)
  "Create a workspace with name NAME."
  (tab-new)
  ;; FIXME: the "tab-new" did also switch tab, but the tab was nameless then
  ;; (and defaulted to +workspaces-main from tab-bar-tab-name-function) 
  ;; so the tab switching hooks did not properly set the workspace for the frame
  (bufler-workspace-frame-set (list name))
  (tab-rename name))

(defun +doom-tab-bar--tab-name-function ()
  "A Doom specific tab-bar-tab-name-function.
Unless the tab name has been specifically set through `tab-rename', return `+workspaces-main'.
Therefore this function only needs to return the default value."
  +workspaces-main)

(setq tab-bar-tab-name-function #'+doom-tab-bar--tab-name-function)

;; TODO: check that the switch-to-buffer action (when listing all buffers) correctly
;; goes through tabs first, before opening the buffer in the current tab ??

;; Add 'doom-switch-buffer-hook that calls `bufler-workspace-buffer-set
;; so that the buffers interactively opened are put in the workspace
;; The current workspace is `(frame-parameter nil 'bufler-workspace-path)`,
;; but this is actually also the tab name by convention now, and it is easier to fetch
(add-hook! '(doom-switch-buffer-hook server-visit-hook)
  (defun +workspaces-add-current-buffer-h ()
    "Add current buffer to focused workspace."
    (bufler-workspace-buffer-name-workspace (alist-get 'name (tab-bar--current-tab)))))

(defvar +workspaces--project-dir nil)
(defun +workspaces-set-project-action-fn ()
  "A `projectile-switch-project-action' that sets the project directory for
`+workspaces-switch-to-project-h'."
  (setq +workspaces--project-dir default-directory))

;; TODO: properly reuse "main" if it's empty/free
(defun +workspaces-switch-to-project-h (&optional dir)
  (unwind-protect
      (when dir
        (setq +workspaces--project-dir dir))
    (let ((tab-name (doom-project-name +workspaces--project-dir)))
      (+workspaces-switch-to-or-create tab-name)
      (funcall +workspaces-switch-project-function +workspaces--project-dir))
    (setq +workspaces--project-dir nil)))

(setq projectile-switch-project-action #'+workspaces-set-project-action-fn
      counsel-projectile-switch-project-action
      '(1 ("o" +workspaces-switch-to-project-h "open project in new workspace")
          ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
          ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
          ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
          ("D" counsel-projectile-switch-project-action-dired "open project in dired")
          ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
          ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
          ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
          ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
          ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
          ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
          ("C" counsel-projectile-switch-project-action-configure "run project configure command")
          ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
          ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
          ("s" (lambda (project)
                 (let ((projectile-switch-project-action
                        (lambda () (call-interactively #'+ivy/project-search))))
                   (counsel-projectile-switch-project-by-name project))) "search project")
          ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
          ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
          ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
          ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project")))

(use-package burly
  :init
  ;; Add hook to fix https://github.com/alphapapa/burly.el/issues/21
  (add-hook 'after-init-hook #'bookmark-maybe-load-default-file)

  ;; Blacklist a few frame-parameters
  ;; Discussion : https://github.com/alphapapa/burly.el/issues/23
  (setq burly-frameset-filter-alist
        '((name . nil)
          (posframe-parent-buffer . :never)
          (posframe-buffer . :never)))

  ;; Custom code to blacklist childframes from being saved in burly
  ;; Discussion : https://github.com/alphapapa/burly.el/issues/23
  (defun doom--not-childframep (&optional frame)
    "Return t if FRAME is a childframe. If FRAME is `nil', call for current frame."
    (unless (frame-parameter frame 'parent-frame)
      t))

  (defvar doom-burly-frames-filter-predicate #'doom--not-childframep
    "A predicate function to call of frames when saving them")

  (defun doom--burly-bookmark-frames-a (name)
    "Bookmark the current frames as NAME.
Override of `burly-bookmark-frames' to filter frames with `doom-burly-frames-save-predicate'."
    (interactive (let ((bookmark-names (cl-loop for bookmark in bookmark-alist
                                                for (_name . params) = bookmark
                                                when (equal #'burly-bookmark-handler (alist-get 'handler params))
                                                collect (car bookmark))))
                   (list (completing-read "Save Burly bookmark: " bookmark-names nil nil burly-bookmark-prefix))))

    (let ((record (list (cons 'url (burly-frames-url (cl-remove-if-not doom-burly-frames-filter-predicate (frame-list))))
                        (cons 'handler #'burly-bookmark-handler))))
      (bookmark-store name record nil)))

  (advice-add 'burly-bookmark-frames :override #'doom--burly-bookmark-frames-a))

(use-package bufler
  :hook ((after-init . bufler-workspace-mode)) ; Set the frame name to the workspace name
  :init
  (setq tab-bar-show 1)
  :config
  ;; disable tab-{bar,line}-mode in Company childframes
  (after! company-box
    (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0)))

  ;; Set the bufler grouping strategy
  (setf bufler-groups
        (bufler-defgroups
          ;; Grouping the named workspace first means that interactively
          ;; opening a special shared buffer like *Messages* will steal the
          ;; buffer from everyone else. Therefore all special-mode buffers are
          ;; handled first.

          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))

          ;; TODO: some special buffers should not fall into this category,
          ;; like compilation buffers or interpreter buffers
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*")))

           ;; Subgroup collecting these "special special" buffers
           ;; separately for convenience.
           (group
            (name-match "**Special**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))

           ;; TODO: Magit buffers should get to a project workspace before being put
           ;; in a magit catchall category
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by directory.
            (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
            (auto-directory))

           ;; Subgroup for Helm buffers.
           (mode-match "*Helm*" (rx bos "helm-"))

           ;; Remaining special buffers are grouped automatically by mode.
           (auto-mode))

          ;; Subgroup collecting all named workspaces.
          (group
           (auto-workspace))

          ;; NOTE: Past this line, we enter fallback territory
          (group
           ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
           ;; `org-directory' is not yet defined).
           (dir (if (bound-and-true-p org-directory)
                    org-directory
                  "~/org"))
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by file.
            ;; This is very useful when used with `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          (group
           ;; Subgroup collecting buffers in a projectile project.
           (auto-projectile))
          (group
           ;; Subgroup collecting buffers in a version-control project,
           ;; grouping them by directory.
           (auto-project))
          ;; All buffers under "~/.emacs.d" (or wherever it is).
          (dir user-emacs-directory)
          ;; All buffers under "~/.doom.d" (or wherever it is).
          (dir doom-private-dir)
          ;; Group remaining buffers by directory, then major mode.
          (auto-directory)
          (auto-mode))))
