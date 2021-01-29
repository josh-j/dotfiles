;;; ~/.config/doom/lisp/+org.el -*- lexical-binding: t; -*-
;; Org specific configuration
;; Note : Do not setq org-directory here, this is a host specific setting and
;; should go in +local_config.el

(defvar gagbo--keep-clock-running nil "Tracks if the clock needs to keep running
when clocking out (to the default organization task typically)")
(defvar gagbo-organization-task-id "6fbc653c-f0e2-458c-bc1f-d0d1895ad5ff"
  "ID of the default organization clocking task. Use `org-id-get-create
to create a new ID for a given task.")

(set-popup-rules!
  '(("^\\*Org.*Export\\*" :side right :size 0.5 :modeline t)))

(after! ox
  (require 'ox-confluence))

(use-package! ox-gfm
  :after org)

(setq org-hugo-front-matter-format "yaml"
      org-roam-directory (file-name-as-directory (concat (file-name-as-directory org-directory) "roam/"))
      org-attach-id-dir (file-name-as-directory (concat (file-name-as-directory org-directory) "data/"))
      org-attach-directory org-attach-id-dir)

(after! org
  (setq
   org-ellipsis " ▼ "
   org-reveal-theme "solarized"

   org-log-done 'time
   org-log-into-drawer t

   org-time-stamp-rounding-minutes '(1 1)

   org-link-mailto-program '(compose-mail "%a" "%s")

   org-use-sub-superscripts "{}"

   org-tags-exclude-from-inheritance '("crypt" "project"))

;;; Workflow states
  (setq
   ;; This setting sets the different workflow states. For the bug
   ;; reports, each project should have its own #+SEQ_TODO property
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)"
               "|"
               "DONE(d)" "CANCELLED(c@/!)" "DEFERRED(f@/!)"
               "PHONE" "MEETING"))
   org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("DEFERRED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))

   ;; The triggers break down to the following rules:

   ;; - Moving a task to =CANCELLED= adds a =CANCELLED= tag
   ;; - Moving a task to =WAITING= adds a =WAITING= tag
   ;; - Moving a task to =HOLD= adds =WAITING= and =HOLD= tags
   ;; - Moving a task to a done state removes =WAITING= and =HOLD= tags
   ;; - Moving a task to =TODO= removes =WAITING=, =CANCELLED=, and =HOLD= tags
   ;; - Moving a task to =NEXT= removes =WAITING=, =CANCELLED=, and =HOLD= tags
   ;; - Moving a task to =DONE= removes =WAITING=, =CANCELLED=, and =HOLD= tags
   org-todo-state-tags-triggers
   '(("CANCELLED" ("cancelled" . t))
     ("WAITING" ("waiting" . t))
     ("HOLD" ("waiting") ("hold" . t))
     (done ("waiting") ("hold"))
     ("TODO" ("waiting") ("cancelled") ("hold"))
     ("NEXT" ("waiting") ("cancelled") ("hold"))
     ("DONE" ("waiting") ("cancelled") ("hold")))

   ;; This settings allows to fixup the state of a todo item without
   ;; triggering notes or log.
   org-treat-S-cursor-todo-selection-as-state-change nil)

;;; Agenda
  (setq
   org-agenda-files `(,(file-name-as-directory org-directory))
   org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org")
   org-default-agenda-file (concat (file-name-as-directory org-directory) "agenda.org")
   org-archive-location (concat (file-name-as-directory org-directory) "archive.org::* Depuis %s")

   org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " ")
     (tags   . " %i %-12:c")
     (search . " %i %-12:c"))
   ;; Warn about deadlines 30 days prior
   ;; To change it for a heavily recurring task, use
   ;;   DEADLINE: <2009-07-01 Wed +1m -0d>
   ;; + => recurrence
   ;; - => deadline warning
   org-deadline-warning-days 30

   ;; Dim blocked tasks
   org-agenda-dim-blocked-tasks t
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   org-agenda-span 'day

   ;; Sorting order for tasks on the agenda
   org-agenda-sorting-strategy
   '((agenda habit-down time-up user-defined-up effort-up category-keep)
     (todo category-up effort-up)
     (tags category-up effort-up)
     (search category-up))

   ;; Start the weekly agenda on Monday
   org-agenda-start-on-weekday 1

   ;; Enable display of the time grid so we can see the marker for the current time
   org-agenda-time-grid '((daily today require-timed)
                          (0800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------")

   ;; Display tags farther right
   org-agenda-tags-column -102

   ;; Use sticky agenda's so they persist
   org-agenda-sticky t)


;;;; Agenda commands
  (setq org-agenda-custom-commands
        '(("b" "Au Bureau" tags-todo "bureau"
           ((org-agenda-overriding-header "Bureau")))
          ("h" "A la maison (home)" tags-todo "maison"
           ((org-agenda-overriding-header "Maison")))
          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))))


;;; Capture templates
  (setq org-capture-templates
        `(("t" "Tâche [inbox]" entry
           (file+headline org-default-notes-file "Tâches")
           "* TODO %i%?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("p" "Projet [inbox]" entry
           (file org-default-notes-file)
           "* %^{Title} [/] %^{CATEGORY}p \n:PROPERTIES:\n:COOKIE_DATA: recursive todo\n:END:\n%?\n** Notes\n** Tâches"
           :clock-in t :clock-resume t)
          ("r" "Réponse à envoyer" entry
           (file org-default-notes-file)
           "* NEXT Répondre à %:from sur %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
          ("n" "note" entry
           (file org-default-notes-file)
           ,(concat "* Note (%a) :note:\n"
                    "/Entered on/ %U\n"
                    "\n" "%?") :clock-in t :clock-resume t)
          ("d" "Rappels" entry
           (file+headline ,(concat (file-name-as-directory org-directory) "rappels.org") "Rappels")
           "* %i %^t%? \n Ajouté le %U")
          ("m" "Meeting" entry  (file+headline org-default-agenda-file "Future")
           (concat "* %? :meeting:\n"
                   "<%<%Y-%m-%d %a %H:00>>") :clock-in t :clock-resume t)
          ("j" "Journal" entry
           (file+olp+datetree ,(concat (file-name-as-directory org-directory) "journal.org"))
           "* %<%H:%M> %^{Title}\n\n%?" :empty-lines 1 :clock-in t :clock-resume t)))

;;; Refile targets
  (setq org-refile-targets `((,(concat (file-name-as-directory org-directory) "projects.org") :regexp . "\\(?:\\(?:Not\\|Tâch\\)es\\)")
                             (,(concat (file-name-as-directory org-directory) "projects.org") :level . 1)))
  (setq org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil)

;;; Default tags

  (setq org-tag-alist '((:startgroup . nil)
                        ("bureau" . ?b) ("maison" . ?h)
                        (:endgroup . nil)
                        ("media" . ?m)
                        ("keyboards" . ?k)))
;;; Clocking
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  (setq
   ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
   org-clock-history-length 23
   ;; Resume clocking task on clock-in if the clock is open
   org-clock-in-resume t
   ;; Change tasks to NEXT when clocking in
   org-clock-in-switch-to-state #'gagbo/clock-in-to-next
   ;; Separate drawers for clocking and logs
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   ;; Save clock data and state changes and notes in the LOGBOOK drawer
   org-clock-into-drawer t
   ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   ;; Cl!ock out when moving task to a done state
   org-clock-out-when-done t
   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
   org-clock-persist t
   ;; Enable auto clock resolution for finding open clocks
   org-clock-auto-clock-resolution 'when-no-clock-is-running
   ;; Include current clocking task in clock reports
   org-clock-report-include-clocking-task t

   org-agenda-clock-consistency-checks
   '(:max-duration "4:00"
     :min-duration 0
     :max-gap 0
     :gap-ok-around ("4:00"))

   ;; Agenda clock report parameters
   org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)

   ;; Set default column view headings: Task Effort Clock_Summary
   org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"

   ;; global Effort estimate values
   ;; global STYLE property values for completion
   org-global-properties
   '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
     ("STYLE_ALL" . "habit"))

   ;; Agenda log mode items to display (closed and state changes by default)
   org-agenda-log-mode-items
   '(closed state))

  (add-hook 'org-clock-out-hook #'gagbo/clock-out-maybe 'append)

  (add-hook 'org-after-todo-state-change-hook #'gagbo--log-todo-next-creation-date-h)

;;; Export and Publishing
  (setq
   org-latex-listings t
   org-table-export-default-format "orgtbl-to-csv"))
