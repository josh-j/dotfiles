(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    auto-complete
    buffer-move
    color-theme
    expand-region
    feature-mode
    highlight-symbol
    ruby-mode
    scss-mode
    starter-kit
    starter-kit-bindings
    starter-kit-eshell
    starter-kit-js
    starter-kit-lisp
    starter-kit-ruby
    magit
    markdown-mode
    marmalade
    window-number
    yasnippet
    ;; themes
    color-theme-sanityinc-tomorrow
    solarized-theme
    zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Adds all first level `parent-dir' subdirs to the
Emacs load path."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
     (not (equal f ".."))
     (not (equal f ".")))
       (add-to-list 'load-path name)))))

(add-to-list 'load-path "~/.emacs.d/vendor")
(prelude-add-subfolders-to-load-path "~/.emacs.d/vendor")

;; Configuration root
(setq config-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path config-dir)

;; setup OS X path
(if (eq system-type 'darwin)
    (setenv "PATH" (concat
                    (shell-command-to-string "/bin/zsh -l -c 'echo -n $PATH'")
                    ":" (getenv "HOME") "/bin")))

;; let emacs use git
(push "/usr/local/git/bin" exec-path)

;; auto follow symlinked files
(setq vc-follow-symlinks t)

;; Emacs is fast for OSX again!
(setq font-lock-verbose nil)

;; load more config files
(load "config-autocomplete")
;(load "config-autocomplete-clang")
(load "config-bindings")
(load "config-cosmetics")
(load "config-defuns")
(load "config-highlight-symbol")
(load "config-hooks")
(load "config-isearch")
(load "config-ispell")
(load "config-osx")
(load "config-registers")
(load "config-workarounds")
(load "config-yasnippet")

;; load major mode configs
(load "config-markdown-mode")
(load "config-ruby-mode")
(load "config-sh-mode")
(load "config-window-number-mode")

;; Put auto-save files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/auto-save-files.
;; (custom-set-variables
;;   '(auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-files/\\1" t)))
;;   '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/auto-save-files/" t)))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/auto-save-files/" t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(custom-safe-themes (quote ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(fci-rule-color "#2a2a2a"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
