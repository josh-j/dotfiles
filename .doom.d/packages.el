;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(unpin! t)


;;; Tools
(package! imenu-list)
;; (package! jinja2-mode)
(package! aggressive-indent)
(package! realgud-lldb)
(package! key-quiz)
;; (package! fish-completion)
(package! outshine)
;; (package! evil-escape :disable t)
;; (package! org-jira)


;;; Themes
;; (package! modus-themes)
(package! solaire-mode :disable t)


;;; UI
(package! page-break-lines)
(package! info-colors)


;;; Org
(package! ox-gfm)


;;; Languages
;;;; Golang
(package! flycheck-golangci-lint)
;;;; Python
(package! blacken)
(package! anaconda-mode :disable t)
(package! nose :disable t)
;;;; Djinni
;; (package! djinni-mode)
;;;; Asciidoc
;; (package! adoc-mode)
;;;; Plantuml
;; (package! plantuml-mode)
;;;; Javascript
;; (package! flycheck-flow)
;; (package! company-flow)
;; (package! flow-minor-mode)
;;;; PKGBUILD
(package! pkgbuild-mode)
;;;; Elisp
(package! flycheck-package)
;;;; Rust (Pest mode)
(package! pest-mode)


;; Local workspaces packages
(package! bufler :recipe (:host github :repo "alphapapa/bufler.el"))
(package! burly :recipe (:host github :repo "alphapapa/burly.el"))
