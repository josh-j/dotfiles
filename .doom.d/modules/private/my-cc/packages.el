;; -*- no-byte-compile: t; -*-
;;; private/my-cc/packages.el

(package! clang-format)
(package! cmake-mode :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))
(package! counsel-test)
(package! flycheck-clang-tidy)
;;(package! company-clang :recipe (:host github :repo "company-mode/company-mode" :files ("*.el")))
;; FIXME straight.el doesn't respect repo's default branch
;; https://github.com/raxod502/straight.el/issues/279
(package! google-c-style
  :recipe (:host github :repo "google/styleguide" :branch "gh-pages"))
;;(package! modern-cpp-font-lock)
