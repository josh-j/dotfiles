;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom
;; quickstart' will do this for you). The `doom!' block below controls what
;; modules are enabled and in what order they will be loaded. Remember to run
;; 'doom refresh' after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(load! "before-init")

(doom! :input
       ;; (layout +bepo)
       :completion
       (company                         ; the ultimate code completion backend
        +childframe)
       (selectrum +orderless)
       ;; (helm +fuzzy)
       ;; (ivy
       ;;  +icons
       ;;  +fuzzy)

       :ui
       doom                   ; what makes DOOM look the way it does
       doom-dashboard         ; a nifty splash screen for Emacs
       doom-quit              ; DOOM quit-message prompts when you quit Emacs
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       nav-flash              ; blink the current line after jumping
       ophints                ; highlight the region an operation acts on
       popup                  ; tame sudden yet inevitable temporary windows
       ligatures              ; replace bits of code with pretty symbols
       ;; (modeline +light)
       (telephone-line
        +minions
        +keycast)                    ; PRIVATE another modeline implementation
       treemacs                      ; a project drawer, like neotree but cooler
       ;; unicode                ; extended unicode support for various languages
       vc-gutter              ; vcs diff in the fringe
       vi-tilde-fringe        ; fringe tildes to mark beyond EOB
       window-select          ; visually switch windows
       ;; tab-workspaces             ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)        ; come to the dark side, we have cookies
       file-templates            ; auto-snippets for empty files
       fold                      ; (nigh) universal code folding
       format                    ; automated prettiness
       ;;lispy                     ; vim for lisp, for people who dont like vim
       multiple-cursors          ; editing in many places at once
       rotate-text               ; cycle region at point between text candidates
       snippets                  ; my elves. They type so I don't have to
       word-wrap                 ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)        ; making dired pretty colorful icons for dired-mode
       electric              ; smarter, keyword-based electric-indent
       vc                    ; version-control and Emacs, sitting in a tree
       (undo +tree)
       (ibuffer +icons)

       :term
                                        ;eshell                         ; a consistent, cross-platform shell (WIP)
                                        ;vterm

       :checkers
       (syntax +childframe)          ; tasing you for every semicolon you forget
       (spell +aspell)               ; tasing you for misspelling mispelling

       :tools
       debugger              ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig                ; let someone else argue about tabs vs spaces
       (eval +overlay)             ; run code, run (also, repls)
       fzf
       (lookup +docsets) ; helps you navigate your code and documentation or in Dash docsets locally
       lsp
       magit                            ; a git porcelain for Emacs
       pdf                              ; pdf enhancements
       rgb                              ; creating color strings
                                        ;tmux                             ; an API for interacting with tmux
       tree-sitter                      ; PRIVATE syntax-tree at speed of light

       :os
       ;; tty

       :lang
                                        ;beancount                        ; PRIVATE accounting tool
       (cc +lsp)                        ; C/C++/Obj-C madness
       data                             ; config/data formats
       emacs-lisp                       ; drown in parentheses
                                        ;(go +lsp)                 ; the hipster dialect
       (haskell +lsp)            ; a language that's lazier than I am
                                        ;(javascript +lsp)         ; all(hope(abandon(ye(who(enter(here))))))
                                        ;latex                     ; writing papers in Emacs has never been so fun
                                        ;ledger                    ; an accounting system in Emacs
       ;; (lua                      ; one-based indices? one-based indices
       ;;  +fennel
       ;;  +lsp)
                                        ;markdown              ; writing docs for people to ignore
                                        ;nix                   ; I hereby declare "nix geht mehr!"
       ;; ocaml                 ; Cocorico vive l'INRIA
       (org                  ; organize your plain life in plain text
        +hugo                ; use Emacs for hugo blogging
        +pandoc              ; export-with-pandoc support
        +present)            ; Emacs for presentations
       ;; plantuml              ; diagrams for confusing people more
       ;;                                  ;powershell            ; PRIVATE Look through the windows ; it's a shell !
       ;;                                  ;(python +conda +lsp +pyright)  ; beautiful is better than ugly
       ;; ;; rest                  ; Emacs as a REST client
       ;; ;; rst
       ;;                                  ; ReST in peace
       ;;                                  ;(rust +lsp)           ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;                                  ;(scala +lsp)          ; java, but good
       ;;                                  ;scheme                ; a fully conniving family of lisps
       ;; (sh +fish)
                                        ; she sells {ba,z,fi}sh shells on the C xor
       ;; solidity              ; do you need a blockchain? No.
       ;; web                              ; the tubes
       ;; yaml
                                        ; All the configs

       :config
       (default +bindings)
       smartparens                      ; PRIVATE smartparens configuration

       :private
       my-cc                        ; PRIVATE configuration for C-like languages
       )
