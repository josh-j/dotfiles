;;; custom.el -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setq projectile-project-test-cmd #'counsel-test-ctest projectile-project-compilation-cmd #'counsel-compile projectile-project-compilation-dir "build" helm-make-build-dir
           (projectile-compilation-dir)
           helm-ctest-dir
           (projectile-compilation-dir))
     (eval setq projectile-project-test-cmd #'helm-ctest projectile-project-compilation-cmd #'helm-make projectile-project-compilation-dir "build" helm-make-build-dir
           (projectile-compilation-dir)
           helm-ctest-dir
           (projectile-compilation-dir))
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (helm-make-args . "-j7")
     (projectile-project-configure-cmd . "cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .")
     (ccls-initialization-options :compilationDatabaseDirectory "build" :cache
                                  (:directory "build/.ccls-cache"))
     (cmake-ide-cmake-opts . "-DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
     (cmake-ide-build-dir . "~/Projects/omega/build")
     (cmake-ide-project-dir . "~/Projects/omega")
     (eval setq projectile-project-test-cmd #'helm-ctest projectile-project-compilation-cmd #'counsel-compile projectile-project-compilation-dir "build" helm-make-build-dir
           (projectile-compilation-dir)
           helm-ctest-dir
           (projectile-compilation-dir))
     (counsel-compile-make-args . "-j7")
     (projectile-project-configure-cmd . "cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S . -B")
     (projectile-project-run-cmd . "make && cd test && ./omega_test")
     (projectile-project-name . "omega")
     (eval setq projectile-project-test-cmd #'counsel-test-ctest projectile-project-compilation-cmd #'counsel-compile projectile-project-compilation-dir "build"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:background nil))))
 '(markdown-markup-face ((t (:foreground nil))))
 '(tree-sitter-hl-face:property ((t (:slant oblique)))))
